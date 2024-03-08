# libraries
library(nimble)
library(tidyverse)
library(ggplot2)
library(readxl)
library(patchwork)
library(coda)
library(dplyr)
library(httr)
library(jsonlite)
library(geosphere)

# data wrangling
# pull in the data from Excel sheets and CSVs as needed

# get sheet names
data_path <- "Data/Hanson2016_CitiesDatabase_OxREP.xlsx"
sheets <- excel_sheets(data_path)
sheets

# pull in raw Excel data
Cities <- read_excel(data_path, 
                    sheet = sheets[1])


# add in pop estimates

pop_data <- read.csv("Data/population_size.csv", 
                    head = T, 
                    as.is = T)

pop_data$city <- str_to_sentence(pop_data$city)

Cities <- left_join(Cities,
                pop_data,
                by = join_by('Ancient Toponym' == 'city'))

Areas <- read_excel(data_path, 
                    sheet = sheets[2])

Monuments <- read_excel(data_path, 
                    sheet = sheets[3])

names(Areas)[3] <- "Basis"

# largest dataset, no filtering
Cities_Areas <- left_join(Cities, 
                        Areas, 
                        by = 'Primary Key')

# now filtered, only cities with walls and non-wall above
# ground monumental constructions (i.e., exluding the following)
not_monumental <- c("Walls|walls|Agora|Forum|grid")
not_mon_idx <- grep(not_monumental, Monuments$Structure)

monument_counts_all <- table(Monuments[, 1])
monument_counts_filt <- table(Monuments[-c(not_mon_idx), 1])

MonumentCount_all <- data.frame(Cities = names(monument_counts_all),
                            Monuments = as.vector(monument_counts_all))

names(MonumentCount_all) <- c("Primary Key", "Monuments")

MonumentCount_filt <- data.frame(Cities = names(monument_counts_filt),
                            Monuments = as.vector(monument_counts_filt))

names(MonumentCount_filt) <- c("Primary Key", "Monuments_filt")

# look at only counting temples

temples_idx <- grep("temple|Temple", Monuments$Structure)

temple_counts <- table(Monuments[temples_idx, 1])

TempleCount <- data.frame(Cities = names(temple_counts),
                        Monuments = as.vector(temple_counts))

names(TempleCount) <- c("Primary Key", "Temples")

# everything but the temples

nottemple_counts <- table(Monuments[-temples_idx, 1])

NotTempleCount <- data.frame(Cities = names(nottemple_counts),
                        Monuments = as.vector(nottemple_counts))

names(NotTempleCount) <- c("Primary Key", "NotTemples")


RomanUrban <- left_join(Cities_Areas, 
                        MonumentCount_all, 
                        by = 'Primary Key')

RomanUrban <- left_join(RomanUrban, 
                        MonumentCount_filt,
                        by = 'Primary Key')

RomanUrban <- left_join(RomanUrban,
                        TempleCount,
                        by = 'Primary Key')

RomanUrban <- left_join(RomanUrban,
                        NotTempleCount,
                        by = 'Primary Key')

# isolate only relevant columns for further analyses
#col_idx <- grep("Primary\ Key|Area|Basis|pop_est|Monuments|Monuments_filt|Temples|NotTemples|Province", 
#                colnames(RomanUrban))

# isolate only relevant columns for further analyses
col_idx <- grep("Primary\ Key|Area|Basis|pop_est|Monuments|Monuments_filt|Province|Longitude|Latitude", 
                colnames(RomanUrban))

RomanUrban <- RomanUrban[, col_idx]
names(RomanUrban)[1] <- "City"

# We add an index variable column for refering to provinces in the model/analyses below
 
RomanUrban$ProvinceIdx = as.numeric(as.factor(RomanUrban$Province))

#isolate only case where we have area estimates
RomanUrban <- subset(RomanUrban, !is.na(Area))

# removing cases with zero monuments because these create problems for log-log models
# there are only 34 such cases in the set containg urban centers with Area estimates
RomanUrban <- subset(RomanUrban, !is.na(Monuments))

# identify cases where Area is determined by walls
walls_idx <- grep("Walls|walls", 
                RomanUrban$Basis)

# get sample size
n_roman <- dim(RomanUrban)[1]
n_roman

# dealing with the population--area relationship linking function...
# quick check with simple glm
glm_pop <- glm(log(area_ha) ~ log(pop_est), data = pop_data)
glm_pop_coefs <- summary(glm_pop)$coefficients
pop_link_sd = sd(residuals(glm_pop))

# Now use a Bayesian approach to simultaneously estimate missing
# population sizes from the available data and then use the 
# 'emprical' and estimated population sizes in a log-log model
# to estimate the scaling parameters given different subsets
# of the data.

# after deciding to look for possible differences between/among the different Roman provinces,
# I've added an index variable that allows for the mean scaling coefficient to vary 
# by province alond with the intercept ('pre-factor'). Just for simplicity,
# I've used a new variable 'v' to refer to a vector of Provinces (coded as integers)
# that will be included in the Nimble model as 'data nodes'.

# set up a Nimble model
scalingCode <- nimbleCode({
    # monument scaling params
    intercept0 ~ dnorm(mean = 0, sd = 100)
    sd0 ~ dunif(1e-7, 100)
    scaling0 ~ dnorm(mean = 0, sd = 100)
    sd1 ~ dunif(1e-7, 100)
    for(k in 1:K){
        intercept[k] ~ dnorm(mean = intercept0, sd = sd0)
        scaling[k] ~ dnorm(mean = scaling0, sd = sd1)
    }
    sigma ~ dunif(1e-07, 100)
    # population--area linking params
    b0 ~ dnorm(mean = 0, sd = 100)
    b1 ~ dnorm(mean = 0, sd = 100)
    sigma_pop ~ dunif(1e-07, 100)
    # main model
    for(n in 1:N){
        pop_mu[n] <- b0 + b1 * x[n]
        pop[n] ~ dnorm(mean = pop_mu[n], sd = sigma_pop)
        mu[n] <- intercept[v[n]] + scaling[v[n]] * pop[n]
        y[n] ~ dnorm(mean = mu[n], sd = sigma)
        #y_hat[n] ~ dnorm(mean = mu[n], sd = sigma)
    }
})

# get row indeces for different data subsets/filters excluding 
# NAs because these will be logged count data (NA here means the raw
# count was zero). The MCMC would treat them as missing otherwise,
# and in such circumstances a decision always has to be made regarding
# how to handle 0's in data that are later log-transformed.

# temples only
temple_idx <- which(!is.na(RomanUrban$Temples))
# above-ground monuments only
filt_idx <- which(!is.na(RomanUrban$Monuments_filt))
# 
not_temples_idx <- which(!is.na(RomanUrban$NotTemples))

# set common mcmc params
niter <- 1000000
nburnin <- 5000
thin <- 10

# run the first analysis: all monuments
N <- dim(RomanUrban[,])[1]
y <- log(RomanUrban[,]$Monuments)
x <- log(RomanUrban[,]$Area)
pop <- log(RomanUrban[,]$pop_est)
v <- RomanUrban[,]$ProvinceIdx
K <- length(unique(RomanUrban$ProvinceIdx))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, N),
            intercept = rep(0, N),
            intercept0 = 0,
            scaling0 = 0,
            sd0 = 1,
            sd1 = 1,
            b0 = 0,
            b1 = 0,
            sigma = 1,
            sigma_pop = 1)

scalingModel <- nimbleModel(code = scalingCode,
                name = "urbanscaling",
                constants = Consts,
                data = Data,
                inits = Inits)

params_to_track <- c("intercept0", 
                    "scaling0", 
                    "sigma", 
                    "b0", 
                    "b1", 
                    #"pop", 
                    "sigma_pop",
                    "mu")

mcmc_out <- nimbleMCMC(model = scalingModel, 
            monitors = params_to_track, thin = thin,
            niter = niter, nburnin = nburnin)

# estimate r-squared for compatibility with previous research

# function for calculating it
rsquared <- function(y_hat, y){
       rsq <- 1 - ( sum( (y - y_hat)^2 ) / sum( (y - mean(y))^2 ))
       return(rsq)
}

# isolate the columns from mcmc output containing samples of the 
# mean prediction for the log-log model (y_hat)
mu_idx <- grep("mu",colnames(mcmc_out))

# calculate r-squared
rsq <- apply(mcmc_out[, mu_idx], 1, rsquared, y = y)

# summarize and save
rsq_summary <- data.frame(analysis = "allmonuments", rsq = round(mean(rsq), 2))
write.table(rsq_summary, 
        file="Output/rsquared.csv",
        row.names = F,
        sep = ",")

mcmc_out <- mcmc_out[, -mu_idx]

# convergence check with Geweke diagnostic
convergence <- geweke.diag(mcmc_out)$z
convergence <- t(c("allmonuments", convergence))
colnames(convergence)[1] <- "analysis"
write.table(convergence, 
        file="Output/geweke.csv",
        row.names = F,
        sep = ",")

# add iteration index to chain matrix for plotting
iter <- seq(nburnin + 1, niter, thin)

mcmc_out <- cbind(iter, mcmc_out)

# chain traceplots
long_mcmc <- pivot_longer(as.data.frame(mcmc_out), 
                names_to = "param",
                values_to = "sample",
                cols = 2:dim(mcmc_out)[2])

tplot <- ggplot(long_mcmc) +
            geom_line(mapping = aes(x = iter, y = sample)) +
            facet_grid(param ~ ., scale = "free")
tplot

ggsave(filename = "Output/tplots_allmonuments.pdf", 
        plot = tplot, 
        device = "pdf")

# summarize the results
posterior_summary <- as.data.frame(HPDinterval(mcmc(mcmc_out[,-1])))
posterior_summary$mean <- apply(mcmc_out[,-1], 2, mean)
posterior_summary$stdd <- apply(mcmc_out[,-1], 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_allmonuments.csv")

#####################################################################
#
#
#
# run the second analysis: all monuments, areas defined by walls only
N <- dim(RomanUrban[walls_idx,])[1]
y <- log(RomanUrban[walls_idx,]$Monuments)
x <- log(RomanUrban[walls_idx,]$Area)
pop <- log(RomanUrban[walls_idx,]$pop_est)
v <- RomanUrban[,]$ProvinceIdx
K <- length(unique(RomanUrban$ProvinceIdx))

#Consts <- list(N = N)
#
#Data <- list(y = y,
#            x = x,
#            pop = pop)

#Inits <- list(scaling = 0,
#            b0 = 0,
#            b1 = 0,
#            intercept = 0,
#            sigma = 1,
#            sigma_pop = 1)

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, N),
            intercept = rep(0, N),
            intercept0 = 0,
            scaling0 = 0,
            sd0 = 1,
            sd1 = 1,
            b0 = 0,
            b1 = 0,
            sigma = 1,
            sigma_pop = 1)

scalingModel <- nimbleModel(code = scalingCode,
                name = "urbanscaling",
                constants = Consts,
                data = Data,
                inits = Inits)

params_to_track <- c("intercept0", 
                    "scaling0", 
                    "sigma", 
                    "b0", 
                    "b1", 
                    #"pop", 
                    "sigma_pop",
                    "mu")

mcmc_out <- nimbleMCMC(model = scalingModel, 
            monitors = params_to_track, thin = thin,
            niter = niter, nburnin = nburnin)

# isolate the columns from mcmc output containing samples of the 
# mean prediction for the log-log model (y_hat)
mu_idx <- grep("mu",colnames(mcmc_out))

# calculate r-squared
rsq <- apply(mcmc_out[, mu_idx], 1, rsquared, y = y)

# summarize and save
rsq_summary <- data.frame(analysis = "allwalls", rsq = round(mean(rsq), 2))
write.table(rsq_summary, 
        file="Output/rsquared.csv",
        row.names = F,
        col.names = F,
        append = T,
        sep = ",")

mcmc_out <- mcmc_out[, -mu_idx]

# convergence check with Geweke diagnostic
convergence <- geweke.diag(mcmc_out)$z
convergence <- t(c("allwalls", convergence))
colnames(convergence)[1] <- "analysis"
write.table(convergence, 
        file="Output/geweke.csv",
        row.names = F,
        append = T,
        sep = ",")

# add iteration index to chain matrix for plotting
iter <- seq(nburnin + 1, niter, thin)

mcmc_out <- cbind(iter, mcmc_out)

# chain traceplots
long_mcmc <- pivot_longer(as.data.frame(mcmc_out), 
                names_to = "param",
                values_to = "sample",
                cols = 2:dim(mcmc_out)[2])

tplot <- ggplot(long_mcmc) +
            geom_line(mapping = aes(x = iter, y = sample)) +
            facet_grid(param ~ ., scale = "free")
tplot

ggsave(filename = "Output/tplots_all_walls.pdf", 
        plot = tplot, 
        device = "pdf")

# summarize the results
posterior_summary <- as.data.frame(HPDinterval(mcmc(mcmc_out[,-1])))
posterior_summary$mean <- apply(mcmc_out[,-1], 2, mean)
posterior_summary$stdd <- apply(mcmc_out[,-1], 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_all_walls.csv")

#####################################################################
#
#
#
# third analysis: filtered monuments (ie, above-ground only)
N <- dim(RomanUrban[filt_idx,])[1]
y <- log(RomanUrban[filt_idx,]$Monuments_filt)
x <- log(RomanUrban[filt_idx,]$Area)
pop <- log(RomanUrban[filt_idx,]$pop_est)
v <- RomanUrban[,]$ProvinceIdx
K <- length(unique(RomanUrban$ProvinceIdx))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, N),
            intercept = rep(0, N),
            intercept0 = 0,
            scaling0 = 0,
            sd0 = 1,
            sd1 = 1,
            b0 = 0,
            b1 = 0,
            sigma = 1,
            sigma_pop = 1)

scalingModel <- nimbleModel(code = scalingCode,
                name = "urbanscaling",
                constants = Consts,
                data = Data,
                inits = Inits)

params_to_track <- c("intercept0", 
                    "scaling0", 
                    "sigma", 
                    "b0", 
                    "b1", 
                    #"pop", 
                    "sigma_pop",
                    "mu")

mcmc_out <- nimbleMCMC(model = scalingModel, 
            monitors = params_to_track, thin = thin,
            niter = niter, nburnin = nburnin)

# isolate the columns from mcmc output containing samples of the 
# mean prediction for the log-log model (y_hat)
mu_idx <- grep("mu",colnames(mcmc_out))

# calculate r-squared
rsq <- apply(mcmc_out[, mu_idx], 1, rsquared, y = y)

# summarize and save
rsq_summary <- data.frame(analysis = "filtmonuments", rsq = round(mean(rsq), 2))
write.table(rsq_summary, 
        file="Output/rsquared.csv",
        row.names = F,
        col.names = F,
        append = T,
        sep = ",")

mcmc_out <- mcmc_out[, -mu_idx]

# summarize
# convergence check with Geweke diagnostic
convergence <- geweke.diag(mcmc_out)$z
convergence <- t(c("filtmonuments", convergence))
colnames(convergence)[1] <- "analysis"
write.table(convergence, 
        file="Output/geweke.csv",
        row.names = F,
        col.names = F,
        append = T,
        sep = ",")

# add iteration index to chain matrix for plotting
iter <- seq(nburnin + 1, niter, thin)

mcmc_out <- cbind(iter, mcmc_out)

# chain traceplots
long_mcmc <- pivot_longer(as.data.frame(mcmc_out), 
                names_to = "param",
                values_to = "sample",
                cols = 2:dim(mcmc_out)[2])

tplot <- ggplot(long_mcmc) +
            geom_line(mapping = aes(x = iter, y = sample)) +
            facet_grid(param ~ ., scale = "free")
tplot

ggsave(filename = "Output/tplots_filtmonuments.pdf", 
        plot = tplot, 
        device = "pdf")

# summarize the results
posterior_summary <- as.data.frame(HPDinterval(mcmc(mcmc_out[,-1])))
posterior_summary$mean <- apply(mcmc_out[,-1], 2, mean)
posterior_summary$stdd <- apply(mcmc_out[,-1], 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_filtmonuments.csv")

# fourth analysis: temples only
N <- dim(RomanUrban[temple_idx,])[1]
y <- log(RomanUrban[temple_idx,]$Temples)
x <- log(RomanUrban[temple_idx,]$Area)
pop <- log(RomanUrban[temple_idx,]$pop_est)

Consts <- list(N = N)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = 0,
            b0 = 0,
            b1 = 0,
            intercept = 0,
            sigma = 1,
            sigma_pop = 1)

scalingModel <- nimbleModel(code = scalingCode,
                name = "urbanscaling",
                constants = Consts,
                data = Data,
                inits = Inits)

params_to_track <- c("intercept", 
                    "scaling", 
                    "sigma", 
                    "b0", 
                    "b1", 
                    #"pop", 
                    "sigma_pop",
                    "mu")

mcmc_out <- nimbleMCMC(model = scalingModel, 
            monitors = params_to_track, thin = thin,
            niter = niter, nburnin = nburnin)

# isolate the columns from mcmc output containing samples of the 
# mean prediction for the log-log model (y_hat)
mu_idx <- grep("mu",colnames(mcmc_out))

# calculate r-squared
rsq <- apply(mcmc_out[, mu_idx], 1, rsquared, y = y)

# summarize and save
rsq_summary <- data.frame(analysis = "temples", rsq = round(mean(rsq), 2))
write.table(rsq_summary, 
        file="Output/rsquared.csv",
        row.names = F,
        col.names = F,
        append = T,
        sep = ",")

mcmc_out <- mcmc_out[, -mu_idx]

# summarize
# convergence check with Geweke diagnostic
convergence <- geweke.diag(mcmc_out)$z
convergence <- t(c("temples", convergence))
colnames(convergence)[1] <- "analysis"
write.table(convergence, 
        file="Output/geweke.csv",
        row.names = F,
        col.names = F,
        append = T,
        sep = ",")

# add iteration index to chain matrix for plotting
iter <- seq(nburnin + 1, niter, thin)

mcmc_out <- cbind(iter, mcmc_out)

# chain traceplots
long_mcmc <- pivot_longer(as.data.frame(mcmc_out), 
                names_to = "param",
                values_to = "sample",
                cols = 2:dim(mcmc_out)[2])

tplot <- ggplot(long_mcmc) +
            geom_line(mapping = aes(x = iter, y = sample)) +
            facet_grid(param ~ ., scale = "free")
tplot

ggsave(filename = "Output/tplots_temples.pdf", 
        plot = tplot, 
        device = "pdf")

# summarize the results
posterior_summary <- as.data.frame(HPDinterval(mcmc(mcmc_out[,-1])))
posterior_summary$mean <- apply(mcmc_out[,-1], 2, mean)
posterior_summary$stdd <- apply(mcmc_out[,-1], 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_temples.csv")

# urban wealth
data_path <- "Data/hnwi_by_city.xlsx"
sheets <- excel_sheets(data_path)
sheets

# pull in raw Excel data
global_hnwi <- read_excel(data_path, 
                    sheet = sheets[1])
# remove duplicates in country names column
pattern <- "(.+?)\\1+"
global_hnwi$Country_cleaned <- sub(pattern, "\\1", global_hnwi$Country)

# manually change country name entry "Hong Kong (SAR China)" to "Hong Kong" for consistency with other database 
# similar revisions were made following manual checking for consistency between the two dataframe naming
# conventions (see below)
row_idx <- grep("Hong Kong", global_hnwi$Country_cleaned)
global_hnwi[row_idx, "Country_cleaned"] <- "Hong Kong"
row_idx <- grep("UK", global_hnwi$Country_cleaned)
global_hnwi[row_idx, "Country_cleaned"] <- "United Kingdom"
row_idx <- grep("UAE", global_hnwi$Country_cleaned)
global_hnwi[row_idx, "Country_cleaned"] <- "United Arab Emirates"
row_idx <- grep("Russian Federation", global_hnwi$Country_cleaned)
global_hnwi[row_idx, "Country_cleaned"] <- "Russia"
row_idx <- grep("Türkiye", global_hnwi$Country_cleaned)
global_hnwi[row_idx, "Country_cleaned"] <- "Turkey"
row_idx <- grep("The Bay Area", global_hnwi$City)
global_hnwi[row_idx, "City"] <- "San Francisco"
row_idx <- grep("St. Petersburg", global_hnwi$City)
global_hnwi[row_idx, "City"] <- "Saint Petersburg"
row_idx <- grep("Herzliya", global_hnwi$City)
global_hnwi[row_idx, "City"] <- "Herzliyya"
row_idx <- grep("Bengaluru", global_hnwi$City)
global_hnwi[row_idx, "City"] <- "Bangalore"
row_idx <- grep("Santa Barbara", global_hnwi$City)
global_hnwi[row_idx, "City"] <- "Santa Barbara"
row_idx <- grep("Dar Es Salaam", global_hnwi$City)
global_hnwi[row_idx, "City"] <- "Dar es Salaam"

global_hnwi$population <- NA
global_hnwi$check_me <- 0

# pull in population data from https://simplemaps.com/data/world-cities
global_pop_by_city <- as_tibble(read.csv("./Data/worldcities.csv"))

global_pop_by_city[grep("Moscow", global_pop_by_city$city_ascii),]

# slow, manual matching because merge wasn't working
# make a new column for searching country names/iso variables all at once
global_pop_by_city$country_labels <- apply(global_pop_by_city, 1, function(x)paste(x[5], x[6], x[7], sep = " "))

for(j in 1:dim(global_hnwi)[1]){
        city <- global_hnwi[j, "City"]
        country <- global_hnwi[j, "Country_cleaned"]
        city_matches <- global_pop_by_city[grep(city, global_pop_by_city$city_ascii), ]
        city_and_country_matches <- grep(country, city_matches$country_labels)
        # just take the top one but alert with a flag in a column
        if(length(city_and_country_matches) > 1){
                global_hnwi[j, "check_me"] <- 1
        }
        global_hnwi[j, "population"] <- city_matches[city_and_country_matches[1], "population"]
}

# still several matches failed and population sizes remain NA
# these need to be checked manually
# The following lines were used to check the rows with NA population and then changes were made 
# to city name spellings in the global_hnwi dataframe in order to make them consistent with the 
# population dataframe spellings. This resolved a number of NA cases, but there were ultimately
# 5 that remained, three of which had no billionaires and so would be excluded from the
# analysis for that reason anyway.

na_pop <- which(is.na(global_hnwi[,"population"]))

global_hnwi[na_pop, ]

global_pop_by_city[grep("Cape", global_pop_by_city$city_ascii),]

global_pop_by_city[grep("Israel", global_pop_by_city$country),]

# with this cleaned/updated data, run the same Bayesian model

scalingCode2 <- nimbleCode({
    # monument scaling params
    intercept ~ dnorm(mean = 0, sd = 100)
    scaling ~ dnorm(mean = 0, sd = 100)
    sigma ~ dunif(1e-07, 100)
    # main model
    for(n in 1:N){
        mu[n] <- intercept + scaling * pop[n]
        y[n] ~ dnorm(mean = mu[n], sd = sigma)
        #y_hat[n] ~ dnorm(mean = mu[n], sd = sigma)
    }
})


# remove rows from the HNWI dataframe with no pop values and no billionaires
remove_rows_idx <- which(is.na(global_hnwi$population) | is.na(global_hnwi$Billionaires))

N <- dim(global_hnwi[-remove_rows_idx,])[1]
y <- log(global_hnwi[-remove_rows_idx,]$Billionaires)
pop <- log(global_hnwi[-remove_rows_idx,]$population)

Consts <- list(N = N)

Data <- list(y = y,
            pop = pop)

Inits <- list(scaling = 0,
            intercept = 0,
            sigma = 1)

scalingModel2 <- nimbleModel(code = scalingCode2,
                name = "urbanscaling2",
                constants = Consts,
                data = Data,
                inits = Inits)

params_to_track <- c("intercept", 
                    "scaling", 
                    "sigma",
                    "mu")

niter <- 1000000
nburnin <- 5000
thin <- 10

mcmc_out <- nimbleMCMC(model = scalingModel2, 
            monitors = params_to_track, thin = thin,
            niter = niter, nburnin = nburnin)

# isolate the columns from mcmc output containing samples of the 
# mean prediction for the log-log model (y_hat)
mu_idx <- grep("mu",colnames(mcmc_out))

# calculate r-squared
rsq <- apply(mcmc_out[, mu_idx], 1, rsquared, y = y)

# summarize and save
rsq_summary <- data.frame(analysis = "hnwi", rsq = round(mean(rsq), 2))
write.table(rsq_summary, 
        file="Output/rsquared.csv",
        row.names = F,
        col.names = F,
        append = T,
        sep = ",")

mcmc_out <- mcmc_out[, -mu_idx]

# summarize
# convergence check with Geweke diagnostic
convergence <- geweke.diag(mcmc_out)$z
convergence <- t(c("hnwi", convergence))
colnames(convergence)[1] <- "analysis"
write.table(convergence, 
        file="Output/geweke_hnwi.csv",
        row.names = F,
        col.names = F,
        sep = ",")

# add iteration index to chain matrix for plotting
iter <- seq(nburnin + 1, niter, thin)

mcmc_out <- cbind(iter, mcmc_out)

# chain traceplots
long_mcmc <- pivot_longer(as.data.frame(mcmc_out), 
                names_to = "param",
                values_to = "sample",
                cols = 2:dim(mcmc_out)[2])

tplot <- ggplot(long_mcmc) +
            geom_line(mapping = aes(x = iter, y = sample)) +
            facet_grid(param ~ ., scale = "free")
tplot

ggsave(filename = "Output/tplots_hnwi.pdf", 
        plot = tplot, 
        device = "pdf")

# summarize the results
posterior_summary <- as.data.frame(HPDinterval(mcmc(mcmc_out[,-1])))
posterior_summary$mean <- apply(mcmc_out[,-1], 2, mean)
posterior_summary$stdd <- apply(mcmc_out[,-1], 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_hnwi.csv")


### Epigraphic Analysis
## checking for consistency when looking at an epigaphic record database and isolating the 
## instances of epigraphic monument/building dedications 



# Set the URL
url <- 'https://zenodo.org/record/4888168/files/EDH_text_cleaned_2021-01-21.json'

# Make the HTTP request and read the content
response <- GET(url)
content <- content(response, "text", encoding = "UTF-8")

# Parse the JSON content to a dataframe
EDH <- fromJSON(content, flatten = TRUE)

# extract relevant inscription types
inscriptions <- "building/dedicatory inscription|
                building/dedicatory inscription?"

# View the dataframe
inscriptions_idx <- grep(inscriptions, EDH$type_of_inscription)

EDH_buildings <- EDH[inscriptions_idx,]

valid_coords_idx <- unlist(lapply(EDH_buildings$coordinates, function(x)length(unlist(x))))
valid_coords_idx[which(valid_coords_idx == 2)] <- 1

EDH_buildings_cleaned <- EDH_buildings[valid_coords_idx==1,]

# Convert 'coordinates' column from list of vectors to two separate columns
coordinates <- do.call(rbind, EDH_buildings_cleaned$coordinates)

EDH_buildings_cleaned$Longitude <- coordinates[, 1]
EDH_buildings_cleaned$Latitude <- coordinates[, 2]

RomanUrban$Radius <- sqrt(RomanUrban$Area / pi)



# Initialize a column for inscription counts
RomanUrban$InscriptionCount <- 0

epigraphic_points <- matrix(c(EDH_buildings_cleaned$Longitude, EDH_buildings_cleaned$Latitude), ncol = 2)

for(i in 1:dim(RomanUrban)[1]) {
  city_coords <- c(RomanUrban$`Longitude (X)`[i], RomanUrban$`Latitude (Y)`[i])
  radius <- RomanUrban$Radius[i] * 1000 # Assuming radius needs to be in meters for distHaversine
  
  # Calculate distances from city to each inscription
  distances <- distHaversine(p1 = city_coords, p2 = epigraphic_points)#apply(epigraphic_points, 1, distHaversine, p2=city_coords)
  #print(distances)
  # Count how many inscriptions fall within the city's radius
  RomanUrban$InscriptionCount[i] <- sum(distances <= radius)
}

df_subset <- subset(RomanUrban, InscriptionCount > 0)
summary(glm(log(InscriptionCount) ~ log(Area), data = df_subset))

# Scaling simplified
# set up a Nimble model
scalingCodeSimple <- nimbleCode({
    # monument scaling params
    intercept ~ dnorm(mean = 0, sd = 100)
    scaling ~ dnorm(mean = 0, sd = 100)
    sigma ~ dunif(1e-07, 100)
    # population--area linking params
    b0 ~ dnorm(mean = 0, sd = 100)
    b1 ~ dnorm(mean = 0, sd = 100)
    sigma_pop ~ dunif(1e-07, 100)
    # main model
    for(n in 1:N){
        pop_mu[n] <- b0 + b1 * x[n]
        pop[n] ~ dnorm(mean = pop_mu[n], sd = sigma_pop)
        mu[n] <- intercept + scaling * pop[n]
        y[n] ~ dnorm(mean = mu[n], sd = sigma)
        #y_hat[n] ~ dnorm(mean = mu[n], sd = sigma)
    }
})

# set common mcmc params
niter <- 1000000
nburnin <- 5000
thin <- 10

# run the first analysis: all monuments
N <- dim(df_subset[,])[1]
y <- log(df_subset[,]$InscriptionCount)
x <- log(df_subset[,]$Area)
pop <- log(df_subset[,]$pop_est)

Consts <- list(N = N)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = 0,
            b0 = 0,
            b1 = 0,
            intercept = 0,
            sigma = 1,
            sigma_pop = 1)

scalingModel <- nimbleModel(code = scalingCodeSimple,
                name = "urbanscaling",
                constants = Consts,
                data = Data,
                inits = Inits)

params_to_track <- c("intercept", 
                    "scaling", 
                    "sigma", 
                    "b0", 
                    "b1", 
                    #"pop", 
                    "sigma_pop",
                    "mu")

mcmc_out <- nimbleMCMC(model = scalingModel, 
            monitors = params_to_track, thin = thin,
            niter = niter, nburnin = nburnin)

# isolate the columns from mcmc output containing samples of the 
# mean prediction for the log-log model (y_hat)
mu_idx <- grep("mu",colnames(mcmc_out))

# calculate r-squared
rsq <- apply(mcmc_out[, mu_idx], 1, rsquared, y = y)

# summarize and save
rsq_summary <- data.frame(analysis = "epigraphy", rsq = round(mean(rsq), 2))
write.table(rsq_summary, 
        file="Output/rsquared.csv",
        row.names = F,
        col.names = F,
        append = T,
        sep = ",")

mcmc_out <- mcmc_out[, -mu_idx]

# summarize
# convergence check with Geweke diagnostic
convergence <- geweke.diag(mcmc_out)$z
convergence <- t(c("epig", convergence))
colnames(convergence)[1] <- "analysis"
write.table(convergence, 
        file="Output/geweke_epig.csv",
        row.names = F,
        col.names = F,
        sep = ",")

# add iteration index to chain matrix for plotting
iter <- seq(nburnin + 1, niter, thin)

mcmc_out <- cbind(iter, mcmc_out)

# chain traceplots
long_mcmc <- pivot_longer(as.data.frame(mcmc_out), 
                names_to = "param",
                values_to = "sample",
                cols = 2:dim(mcmc_out)[2])

tplot <- ggplot(long_mcmc) +
            geom_line(mapping = aes(x = iter, y = sample)) +
            facet_grid(param ~ ., scale = "free")
tplot

ggsave(filename = "Output/tplots_epig.pdf", 
        plot = tplot, 
        device = "pdf")


# summarize the results
posterior_summary <- as.data.frame(HPDinterval(mcmc(mcmc_out[,])))
posterior_summary$mean <- apply(mcmc_out[,], 2, mean)
posterior_summary$stdd <- apply(mcmc_out[,], 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_epigraphy.csv")