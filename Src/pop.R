# libraries
library(nimble)
library(tidyverse)
library(ggplot2)
#library(ggpubr)
library(readxl)
library(patchwork)
library(coda)
library(dplyr)

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
col_idx <- grep("Primary\ Key|Area|Basis|pop_est|Monuments|Monuments_filt|Temples|NotTemples", 
                colnames(RomanUrban))

RomanUrban <- RomanUrban[, col_idx]
names(RomanUrban)[1] <- "City"

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

# set up a Nimble model
scalingCode <- nimbleCode({
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
            niter = 100000, nburnin = nburnin)

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

# run the second analysis: all monuments, areas defined by walls only
N <- dim(RomanUrban[walls_idx,])[1]
y <- log(RomanUrban[walls_idx,]$Monuments)
x <- log(RomanUrban[walls_idx,]$Area)
pop <- log(RomanUrban[walls_idx,]$pop_est)

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
rsq_summary <- data.frame(analysis = "allwalls", rsq = round(mean(rsq), 2))
write.table(rsq_summary, 
        file="Output/rsquared.csv",
        row.names = F,
        col.names = F,
        append = T,
        sep = ",")

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

# third analysis: filtered monuments (ie, above-ground only)
N <- dim(RomanUrban[filt_idx,])[1]
y <- log(RomanUrban[filt_idx,]$Monuments_filt)
x <- log(RomanUrban[filt_idx,]$Area)
pop <- log(RomanUrban[filt_idx,]$pop_est)

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
rsq_summary <- data.frame(analysis = "filtmonuments", rsq = round(mean(rsq), 2))
write.table(rsq_summary, 
        file="Output/rsquared.csv",
        row.names = F,
        col.names = F,
        append = T,
        sep = ",")

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

# now the UK Churches data

coe_churches <- read.csv("./Data/churches.csv")
# get sheet names
data_path <- "./Data/nomis_2023_08_14_102454.xlsx"
sheets <- excel_sheets(data_path)
uk_pop <- read_excel(data_path, 
                    sheet = sheets[1],
                    skip = 6)
names(uk_pop)[2] <- "population_2020"
coe_churches <- left_join(coe_churches,
                uk_pop,
                by = join_by('TCITY15NM' == 'major town and city'))

# find 0 count for churches---these will be excluded
no_churches_idx <- which(coe_churches$churches == 0)

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

N <- dim(coe_churches[-no_churches_idx,])[1]
y <- log(coe_churches[-no_churches_idx,]$churches)
pop <- log(coe_churches[-no_churches_idx,]$population_2020)

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
rsq_summary <- data.frame(analysis = "churches", rsq = round(mean(rsq), 2))
write.table(rsq_summary, 
        file="Output/rsquared.csv",
        row.names = F,
        col.names = F,
        append = T,
        sep = ",")

# summarize
# convergence check with Geweke diagnostic
convergence <- geweke.diag(mcmc_out)$z
convergence <- t(c("coe", convergence))
colnames(convergence)[1] <- "analysis"
write.table(convergence, 
        file="Output/geweke_coe.csv",
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

ggsave(filename = "Output/tplots_coe.pdf", 
        plot = tplot, 
        device = "pdf")

# summarize the results
posterior_summary <- as.data.frame(HPDinterval(mcmc(mcmc_out[,-1])))
posterior_summary$mean <- apply(mcmc_out[,-1], 2, mean)
posterior_summary$stdd <- apply(mcmc_out[,-1], 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_coe.csv")

# scatter plots
plt1 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = log(Area), y = log(Monuments))) +
                labs(title = "All Monuments") +
                theme_minimal()
plt2 <- ggplot(RomanUrban[walls_idx, ]) +
                geom_point(mapping = aes(x = log(Area), y = log(Monuments))) +
                labs(title = "All Monuments (walls only)") +
                theme_minimal()
plt3 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = log(Area), y = log(Monuments_filt))) +
                labs(title = "Above-groud Monuments") +
                theme_minimal()
plt4 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = log(Area), y = log(Temples))) +
                labs(title = "Temples") +
                theme_minimal()
plt5 <- ggplot(coe_churches) +
                geom_point(mapping = aes(x = log(area), y = log(churches))) +
                labs(title = "Churches (UK)") +
                theme_minimal()
plt1 + plt2 + plt3 + plt4 + plt5 + plot_layout(ncol = 3, byrow = TRUE)

ggsave("Output/point_scatters.pdf",
        height = 15,
        width = 15,
        units = "cm",
        scale = 1.5,
        device = "pdf")