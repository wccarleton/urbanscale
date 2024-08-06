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
stop(paste("WTF YOU DOING?"))
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


RomanUrban <- left_join(Cities_Areas, 
                        MonumentCount_all, 
                        by = 'Primary Key')

RomanUrban <- left_join(RomanUrban, 
                        MonumentCount_filt,
                        by = 'Primary Key')

# isolate only relevant columns for further analyses
col_idx <- grep("Primary\ Key|Area|Basis|pop_est|Monuments|Monuments_filt|Province|Longitude|Latitude", 
                colnames(RomanUrban))

RomanUrban <- RomanUrban[, col_idx]
names(RomanUrban)[1] <- "City"

#isolate only case where we have area estimates
RomanUrban <- subset(RomanUrban, !is.na(Area))

# removing cases with zero monuments because these create problems for log-log models
# there are only 34 such cases in the set containg urban centers with Area estimates
#RomanUrban <- subset(RomanUrban, !is.na(Monuments))

# set momument NAs to 0
RomanUrban[which(is.na(RomanUrban$Monuments)),"Monuments"] <- 0
RomanUrban[which(is.na(RomanUrban$Monuments)),"Monuments_filt"] <- 0

# identify cases where Area is determined by walls
walls_idx <- grep("Walls|walls", 
                RomanUrban$Basis)

# get sample size
n_roman <- dim(RomanUrban)[1]
n_roman

# dealing with the population--area relationship linking function...
# quick check with simple glm
glm_pop <- glm(log(pop_est)~log(area_ha), data = pop_data)
glm_pop_coefs <- summary(glm_pop)$coefficients
pop_link_sd = sd(residuals(glm_pop))

# Now use a Bayesian approach to simultaneously estimate missing
# population sizes from the available data and then use the 
# 'emprical' and estimated population sizes in a log-log model
# to estimate the scaling parameters given different subsets
# of the data.

# after deciding to look for possible differences between/among the different Roman provinces,
# I've added an index variable that allows for the mean scaling coefficient to vary 
# by province along with the intercept ('pre-factor'). Just for simplicity,
# I've used a new variable 'v' to refer to a vector of Provinces (coded as integers)
# that will be included in the Nimble model as 'data nodes'.

# REVISED
# In response to reviewer comments, we adapted the model to use a count/integer
# distribution, which involves some tranformations between logged and non-logged
# versions of certain parameters in order to arrive at a model equivalent to
# the original log-log Gaussian model. This was done as an alternative
# to censoring the zero-count data. Since the count data appear in each case below
# to be overdispersed, we opted for the Negative-Binomial distribution.

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

    # Hyperpriors for size
    shape_hyper ~ dgamma(2, 1)  # Hyperprior for the shape of the gamma distribution
    rate_hyper ~ dgamma(2, 1)   # Hyperprior for the rate of the gamma distribution
    
    # Hierarchical prior for the size parameter
    for(k in 1:K) {
        size[k] ~ dgamma(shape = shape_hyper, rate = rate_hyper)
    }

    # population--area linking params
    b0 ~ dnorm(mean = 0, sd = 100)
    b1 ~ dnorm(mean = 0, sd = 100)
    sigma_pop ~ dunif(1e-07, 100)

    # main model
    for(n in 1:N){
        pop_mu[n] <- b0 + b1 * x[n]
        pop[n] ~ dnorm(mean = pop_mu[n], sd = sigma_pop)
        log_mu[n] <- intercept[v[n]] + scaling[v[n]] * pop[n]
        mu[n] <- exp(log_mu[n])
        y[n] ~ dnegbin(prob = size[v[n]] / (size[v[n]] + mu[n]), size = size[v[n]])
    }
})

# set common mcmc params
niter <- 10000#500000
nburnin <- 5000
thin <- 10

#################### FIRST ANALYSIS: ALL MONUMENTS #####################

N <- dim(RomanUrban[,])[1]
y <- RomanUrban[,]$Monuments
x <- log(RomanUrban[,]$Area)
pop <- log(RomanUrban[,]$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(RomanUrban[,]$Province))
K <- length(unique(RomanUrban[,]$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, K),
            intercept = rep(0, K),
            intercept0 = 0,
            scaling0 = 0,
            size = rep(1, K),
            shape_hyper = 2,
            size_hyper = 1,
            sd0 = 1,
            sd1 = 1,
            b0 = 0,
            b1 = 0,
            sigma_pop = 1)

# given the additional complexity of the new model we
# needed to employ more sophisticated samplers in oder 
# to produce well-mixed mcmc chains

# Create the Nimble model
scalingModel <- nimbleModel(code = scalingCode,
                            name = "allmonuments",
                            constants = Consts,
                            data = Data,
                            inits = Inits)

# Compile the model
compiled_model <- compileNimble(scalingModel)

# Configure the MCMC
mcmc_config <- configureMCMC(scalingModel)

# Replace samplers for correlated parameters
# Block sampling for intercept0 and scaling0
mcmc_config$removeSamplers(c('intercept0', 'scaling0'))
mcmc_config$addSampler(target = c('intercept0', 'scaling0'), type = 'AF_slice')

# Block sampling for b0 and b1
mcmc_config$removeSamplers(c('b0', 'b1'))
mcmc_config$addSampler(target = c('b0', 'b1'), type = 'AF_slice')

# Optionally, block sampling for each group's intercept and scaling
for (k in 1:Consts$K) {
  mcmc_config$removeSamplers(c(paste0('intercept[', k, ']'), paste0('scaling[', k, ']')))
  mcmc_config$addSampler(target = c(paste0('intercept[', k, ']'), paste0('scaling[', k, ']')), type = 'AF_slice')
}

# Select parameters to track
params_to_track <- c("intercept0",
                "scaling0",
                "scaling", 
                "intercept",
                "b0", 
                "b1")
mcmc_config$monitors <- params_to_track

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

# Run the MCMC
mcmc_out <- runMCMC(compiled_mcmc, 
                niter = niter, 
                nburnin = nburnin, 
                thin = thin, 
                nchains = 1, 
                samplesAsCodaMCMC = TRUE)

# Function to create trace plots with mode switch for stacked or faceted visualization
stacked_traceplot <- function(mcmc_obj, params, mode = "facet") {
  # Check if the input is an mcmc.list or a single mcmc object
  if (is.mcmc.list(mcmc_obj)) {
    # For mcmc.list, convert each chain to a data frame and combine
    mcmc_df <- do.call(rbind, lapply(mcmc_obj, as.data.frame))
    # Add iteration numbers for each chain
    mcmc_df$Iteration <- rep(1:nrow(mcmc_obj[[1]]), times = length(mcmc_obj))
    # Add a Chain identifier
    mcmc_df$Chain <- rep(seq_along(mcmc_obj), each = nrow(mcmc_obj[[1]]))
  } else if (is.mcmc(mcmc_obj)) {
    # For single mcmc objects, convert directly to a data frame
    mcmc_df <- as.data.frame(mcmc_obj)
    # Add iteration numbers
    mcmc_df$Iteration <- 1:nrow(mcmc_df)
    # Single chain case, so add Chain identifier as 1
    mcmc_df$Chain <- 1
  } else {
    stop("Input object must be of class 'mcmc' or 'mcmc.list'")
  }
  
  # Check if all specified parameters exist in the data
  missing_params <- setdiff(params, colnames(mcmc_df))
  if (length(missing_params) > 0) {
    stop("The following parameters are missing in the MCMC output: ", paste(missing_params, collapse = ", "))
  }

  # Transform data to long format for specified parameters
  long_mcmc <- pivot_longer(mcmc_df, 
                            cols = all_of(params), 
                            names_to = "Parameter", 
                            values_to = "Sample")

  # Plotting based on mode selection
  if (mode == "facet") {
    # Facet mode: separate panels for each parameter
    plot <- ggplot(long_mcmc, aes(x = Iteration, y = Sample, color = factor(Chain))) +
      geom_line(alpha = 0.7) +
      facet_grid(Parameter ~ ., scales = "free_y") +
      labs(title = "Trace Plots for Selected Parameters (Facet Mode)",
           x = "Iteration",
           y = "Sample Value",
           color = "Chain") +
      theme_minimal() +
      theme(legend.position = "bottom")
  } else if (mode == "stacked") {
    # Stacked mode: all traces in the same plot space
    plot <- ggplot(long_mcmc, aes(x = Iteration, y = Sample, color = Parameter, group = interaction(Parameter, Chain))) +
      geom_line(alpha = 0.7) +
      labs(title = "Trace Plots for Selected Parameters (Stacked Mode)",
           x = "Iteration",
           y = "Sample Value",
           color = "Parameter") +
      theme_minimal() +
      theme(legend.position = "bottom")
  } else {
    stop("Invalid mode. Choose 'facet' or 'stacked'.")
  }

  return(plot)
}

# Trace plots for key parameters
top_lvl_param_names <- c("intercept0", "scaling0", "b0", "b1")
tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

ggsave(filename = "Output/tplots_allmonuments.pdf", 
        plot = tplot, 
        device = "pdf")

# and now to look at variability among provinces
province_scaling_names <- colnames(mcmc_out)[grep("scaling\\[", colnames(mcmc_out))]
tplot <- stacked_traceplot(mcmc_out, province_scaling_names, mode = "stacked")

ggsave(filename = "Output/tplots_allmonuments_provinces.pdf", 
        plot = tplot, 
        device = "pdf")

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

# summarize the results
posterior_summary <- as.data.frame(HPDinterval(mcmc_out, prob = 0.99))
posterior_summary$mean <- apply(mcmc_out, 2, mean)
posterior_summary$stdd <- apply(mcmc_out, 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_allmonuments.csv")

#################### SECOND AMALYSIS: WALLED ONLY #####################

N <- dim(RomanUrban[walls_idx,])[1]
y <- RomanUrban[walls_idx,]$Monuments
x <- log(RomanUrban[walls_idx,]$Area)
pop <- log(RomanUrban[walls_idx,]$pop_est)
# get provinces as integers again
v <- as.numeric(as.factor(RomanUrban[walls_idx,]$Province))
K <- length(unique(RomanUrban[walls_idx,]$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, K),
            intercept = rep(0, K),
            intercept0 = 0,
            scaling0 = 0,
            size = rep(1, K),
            shape_hyper = 2,
            size_hyper = 1,
            sd0 = 1,
            sd1 = 1,
            b0 = 0,
            b1 = 0,
            sigma_pop = 1)

# Create the Nimble model
scalingModel <- nimbleModel(code = scalingCode,
                            name = "allwalls",
                            constants = Consts,
                            data = Data,
                            inits = Inits)

# Compile the model
compiled_model <- compileNimble(scalingModel)

# Configure the MCMC
mcmc_config <- configureMCMC(scalingModel)

# Replace samplers for correlated parameters
# Block sampling for intercept0 and scaling0
mcmc_config$removeSamplers(c('intercept0', 'scaling0'))
mcmc_config$addSampler(target = c('intercept0', 'scaling0'), type = 'AF_slice')

# Block sampling for b0 and b1
mcmc_config$removeSamplers(c('b0', 'b1'))
mcmc_config$addSampler(target = c('b0', 'b1'), type = 'AF_slice')

# Optionally, block sampling for each group's intercept and scaling
for (k in 1:Consts$K) {
  mcmc_config$removeSamplers(c(paste0('intercept[', k, ']'), paste0('scaling[', k, ']')))
  mcmc_config$addSampler(target = c(paste0('intercept[', k, ']'), paste0('scaling[', k, ']')), type = 'AF_slice')
}

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

# Run the MCMC
mcmc_out <- runMCMC(compiled_mcmc, 
                niter = niter, 
                nburnin = nburnin, 
                thin = thin, 
                nchains = 1, 
                samplesAsCodaMCMC = TRUE)

# trace plots for key parameters
top_lvl_param_names <- c("intercept0", "scaling0", "b0", "b1")
stacked_traceplot(mcmc_out, top_lvl_param_names)

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
posterior_summary <- as.data.frame(HPDinterval(mcmc(mcmc_out[,-1]), prob = 0.99))
posterior_summary$mean <- apply(mcmc_out[,-1], 2, mean)
posterior_summary$stdd <- apply(mcmc_out[,-1], 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_all_walls.csv")

#################### THIRD ANALYSIS: ABOVE GROUND ONLY #####################

filt_idx <- which(!is.na(RomanUrban$Monuments_filt))

N <- dim(RomanUrban[filt_idx,])[1]
y <- RomanUrban[filt_idx,]$Monuments_filt
x <- log(RomanUrban[filt_idx,]$Area)
pop <- log(RomanUrban[filt_idx,]$pop_est)
v <- RomanUrban[filt_idx,]$ProvinceIdx
K <- length(unique(RomanUrban[,]$ProvinceIdx))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, K),
            intercept = rep(0, K),
            intercept0 = 0,
            scaling0 = 0,
            sd0 = 1,
            sd1 = 1,
            b0 = 0,
            b1 = 0,
            sigma = 1,
            sigma_pop = 1)

scalingModel <- nimbleModel(code = scalingCode,
                name = "filtmonuments",
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
posterior_summary <- as.data.frame(HPDinterval(mcmc(mcmc_out[,-1]), prob = 0.99))
posterior_summary$mean <- apply(mcmc_out[,-1], 2, mean)
posterior_summary$stdd <- apply(mcmc_out[,-1], 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_filtmonuments.csv")

#################### FOURTH ANALYSIS: HNWI #####################

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
    # scaling params
    intercept ~ dnorm(mean = 0, sd = 100)
    scaling ~ dnorm(mean = 0, sd = 100)

    # Hyperpriors for size
    shape_hyper ~ dgamma(2, 1)  # Hyperprior for the shape of the gamma distribution
    rate_hyper ~ dgamma(2, 1)   # Hyperprior for the rate of the gamma distribution
    
    size ~ dgamma(shape = shape_hyper, rate = rate_hyper)

    # main model
    for(n in 1:N){
        log_mu[n] <- intercept + scaling * pop[n]
        mu[n] <- exp(log_mu[n]) 
        y[n] ~ dnegbin(prob = size / (size + mu[n]), size = size)
    }
})

# remove rows from the HNWI dataframe with no pop values
remove_rows_idx <- which(is.na(global_hnwi$population))

N <- dim(global_hnwi[-remove_rows_idx,])[1]
y <- global_hnwi[-remove_rows_idx,]$Billionaires
pop <- log(global_hnwi[-remove_rows_idx,]$population)

Consts <- list(N = N)

Data <- list(y = y,
            pop = pop)

Inits <- list(scaling = 0,
            intercept = 0,
            sigma = 1)

scalingModel2 <- nimbleModel(code = scalingCode2,
                name = "hnwi",
                constants = Consts,
                data = Data,
                inits = Inits)

params_to_track <- c("intercept", 
                    "scaling", 
                    "mu")

mcmc_out <- nimbleMCMC(model = scalingModel2, 
            monitors = params_to_track, thin = thin,
            niter = niter, nburnin = nburnin)

# Quick trace plots for key parameters
top_lvl_param_names <- c("intercept", "scaling")
stacked_traceplot(as.mcmc(mcmc_out), top_lvl_param_names)

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
posterior_summary <- as.data.frame(HPDinterval(mcmc(mcmc_out[,-1]), prob = 0.99))
posterior_summary$mean <- apply(mcmc_out[,-1], 2, mean)
posterior_summary$stdd <- apply(mcmc_out[,-1], 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_hnwi.csv")

#################### FIFTH ANALYSIS: EPIGRAPHY #####################

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
  # Count how many inscriptions fall within the city's radius
  RomanUrban$InscriptionCount[i] <- sum(distances <= radius)
}

# Scaling simplified since province turned out to be irrelevant above
# set up a Nimble model

# set up a Nimble model
scalingCode3 <- nimbleCode({
    # Priors for monument scaling parameters
    intercept ~ dnorm(0, 100)  # intercept on log scale
    scaling ~ dnorm(0, 100)    # Scaling factor for logged population
    sigma ~ dunif(1e-7, 100)   # Standard deviation for population model

    # Priors for population-area linking parameters
    b0 ~ dnorm(0, 100)         # Intercept for population model
    b1 ~ dnorm(0, 100)         # Slope for population model
    sigma_pop ~ dunif(1e-7, 100) # Standard deviation for population link

    # Hyperpriors for size
    shape_hyper ~ dgamma(2, 1)  # Hyperprior for the shape of the gamma distribution
    rate_hyper ~ dgamma(2, 1)   # Hyperprior for the rate of the gamma distribution
    
    size ~ dgamma(shape = shape_hyper, rate = rate_hyper)

    # Main model loop
    for(n in 1:N) {
        # Population model: Predictive model for log-population
        # the iput variables are already log-transformed (see below)
        pop_mu[n] <- b0 + b1 * x[n]
        pop[n] ~ dnorm(pop_mu[n], sigma_pop)

        # Poisson model for count data with log-log scaling
        log_mu[n] <- intercept + scaling * pop[n]  # log model, pop is on log-scale
        mu[n] <- exp(log_mu[n])  # Convert from log scale to linear scale
        #y[n] ~ dpois(mu[n])  # Poisson distribution for count data
        y[n] ~ dnegbin(prob = size / (size + mu[n]), size = size)
    }
})

N <- dim(RomanUrban)[1]
y <- RomanUrban$InscriptionCount
x <- log(RomanUrban$Area)
pop <- log(RomanUrban$pop_est)

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

scalingModel <- nimbleModel(code = scalingCode3,
                name = "epigraphy",
                constants = Consts,
                data = Data,
                inits = Inits)

params_to_track <- c("intercept", 
                    "scaling", 
                    "b0", 
                    "b1", 
                    "sigma_pop",
                    "mu")

mcmc_out <- nimbleMCMC(model = scalingModel, 
            monitors = params_to_track, thin = thin,
            niter = niter, nburnin = nburnin,
            samplesAsCodaMCMC = TRUE)

# Quick trace plots for key parameters
top_lvl_param_names <- c("intercept", "scaling")
stacked_traceplot(mcmc_out, top_lvl_param_names)

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
posterior_summary <- as.data.frame(HPDinterval(mcmc(mcmc_out[,]), prob = 0.99))
posterior_summary$mean <- apply(mcmc_out[,], 2, mean)
posterior_summary$stdd <- apply(mcmc_out[,], 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_epigraphy.csv")


#################### SIXTH ANALYSIS: TALL BUILDINGS #####################

## Here we anlyze another, exclusive global dataset of tall buildings
## These are usually indicative of wealth and and are frequently 
## monumental in the sense that the sheer height/size of these
## buildings has frequently been a display of an individual or
## group's wealth and engineering prowess.

# get data
data_path <- "Data/MPI_150m_Dataset.xlsx"
sheets <- excel_sheets(data_path)
sheets

# pull in raw Excel data
tall_buildings <- read_excel(data_path, 
                    sheet = sheets[1])

# remove rows from the dataframe with no pop values
remove_rows_idx <- which(is.na(tall_buildings$Population))

N <- dim(tall_buildings[-remove_rows_idx,])[1]
y <- tall_buildings[-remove_rows_idx,]$`150 m+ Buildings`
pop <- log(tall_buildings[-remove_rows_idx,]$Population)

Consts <- list(N = N)

Data <- list(y = y,
            pop = pop)

Inits <- list(scaling = 0,
            intercept = 0,
            sigma = 1)

scalingModel2 <- nimbleModel(code = scalingCode2,
                name = "tallbuildings",
                constants = Consts,
                data = Data,
                inits = Inits)

params_to_track <- c("intercept", 
                    "scaling")

niter <- 500000
nburnin <- 5000
thin <- 10

mcmc_out <- nimbleMCMC(model = scalingModel2, 
            monitors = params_to_track, thin = thin,
            niter = niter, nburnin = nburnin)

# Quick trace plots for key parameters
top_lvl_param_names <- c("intercept", "scaling")
stacked_traceplot(as.mcmc(mcmc_out), top_lvl_param_names)

# isolate the columns from mcmc output containing samples of the 
# mean prediction for the log-log model (y_hat)
mu_idx <- grep("mu",colnames(mcmc_out))

# calculate r-squared
rsq <- apply(mcmc_out[, mu_idx], 1, rsquared, y = y)

# summarize and save
rsq_summary <- data.frame(analysis = "tallbuildings", rsq = round(mean(rsq), 2))
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
convergence <- t(c("tallbuildings", convergence))
colnames(convergence)[1] <- "analysis"
write.table(convergence, 
        file="Output/geweke_tallbuildings.csv",
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

ggsave(filename = "Output/tplots_tallbuildings.pdf", 
        plot = tplot, 
        device = "pdf")

# summarize the results
posterior_summary <- as.data.frame(HPDinterval(mcmc(mcmc_out[,-1]), prob = 0.99))
posterior_summary$mean <- apply(mcmc_out[,-1], 2, mean)
posterior_summary$stdd <- apply(mcmc_out[,-1], 2, sd)

write.csv(round(posterior_summary,2), 
        file = "Output/posterior_summary_tallbuildings.csv")


#################### BASIC SCATTER PLOTS #####################

# scatter plots
plt1 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = log(Area), y = log(Monuments))) +
                labs(title = "All Monuments") +
                theme_minimal()
plt2 <- ggplot(RomanUrban[walls_idx, ]) +
                geom_point(mapping = aes(x = log(Area), y = log(Monuments))) +
                labs(title = "All Monuments (walls only)") +
                theme_minimal()
plt3 <- ggplot(RomanUrban[filt_idx, ]) +
                geom_point(mapping = aes(x = log(Area), y = log(Monuments_filt))) +
                labs(title = "Symbolic Monuments") +
                theme_minimal()

plt4 <- ggplot(df_subset) +
                geom_point(mapping = aes(x = log(Area), y = log(InscriptionCount))) +
                labs(title = "Epigraphy") +
                theme_minimal()
plt5 <- ggplot(global_hnwi[-remove_rows_idx, ]) +
                geom_point(mapping = aes(x = log(population), y = log(Billionaires))) +
                labs(title = "Billionaires") +
                theme_minimal()

layout <- "
AABBCC
#EEFF#
"

plt1 + plt2 + plt3 + plt4 + plt5 + plot_layout(design = layout)

ggsave("Output/point_scatters.pdf",
        height = 10,
        width = 15,
        units = "cm",
        scale = 2.5,
        device = "pdf")
