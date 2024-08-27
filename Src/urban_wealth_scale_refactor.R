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
library(RColorBrewer)
library(ggpattern)
library(loo)

### DATA WRANGLING #############################################################

# pull in the data from Excel sheets and CSVs as needed

#### Get Hanson Roman cities data

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

# isolate only case where we have area estimates
RomanUrban <- subset(RomanUrban, !is.na(Area))

# set momument NAs to 0
RomanUrban[which(is.na(RomanUrban$Monuments)),"Monuments"] <- 0
RomanUrban[which(is.na(RomanUrban$Monuments_filt)),"Monuments_filt"] <- 0

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

#### Get epigraphic data

# checking for consistency when looking at an epigaphic record database and 
# isolating the instances of epigraphic monument/building dedications 

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
  distances <- distHaversine(p1 = city_coords, p2 = epigraphic_points)
  # Count how many inscriptions fall within the city's radius
  RomanUrban$InscriptionCount[i] <- sum(distances <= radius)
}

#### Get high net worth individuals data

data_path <- "Data/hnwi_by_city.xlsx"
sheets <- excel_sheets(data_path)
sheets

# pull in raw Excel data
global_hnwi <- read_excel(data_path, 
                    sheet = sheets[1])
# remove duplicates in country names column
pattern <- "(.+?)\\1+"
global_hnwi$Country_cleaned <- sub(pattern, "\\1", global_hnwi$Country)

# manually change country name entry "Hong Kong (SAR China)" to "Hong Kong" for 
# consistency with other database similar revisions were made following manual 
# checking for consistency between the two dataframe naming conventions (see below)

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

# The following lines were used to check the rows with NA population and then 
# changes were made to city name spellings in the global_hnwi dataframe in order 
# to make them consistent with the population dataframe spellings. This resolved 
# a number of NA cases, but there were ultimately 5 that remained and were 
# excluded for that reason.

na_pop <- which(is.na(global_hnwi[,"population"]))

global_hnwi[na_pop, ]

global_pop_by_city[grep("Cape", global_pop_by_city$city_ascii),]

global_pop_by_city[grep("Israel", global_pop_by_city$country),]

# remove rows from the HNWI dataframe with no pop values
remove_rows_idx <- which(is.na(global_hnwi$population))
global_hnwi <- global_hnwi[-remove_rows_idx,]
global_hnwi[which(is.na(global_hnwi$Billionaires)), 'Billionaires'] <- 0

#### Get tall buildings data

# get data
data_path <- "Data/MPI_150m_Dataset.xlsx"
sheets <- excel_sheets(data_path)
sheets

# pull in raw Excel data
tall_buildings <- read_excel(data_path, 
                    sheet = sheets[1])

# remove rows from the dataframe with no pop values
remove_rows_idx <- which(is.na(tall_buildings$Population))
tall_buildings <- tall_buildings[-remove_rows_idx,]

### MAIN BAYESIAN/NIMBLE MODELS ################################################

# Now use a Bayesian approach to simultaneously estimate missing
# population sizes from the available data and then use the 
# 'emprical' and estimated population sizes in a log-log model
# to estimate the scaling parameters given different subsets
# of the data.

# We accounted for possible differences between/among the different Roman provinces
# with an index variable that allows for the mean scaling coefficient to vary 
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

#### Common mcmc params ########################################################
niter <- 80000
nburnin <- floor(niter * 0.25)
thin <- 3 # given niter = 80K, thin = 3 means 20K samples per chain
thin2 <- 3
nchains = 4

# note that for some of the processes below, additional thinning is performed
# because of memory limitations, especially for plotting

# set up a Nimble model

#### ARCHAEOLOGICAL MODELS #####################################################
# power law
scalingCode <- nimbleCode({
    # monument scaling params
    intercept0 ~ dnorm(mean = 0, sd = 5)
    sd0 ~ dexp(rate = 0.5)
    scaling0 ~ dnorm(mean = 0, sd = 5)
    sd1 ~ dexp(rate = 0.5)
    for(k in 1:K){
        intercept_raw[k] ~ dnorm(0, 1)
        intercept[k] <- intercept0 + intercept_raw[k] * sd0
        scaling_raw[k] ~ dnorm(0, 1)
        scaling[k] <- scaling0 + scaling_raw[k] * sd1
    }

    # prior for negbinom size parameter (dispersion)
    size ~ dexp(rate = 0.5)

    # population--area linking params
    b0 ~ dnorm(mean = 0, sd = 10)
    b1 ~ dnorm(mean = 0, sd = 10)
    sigma_pop ~ dexp(rate = 1)

    # main model
    for(n in 1:N){
        pop_mu[n] <- b0 + b1 * x[n] # pop_mu, x are on log scale
        pop[n] ~ dnorm(mean = pop_mu[n], sd = sigma_pop) # pop is on log scale
        log_mu[n] <- intercept[v[n]] + scaling[v[n]] * pop[n] # log_mu, is on 
                                                              # log scale
        mu[n] <- exp(log_mu[n]) # mu is now on original, non-log scale
        p[n] <- size / (size + mu[n])
        y[n] ~ dnegbin(prob = p[n], size = size) # original scale for count data, 
                                                 # but power-law model for mean
        y_hat[n] <- (size * (1 - p[n])) / p[n]
    }
})

# linear-log
scalingCode_linlog <- nimbleCode({
    # monument scaling params
    intercept0 ~ dnorm(mean = 0, sd = 5)
    sd0 ~ dexp(rate = 0.5)
    scaling0 ~ dnorm(mean = 0, sd = 5)
    sd1 ~ dexp(rate = 0.5)
    for(k in 1:K){
        intercept_raw[k] ~ dnorm(0, 1)
        intercept[k] <- intercept0 + intercept_raw[k] * sd0
        scaling_raw[k] ~ dnorm(0, 1)
        scaling[k] <- scaling0 + scaling_raw[k] * sd1
    }

    # prior for negbinom size parameter (dispersion)
    size ~ dexp(rate = 0.5)

    # population--area linking params
    b0 ~ dnorm(mean = 0, sd = 10)
    b1 ~ dnorm(mean = 0, sd = 10)
    sigma_pop ~ dexp(rate = 1)

    # main model
    for(n in 1:N){
        pop_mu[n] <- b0 + b1 * x[n] # pop_mu, x are on log scale
        pop[n] ~ dnorm(mean = pop_mu[n], sd = sigma_pop) # pop is on log scale
        mu[n] <- intercept[v[n]] + scaling[v[n]] * pop[n] # mu is log scale
        p[n] <- size / (size + mu[n])
        y[n] ~ dnegbin(prob = p[n], size = size) # original scale for count data, 
                                                # but linear-log model for mean 
                                                # (mu is still on log-scale)
        y_hat[n] <- (size * (1 - p[n])) / p[n]
    }
})

#### MODERN MODELS #############################################################
# power-law
scalingCode2 <- nimbleCode({
    # scaling params
    intercept ~ dnorm(mean = 0, sd = 5)
    scaling ~ dnorm(mean = 0, sd = 5)
    
    size ~ dexp(rate = 0.5)

    # main model
    for(n in 1:N){
        log_mu[n] <- intercept + scaling * pop[n]
        mu[n] <- exp(log_mu[n]) 
        p[n] <- size / (size + mu[n])
        y[n] ~ dnegbin(prob = p[n], size = size)
        y_hat[n] <- (size * (1 - p[n])) / p[n]
    }
})

# linear-log
scalingCode2_linlog <- nimbleCode({
    # scaling params
    intercept ~ dnorm(mean = 0, sd = 5)
    scaling ~ dnorm(mean = 0, sd = 5)
    
    size ~ dexp(rate = 0.5)

    # main model
    for(n in 1:N){
        mu[n] <- intercept + scaling * pop[n] 
        p[n] <- size / (size + mu[n])
        y[n] ~ dnegbin(prob = p[n], size = size)
        y_hat[n] <- (size * (1 - p[n])) / p[n]
    }
})

### UTILITY FUNCTIONS ##########################################################

# Function to create trace plots with mode switch for stacked or faceted 
# visualization
stacked_traceplot <- function(mcmc_obj, params, mode = "facet", thin = 1) {
  # Check if the input is an mcmc.list or a single mcmc object
  if (is.mcmc.list(mcmc_obj)) {
        mcmc_df <- do.call(rbind, lapply(mcmc_obj, as.data.frame))
        # Thinning the chains
        mcmc_df <- mcmc_df[seq(1, nrow(mcmc_df), by = thin), ]
        # Add iteration numbers for each chain
        mcmc_df$Iteration <- rep(seq(1, nrow(mcmc_obj[[1]]), by = thin), 
                                times = length(mcmc_obj))
        # Add a Chain identifier
        mcmc_df$Chain <- rep(seq_along(mcmc_obj), 
                        each = length(seq(1, nrow(mcmc_obj[[1]]), by = thin)))
  } else if (is.mcmc(mcmc_obj)) {
        mcmc_df <- as.data.frame(mcmc_obj)
        # Thinning the chain
        mcmc_df <- mcmc_df[seq(1, nrow(mcmc_df), by = thin), ]
        # Add iteration numbers
        mcmc_df$Iteration <- seq(1, nrow(mcmc_df), by = thin)
        # Single chain case, so add Chain identifier as 1
        mcmc_df$Chain <- 1
  } else {
        stop("Input object must be of class 'mcmc' or 'mcmc.list'")
  }
  
  # Check if all specified parameters exist in the data
  missing_params <- setdiff(params, colnames(mcmc_df))
  if (length(missing_params) > 0) {
    stop("The following parameters are missing in the MCMC output: ", 
        paste(missing_params, collapse = ", "))
  }

  # Transform data to long format for specified parameters
  long_mcmc <- pivot_longer(mcmc_df, 
                            cols = all_of(params), 
                            names_to = "Parameter", 
                            values_to = "Sample")

  # Plotting based on mode selection
  if (mode == "facet") {
    plot <- ggplot(long_mcmc, aes(x = Iteration, 
                                y = Sample, 
                                color = factor(Chain))) +
      geom_line(alpha = 0.7) +
      facet_grid(Parameter ~ ., scales = "free_y") +
      labs(title = "Trace Plots for Selected Parameters (Facet Mode)",
           x = "Iteration",
           y = "Sample Value",
           color = "Chain") +
      theme_minimal() +
      theme(legend.position = "bottom")
  } else if (mode == "stacked") {
    plot <- ggplot(long_mcmc, aes(x = Iteration, 
                                y = Sample, 
                                color = Parameter, 
                                group = interaction(Parameter, Chain))) +
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

# plot model residuals
plot_residual_intervals <- function(mcmc_obj, 
                                    y, 
                                    y_hat_param = "y_hat", 
                                    cr_level = 0.95,
                                    additional_thin = NULL,
                                    outlier_indices = NULL,  # Indices of outliers
                                    df = NULL,  # Dataframe containing the database IDs
                                    id_col = "id") {  # Column name for database IDs in df
  # if mcmc is a list, then there are multiple chains; combine them
  if (is.mcmc.list(mcmc_obj)) {
    mcmc_df <- do.call(rbind, lapply(mcmc_obj, as.data.frame))
    mcmc_df$Chain <- rep(seq_along(mcmc_obj), each = nrow(mcmc_obj[[1]]))
  } else if (is.mcmc(mcmc_obj)) {
    mcmc_df <- as.data.frame(mcmc_obj)
    mcmc_df$Chain <- 1
  } else {
    stop("Input object must be of class 'mcmc' or 'mcmc.list'")
  }

  if(!is.null(additional_thin)){
        mcmc_df <- mcmc_df[seq(1, nrow(mcmc_df), additional_thin), ]
  }

  y_hat_idx <- grep(y_hat_param, colnames(mcmc_df))
  if (length(y_hat_idx) == 0) stop(paste("The parameter", 
                                y_hat_param, 
                                "is missing in the MCMC output."))

  mcmc_residuals <- t(apply(mcmc_df[, y_hat_idx], 1, function(yhat, y) y - yhat, y = y))
  
  mean_y_hat <- colMeans(mcmc_df[, y_hat_idx])
  
  # Calculate credible intervals for residuals
  lower_bound <- (1 - cr_level) / 2
  upper_bound <- 1 - lower_bound
  
  lower_cr <- apply(mcmc_residuals, 2, quantile, probs = lower_bound)
  upper_cr <- apply(mcmc_residuals, 2, quantile, probs = upper_bound)

  # Prepare data for the plot
  residuals_df <- data.frame(
    mean_y_hat = mean_y_hat,
    lower_cr = lower_cr,
    upper_cr = upper_cr,
    outlier_id = NA  # Initialize column for outlier IDs
  )
  
  # Assign outlier IDs based on the indices
  if (!is.null(outlier_indices) && !is.null(df)) {
    residuals_df[outlier_indices, 'outlier_id'] <- as.vector(df[outlier_indices, id_col])
  }

  # Plot the 95% CR for residuals with vertical line at zero
  plot <- ggplot(residuals_df, aes(y = mean_y_hat)) +
    geom_linerange(aes(xmin = lower_cr, xmax = upper_cr), color = "blue", linewidth = 1.5) +
    geom_point(aes(x = (lower_cr + upper_cr) / 2), color = "blue", size = 2) +
    geom_vline(xintercept = 0, color = "grey", linetype = "dashed", linewidth = 1.2) +
    labs(title = paste0(100 * cr_level, "% Credible Intervals for Residuals Across Predicted Values"),
         x = "Residuals", y = "Predicted Values (y_hat)") +
    theme_minimal()
  
  # Add labels for outliers
  plot <- plot +
    geom_text(aes(x = (lower_cr + upper_cr) / 2, 
                  label = outlier_id), 
              hjust = -0.1, vjust = 0, size = 3, color = "red", na.rm = TRUE)

  return(plot)
}

# extract residual summaries
get_residual_summaries <- function(mcmc_obj, y, y_hat_param = "y_hat") {
  if (is.mcmc.list(mcmc_obj)) {
    mcmc_df <- do.call(rbind, lapply(mcmc_obj, as.data.frame))
    mcmc_df$Chain <- rep(seq_along(mcmc_obj), each = nrow(mcmc_obj[[1]]))
  } else if (is.mcmc(mcmc_obj)) {
    mcmc_df <- as.data.frame(mcmc_obj)
    mcmc_df$Chain <- 1
  } else {
    stop("Input object must be of class 'mcmc' or 'mcmc.list'")
  }

  y_hat_idx <- grep(y_hat_param, colnames(mcmc_df))
  if (length(y_hat_idx) == 0) stop(paste("The parameter", y_hat_param, "is missing in the MCMC output."))

  # Calculate residuals
  mcmc_residuals <- t(apply(mcmc_df[, y_hat_idx], 1, function(yhat, y) y - yhat, y = y))
  
  # Calculate mean and median residuals for each data point
  mean_residuals <- colMeans(mcmc_residuals)
  median_residuals <- apply(mcmc_residuals, 2, median)

  # Prepare the output
  residuals_summary <- data.frame(
    Data_Point = 1:length(y),
    Mean_Residual = mean_residuals,
    Median_Residual = median_residuals
  )
  
  return(residuals_summary)
}

# convergence checking and diagnostics
# Geweke
output_geweke <- function(g, 
                        modelname, 
                        outfolder = "Output/"){

        outpath = paste(outfolder, 
                        "geweke_",
                        modelname, 
                        ".csv", 
                        sep = "")

        if(class(g) == "geweke.diag"){
                convergence <- data.frame(
                                chain = rep(1, length(g$z)),
                                model = rep(modelname, length(g$z)),
                                parameter = names(g$z),
                                z = as.vector(g$z))
        }else if(class(g) == "list"){
                convergence <- map2_df(g, seq_along(g), 
                               ~ data.frame(
                                   chain = rep(.y, length(.x$z)),
                                   model = rep(modelname, length(.x$z)),
                                   parameter = names(.x$z),
                                   z = as.vector(.x$z))
                               )
        }else{
                stop("Unrecognized object passed as 'g'")
        }
        write.table(convergence, 
                file = outpath,
                row.names = FALSE,
                sep = ",")
}

# Gelman-Rubin R_hat
output_gr_rhat <- function(mcmc_out, 
                        modelname, 
                        outfolder = "Output/"){
        
        outpath = paste(outfolder, 
                        "grrhat_",
                        modelname, 
                        ".csv", 
                        sep = "")
        
        gr_rhat <- gelman.diag(mcmc_out)$psrf
        gr_rhat <- data.frame(parameter = rownames(gr_rhat),
                                point_estimate = gr_rhat[, 1],
                                upper_ci = gr_rhat[, 2])

        write.table(gr_rhat, 
                file = outpath,
                row.names = FALSE,
                sep = ",")
}

# Function to calculate and output Effective Sample Size (ESS)
output_ess <- function(mcmc_out, 
                        modelname, 
                        outfolder = "Output/"){
        
        # Construct the output file path
        outpath <- paste(outfolder, 
                        "ess_",
                        modelname, 
                        ".csv", 
                        sep = "")
        
        # Calculate ESS using the effectiveSize function from coda package
        ess_values <- effectiveSize(mcmc_out)
        
        # Convert to a data frame for easy output
        ess_df <- data.frame(parameter = names(ess_values),
                             ess = ess_values)
        
        # Write the ESS values to a CSV file
        write.table(ess_df, 
                    file = outpath,
                    row.names = FALSE,
                    sep = ",")
}

# output posterior summary
# multiple chains are combined here assuming convergence

output_posterior_summary <- function(mcmc_out,
                                modelname, 
                                outfolder = "Output/"){

        outpath = paste(outfolder, 
                        "post_summary_",
                        modelname, 
                        ".csv", 
                        sep = "")

        if(is.mcmc.list(mcmc_out)){
                mcmc_out <- as.mcmc(do.call(rbind, mcmc_out))
        }
        
        # summarize the results
        posterior_summary <- as.data.frame(HPDinterval(mcmc_out, prob = 0.99))
        posterior_summary$mean <- apply(mcmc_out, 2, mean)
        posterior_summary$stdd <- apply(mcmc_out, 2, sd)

        write.csv(round(posterior_summary,2), 
                file = outpath)

}

# estimate pseudo r-squared for compatibility with previous research
# function for calculating it
rsquared <- function(y_hat, y){
        TSS <- sum( (y - mean(y))^2 )
        RSS <- sum( (y - y_hat)^2 )
        rsq <- 1 - ( RSS / TSS )
        return(rsq)
}

output_rsquared <- function(y = y,
                        y_hat_idx = y_hat_idx,
                        mcmc_out = mcmc_out,
                        modelname, 
                        outpath = "Output/rsquared.csv",
                        thin = 1){

        append <- file.exists(outpath)

        if(is.mcmc.list(mcmc_out)){
                y = rep(y, times = length(mcmc_out))
                mcmc_out <- as.mcmc(do.call(rbind, mcmc_out))
        }

        mcmc_out <- mcmc_out[seq(1, nrow(mcmc_out), by = thin), ]

        # calculate r-squared
        rsq <- apply(mcmc_out[, y_hat_idx], 1, rsquared, y = y)

        # summarize and save
        rsq_summary <- data.frame(analysis = modelname, 
                                rsq = round(mean(rsq), 2))

        write.table(rsq_summary, 
                file = outpath,
                row.names = F,
                col.names = !append,
                sep = ",",
                append = append)
}

# output WAIC
output_waic <- function(waic = waic,
                        modelname, 
                        outpath = "Output/waic.csv"){

        append <- file.exists(outpath)

        waic_out <- data.frame(model = modelname,
                                waic = round(waic$WAIC))

        write.table(waic_out, 
                file = outpath,
                row.names = F,
                col.names = !append,
                sep = ",",
                append = append)
}

# output lppd
output_lppd <- function(waic_dets = waic_dets,
                        modelname,
                        modeltype,
                        outpath = "Output/lppd.csv",
                        node_idx = NULL){
        append <- file.exists(outpath)
        
        if(!is.null(node_idx)){
                lppd_elements <- waic_dets$lppd_elements[node_idx]
        }else {
                lppd_elements <- waic_dets$lppd_elements
        }
        n_lppd <- length(lppd_elements)
        
        lppd_df <- data.frame(modelname = rep(modelname, n_lppd),
                        modeltype = modeltype,
                        lppd = lppd_elements)
        
        write.table(lppd_df, 
                file = outpath,
                row.names = F,
                col.names = !append,
                sep = ",",
                append = append)
}

# output the pointwise lppd-based model comparison scores
output_lppd_diff <- function(modelname,
                        outpath = "Output/model_comparison.csv"){
        append <- file.exists(outpath)

        lppds <- tibble(read.csv("Output/lppd.csv"))
        modelname1 <- sub("_.*", "", modelname)

        power_model_lppd <- lppds[which(lppds[,'modelname'] == modelname1), 'lppd']
        linlog_model_lppd <- lppds[which(lppds[,'modelname'] == modelname), 'lppd']

        lppd_diff <- power_model_lppd[, 1] - linlog_model_lppd[, 1]

        power_model_pref <- round(mean(lppd_diff > 0), 2)
        linlog_model_pref <- round(mean(lppd_diff < 0), 2)

        models_compared <- data.frame(model_1 = paste(modelname1, "_power", sep = ""),
                                        model_2 = modelname,
                                        p_model_1 = power_model_pref,
                                        p_model_2 = linlog_model_pref)

        write.table(models_compared, 
                file = outpath,
                row.names = F,
                col.names = !append,
                sep = ",",
                append = append)
}

# extract target scaling parameter mcmc samples and write to csv in long format
# for plotting with the others later

output_scaling <- function(mcmc_out,
                        modelname,
                        parameter,
                        outpath = "Output/scaling_samples.csv"){

        append <- file.exists(outpath)
        
        if(is.mcmc.list(mcmc_out)){
                nchains = length(mcmc_out)
                thin_idx <- seq(1, dim(mcmc_out[[1]])[1] * nchains, nchains)
                mcmc_out <- as.mcmc(do.call(rbind, mcmc_out))[thin_idx, ]
        }

        scaling_samples <- data.frame(analysis = rep(modelname, dim(mcmc_out)[1]),
                                        scaling =  mcmc_out[, parameter])
        
        write.table(scaling_samples, 
                file = outpath,
                row.names = F,
                col.names = !append,
                sep = ",",
                append = append)
}

### CORE ANALYSIS WRAPPER ######################################################

run_scaling_analysis <- function(df = df,
                                modelname = modelname,
                                modeltype = modeltype,
                                modern = FALSE,
                                fit_diagnostics = TRUE,
                                scalingCode = scalingCode,
                                Consts = Consts,
                                Data = Data,
                                inits = Inits,
                                niter = niter,
                                thin = thin,
                                thin2 = thin2,
                                nchains = nchains,
                                lppd_diff = FALSE,
                                output_scaling_samples = TRUE){

# Create the Nimble model
scalingModel <- nimbleModel(code = scalingCode,
                            name = modelname,
                            constants = Consts,
                            data = Data,
                            inits = Inits)

# Compile the model
compiled_model <- compileNimble(scalingModel)

# Configure the MCMC
if(fit_diagnostics){
        mcmc_config <- configureMCMC(scalingModel, enableWAIC = TRUE)
}else{
        mcmc_config <- configureMCMC(scalingModel)
}

if(modern){
        # Replace samplers for correlated parameters
        # Block sampling for intercept and scaling
        mcmc_config$removeSamplers(c('intercept', 'scaling'))
        mcmc_config$addSampler(target = c('intercept', 'scaling'), type = 'AF_slice')

        # Parameters to track
        params_to_track <- c("intercept", 
                        "scaling",
                        "size",
                        "y_hat")
}else{
        # Replace samplers for correlated parameters
        # Block sampling for intercept0 and scaling0
        mcmc_config$removeSamplers(c('intercept0', 'scaling0'))
        mcmc_config$addSampler(target = c('intercept0', 'scaling0'), 
                                type = 'AF_slice')

        # Block sampling for b0 and b1
        mcmc_config$removeSamplers(c('b0', 'b1'))
        mcmc_config$addSampler(target = c('b0', 'b1'), 
                                type = 'AF_slice')

        # Select parameters to track
        params_to_track <- c("intercept0",
                        "scaling0",
                        "sd0",
                        "sd1",
                        "size",
                        "scaling", 
                        "intercept",
                        "b0", 
                        "b1",
                        "sigma_pop",
                        "y_hat")

}

mcmc_config$monitors <- params_to_track

if(fit_diagnostics){
        mcmc_config$addMonitors2("logProb_y")
}

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

if(fit_diagnostics){
        # Run the MCMC
        mcmc_out <- runMCMC(compiled_mcmc, 
                        niter = niter, 
                        nburnin = nburnin, 
                        thin = thin,
                        thin2 = thin2, 
                        nchains = nchains, 
                        samplesAsCodaMCMC = TRUE,
                        WAIC = TRUE)

        # WAIC
        mcmc_waic <- mcmc_out$WAIC
        mcmc_out2 <- mcmc_out$samples2
        mcmc_out <- mcmc_out$samples
        output_waic(waic = mcmc_waic,
                        modelname = modelname)

        # lppd
        waic_dets <- compiled_mcmc$getWAICdetails(returnElements = TRUE)

        # only want values for data nodes, 'y', lppd
        y_node_idx <- grep("y\\[", compiled_model$getNodeNames(dataOnly = TRUE))

        output_lppd(waic_dets,
                modelname,
                modeltype,
                node_idx = y_node_idx)

        # psis-loo, alternative to WAIC and pWAIC for assessing predictive utility
        if(is.mcmc.list(mcmc_out2)){
                mcmc_out2_long <- do.call(rbind, mcmc_out2)
                n_per_chain = nrow(mcmc_out2[[1]])
                psis_loo <- loo(as.matrix(mcmc_out2_long), 
                        r_eff = relative_eff(as.matrix(mcmc_out2_long), 
                                        chain_id = rep(1:nchains, each = n_per_chain)))
        }else{
                psis_loo <- loo(as.matrix(mcmc_out2), 
                        r_eff = relative_eff(as.matrix(mcmc_out2), 
                                        chain_id = rep(1, nrow(mcmc_out2))))
        }

        write.table(psis_loo$pointwise,
                file = paste("Output/loo_pw_", modelname, ".csv", sep = ""),
                row.names = FALSE,
                sep = ",")

        write.table(psis_loo$estimates,
                file = paste("Output/loo_est_", modelname, ".csv", sep = ""),
                sep = ",")

        # output the city identifiers that correspond with extreme Pareto k diagnostics
        outliers_idx <- which(psis_loo$pointwise[,'influence_pareto_k'] > 0.7)

        outpath <- paste("Output/city_outliers_", modelname, ".csv", sep = "")

        write.table(df[outliers_idx, ], 
                        file = outpath,
                        row.names = FALSE,
                        sep = ",")
        rm(outpath)
}else{
        # Run the MCMC
        mcmc_out <- runMCMC(compiled_mcmc, 
                        niter = niter, 
                        nburnin = nburnin, 
                        thin = thin, 
                        nchains = nchains, 
                        samplesAsCodaMCMC = TRUE)
}

# Trace plots for key parameters
if(modern){
        top_lvl_param_names <- c("intercept", 
                                "scaling", 
                                "size")
}else{
        top_lvl_param_names <- c("intercept0", 
                                "scaling0",
                                "sd0",
                                "sd1", 
                                "b0", 
                                "b1",
                                "sigma_pop",
                                "size")
}

tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

ggsave(filename = paste("Output/tplots_", modelname, ".pdf", sep = ""), 
        plot = tplot, 
        device = "pdf")
ggsave(filename = paste("Output/tplots_", modelname, ".png", sep = ""), 
        plot = tplot, 
        device = "png")

if(!modern){
        # and now to look at variability in scaling parameter among provinces
        if(is.mcmc.list(mcmc_out)){
                province_scaling_names <- colnames(mcmc_out[[1]])[grep("scaling\\[", colnames(mcmc_out[[1]]))]
        }else{
                province_scaling_names <- colnames(mcmc_out)[grep("scaling\\[", colnames(mcmc_out))]
        }

        tplot_provinces <- stacked_traceplot(mcmc_out, 
                                        province_scaling_names, 
                                        mode = "stacked", 
                                        thin = 10)

        ggsave(filename = paste("Output/tplots_", modelname, "_provinces.pdf", sep = ""), 
                plot = tplot_provinces, 
                device = "pdf")
        ggsave(filename = paste("Output/tplots_", modelname, "_provinces.png", sep = ""), 
                plot = tplot_provinces, 
                device = "png")
}

# convergence checking
# ignore y_hat from the chain variables
if(is.mcmc.list(mcmc_out)){
        y_hat_idx <- grep("y_hat",colnames(mcmc_out[[1]]))
        mcmc_out_subset <- lapply(mcmc_out, function(x, pidx){x[, -c(pidx)]}, y_hat_idx)
        mcmc_out_subset <- as.mcmc.list(mcmc_out_subset)
}else{
        y_hat_idx <- grep("y_hat",colnames(mcmc_out))
        mcmc_out_subset <- mcmc_out[,-y_hat_idx]
}

# convergence
g <- geweke.diag(mcmc_out_subset)

output_geweke(g, modelname, "Output/")

if(is.mcmc.list(mcmc_out_subset)){
        output_gr_rhat(mcmc_out_subset, modelname)
}

# posterior summaries
output_posterior_summary(mcmc_out_subset,
                        modelname,
                        outfolder = "Output/")

# pseudo r-sqaured
output_rsquared(y = y,
                y_hat_idx = y_hat_idx,
                mcmc_out = mcmc_out,
                modelname = modelname,
                thin = nchains)

# exract scaling parameter chain and save to csv for plotting all
# of them together at the end
if(modern){
        scaling_param = "scaling"
}else{
        scaling_param = "scaling0"
}

if(output_scaling_samples){
        output_scaling(mcmc_out = mcmc_out,
                modelname = modelname,
                parameter = scaling_param,
                outpath = "Output/scaling_samples.csv")
}

if(fit_diagnostics){
        # residuals-v-predicted diagnostic plot, used in our later supplemental analysis
        resid_plot <- plot_residual_intervals(mcmc_out, 
                                                y, 
                                                y_hat_param = "y_hat", 
                                                cr_level = 0.95,
                                                additional_thin = 5,
                                                outlier_indices = as.vector(outliers_idx),
                                                df = df,
                                                id_col = 'City')

        ggsave(filename = paste("Output/resid_", modelname, ".pdf", sep = ""), 
                plot = resid_plot, 
                device = "pdf")
        ggsave(filename = paste("Output/resid_", modelname, ".png", sep = ""), 
                plot = resid_plot, 
                device = "png")
}

if(lppd_diff){
        # output the final point-wise lppd diff based model comparison
        output_lppd_diff(modelname)
}

}

### BASIC SCATTER PLOTS ########################################################

# scatter plots with +1 offset to manage zeros
plt1 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = log(Area), y = log(Monuments + 1))) +
                labs(title = "All Monuments") +
                theme_minimal()
plt2 <- ggplot(RomanUrban[walls_idx, ]) +
                geom_point(mapping = aes(x = log(Area), y = log(Monuments + 1))) +
                labs(title = "All Monuments (walls only)") +
                theme_minimal()
plt3 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = log(Area), y = log(Monuments_filt + 1))) +
                labs(title = "Symbolic Monuments") +
                theme_minimal()
plt4 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = log(Area), y = log(InscriptionCount + 1))) +
                labs(title = "Epigraphy") +
                theme_minimal()
plt5 <- ggplot(global_hnwi) +
                geom_point(mapping = aes(x = log(population), y = log(Billionaires + 1))) +
                labs(title = "Billionaires") +
                theme_minimal()
plt6 <- ggplot(tall_buildings) +
                geom_point(mapping = aes(x = log(Population), y = log(`150 m+ Buildings` + 1))) +
                labs(title = "Tall Buildings") +
                theme_minimal()

layout <- "
AABBCC
DDEEFF
"

scatter_plots <- plt1 + plt2 + plt3 + plt4 + plt5 + plt6 + plot_layout(design = layout)

ggsave("Output/point_scatters.pdf",
        plot = scatter_plots,
        height = 10,
        width = 15,
        units = "cm",
        scale = 2.5,
        device = "pdf")

ggsave("Output/point_scatters.png",
        plot = scatter_plots,
        height = 10,
        width = 15,
        units = "cm",
        scale = 2.5,
        device = "png")

# scatter plots, linear
plt1 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = Area, y = Monuments)) +
                labs(title = "All Monuments") +
                theme_minimal()
plt2 <- ggplot(RomanUrban[walls_idx, ]) +
                geom_point(mapping = aes(x = Area, y = Monuments)) +
                labs(title = "All Monuments (walls only)") +
                theme_minimal()
plt3 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = Area, y = Monuments_filt)) +
                labs(title = "Symbolic Monuments") +
                theme_minimal()
plt4 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = Area, y = InscriptionCount)) +
                labs(title = "Epigraphy") +
                theme_minimal()
plt5 <- ggplot(global_hnwi) +
                geom_point(mapping = aes(x = population, y = Billionaires)) +
                labs(title = "Billionaires") +
                theme_minimal()
plt6 <- ggplot(tall_buildings) +
                geom_point(mapping = aes(x = Population, y = `150 m+ Buildings`)) +
                labs(title = "Tall Buildings") +
                theme_minimal()

layout <- "
AABBCC
DDEEFF
"

scatter_plots <- plt1 + plt2 + plt3 + plt4 + plt5 + plt6 + plot_layout(design = layout)

ggsave("Output/point_scatters_linear.pdf",
        plot = scatter_plots,
        height = 10,
        width = 15,
        units = "cm",
        scale = 2.5,
        device = "pdf")

ggsave("Output/point_scatters_linear.png",
        plot = scatter_plots,
        height = 10,
        width = 15,
        units = "cm",
        scale = 2.5,
        device = "png")

# scatter plots, linear-log, with +1 offset to manage zeros
plt1 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = Area, y = log(Monuments + 1))) +
                labs(title = "All Monuments") +
                theme_minimal()
plt2 <- ggplot(RomanUrban[walls_idx, ]) +
                geom_point(mapping = aes(x = Area, y = log(Monuments + 1))) +
                labs(title = "All Monuments (walls only)") +
                theme_minimal()
plt3 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = Area, y = log(Monuments_filt + 1))) +
                labs(title = "Symbolic Monuments") +
                theme_minimal()
plt4 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = Area, y = log(InscriptionCount + 1))) +
                labs(title = "Epigraphy") +
                theme_minimal()
plt5 <- ggplot(global_hnwi) +
                geom_point(mapping = aes(x = population, y = log(Billionaires + 1))) +
                labs(title = "Billionaires") +
                theme_minimal()
plt6 <- ggplot(tall_buildings) +
                geom_point(mapping = aes(x = Population, y = log(`150 m+ Buildings` + 1))) +
                labs(title = "Tall Buildings") +
                theme_minimal()

layout <- "
AABBCC
DDEEFF
"

scatter_plots <- plt1 + plt2 + plt3 + plt4 + plt5 + plt6 + plot_layout(design = layout)

ggsave("Output/point_scatters_linear_log.pdf",
        plot = scatter_plots,
        height = 10,
        width = 15,
        units = "cm",
        scale = 2.5,
        device = "pdf")

ggsave("Output/point_scatters_linear_log.png",
        plot = scatter_plots,
        height = 10,
        width = 15,
        units = "cm",
        scale = 2.5,
        device = "png")

### FIRST ANALYSIS: ALL MONUMENTS ##############################################

modelname = "allmonuments"
modeltype = "power_law"

# using full dataset
df <- RomanUrban

N <- dim(df)[1]
y <- df$Monuments
x <- log(df$Area)
pop <- log(df$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(df$Province))
K <- length(unique(df$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

# Inits$scaling <- rep(0, K)
# Inits$intercept <- rep(0, K)
# Inits$scaling_raw <- rep(0.5, K)
# Inits$intercept_raw <- rep(0.5, K)

Inits <- list(scaling = rep(0, K),
            scaling_raw = rep(0.5, K),
            intercept = rep(0, K),
            intercept_raw = rep(0.5, K),
            intercept0 = 0,
            scaling0 = 0,
            size = 0.5,
            sd0 = 0.5,
            sd1 = 0.5,
            b0 = 4,
            b1 = 0.8,
            sigma_pop = 0.5)

run_scaling_analysis(df = df,
                modelname = modelname,
                modeltype = modeltype,
                modern = FALSE,
                fit_diagnostics = TRUE,
                scalingCode = scalingCode,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains)

#### Exluding Zeros ############################################################

# model name for paths
modelname <- "allmonuments_nozero"
modeltype = "power_law"

nonzero_idx <- which(RomanUrban$Monuments > 0)
df <- RomanUrban[nonzero_idx,]

N <- dim(df)[1]
y <- df$Monuments
x <- log(df$Area)
pop <- log(df$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(df$Province))
K <- length(unique(df$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, K),
            scaling_raw = rep(0.5, K),
            intercept = rep(0, K),
            intercept_raw = rep(0.5, K),
            intercept0 = 0,
            scaling0 = 0,
            size = 0.5,
            sd0 = 0.5,
            sd1 = 0.5,
            b0 = 4,
            b1 = 0.8,
            sigma_pop = 0.5)

run_scaling_analysis(df = df,
                modelname = modelname,
                modeltype = modeltype,
                modern = FALSE,
                fit_diagnostics = FALSE,
                scalingCode = scalingCode,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains)

### SECOND ANALYSIS: WALLED ONLY ###############################################

modelname = "allwalls"
modeltype = "power_law"

df <- RomanUrban[walls_idx,]

N <- dim(df)[1]
y <- df$Monuments
x <- log(df$Area)
pop <- log(df$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(df$Province))
K <- length(unique(df$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, K),
            scaling_raw = rep(0.5, K),
            intercept = rep(0, K),
            intercept_raw = rep(0.5, K),
            intercept0 = 0,
            scaling0 = 0,
            size = 0.5,
            sd0 = 0.5,
            sd1 = 0.5,
            b0 = 4,
            b1 = 0.8,
            sigma_pop = 0.5)

run_scaling_analysis(df = df,
                modelname = modelname,
                modeltype = modeltype,
                modern = FALSE,
                fit_diagnostics = TRUE,
                scalingCode = scalingCode,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains)

#### Excluding Zeros ###########################################################

modelname = "allwalls_nozero"
modeltype = "power_law"

df <- RomanUrban[walls_idx,]

nonzero_idx <- which(df$Monuments > 0)
df <- df[nonzero_idx,]

N <- dim(df)[1]
y <- df$Monuments
x <- log(df$Area)
pop <- log(df$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(df$Province))
K <- length(unique(df$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, K),
            scaling_raw = rep(0.5, K),
            intercept = rep(0, K),
            intercept_raw = rep(0.5, K),
            intercept0 = 0,
            scaling0 = 0,
            size = 0.5,
            sd0 = 0.5,
            sd1 = 0.5,
            b0 = 4,
            b1 = 0.8,
            sigma_pop = 0.5)

run_scaling_analysis(df = df,
                modelname = modelname,
                modeltype = modeltype,
                modern = FALSE,
                fit_diagnostics = FALSE,
                scalingCode = scalingCode,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains)

### THIRD ANALYSIS: ABOVE GROUND ONLY ##########################################

modelname <- "filtmonuments"
modeltype = "power_law"

df <- RomanUrban

N <- dim(df)[1]
y <- df$Monuments_filt
x <- log(df$Area)
pop <- log(df$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(df$Province))
K <- length(unique(df$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, K),
            scaling_raw = rep(0.5, K),
            intercept = rep(0, K),
            intercept_raw = rep(0.5, K),
            intercept0 = 0,
            scaling0 = 0,
            size = 0.5,
            sd0 = 0.5,
            sd1 = 0.5,
            b0 = 4,
            b1 = 0.8,
            sigma_pop = 0.5)

run_scaling_analysis(df = df,
                modelname = modelname,
                modeltype = modeltype,
                modern = FALSE,
                fit_diagnostics = TRUE,
                scalingCode = scalingCode,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains)

#### Excluding Zeros ###########################################################

modelname = "filtmonuments_nozero"
modeltype = "power_law"

df <- RomanUrban

nonzero_idx <- which(df$Monuments_filt > 0)
df <- df[nonzero_idx,]

N <- dim(df)[1]
y <- df$Monuments_filt
x <- log(df$Area)
pop <- log(df$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(df$Province))
K <- length(unique(df$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, K),
            scaling_raw = rep(0.5, K),
            intercept = rep(0, K),
            intercept_raw = rep(0.5, K),
            intercept0 = 0,
            scaling0 = 0,
            size = 0.5,
            sd0 = 0.5,
            sd1 = 0.5,
            b0 = 4,
            b1 = 0.8,
            sigma_pop = 0.5)

run_scaling_analysis(df = df,
                modelname = modelname,
                modeltype = modeltype,
                modern = FALSE,
                fit_diagnostics = FALSE,
                scalingCode = scalingCode,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains)

### FOURTH ANALYSIS: EPIGRAPHY #################################################

modelname = "epigraphy"
modeltype = "power_law"

df <- RomanUrban

N <- dim(df)[1]
y <- df$InscriptionCount
x <- log(df$Area)
pop <- log(df$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(df$Province))
K <- length(unique(df$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, K),
            scaling_raw = rep(0.5, K),
            intercept = rep(0, K),
            intercept_raw = rep(0.5, K),
            intercept0 = 0,
            scaling0 = 0,
            size = 0.5,
            sd0 = 0.5,
            sd1 = 0.5,
            b0 = 4,
            b1 = 0.8,
            sigma_pop = 0.5)

run_scaling_analysis(df = df,
                modelname = modelname,
                modeltype = modeltype,
                modern = FALSE,
                fit_diagnostics = TRUE,
                scalingCode = scalingCode,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains)

#### Excluding Zeros ###########################################################

modelname = "epigraphy_nozero"
modeltype = "power_law"

df <- RomanUrban[which(RomanUrban$InscriptionCount > 0),]

N <- dim(df)[1]
y <- df$InscriptionCount
x <- log(df$Area)
pop <- log(df$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(df$Province))
K <- length(unique(df$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(0, K),
            scaling_raw = rep(0.5, K),
            intercept = rep(0, K),
            intercept_raw = rep(0.5, K),
            intercept0 = 0,
            scaling0 = 0,
            size = 0.5,
            sd0 = 0.5,
            sd1 = 0.5,
            b0 = 4,
            b1 = 0.8,
            sigma_pop = 0.5)

run_scaling_analysis(df = df,
                modelname = modelname,
                modeltype = modeltype,
                modern = FALSE,
                fit_diagnostics = FALSE,
                scalingCode = scalingCode,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains)

### FIFTH ANALYSIS: HNWI #######################################################

modelname = "hnwi"
modeltype = "power_law"

N <- dim(global_hnwi)[1]
y <- global_hnwi$Billionaires
pop <- log(global_hnwi$population)

Consts <- list(N = N)

Data <- list(y = y,
            pop = pop)

Inits <- list(scaling = 0,
            intercept = 0,
            size = 1)

run_scaling_analysis(df = global_hnwi,
                modelname = modelname,
                modeltype = modeltype,
                modern = TRUE,
                fit_diagnostics = TRUE,
                scalingCode = scalingCode2,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains)

### SIXTH ANALYSIS: TALL BUILDINGS #############################################

# Here we anlyze another, exclusive global dataset of tall buildings
# These are usually indicative of wealth and and are frequently 
# monumental in the sense that the sheer height/size of these
# buildings has frequently been a display of an individual or
# group's wealth and engineering prowess.

modelname = "tallbuildings"
modeltype = "power_law"

N <- dim(tall_buildings)[1]
y <- tall_buildings$`150 m+ Buildings`
pop <- log(tall_buildings$Population)

Consts <- list(N = N)

Data <- list(y = y,
            pop = pop)

Inits <- list(scaling = 0,
            intercept = 0,
            size = 1)

run_scaling_analysis(df = tall_buildings,
                modelname = modelname,
                modeltype = modeltype,
                modern = TRUE,
                fit_diagnostics = TRUE,
                scalingCode = scalingCode2,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains)

### PLOT SCALING PARAM POSTERIOR DENSITIES #####################################

scaling_samples_all_analyses <- tibble(read.csv(file = "Output/scaling_samples.csv"))
names(scaling_samples_all_analyses)[2] <- "scaling"

# High-contrast color palette with 5 distinct colors
color_palette <- c("#E69F00", 
                "#56B4E9", 
                "#009E73", 
                "#F0E442", 
                "#0072B2",
                "#E69F00", 
                "#56B4E9", 
                "#009E73", 
                "#F0E442", 
                "#0072B2")

# Define pattern types, repeating them for pairs
pattern_types <- c("none", "stripe", "none", "stripe", "none", 
                   "stripe", "none", "stripe", "none", "stripe")

# Create the plot with patterns and colors
ggplot(data = scaling_samples_all_analyses) +
  geom_density_pattern(mapping = aes(x = scaling, 
                                     fill = analysis,
                                     pattern = analysis),
                       pattern_density = 0.1,
                       pattern_fill = "black", 
                       pattern_spacing = 0.02,
                       color = "black", 
                       alpha = 0.8) +
  scale_fill_manual(values = color_palette) +
  scale_pattern_manual(values = pattern_types) +
  geom_vline(xintercept = 0.15, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 0.85, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Posterior Densities for Scaling Parameter",
       x = "Scaling Parameter Value",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("Output/scaling_posteriors.pdf",
        height = 10,
        width = 15,
        units = "cm",
        scale = 2.5,
        device = "pdf")

ggsave("Output/scaling_posteriors.png",
        height = 10,
        width = 15,
        units = "cm",
        scale = 2.5,
        device = "png")

### COLLATE MAIN RESULTS TABLE #################################################

output_file_paths <- list.files("Output/", full.names = TRUE)
posterior_summary_files <- output_file_paths[grep("post_", output_file_paths)]

rsq_csv <- tibble(read.csv(file = "Output/rsquared.csv"))

output_df <- data.frame()

for(j in 1:length(posterior_summary_files)){
        analysis_file_name <- tools::file_path_sans_ext(basename(posterior_summary_files[j]))
        analysis_name <- gsub("post_summary_", "", analysis_file_name)
        post_csv <- tibble(read.csv(file = posterior_summary_files[j]))
        scaling_row_idx <- grep("scaling0$|scaling$", post_csv$X)
        rsq_row_idx <- grep(paste(analysis_name, "$", sep = ""), rsq_csv$'analysis')
        temp_output_df <- post_csv[scaling_row_idx, -1]
        temp_output_df$'analysis' <- analysis_name
        temp_output_df$'r_squared' <- rsq_csv[rsq_row_idx, 'rsq']$rsq
        output_df <- rbind(output_df, temp_output_df)
}
output_df <- select(output_df, analysis, everything(), r_squared)

write.table(output_df, 
        file = "Output/analysis_results_summary.csv",
        row.names = F,
        sep = ",")

### SUPPLEMENTAL: MODEL COMPARISON #############################################
# Here we aim to check whether the power-law model is more appropriate than
# a reasonable alternative, namely a linear-log model.

#### global mcmc params ########################################################
niter = 80000
nburnin = floor(niter * 0.25)
nchains = 4
thin = 3
thin2 = 3

#### FIRST ANALYSIS, ALL MONUMENTS #############################################

modelname = "allmonuments_linlog"
modeltype = "linear_log"

# using full dataset
df <- RomanUrban

N <- dim(df)[1]
y <- df$Monuments
x <- log(df$Area)
pop <- log(df$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(df$Province))
K <- length(unique(df$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(1, K),
            scaling_raw = rep(0.5, K),
            intercept = rep(1, K),
            intercept_raw = rep(0.5, K),
            intercept0 = 1,
            scaling0 = 1,
            size = 1,
            sd0 = 1,
            sd1 = 1,
            b0 = 4,
            b1 = 0.8,
            sigma_pop = 0.5)

run_scaling_analysis(df = df,
                modelname = modelname,
                modeltype = modeltype,
                modern = FALSE,
                fit_diagnostics = TRUE,
                scalingCode = scalingCode_linlog,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains,
                lppd_diff = TRUE)

#### SECOND ANALYSIS, WALLS ONLY ###############################################

modelname = "allwalls_linlog"
modeltype = "lin_log"

df <- RomanUrban[walls_idx,]

N <- dim(df)[1]
y <- df$Monuments
x <- log(df$Area)
pop <- log(df$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(df$Province))
K <- length(unique(df$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(1, K),
            scaling_raw = rep(0.5, K),
            intercept = rep(1, K),
            intercept_raw = rep(0.5, K),
            intercept0 = 1,
            scaling0 = 1,
            size = 1,
            sd0 = 1,
            sd1 = 1,
            b0 = 4,
            b1 = 0.8,
            sigma_pop = 0.5)

run_scaling_analysis(df = df,
                modelname = modelname,
                modeltype = modeltype,
                modern = FALSE,
                fit_diagnostics = TRUE,
                scalingCode = scalingCode_linlog,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains,
                lppd_diff = TRUE)

#### THIRD ANALYSIS, ABOVE GROUND ONLY #########################################

modelname <- "filtmonuments_loglin"
modeltype = "log_lin"

df <- RomanUrban

N <- dim(df)[1]
y <- df$Monuments_filt
x <- log(df$Area)
pop <- log(df$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(df$Province))
K <- length(unique(df$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(1, K),
            scaling_raw = rep(0.5, K),
            intercept = rep(1, K),
            intercept_raw = rep(0.5, K),
            intercept0 = 1,
            scaling0 = 1,
            size = 1,
            sd0 = 1,
            sd1 = 1,
            b0 = 4,
            b1 = 0.8,
            sigma_pop = 0.5)

run_scaling_analysis(df = df,
                modelname = modelname,
                modeltype = modeltype,
                modern = FALSE,
                fit_diagnostics = TRUE,
                scalingCode = scalingCode_linlog,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains,
                lppd_diff = TRUE)

#### FOURTH ANALYSIS, EPIGRAPHIC ###############################################

modelname = "epigraphy_linlog"
modeltype = "lin_log"

df <- RomanUrban

N <- dim(df)[1]
y <- df$InscriptionCount
x <- log(df$Area)
pop <- log(df$pop_est)
# get provinces as integer indeces instead of character/factor levels
v <- as.numeric(as.factor(df$Province))
K <- length(unique(df$Province))

Consts <- list(N = N,
                v = v,
                K = K)

Data <- list(y = y,
            x = x,
            pop = pop)

Inits <- list(scaling = rep(1, K),
            scaling_raw = rep(0.5, K),
            intercept = rep(1, K),
            intercept_raw = rep(0.5, K),
            intercept0 = 1,
            scaling0 = 1,
            size = 1,
            sd0 = 1,
            sd1 = 1,
            b0 = 4,
            b1 = 0.8,
            sigma_pop = 0.5)

run_scaling_analysis(df = df,
                modelname = modelname,
                modeltype = modeltype,
                modern = FALSE,
                fit_diagnostics = TRUE,
                scalingCode = scalingCode_linlog,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains,
                lppd_diff = TRUE)

#### FIFTH ANALYSIS, HNWI ######################################################

modelname = "hnwi_linlog"
modeltype = "lin_log"

N <- dim(global_hnwi)[1]
y <- global_hnwi$Billionaires
pop <- log(global_hnwi$population)

Consts <- list(N = N)

Data <- list(y = y,
            pop = pop)

Inits <- list(scaling = 1,
            intercept = 1,
            size = 0.5)

run_scaling_analysis(df = global_hnwi,
                modelname = modelname,
                modeltype = modeltype,
                modern = TRUE,
                fit_diagnostics = TRUE,
                scalingCode = scalingCode2_linlog,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains,
                lppd_diff = TRUE)

#### SIXTH ANALYSIS, TALL BUILDINGS ############################################

modelname = "tallbuildings_linlog"
modeltype = "lin_log"

N <- dim(tall_buildings)[1]
y <- tall_buildings$`150 m+ Buildings`
pop <- log(tall_buildings$Population)

Consts <- list(N = N)

Data <- list(y = y,
            pop = pop)

Inits <- list(scaling = 1,
            intercept = 1,
            size = 0.5)

run_scaling_analysis(df = tall_buildings,
                modelname = modelname,
                modeltype = modeltype,
                modern = TRUE,
                fit_diagnostics = TRUE,
                scalingCode = scalingCode2_linlog,
                Consts = Consts,
                Data = Data,
                inits = Inits,
                niter = niter,
                thin = thin,
                thin2 = thin2,
                nchains = nchains,
                lppd_diff = TRUE)

#### COMPILE WAIC/LOOIC SUMMARY RESULTS ########################################

# add a column to the waic.csv for looic values

waic_path <- "Output/waic.csv"

if(file.exists(waic_path)){
        waic_df <- read.csv(waic_path, header = TRUE)

        # Get the list of files containing looic values
        file_list <- list.files(path = "Output/", 
                                pattern = "^loo_est_", 
                                full.names = TRUE)
        
          # Initialize an empty list to store the looic dataframes
        looic_list <- list()

        # Loop through each file in the list
        for (file in file_list) {
                # Extract the model name from the file name by removing 
                # "loo_est_" and ".csv"
                model_name <- gsub("^loo_est_|\\.csv$", "", basename(file))
                
                # Read the file while skipping the first line, and add the 
                # 'model' column
                looic_df <- read.csv(file, skip = 1, header = FALSE, 
                                        col.names = c("parameter", 
                                                        "estimate", 
                                                        "se"))
                looic_df$model <- model_name
                
                # Append to the looic_list
                looic_list[[length(looic_list) + 1]] <- looic_df
        }

        # Combine all looic dataframes into one dataframe
        looic_df <- do.call(rbind, looic_list)

        # Select only the relevant rows (parameter == "looic")
        looic_df <- looic_df[looic_df$parameter == "looic", 
                                c("model", "estimate", "se")]
        
        # Rename columns for clarity before merging
        names(looic_df) <- c("model", "looic_estimate", "looic_se")

        # round looic values to significant values
        looic_df$looic_estimate <- round(looic_df$looic_estimate)
        looic_df$looic_se <- round(looic_df$looic_se)

        # Merge waic_df and looic_df on the 'model' column
        merged_df <- merge(waic_df, looic_df, by = "model", all.x = TRUE)

        # new output path
        outpath = "Output/waic_looic_csv"

        # Save the updated dataframe back to waic.csv
        write.csv(merged_df, outpath, row.names = FALSE)
}else {
        warning("No waic.csv file found.")
}

### SUPPLEMENTAL: OUTLIER EFFECTS ##############################################
# Here we have a look at the main models in terms of whether extreme outliers 
# have significantly affected the primary scaling parameter estimates.

#### FIRST ANALYSIS: ALL MOUNUMENTS ############################################

# get the outlier cities identified earlier
outlier_cities_df <- read.csv("Output/city_outliers_allmonuments.csv", 
                        head = TRUE)

# find the indeces again in the RomanUrban dataframe
outliers_idx <- which(RomanUrban$City %in% outlier_cities_df$City)

if(length(outliers_idx) != 0){
        # double check
        if(!all(outlier_cities_df[, 'City'] == RomanUrban[outliers_idx, 'City'])){
                error("City names don't match.")
        }

        # run the analysis
        modelname = "allmonuments_sup"
        modeltype = "power_law"

        # using full dataset
        df <- RomanUrban[-outliers_idx,]

        N <- dim(df)[1]
        y <- df$Monuments
        x <- log(df$Area)
        pop <- log(df$pop_est)
        # get provinces as integer indeces instead of character/factor levels
        v <- as.numeric(as.factor(df$Province))
        K <- length(unique(df$Province))

        Consts <- list(N = N,
                        v = v,
                        K = K)

        Data <- list(y = y,
                x = x,
                pop = pop)

        Inits <- list(scaling = rep(0, K),
                scaling_raw = rep(0.5, K),
                intercept = rep(0, K),
                intercept_raw = rep(0.5, K),
                intercept0 = 0,
                scaling0 = 0,
                size = 0.5,
                sd0 = 0.5,
                sd1 = 0.5,
                b0 = 4,
                b1 = 0.8,
                sigma_pop = 0.5)

        run_scaling_analysis(df = df,
                        modelname = modelname,
                        modeltype = modeltype,
                        modern = FALSE,
                        fit_diagnostics = FALSE,
                        scalingCode = scalingCode,
                        Consts = Consts,
                        Data = Data,
                        inits = Inits,
                        niter = niter,
                        thin = thin,
                        thin2 = thin2,
                        nchains = nchains)
}else{
        warning("No outlier cities in list. Skipping this analysis.")
}

#### SECOND ANALYSIS: HNWI #####################################################

# get the outlier cities identified earlier
outlier_cities_df <- read.csv("Output/city_outliers_hnwi.csv", 
                        head = TRUE)

# find the indeces again in the RomanUrban dataframe
outliers_idx <- which(global_hnwi$City %in% outlier_cities_df$City)
if(length(outliers_idx) != 0){
        # double check
        if(!all(outlier_cities_df[, 'City'] == global_hnwi[outliers_idx, 'City'])){
                error("City names don't match.")
        }

        # run the analysis
        modelname = "hnwi_sup"
        modeltype = "power_law"

        N <- dim(global_hnwi)[1]
        y <- global_hnwi$Billionaires
        pop <- log(global_hnwi$population)

        Consts <- list(N = N)

        Data <- list(y = y,
                pop = pop)

        Inits <- list(scaling = 0,
                intercept = 0,
                size = 1)

        run_scaling_analysis(df = global_hnwi,
                        modelname = modelname,
                        modeltype = modeltype,
                        modern = TRUE,
                        fit_diagnostics = FALSE,
                        scalingCode = scalingCode2,
                        Consts = Consts,
                        Data = Data,
                        inits = Inits,
                        niter = niter,
                        thin = thin,
                        thin2 = thin2,
                        nchains = nchains)
}else{
        warning("No outlier cities in list. Skipping this analysis.")
}

#### THIRD ANALYSIS: TALL BUILDINGS ############################################

# get the outlier cities identified earlier
outlier_cities_df <- read.csv("Output/city_outliers_tallbuildings.csv", 
                        head = TRUE)

# find the indeces again in the RomanUrban dataframe
outliers_idx <- which(tall_buildings$City %in% outlier_cities_df$City)
if(length(outliers_idx) != 0){
        # double check
        if(!all(outlier_cities_df[, 'City'] == tall_buildings[outliers_idx, 'City'])){
                error("City names don't match.")
        }

        # run the analysis
        modelname = "tallbuildings_sup"
        modeltype = "power_law"

        N <- dim(tall_buildings)[1]
        y <- tall_buildings$`150 m+ Buildings`
        pop <- log(tall_buildings$Population)

        Consts <- list(N = N)

        Data <- list(y = y,
                pop = pop)

        Inits <- list(scaling = 0,
                intercept = 0,
                size = 1)

        run_scaling_analysis(df = tall_buildings,
                        modelname = modelname,
                        modeltype = modeltype,
                        modern = TRUE,
                        fit_diagnostics = FALSE,
                        scalingCode = scalingCode2,
                        Consts = Consts,
                        Data = Data,
                        inits = Inits,
                        niter = niter,
                        thin = thin,
                        thin2 = thin2,
                        nchains = nchains)
}else{
        warning("No outlier cities in list. Skipping this analysis.")
}