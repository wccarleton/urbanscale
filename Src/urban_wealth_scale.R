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

### MAIN BAYESIAN/NIMBLE MODEL #################################################

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

# set up a Nimble model
scalingCode <- nimbleCode({
    # monument scaling params
    intercept0 ~ dnorm(mean = 0, sd = 5)
    sd0 ~ dexp(rate = 1)
    scaling0 ~ dnorm(mean = 0, sd = 5)
    sd1 ~ dexp(rate = 1)
    for(k in 1:K){
        intercept_raw[k] ~ dnorm(0, 1)
        intercept[k] <- intercept0 + intercept_raw[k] * sd0
        scaling_raw[k] ~ dnorm(0, 1)
        scaling[k] <- scaling0 + scaling_raw[k] * sd1
    }

    # prior for negbinom size parameter (dispersion)
    size ~ dexp(rate = 1)

    # population--area linking params
    b0 ~ dnorm(mean = 0, sd = 10)
    b1 ~ dnorm(mean = 0, sd = 10)
    sigma_pop ~ dexp(rate = 0.5)

    # main model
    for(n in 1:N){
        pop_mu[n] <- b0 + b1 * x[n]
        pop[n] ~ dnorm(mean = pop_mu[n], sd = sigma_pop)
        log_mu[n] <- intercept[v[n]] + scaling[v[n]] * pop[n]
        mu[n] <- exp(log_mu[n])
        p[n] <- size / (size + mu[n])
        y[n] ~ dnegbin(prob = p[n], size = size)
        y_hat[n] <- (size * (1 - p[n])) / p[n]
    }
})

# set common mcmc params
niter <- 100000
nburnin <- floor(niter * 0.25)
thin <- 10
nchains = 4

# these initial values will be used in the first 8 Nimble models, but
# in each case the K variable can be different owing to different subsetting
# between runs. In each script portion below where a Nimble model is going to be
# run, the 'NA' elements of this list are replaced with the relevant value.
Inits <- list(scaling = NA,
            scaling_raw = NA,
            intercept = NA,
            intercept_raw = NA,
            intercept0 = 0,
            scaling0 = 0,
            size = 1,
            sd0 = 1,
            sd1 = 1,
            b0 = 0,
            b1 = 0,
            sigma_pop = 1)

### UTILITY FUNCTIONS ##########################################################

# Function to create trace plots with mode switch for stacked or faceted visualization
stacked_traceplot <- function(mcmc_obj, params, mode = "facet", thin = 1) {
  # Check if the input is an mcmc.list or a single mcmc object
  if (is.mcmc.list(mcmc_obj)) {
        mcmc_df <- do.call(rbind, lapply(mcmc_obj, as.data.frame))
        # Thinning the chains
        mcmc_df <- mcmc_df[seq(1, nrow(mcmc_df), by = thin), ]
        # Add iteration numbers for each chain
        mcmc_df$Iteration <- rep(seq(1, nrow(mcmc_obj[[1]]), by = thin), times = length(mcmc_obj))
        # Add a Chain identifier
        mcmc_df$Chain <- rep(seq_along(mcmc_obj), each = length(seq(1, nrow(mcmc_obj[[1]]), by = thin)))
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
    stop("The following parameters are missing in the MCMC output: ", paste(missing_params, collapse = ", "))
  }

  # Transform data to long format for specified parameters
  long_mcmc <- pivot_longer(mcmc_df, 
                            cols = all_of(params), 
                            names_to = "Parameter", 
                            values_to = "Sample")

  # Plotting based on mode selection
  if (mode == "facet") {
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

### FIRST ANALYSIS: ALL MONUMENTS ##############################################

modelname = "allmonuments"

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

Inits$scaling <- rep(0, K)
Inits$intercept <- rep(0, K)
Inits$scaling_raw <- rep(0, K)
Inits$intercept_raw <- rep(0, K)

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
                "sd0",
                "sd1",
                "size",
                "scaling", 
                "intercept",
                "b0", 
                "b1",
                "sigma_pop",
                "y_hat")
mcmc_config$monitors <- params_to_track

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

# Run the MCMC
mcmc_out <- runMCMC(compiled_mcmc, 
                niter = niter, 
                nburnin = nburnin, 
                thin = thin, 
                nchains = nchains, 
                samplesAsCodaMCMC = TRUE)

# Trace plots for key parameters
top_lvl_param_names <- c("intercept0", 
                        "scaling0",
                        "sd0",
                        "sd1", 
                        "b0", 
                        "b1",
                        "sigma_pop",
                        "size")

tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

ggsave(filename = "Output/tplots_allmonuments.pdf", 
        plot = tplot, 
        device = "pdf")

# and now to look at variability in scaling parameter among provinces
if(is.mcmc.list(mcmc_out)){
        province_scaling_names <- colnames(mcmc_out[[1]])[grep("scaling\\[", colnames(mcmc_out[[1]]))]
}else{
        province_scaling_names <- colnames(mcmc_out)[grep("scaling\\[", colnames(mcmc_out))]
}

tplot_provinces <- stacked_traceplot(mcmc_out, province_scaling_names, mode = "stacked", thin = 10)

ggsave(filename = "Output/tplots_allmonuments_provinces.pdf", 
        plot = tplot_provinces, 
        device = "pdf")

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

output_scaling(mcmc_out = mcmc_out,
                modelname = modelname,
                parameter = "scaling0",
                outpath = "Output/scaling_samples.csv")

#### Exluding Zeros ############################################################

# model name for paths
modelname <- "allmonuments_nozero"

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

Inits$scaling <- rep(0, K)
Inits$intercept <- rep(0, K)
Inits$scaling_raw <- rep(0, K)
Inits$intercept_raw <- rep(0, K)

# Create the Nimble model
scalingModel <- nimbleModel(code = scalingCode,
                            name = "allmonuments_nozero",
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
                "sd0",
                "sd1",
                "size",
                "scaling", 
                "intercept",
                "b0", 
                "b1",
                "sigma_pop",
                "y_hat")
mcmc_config$monitors <- params_to_track

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

# Run the MCMC
mcmc_out <- runMCMC(compiled_mcmc, 
                niter = niter, 
                nburnin = nburnin, 
                thin = thin, 
                nchains = nchains, 
                samplesAsCodaMCMC = TRUE)

# Trace plots for key parameters
top_lvl_param_names <- c("intercept0", 
                        "scaling0",
                        "sd0",
                        "sd1", 
                        "b0", 
                        "b1",
                        "sigma_pop",
                        "size")
tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

ggsave(filename = paste("Output/tplots_", modelname, ".pdf", sep = ""), 
        plot = tplot, 
        device = "pdf")

# and now to look at variability among provinces
if(is.mcmc.list(mcmc_out)){
        province_scaling_names <- colnames(mcmc_out[[1]])[grep("scaling\\[", colnames(mcmc_out[[1]]))]
}else{
        province_scaling_names <- colnames(mcmc_out)[grep("scaling\\[", colnames(mcmc_out))]
}
tplot_provinces <- stacked_traceplot(mcmc_out, province_scaling_names, mode = "stacked", thin = 10)


ggsave(filename = paste("Output/tplots_", modelname, "_provinces.pdf", sep = ""), 
        plot = tplot_provinces, 
        device = "pdf")

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
                        modelname = modelname,
                        outfolder = "Output/")

# pseudo r-sqaured
output_rsquared(y = y,
                y_hat_idx = y_hat_idx,
                mcmc_out = mcmc_out,
                modelname = modelname,
                thin = nchains)

# exract scaling parameter chain and save to csv for plotting all
# of them together at the end

output_scaling(mcmc_out = mcmc_out,
                modelname = modelname,
                parameter = "scaling0",
                outpath = "Output/scaling_samples.csv")

### SECOND ANALYSIS: WALLED ONLY ###############################################

modelname = "allwalls"

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

Inits$scaling <- rep(0, K)
Inits$intercept <- rep(0, K)
Inits$scaling_raw <- rep(0, K)
Inits$intercept_raw <- rep(0, K)

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

# Select parameters to track
params_to_track <- c("intercept0",
                "scaling0",
                "sd0",
                "sd1",
                "scaling", 
                "intercept",
                "size",
                "b0", 
                "b1",
                "sigma_pop",
                "y_hat")
mcmc_config$monitors <- params_to_track

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

# Run the MCMC
mcmc_out <- runMCMC(compiled_mcmc, 
                niter = niter, 
                nburnin = nburnin, 
                thin = thin, 
                nchains = nchains, 
                samplesAsCodaMCMC = TRUE)

# Trace plots for key parameters
top_lvl_param_names <- c("intercept0", 
                        "scaling0",
                        "sd0",
                        "sd1", 
                        "b0", 
                        "b1",
                        "sigma_pop",
                        "size")
tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

ggsave(filename = paste("Output/tplots_", modelname, ".pdf", sep = ""), 
        plot = tplot, 
        device = "pdf")

# and now to look at variability among provinces
if(is.mcmc.list(mcmc_out)){
        province_scaling_names <- colnames(mcmc_out[[1]])[grep("scaling\\[", colnames(mcmc_out[[1]]))]
}else{
        province_scaling_names <- colnames(mcmc_out)[grep("scaling\\[", colnames(mcmc_out))]
}
tplot_provinces <- stacked_traceplot(mcmc_out, province_scaling_names, mode = "stacked", thin = 10)

ggsave(filename = paste("Output/tplots_", modelname, "_provinces.pdf", sep = ""), 
        plot = tplot_provinces, 
        device = "pdf")

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
                        modelname = modelname,
                        outfolder = "Output/")

# pseudo r-sqaured
output_rsquared(y = y,
                y_hat_idx = y_hat_idx,
                mcmc_out = mcmc_out,
                modelname = modelname,
                thin = nchains)

output_scaling(mcmc_out = mcmc_out,
                modelname = modelname,
                parameter = "scaling0",
                outpath = "Output/scaling_samples.csv")

#### Excluding Zeros ###########################################################

modelname = "allwalls_nozero"

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

Inits$scaling <- rep(0, K)
Inits$intercept <- rep(0, K)
Inits$scaling_raw <- rep(0, K)
Inits$intercept_raw <- rep(0, K)

# Create the Nimble model
scalingModel <- nimbleModel(code = scalingCode,
                            name = "allwalls_nozero",
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
                "sd0",
                "sd1",
                "size",
                "scaling", 
                "intercept",
                "b0", 
                "b1",
                "sigma_pop",
                "y_hat")
mcmc_config$monitors <- params_to_track

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

# Run the MCMC
mcmc_out <- runMCMC(compiled_mcmc, 
                niter = niter, 
                nburnin = nburnin, 
                thin = thin, 
                nchains = nchains, 
                samplesAsCodaMCMC = TRUE)

# Trace plots for key parameters
top_lvl_param_names <- c("intercept0", 
                        "scaling0",
                        "sd0",
                        "sd1", 
                        "b0", 
                        "b1",
                        "sigma_pop",
                        "size")
tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

ggsave(filename = paste("Output/tplots_", modelname, ".pdf", sep = ""), 
        plot = tplot, 
        device = "pdf")

# and now to look at variability among provinces
if(is.mcmc.list(mcmc_out)){
        province_scaling_names <- colnames(mcmc_out[[1]])[grep("scaling\\[", colnames(mcmc_out[[1]]))]
}else{
        province_scaling_names <- colnames(mcmc_out)[grep("scaling\\[", colnames(mcmc_out))]
}
tplot_provinces <- stacked_traceplot(mcmc_out, province_scaling_names, mode = "stacked", thin = 10)

ggsave(filename = paste("Output/tplots_", modelname, "_provinces.pdf", sep = ""), 
        plot = tplot_provinces, 
        device = "pdf")

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
                        modelname = modelname,
                        outfolder = "Output/")

# pseudo r-sqaured
output_rsquared(y = y,
                y_hat_idx = y_hat_idx,
                mcmc_out = mcmc_out,
                modelname = modelname,
                thin = nchains)

# output scaling parameter samples
output_scaling(mcmc_out = mcmc_out,
                modelname = modelname,
                parameter = "scaling0",
                outpath = "Output/scaling_samples.csv")

### THIRD ANALYSIS: ABOVE GROUND ONLY ##########################################

modelname <- "filtmonuments"

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

Inits$scaling <- rep(0, K)
Inits$intercept <- rep(0, K)
Inits$scaling_raw <- rep(0, K)
Inits$intercept_raw <- rep(0, K)

# Create the Nimble model
scalingModel <- nimbleModel(code = scalingCode,
                            name = "filtmonuments",
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
mcmc_config$addSampler(target = c('b0', 'b1'), type = 'RW_block')#'AF_slice')

# Optionally, block sampling for each group's intercept and scaling
for (k in 1:Consts$K) {
  mcmc_config$removeSamplers(c(paste0('intercept[', k, ']'), paste0('scaling[', k, ']')))
  mcmc_config$addSampler(target = c(paste0('intercept[', k, ']'), paste0('scaling[', k, ']')), type = 'AF_slice')
}

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
mcmc_config$monitors <- params_to_track

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

# Run the MCMC
mcmc_out <- runMCMC(compiled_mcmc, 
                niter = niter, 
                nburnin = nburnin, 
                thin = thin, 
                nchains = nchains, 
                samplesAsCodaMCMC = TRUE)

# Trace plots for key parameters
top_lvl_param_names <- c("intercept0", 
                        "scaling0",
                        "sd0",
                        "sd1", 
                        "b0", 
                        "b1",
                        "sigma_pop",
                        "size")
tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

ggsave(filename = paste("Output/tplots_", modelname, ".pdf", sep = ""), 
        plot = tplot, 
        device = "pdf")

# and now to look at variability among provinces
if(is.mcmc.list(mcmc_out)){
        province_scaling_names <- colnames(mcmc_out[[1]])[grep("scaling\\[", colnames(mcmc_out[[1]]))]
}else{
        province_scaling_names <- colnames(mcmc_out)[grep("scaling\\[", colnames(mcmc_out))]
}
tplot_provinces <- stacked_traceplot(mcmc_out, province_scaling_names, mode = "stacked", thin = 10)

ggsave(filename = paste("Output/tplots_", modelname, "_provinces.pdf", sep = ""), 
        plot = tplot_provinces, 
        device = "pdf")

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
                        modelname = modelname,
                        outfolder = "Output/")

# pseudo r-sqaured
output_rsquared(y = y,
                y_hat_idx = y_hat_idx,
                mcmc_out = mcmc_out,
                modelname = modelname,
                thin = nchains)

# output scaling parameter samples
output_scaling(mcmc_out = mcmc_out,
                modelname = modelname,
                parameter = "scaling0",
                outpath = "Output/scaling_samples.csv")

#### Excluding Zeros ###########################################################

modelname = "filtmonuments_nozero"

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

Inits$scaling <- rep(0, K)
Inits$intercept <- rep(0, K)
Inits$scaling_raw <- rep(0, K)
Inits$intercept_raw <- rep(0, K)

# Create the Nimble model
scalingModel <- nimbleModel(code = scalingCode,
                            name = "filtmonuments_nozero",
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
                "sd0",
                "sd1",
                "size",
                "scaling", 
                "intercept",
                "b0", 
                "b1",
                "sigma_pop",
                "y_hat")
mcmc_config$monitors <- params_to_track

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

# Run the MCMC
mcmc_out <- runMCMC(compiled_mcmc, 
                niter = niter, 
                nburnin = nburnin, 
                thin = thin, 
                nchains = nchains, 
                samplesAsCodaMCMC = TRUE)

# Trace plots for key parameters
top_lvl_param_names <- c("intercept0", 
                        "scaling0",
                        "sd0",
                        "sd1", 
                        "b0", 
                        "b1",
                        "sigma_pop",
                        "size")
tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

ggsave(filename = paste("Output/tplots_", modelname, ".pdf", sep = ""), 
        plot = tplot, 
        device = "pdf")

# and now to look at variability among provinces
if(is.mcmc.list(mcmc_out)){
        province_scaling_names <- colnames(mcmc_out[[1]])[grep("scaling\\[", colnames(mcmc_out[[1]]))]
}else{
        province_scaling_names <- colnames(mcmc_out)[grep("scaling\\[", colnames(mcmc_out))]
}
tplot_provinces <- stacked_traceplot(mcmc_out, province_scaling_names, mode = "stacked", thin = 10)

ggsave(filename = paste("Output/tplots_", modelname, "_provinces.pdf", sep = ""), 
        plot = tplot_provinces, 
        device = "pdf")

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
                        modelname = modelname,
                        outfolder = "Output/")

# pseudo r-sqaured
output_rsquared(y = y,
                y_hat_idx = y_hat_idx,
                mcmc_out = mcmc_out,
                modelname = modelname,
                thin = nchains)

# output scaling parameter samples
output_scaling(mcmc_out = mcmc_out,
                modelname = modelname,
                parameter = "scaling0",
                outpath = "Output/scaling_samples.csv")

### FOURTH ANALYSIS: EPIGRAPHY #################################################

modelname = "epigraphy"

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

Inits$scaling <- rep(0, K)
Inits$intercept <- rep(0, K)
Inits$scaling_raw <- rep(0, K)
Inits$intercept_raw <- rep(0, K)

# Create the Nimble model
scalingModel <- nimbleModel(code = scalingCode,
                            name = "epigraphy",
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
                "sd0",
                "sd1",
                "size",
                "scaling", 
                "intercept",
                "b0", 
                "b1",
                "sigma_pop",
                "y_hat")
mcmc_config$monitors <- params_to_track

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

# Run the MCMC
mcmc_out <- runMCMC(compiled_mcmc, 
                niter = niter, 
                nburnin = nburnin, 
                thin = thin, 
                nchains = nchains, 
                samplesAsCodaMCMC = TRUE)

# Trace plots for key parameters
top_lvl_param_names <- c("intercept0", 
                        "scaling0",
                        "sd0",
                        "sd1", 
                        "b0", 
                        "b1",
                        "sigma_pop",
                        "size")
tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

ggsave(filename = paste("Output/tplots_", modelname, ".pdf", sep = ""), 
        plot = tplot, 
        device = "pdf")

# and now to look at variability among provinces
if(is.mcmc.list(mcmc_out)){
        province_scaling_names <- colnames(mcmc_out[[1]])[grep("scaling\\[", colnames(mcmc_out[[1]]))]
}else{
        province_scaling_names <- colnames(mcmc_out)[grep("scaling\\[", colnames(mcmc_out))]
}
tplot_provinces <- stacked_traceplot(mcmc_out, province_scaling_names, mode = "stacked", thin = 10)

ggsave(filename = paste("Output/tplots_", modelname, "_provinces.pdf", sep = ""), 
        plot = tplot_provinces, 
        device = "pdf")

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
                        modelname = modelname,
                        outfolder = "Output/")

# pseudo r-sqaured
output_rsquared(y = y,
                y_hat_idx = y_hat_idx,
                mcmc_out = mcmc_out,
                modelname = modelname,
                thin = nchains)

# output scaling parameter samples
output_scaling(mcmc_out = mcmc_out,
                modelname = modelname,
                parameter = "scaling0",
                outpath = "Output/scaling_samples.csv")

#### Excluding Zeros ###########################################################

modelname = "epigraphy_nozero"

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

Inits$scaling <- rep(0, K)
Inits$intercept <- rep(0, K)
Inits$scaling_raw <- rep(0, K)
Inits$intercept_raw <- rep(0, K)

# Create the Nimble model
scalingModel <- nimbleModel(code = scalingCode,
                            name = "epigraphy_nozero",
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
                "sd0",
                "sd1",
                "size",
                "scaling", 
                "intercept",
                "b0", 
                "b1",
                "sigma_pop",
                "y_hat")
mcmc_config$monitors <- params_to_track

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

# Run the MCMC
mcmc_out <- runMCMC(compiled_mcmc, 
                niter = niter, 
                nburnin = nburnin, 
                thin = thin, 
                nchains = nchains, 
                samplesAsCodaMCMC = TRUE)

# Trace plots for key parameters
top_lvl_param_names <- c("intercept0", 
                        "scaling0",
                        "sd0",
                        "sd1", 
                        "b0", 
                        "b1",
                        "sigma_pop",
                        "size")
tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

ggsave(filename = paste("Output/tplots_", modelname, ".pdf", sep = ""), 
        plot = tplot, 
        device = "pdf")

# and now to look at variability among provinces
if(is.mcmc.list(mcmc_out)){
        province_scaling_names <- colnames(mcmc_out[[1]])[grep("scaling\\[", colnames(mcmc_out[[1]]))]
}else{
        province_scaling_names <- colnames(mcmc_out)[grep("scaling\\[", colnames(mcmc_out))]
}
tplot_provinces <- stacked_traceplot(mcmc_out, province_scaling_names, mode = "stacked", thin = 10)

ggsave(filename = paste("Output/tplots_", modelname, "_provinces.pdf", sep = ""), 
        plot = tplot_provinces, 
        device = "pdf")

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
                        modelname = modelname,
                        outfolder = "Output/")

# pseudo r-sqaured
output_rsquared(y = y,
                y_hat_idx = y_hat_idx,
                mcmc_out = mcmc_out,
                modelname = modelname,
                thin = nchains)

# output scaling parameter samples
output_scaling(mcmc_out = mcmc_out,
                modelname = modelname,
                parameter = "scaling0",
                outpath = "Output/scaling_samples.csv")

### FIFTH ANALYSIS: HNWI #######################################################

modelname = "hnwi"

# with this cleaned/updated data, run the same Bayesian model, though of course 
# not divided by Roman provinces, which simplifies the model substantially

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

N <- dim(global_hnwi)[1]
y <- global_hnwi$Billionaires
pop <- log(global_hnwi$population)

Consts <- list(N = N)

Data <- list(y = y,
            pop = pop)

Inits <- list(scaling = 0,
            intercept = 0,
            size = 1)

scalingModel2 <- nimbleModel(code = scalingCode2,
                name = "hnwi",
                constants = Consts,
                data = Data,
                inits = Inits)

# Compile the model
compiled_model <- compileNimble(scalingModel2)

# Configure the MCMC
mcmc_config <- configureMCMC(scalingModel2)

# Replace samplers for correlated parameters
# Block sampling for intercept0 and scaling0
mcmc_config$removeSamplers(c('intercept', 'scaling'))
mcmc_config$addSampler(target = c('intercept', 'scaling'), type = 'AF_slice')

# Parameters to track
params_to_track <- c("intercept", 
                    "scaling",
                    "size",
                    "y_hat")

mcmc_config$monitors <- params_to_track

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

# Run the MCMC
mcmc_out <- runMCMC(compiled_mcmc, 
                niter = niter, 
                nburnin = nburnin, 
                thin = thin, 
                nchains = nchains, 
                samplesAsCodaMCMC = TRUE)

# Trace plots for key parameters
top_lvl_param_names <- c("intercept", "scaling", "size")
tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

ggsave(filename = paste("Output/tplots_", modelname, ".pdf", sep = ""), 
        plot = tplot, 
        device = "pdf")

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
                        modelname = modelname,
                        outfolder = "Output/")

# pseudo r-sqaured
output_rsquared(y = y,
                y_hat_idx = y_hat_idx,
                mcmc_out = mcmc_out,
                modelname = modelname,
                thin = nchains)

# output scaling parameter samples
output_scaling(mcmc_out = mcmc_out,
                modelname = modelname,
                parameter = "scaling",
                outpath = "Output/scaling_samples.csv")

### SIXTH ANALYSIS: TALL BUILDINGS #############################################

# Here we anlyze another, exclusive global dataset of tall buildings
# These are usually indicative of wealth and and are frequently 
# monumental in the sense that the sheer height/size of these
# buildings has frequently been a display of an individual or
# group's wealth and engineering prowess.

modelname = "tallbuildings"

N <- dim(tall_buildings)[1]
y <- tall_buildings$`150 m+ Buildings`
pop <- log(tall_buildings$Population)

Consts <- list(N = N)

Data <- list(y = y,
            pop = pop)

Inits <- list(scaling = 0,
            intercept = 0,
            size = 1)

scalingModel2 <- nimbleModel(code = scalingCode2,
                name = "tallbuildings",
                constants = Consts,
                data = Data,
                inits = Inits)

# Compile the model
compiled_model <- compileNimble(scalingModel2)

# Configure the MCMC
mcmc_config <- configureMCMC(scalingModel2)

# Replace samplers for correlated parameters
# Block sampling for intercept0 and scaling0
mcmc_config$removeSamplers(c('intercept', 'scaling'))
mcmc_config$addSampler(target = c('intercept', 'scaling'), type = 'AF_slice')

# Parameters to track
params_to_track <- c("intercept", 
                    "scaling",
                    "size",
                    "y_hat")

mcmc_config$monitors <- params_to_track

# Build and compile the MCMC
mcmc_object <- buildMCMC(mcmc_config)
compiled_mcmc <- compileNimble(mcmc_object, project = compiled_model)

# Run the MCMC
mcmc_out <- runMCMC(compiled_mcmc, 
                niter = niter, 
                nburnin = nburnin, 
                thin = thin, 
                nchains = nchains, 
                samplesAsCodaMCMC = TRUE)

# Trace plots for key parameters
top_lvl_param_names <- c("intercept", "scaling", "size")
tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

ggsave(filename = paste("Output/tplots_", modelname, ".pdf", sep = ""), 
        plot = tplot, 
        device = "pdf")

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
                        modelname = modelname,
                        outfolder = "Output/")

# pseudo r-sqaured
output_rsquared(y = y,
                y_hat_idx = y_hat_idx,
                mcmc_out = mcmc_out,
                modelname = modelname,
                thin = nchains)

# output scaling parameter samples
output_scaling(mcmc_out = mcmc_out,
                modelname = modelname,
                parameter = "scaling",
                outpath = "Output/scaling_samples.csv")

### PLOT SCALING PARAM POSTERIOR DENSITIES #####################################

scaling_samples_all_analyses <- tibble(read.csv(file = "Output/scaling_samples.csv"))

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

### Collate Main Results Table #################################################

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
