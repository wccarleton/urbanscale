# libraries
library(nimble)
library(tidyverse)
library(ggplot2)
library(readxl)
library(patchwork)
library(coda)
library(dplyr)

# data wrangling
# pull in the data from Excel sheets and CSVs as needed

# first the population data
# get sheet names
data_path <- "Data/WUP2018-F12-Cities_Over_300K.xlsx"
sheets <- excel_sheets(data_path)
sheets

# pull in raw Excel data
un_population <- read_excel(data_path, 
                    sheet = sheets[1],
                    skip = 16)

# now the tallest buildings data
data_path <- "Data/tallest_buildings.xlsx"
sheets <- excel_sheets(data_path)
sheets

tallest <- read_excel(data_path, 
                    sheet = sheets[1])

# count tallest per city
tallest_table <- table(tallest$City)

tallest_count <- data.frame(city = names(tallest_table),
                            n_tallest = as.vector(tallest_table))

# merge population data
# first, isolate city names from other annotations in tallest_table
names_only <- strsplit(tallest_count$city, split = " (", fixed = T)
l = length(names_only)[1] * 2
names_only <- unlist(names_only)[seq(from = 1, 
                    to = l, 
                    by = 2)]
tallest_count$city_name <- names_only
tallest_count$population <- NA

# need to use grep, so the easiest solution is a simple loop
# at this point. this may be fragile, though, if there are 
# multiple grep matches between tallest_count$city_name and 
# un_population$'Urban Aggomeration'
n <- dim(tallest_count)[1]
for(j in 1:n){
    pop_idx <- grep(tallest_count$city_name[j], 
                    un_population$'Urban Agglomeration')
    if(any(pop_idx)){
        tallest_count$population[j] <- un_population$'2020'[pop_idx]
    }else{
        warning(paste("city name not found for iteration", j))
    }
}

# data on counts compiled by CTBUH

data_path <- "Data/tallest_counts_ctbuh.xlsx"
sheets <- excel_sheets(data_path)
sheets

tallest_ctbuh <- read_excel(data_path, 
                    sheet = sheets[2])

tallest_ctbuh <- drop_na(tallest_ctbuh, all_of('Rank'))

names(tallest_ctbuh)[5:7] <- c("m150","m200","m300")
tallest_ctbuh$geonames_pop <- NA

# population data

data_path <- "Data/worldcities.xlsx"
sheets <- excel_sheets(data_path)
sheets

geonames_population <- read_excel(data_path, 
                    sheet = sheets[1])
                    #n_max = 1530,
                    #guess_max = 1530)

city_proper_idx <- grep('City prop', un_population$'City type')

geonames_population[grep(tallest_ctbuh$City[dupes[2]], 
            geonames_population$'ASCII Name', 
            ignore.case = T),] #'Population']


n <- dim(tallest_ctbuh)[1]
dupes <- c()
for(j in 1:n){
    pattern <- paste("\\b", tallest_ctbuh$City[j], "\\b", sep = "")
    pop_idx <- grep(pattern, 
                    geonames_population$'ASCII Name',
                    ignore.case = TRUE)
    if(any(pop_idx)){
        if(length(pop_idx) > 1){
            message(paste("duplicate(s) found for row", j, 'of', n))
            dupes <- c(dupes, j)
            print_cols <- c('ASCII Name', 'Country name EN', 'Alternate Names', 'Population')
            print(geonames_population[pop_idx, print_cols], n = Inf)
            user_choice <- readline("select an option: ")
            selection_idx <- as.numeric(unlist(strsplit(user_choice, ",")))
            tallest_ctbuh$geonames_pop[j] <- geonames_population$Population[pop_idx[selection_idx]]
        }else{
            tallest_ctbuh$geonames_pop[j] <- geonames_population$Population[pop_idx]
        }
    }else{
        warning(paste("city name not found for row ", j))
    }
}


n <- dim(tallest_ctbuh)[1]
dupes <- c()
for(j in 1:n){
    pattern <- paste("\\b", tallest_ctbuh$City[j], "\\b", sep = "")
    pop_idx <- grep(pattern, 
                    geonames_population$'city_ascii',
                    ignore.case = TRUE)
    if(any(pop_idx)){
        if(length(pop_idx) > 1){
            message(paste("duplicate(s) found for row", j, 'of', n))
            dupes <- c(dupes, j)
            print_cols <- c('city_ascii', 'country', 'population')
            print(geonames_population[pop_idx, print_cols], n = Inf)
            user_choice <- readline("select an option: ")
            selection_idx <- as.numeric(unlist(strsplit(user_choice, ",")))
            tallest_ctbuh$geonames_pop[j] <- geonames_population$population[pop_idx[selection_idx]]
        }else{
            tallest_ctbuh$geonames_pop[j] <- geonames_population$population[pop_idx]
        }
    }else{
        warning(paste("city name not found for row ", j))
    }
}

tallest_ctbuh[which(tallest_ctbuh$geonames_pop == 0), 'geonames_pop'] <- NA
tallest_ctbuh$check_me[is.na(tallest_ctbuh$geonames_pop)] <- 1

tallest_ctbuh$pop_combined <- tallest_ctbuh$Population
na_idx <- which(is.na(tallest_ctbuh$Population))
tallest_ctbuh[na_idx, 'pop_combined'] <- tallest_ctbuh[na_idx, 'geonames_pop']

check_these_idx <- which(is.na(tallest_ctbuh$Population) & 
                                tallest_ctbuh$check_me == 1)

zero_idx <- which(tallest_ctbuh$m200 == 0)

skyscrape_scale <- lm(log(m150)~log(pop_combined), 
                        data = tallest_ctbuh[-zero_idx,])
