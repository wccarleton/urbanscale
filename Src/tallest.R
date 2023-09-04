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
tallest_ctbuh$un_pop <- NA

n <- dim(tallest_ctbuh)[1]
for(j in 1:n){
    pop_idx <- grep(tallest_ctbuh$City[j], 
                    un_population$'Urban Agglomeration')
    if(any(pop_idx)){
        tallest_ctbuh$un_pop[j] <- un_population$'2020'[pop_idx] * 1000
    }else{
        warning(paste("city name not found for iteration", j))
    }
}

zero_idx <- tallest_ctbuh$m200 == 0

summary(lm(log(m150) ~ log(Population),
            data = tallest_ctbuh[!zero_idx,]))
