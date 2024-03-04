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

# get sheet names for ctbuh data
data_path <- "Data/ctbuh.xlsx"
sheets <- excel_sheets(data_path)
sheets

# pull in raw Excel data
ctbuh <- read_excel(data_path, 
                    sheet = sheets[1])

ctbuh_complete <- drop_na(ctbuh, Population)

###
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


### PLOTS
# scatter plots
plt1 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = log(Area), y = log(Monuments))) +
                labs(title = "Roman Monuments") +
                theme_classic()
plt2 <- ggplot(RomanUrban[walls_idx, ]) +
                geom_point(mapping = aes(x = log(Area), y = log(Monuments))) +
                labs(title = "Roman Monuments (filtered)") +
                theme_classic()
plt3 <- ggplot(ctbuh_complete) +
                geom_point(mapping = aes(x = log(Population), y = log(h150m))) +
                labs(title = "Modern Tall Buildings") +
                theme_classic()
plt4 <- ggplot(global_hnwi) +
                geom_point(mapping = aes(x = log(population), y = log(Billionaires))) +
                labs(title = "Modern Wealthy Elite") +
                theme_classic()
plt5 <- ggplot(RomanUrban) +
                geom_point(mapping = aes(x = log(Area), y = log(InscriptionCount))) +
                labs(title = "Roman Dedication Inscriptions") +
                theme_classic()

plt1 + plt5 + plt3 + plt4 + plot_layout(ncol = 2, byrow = TRUE)

#layout = c(area(1,1,1,2),
#        area(1,3,1,4),
#        area(2,2,2,3))

#plot(layout)

#plt1 + plt3 + plt4 + plot_layout(design = layout)

ggsave("Output/point_scatters_wealthy_elite.png",
        height = 15,
        width = 15,
        units = "cm",
        scale = 1.5,
        device = "png")

# urban wealth
data_path <- "Data/hnwi_by_city.xlsx"
sheets <- excel_sheets(data_path)
sheets