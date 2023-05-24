# libraries
library(nimble)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(readxl)
library(patchwork)

# data wrangling
# pull in the data from Excel sheets and CSVs as needed

# get sheet names
data_path <- "./Data/Hanson2016_CitiesDatabase_OxREP.xlsx"
sheets <- excel_sheets(data_path)
sheets

# pull in raw Excel data
Cities <- read_excel(data_path, 
                    sheet = sheets[1])

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

MonumentCount_all <- data.frame(Cities = names(monument_counts),
                            Monuments = as.vector(monument_counts))

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

# identify cases where Area is determined by walls
walls_idx <- grep("Walls|walls", 
                RomanUrban$Basis)

# isolate only relevant columns for further analyses
col_idx <- grep("Primary\ Key|Area|Monuments|Monuments_filt|Temples|NotTemples", 
                colnames(RomanUrban))

RomanUrban <- RomanUrban[, col_idx]
names(RomanUrban)[1] <- "City"

#isolate only case where we have area estimates
RomanUrban <- subset(RomanUrban, !is.na(Area))

# get sample size
n_roman <- dim(RomanUrban)[1]
n_roman

# now the UK church data

Churches <- read.csv("Data/churches.csv")

# obvious data checks...
sum(Churches$churches == 0)
any(is.na(Churches$churches))
any(is.na(Churches$area))

n_churches <- dim(Churches)[1]

# from here on these dataframes are the main datasets

# initial plot, all four datasets
plot(y = log(RomanUrban$Monuments),
    x = log(RomanUrban$Area))

p1 <- ggplot(RomanUrban) +
        geom_point(mapping = aes(x = log(Area), y = log(Monuments)),
                    colour = "black",
                    alpha = 0.7) +
        labs(title = "All Cities") +
        theme_minimal()

p2 <- ggplot(RomanUrban[walls_idx, ]) +
        geom_point(mapping = aes(x = log(Area), y = log(Monuments)),
                    colour = "red",
                    alpha = 0.7) +
        labs(title = "Walled Cities") +
        theme_minimal()

p3 <- ggplot(RomanUrban[walls_idx, ]) +
        geom_point(mapping = aes(x = log(Area), y = log(Monuments_filt)),
                    colour = "blue",
                    alpha = 0.7) +
        labs(title = "Walled Cities\nFiltered Monuments") +
        theme_minimal()

p4 <- ggplot(Churches) +
        geom_point(mapping = aes(x = log(area), y = log(churches)),
                    colour = "green",
                    alpha = 0.7) +
        labs(title = "CoE Churches") +
        theme_minimal()

plt_data <- p1 + p2 + p3 + p4

plt_data

ggsave("./Output/point_scatters.pdf",
        height = 15,
        width = 15,
        units = "cm",
        device = "pdf")

#simple glms
glm_1 <- summary(lm(log(Monuments) ~ log(Area), 
                data = RomanUrban))

glm_2 <- summary(lm(log(Monuments) ~ log(Area), 
                data = RomanUrban[walls_idx, ]))

glm_3 <- summary(lm(log(Monuments_filt) ~ log(Area), 
                data = RomanUrban[walls_idx, ]))

glm_4 <- summary(lm(log(Temples) ~ log(Area), 
                data = RomanUrban[walls_idx, ]))

glm_5 <- summary(lm(log(NotTemples) ~ log(Area), 
                data = RomanUrban[walls_idx, ]))

glm_6 <- summary(lm(log(churches) ~ log(area), 
                data = Churches))

glm_coefs <- data.frame(model = c("Complete", 
                                "Complete", 
                                "Walled",
                                "Walled",
                                "Walled and Filtered",
                                "Walled and Filtered",
                                "Walled and Temples",
                                "Walled and Temples",
                                "Churches",
                                "Churches"))

glm_coefs$param <- rep(c("Intercept", 
                            "Scaling"),
                        times = 5)

glm_coefs <- cbind(glm_coefs,
                    rbind(round(glm_1$coefficients, 4), 
                        round(glm_2$coefficients, 4), 
                        round(glm_3$coefficients, 4),
                        round(glm_4$coefficients, 4), 
                        round(glm_6$coefficients, 4)))

glm_coefs$r_squared <- c(round(glm_1$r.squared, 2), NA, 
                        round(glm_2$r.squared, 2), NA,
                        round(glm_3$r.squared, 2), NA,
                        round(glm_4$r.squared, 2), NA,
                        round(glm_6$r.squared, 2), NA) 

write.table(glm_coefs, 
            file = "Output/glm_results.csv", 
            row.names = F)

# set up a Nimble model
scalingCode <- nimbleCode({
    if(J > 1){
        for(j in 1:J){
            b[j] ~ dnorm(mean = 0, sd = 100)
        }
    }else{
        b ~ dnorm(mean = 0, sd = 100)
    }
    sigma ~ dunif(1e-07, 100)
    for(n in 1:N){
        if(J > 1){
            mu[n] <- inprod(b[1:J], x[n, 1:J])
        }else{
            mu[n] <- b * x[n]
        }
        y[n] ~ dnorm(mean = mu[n], sd = sigma)
        y_hat[n] ~ dnorm(mean = mu[n], sd = sigma)
    }
})

N <- dim(RomanUrban)[1]
y <- log(RomanUrban$Monuments)
x <- data.frame(intercept = rep(1, N),
                area = log(RomanUrban$Area))

J = dim(x)[2]

Consts <- list(N = N,
            J = J)

Data <- list(y = y,
            x = x)

Inits <- list(b = c(0, 0),
            sigma = 1)

scalingModel <- nimbleModel(code = scalingCode,
                name = "urbanscaling",
                constants = Consts,
                data = Data,
                inits = Inits)

params_to_track <- c("b", "sigma", "y_hat")

mcmc_out <- nimbleMCMC(model = scalingModel, 
            monitors = params_to_track, thin = 1,
            niter = 20000, nburnin = 1000)

plot(mcmc_out[, 1], type = "l")

predictions <- as.data.frame(t(apply(mcmc_out[, -c(1:(J + 1))],
                                    2,
                                    quantile,
                                    probs = c(0.05, 0.95))))

if(J > 1){
    b_mean = apply(mcmc_out[, c(1, 2)], 2, mean)
}else{
    b_mean = mean(mcmc_out[, 1])
}
y_pred_mean = as.matrix(x) %*% b_mean
predictions$y_mean <- y_pred_mean

RomanUrban$L05 <- predictions$'5%'
RomanUrban$U95 <- predictions$'95%'
RomanUrban$Mean <- predictions$y_mean

plt <- ggplot(data = RomanUrban) +
            geom_ribbon(aes(x = log(Area), ymin = L05, ymax = U95),
                fill = "steelblue",
                alpha = 0.5) +
            geom_line(mapping = aes(y = Mean, x = log(Area))) +
            geom_point(mapping = aes(y = log(Monuments), x = log(Area))) +
            labs(title = "Log-Log", 
                y = "log(Monuments)", 
                x = "log(Area)") +
            theme_minimal()
plt

plt2 <- ggplot(data = RomanUrban) +
            geom_ribbon(aes(x = log(Area), ymin = exp(L05), ymax = exp(U95)),
                fill = "steelblue",
                alpha = 0.5) +
            geom_line(mapping = aes(y = exp(Mean), x = log(Area))) +
            geom_point(mapping = aes(y = Monuments, x = log(Area))) +
            labs(title = "Normal-Log", 
                y = "Monuments", 
                x = "log(Area)") +
            theme_minimal()
plt2

posts <- as.data.frame(mcmc_out[, c(1:3)])
names(posts) <- c("Intercept", "Scaling", "Sigma")

plt3 <- ggplot(posts) +
            geom_density(mapping = aes(x = Scaling),
                fill = "#01a501",
                colour = "darkgreen",
                alpha = 0.75) +
            labs(title = "Scaling Parameter Posterior",
                y = "Density", 
                x = "Power") +
            theme_minimal()
plt3

plt_all <- (plt + plt2) / plt3

plt_all

ggsave("./Output/scaling_results_complete.pdf",
        device = "pdf")

# now again, but wil walled cities only

RomanUrbanWalled <- RomanUrban[walls_idx, ]

complete_idx <- complete.cases(RomanUrbanWalled)

RomanUrbanWalled <- RomanUrbanWalled[complete_idx, ]

N <- dim(RomanUrbanWalled)[1]
y <- log(RomanUrbanWalled$Monuments)
x <- data.frame(intercept = rep(1, N),
                area = log(RomanUrbanWalled$Area))

J = dim(x)[2]

Consts <- list(N = N,
            J = J)

Data <- list(y = y,
            x = x)

Inits <- list(b = c(0, 0),
            sigma = 1)

scalingModel <- nimbleModel(code = scalingCode,
                name = "urbanscaling",
                constants = Consts,
                data = Data,
                inits = Inits)

params_to_track <- c("b", "sigma", "y_hat")

mcmc_out <- nimbleMCMC(model = scalingModel, 
            monitors = params_to_track, thin = 1,
            niter = 20000, nburnin = 1000)

plot(mcmc_out[, 2], type = "l")

predictions <- as.data.frame(t(apply(mcmc_out[, -c(1:(J + 1))],
                                    2,
                                    quantile,
                                    probs = c(0.05, 0.95))))

if(J > 1){
    b_mean = apply(mcmc_out[, c(1, 2)], 2, mean)
}else{
    b_mean = mean(mcmc_out[, 1])
}
y_pred_mean = as.matrix(x) %*% b_mean
predictions$y_mean <- y_pred_mean

RomanUrbanWalled$L05 <- predictions$'5%'
RomanUrbanWalled$U95 <- predictions$'95%'
RomanUrbanWalled$Mean <- predictions$y_mean

plt4 <- ggplot(data = RomanUrbanWalled) +
            geom_ribbon(aes(x = log(Area), ymin = L05, ymax = U95),
                fill = "steelblue",
                alpha = 0.5) +
            geom_line(mapping = aes(y = Mean, x = log(Area))) +
            geom_point(mapping = aes(y = log(Monuments), x = log(Area))) +
            labs(title = "Log-Log", 
                y = "log(Monuments)", 
                x = "log(Area)") +
            theme_minimal()
plt4

plt5 <- ggplot(data = RomanUrbanWalled) +
            geom_ribbon(aes(x = log(Area), ymin = exp(L05), ymax = exp(U95)),
                fill = "steelblue",
                alpha = 0.5) +
            geom_line(mapping = aes(y = exp(Mean), x = log(Area))) +
            geom_point(mapping = aes(y = Monuments, x = log(Area))) +
            labs(title = "Normal-Log", 
                y = "Monuments", 
                x = "log(Area)") +
            theme_minimal()
plt5

posts <- as.data.frame(mcmc_out[, c(1:3)])
names(posts) <- c("Intercept", "Scaling", "Sigma")

plt6 <- ggplot(posts) +
            geom_density(mapping = aes(x = Scaling),
                fill = "#01a501",
                colour = "darkgreen",
                alpha = 0.75) +
            labs(title = "Scaling Parameter Posterior",
                y = "Density", 
                x = "Power") +
            theme_minimal()
plt6

plt_all <- (plt4 + plt5) / plt6

plt_all

ggsave("./Output/scaling_results_walled.pdf",
        device = "pdf")

# and now with monuments filtered

RomanUrbanFilt <- RomanUrban[walls_idx, ]

complete_idx <- complete.cases(RomanUrbanFilt)

RomanUrbanFilt <- RomanUrbanFilt[complete_idx, ]

N <- dim(RomanUrbanFilt)[1]
y <- log(RomanUrbanFilt$Monuments_filt)
x <- data.frame(intercept = rep(1, N),
                area = log(RomanUrbanFilt$Area))

J = dim(x)[2]

Consts <- list(N = N,
            J = J)

Data <- list(y = y,
            x = x)

Inits <- list(b = c(0, 0),
            sigma = 1)

scalingModel <- nimbleModel(code = scalingCode,
                name = "urbanscaling",
                constants = Consts,
                data = Data,
                inits = Inits)

params_to_track <- c("b", "sigma", "y_hat")

mcmc_out <- nimbleMCMC(model = scalingModel, 
            monitors = params_to_track, thin = 1,
            niter = 20000, nburnin = 1000)

plot(mcmc_out[, 2], type = "l")

predictions <- as.data.frame(t(apply(mcmc_out[, -c(1:(J + 1))],
                                    2,
                                    quantile,
                                    probs = c(0.05, 0.95))))

if(J > 1){
    b_mean = apply(mcmc_out[, c(1, 2)], 2, mean)
}else{
    b_mean = mean(mcmc_out[, 1])
}
y_pred_mean = as.matrix(x) %*% b_mean
predictions$y_mean <- y_pred_mean

RomanUrbanFilt$L05 <- predictions$'5%'
RomanUrbanFilt$U95 <- predictions$'95%'
RomanUrbanFilt$Mean <- predictions$y_mean

plt7 <- ggplot(data = RomanUrbanFilt) +
            geom_ribbon(aes(x = log(Area), ymin = L05, ymax = U95),
                fill = "steelblue",
                alpha = 0.5) +
            geom_line(mapping = aes(y = Mean, x = log(Area))) +
            geom_point(mapping = aes(y = log(Monuments), x = log(Area))) +
            labs(title = "Log-Log", 
                y = "log(Monuments)", 
                x = "log(Area)") +
            theme_minimal()
plt7

plt8 <- ggplot(data = RomanUrbanFilt) +
            geom_ribbon(aes(x = log(Area), ymin = exp(L05), ymax = exp(U95)),
                fill = "steelblue",
                alpha = 0.5) +
            geom_line(mapping = aes(y = exp(Mean), x = log(Area))) +
            geom_point(mapping = aes(y = Monuments, x = log(Area))) +
            labs(title = "Normal-Log", 
                y = "Monuments", 
                x = "log(Area)") +
            theme_minimal()
plt8

posts <- as.data.frame(mcmc_out[, c(1:3)])
names(posts) <- c("Intercept", "Scaling", "Sigma")

plt9 <- ggplot(posts) +
            geom_density(mapping = aes(x = Scaling),
                fill = "#01a501",
                colour = "darkgreen",
                alpha = 0.75) +
            labs(title = "Scaling Parameter Posterior",
                y = "Density", 
                x = "Power") +
            theme_minimal()
plt9

plt_all <- (plt7 + plt8) / plt9

plt_all

ggsave("./Output/scaling_results_filt.pdf",
        device = "pdf")


# plot all scaling parameters

plt_all_scale <- (plt3 + plt + plt2) / (plt6 + plt4 + plt5) / (plt9 + plt7 + plt8)
plt_all_scale

ggsave("./Output/scaling_results_all.pdf",
        height = 8,
        width = 15,
        units = "cm",
        scale = 3,
        device = "pdf")

# RJ for variable selection

scalingModel_c <- compileNimble(scalingModel)
mcmc_config <- configureMCMC(scalingModel_c)
mcmc_config$setMonitors(params_to_track)

configureRJ(mcmc_config,
            targetNodes = 'b',
            priorProb = 0.5,
            control = list(mean = 0, scale = 0.2))

# build mcmc
scaling_mcmc <- buildMCMC(mcmc_config)
scaling_mcmc_c <- compileNimble(scaling_mcmc)

# run mcmc
mcmc_out_rj <- runMCMC(scaling_mcmc_c, 
                        niter = 50000,
                        nburnin = 5000)

# extract inclusion probabilities
betas <- grep("b\\[", colnames(mcmc_out_rj))
post_inclusion_probs <- colMeans(apply(mcmc_out_rj[, betas], 
                                2, 
                                function(x) x != 0))