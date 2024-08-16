
run_analysis <- function(dataset,
                        modelname,
                        type,
                        niter = 100000,
                        thin = 10,
                        nchains = 4){

    # set common mcmc params
    nburnin <- floor(niter * 0.2)

    # the models differ depending on the dataset

    if(type == "roman"){
        ###################### MAIN BAYESIAN/NIMBLE MODEL #####################

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
            intercept0 ~ dnorm(mean = 0, sd = 10)
            sd0 ~ dunif(1e-7, 10)
            scaling0 ~ dnorm(mean = 0, sd = 10)
            sd1 ~ dunif(1e-7, 10)
            for(k in 1:K){
                intercept_raw[k] ~ dnorm(0, 1)
                intercept[k] <- intercept0 + intercept_raw[k] * sd0
                scaling_raw[k] ~ dnorm(0, 1)
                scaling[k] <- scaling0 + scaling_raw[k] * sd1
            }

            # prior for negbinom size parameter (dispersion)
            size ~ dexp(rate = 0.1)

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
                p[n] <- size / (size + mu[n])
                y[n] ~ dnegbin(prob = p[n], size = size)
                y_hat[n] <- (size * (1 - p[n])) / p[n]
            }
        })

        # Select parameters to track during MCMC
        params_to_track <- c("intercept0",
                        "scaling0",
                        "sd0",
                        "sd1",
                        "size",
                        "scaling", 
                        "intercept",
                        "b0", 
                        "b1",
                        "y_hat")

        if(modelname == "allmonuments"){
            df <- dataset
            y <- df$Monuments
        }else if(modelname == "allmonuments_nozero"){
            nonzero_idx <- which(dataset$Monuments > 0)
            df <- dataset[nonzero_idx,]
            y <- df$Monuments
        }else if(modelname == "allwalls"){
            df <- dataset[walls_idx,]
            y <- df$Monuments
        }else if(modelname == "allwalls_nozero"){
            df <- dataset[walls_idx,]
            nonzero_idx <- which(df$Monuments > 0)
            df <- df[nonzero_idx,]
            y <- df$Monuments
        }else if(modelname == "filtmonuments"){
            df <- dataset
            y <- df$Monuments_filt
        }else if(modelname == "filtmonuments_nozero"){
            nonzero_idx <- which(dataset$Monuments_filt > 0)
            df <- dataset[nonzero_idx,]
            y <- df$Monuments_filt
        }else if(modelname == "epigraphy"){
            df <- dataset
            y <- df$InscriptionCount
        }else if(modelname == "epigraphy_nozero"){
            df <- dataset[which(dataset$InscriptionCount > 0),]
            y <- df$InscriptionCount
        }

        N <- dim(df)[1]
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
                    scaling_raw = rep(0, K),
                    intercept = rep(0, K),
                    intercept_raw = rep(0, K),
                    intercept0 = 0,
                    scaling0 = 0,
                    size = 1,
                    sd0 = 1,
                    sd1 = 1,
                    b0 = 0,
                    b1 = 0,
                    sigma_pop = 1)
    }else if(type == "modern"){
        scalingCode <- nimbleCode({
            # scaling params
            intercept ~ dnorm(mean = 0, sd = 100)
            scaling ~ dnorm(mean = 0, sd = 100)
            
            size ~ dexp(rate = 0.1)

            # main model
            for(n in 1:N){
                log_mu[n] <- intercept + scaling * pop[n]
                mu[n] <- exp(log_mu[n]) 
                p[n] <- size / (size + mu[n])
                y[n] ~ dnegbin(prob = p[n], size = size)
                y_hat[n] <- (size * (1 - p[n])) / p[n]
            }
        })

        if(modelname == "hnwi"){
            N <- dim(dataset)[1]
            y <- dataset$Billionaires
            pop <- log(dataset$population)
        }else if(modelname == "tallbuildings"){
            N <- dim(dataset)[1]
            y <- dataset$`150 m+ Buildings`
            pop <- log(dataset$Population)
        }

        Consts <- list(N = N)

        Data <- list(y = y,
                    pop = pop)

        Inits <- list(scaling = 0,
                    intercept = 0,
                    size = 1)

        params_to_track <- c("intercept", 
                            "scaling",
                            "size",
                            "y_hat")
    }

    # Create the Nimble model
    scalingModel <- nimbleModel(code = scalingCode,
                                name = modelname,
                                constants = Consts,
                                data = Data,
                                inits = Inits)

    # Compile the model
    compiled_model <- compileNimble(scalingModel)

    # Configure the MCMC
    mcmc_config <- configureMCMC(scalingModel)

    if(type == "roman"){
        # given the additional complexity of the hierarchical model we
        # needed to employ more sophisticated samplers in oder 
        # to produce well-mixed mcmc chains

        # Replace samplers for correlated parameters
        # Block sampling for intercept0 and scaling0
        mcmc_config$removeSamplers(c('intercept0', 'scaling0'))
        mcmc_config$addSampler(target = c('intercept0', 'scaling0'), type = 'AF_slice')

        # Block sampling for b0 and b1
        mcmc_config$removeSamplers(c('b0', 'b1'))
        mcmc_config$addSampler(target = c('b0', 'b1'), type = 'AF_slice')

        # block sampling for each group's intercept and scaling
        for (k in 1:Consts$K) {
            mcmc_config$removeSamplers(c(paste0('intercept[', k, ']'), paste0('scaling[', k, ']')))
            mcmc_config$addSampler(target = c(paste0('intercept[', k, ']'), paste0('scaling[', k, ']')), type = 'AF_slice')
        }
    }

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

    return(mcmc_out)
}

gather_results <- function(mcmc_out,
                            type){

    if(type == "roman"){
        # Trace plots for key parameters
        top_lvl_param_names <- c("intercept0", 
                                "scaling0",
                                "sd0",
                                "sd1", 
                                "b0", 
                                "b1",
                                "size")

        # and now to look at variability in scaling parameter among provinces
        if(is.mcmc.list(mcmc_out)){
                province_scaling_names <- colnames(mcmc_out[[1]])[grep("scaling\\[", colnames(mcmc_out[[1]]))]
        }else{
                province_scaling_names <- colnames(mcmc_out)[grep("scaling\\[", colnames(mcmc_out))]
        }

        tplot_provinces <- stacked_traceplot(mcmc_out, province_scaling_names, mode = "stacked", thin = 10)

        ggsave(filename = paste("Output/tplots_", modelname,"_provinces.pdf", sep = ""), 
                plot = tplot_provinces, 
                device = "pdf")
    }else if(type == "modern"){
        top_lvl_param_names <- c("intercept", "scaling", "size")
    }

    # plot top level traces
    tplot <- stacked_traceplot(mcmc_out, top_lvl_param_names)

    ggsave(filename = paste("Output/tplots_", modelname, ".pdf", sep = ""), 
            plot = tplot, 
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
}



mcmc_out <- run_analysis(dataset = RomanUrban,
                        modelname = "allmonuments",
                        type = "roman",
                        niter = 10000,
                        thin = 10,
                        nchains = 2)
                        
gather_results(mcmc_out, type = "roman")