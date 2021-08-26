#extract fit info ----
extract.info = function(model.fit) { #convergence info, autoregressive and cross-lagged effects and standard errors
  model.info = data.frame(
    converged = as.numeric(lavInspect(model.fit, what = c("converged"))), 
    BIC = BIC(model.fit),
    RMSEA = fitMeasures(model.fit, "RMSEA"),
    CFI = fitMeasures(model.fit, "CFI"),
    ARx = coef(model.fit)["ARx"], 
    ARx.se = parameterEstimates(model.fit)[parameterEstimates(model.fit)$label == "ARx", "se"][1],
    ARy = coef(model.fit)["ARy"],
    ARy.se = parameterEstimates(model.fit)[parameterEstimates(model.fit)$label == "ARy", "se"][1],
    CLxy = coef(model.fit)["CLxy"],
    CLxy.se = parameterEstimates(model.fit)[parameterEstimates(model.fit)$label == "CLxy", "se"][1],
    CLyx = coef(model.fit)["CLyx"],
    CLyx.se = parameterEstimates(model.fit)[parameterEstimates(model.fit)$label == "CLyx", "se"][1])
  return(model.info)
}

#fit models to datasets ----
fit = function(j) { 
  lapply(seq_along(ds$dataset), #for each element in the list
         function(i) {
           library(lavaan) #crucial to load libraries here as it is needed to paralellise the process
           library(simsalapar)
           if(class(try(extract.info(lavaan(models_list[[j]], ds$dataset[[i]])), TRUE)) == "try-error") #if model fit results in error assign NAs to all info
             c(model = names(models_list[j]), sample = i, warning = NA, converged = NA, BIC = NA, RMSEA = NA,
               CFI = NA, ARx = NA, ARx.se = NA, ARy = NA, ARy.se = NA, CLxy = NA, CLxy.se = NA, CLyx = NA, CLyx.se = NA)
           else {warning = paste("W:", tryCatch.W.E(extract.info(lavaan(unlist(unname(models_list[j])), ds$data[[i]])))$warning) #else capture warning
           unlist(c(model = names(models_list[j]), sample = i, warning = as.character(warning), 
                    extract.info(lavaan(unlist(unname(models_list[j])), ds$data[[i]])))) #and extract info
           }
         })
}
