#libraries ----
library(MASS)
library(tidyr)
library(dplyr)
library(lavaan)
library(purrr)
library(here)
library(simsalapar)
library(doParallel)
library(parallel)

#parameters ----
CLxy = c(0.08, 0.15) #cross-lagged effect of x on y
ARx = 0.5 #autoregressive effect of x
ARy = 0.5 #autoregressive effect of y
Niter = 1:500 #number of samples per condition

conditions = crossing(CLxy, ARx, ARy, Niter) #all combinations of conditions

#data-generating function ----
clpm_gen = function(CLxy, ARx, ARy, Niter) {
  
  #(residual) means
  mu = c(50, #x1
         50, #residual x2
         50, #residual x3
         50, #residual x4
         30, #y1
         30, #residual y2
         30, #residual y3
         30) #residual y4
  
  #(residuals) correlation matrix
  r = matrix(
    c(1.0, 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0,
      0.0, 1.0, 0.0, 0.0, 0.0, 0.2, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.2, 0.0,
      0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.2,
      0.2, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
      0.0, 0.2, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0,
      0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 1.0, 0.0,
      0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 1.0),
    nrow = 8, 
    ncol = 8) 
  
  #(residual) variances
  v = diag(c(20^0.5, #x1
             20^0.5, #residual x2
             20^0.5, 
             20^0.5, 
             30^0.5, #y1
             30^0.5, #residual y2
             30^0.5, 
             30^0.5)) 
  
  #(residual) variance-covariance matrix
  s = v%*%r%*%v 
  
  rownames(s) = c("Vx1", #variance of x1
                  "Rx2", "Rx3", "Rx4", #residual variance (variance of xt net variance explained by cross-lagged and autoregressive effects)
                  "Vy1", 
                  "Ry2", "Ry3", "Ry4") 
  
  colnames(s) = c("Vx1", "Rx2", "Rx3", "Rx4",
                  "Vy1", "Ry2", "Ry3", "Ry4")
  
  res = as.data.frame(mvrnorm(1000, mu, s, tol = 1e-7, empirical = FALSE)) #generate from a multivariate normal distribution
  
  clpm_data = res %>%
    
    mutate(x1 = Vx1) %>% 
    mutate(y1 = Vy1) %>%
    
    mutate(x2 = ARx*x1 + 0.08*y1 + Rx2) %>% #total variance of x2 made up from autoregressive, cross-lagged effects and residual variance
    mutate(y2 = ARy*y1 + CLxy*x1 + Ry2) %>% #total variance of y2 made up from autoregressive, cross-lagged effects and residual variance
    
    mutate(x3 = ARx*x2 + 0.08*y2 + Rx3) %>%
    mutate(y3 = ARy*y2 + CLxy*x2 + Ry3) %>%
    
    mutate(x4 = ARx*x3 + 0.08*y3 + Rx4) %>%
    mutate(y4 = ARy*y3 + CLxy*x3 + Ry4) %>%
    
    select(c(x1, x2, x3, x4, y1, y2, y3, y4))
  
  return(clpm_data)
}

#generate data ----
set.seed(1) #to ensure replicability

clpm_data = conditions %>%
  mutate(dataset = pmap(list(CLxy, ARx, ARy, Niter), 
                        possibly(clpm_gen, NA))) #generate and store a list of datasets for each condition

clpm_data$index = 1:nrow(clpm_data) #index each simulated data set

#paralellise fitting ----
source(here("0_models_syntax.R")) #define models
source(here("0_functions_extract-info_fit-models.R")) #define functions (extracting info and model fitting)
models_list = mget(c("clpm", "riclpm", "lgcmsr")) #loaded previously
ds = clpm_data[ , c("index", "dataset")] #assign dataset values to the variable used in the functions
clpm_data = dplyr::select(clpm_data, -dataset) #remove datasets from initial data frame to save memory

cores = makeCluster(detectCores()-1) 
registerDoParallel(cores)
clusterExport(cores, list('fit', 'ds', 'extract.info', 'models_list')) #export needed data and functions

#fit models ----
fit_list = parLapply(cores, seq_along(models_list), fun = fit) #fit all models to all datasets and extract fit info
clpm_log = as.data.frame(bind_rows(fit_list)) 
clpm_log[ , c(2, 4:14)] = as.numeric(unlist(clpm_log[ , c(2, 4:14)])) 

#save files ----
write.csv(clpm_log, "1_log_clpm.csv") #save model fitting log
write.csv(clpm_data, "1_conditions_clpm.csv") #save conditions data frame
