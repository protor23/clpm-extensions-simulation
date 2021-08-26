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
IxIy = c(0.1, 0.5) #intercepts correlation
VIx = c(2.44, 9.76) #intercept variance (x)
VIy = c(3.22, 12.88) #intercept variance (y)
CLxy = c(0.08, 0.15) #cross-lagged effect of x on y
ARx = 0.5 #autoregressive effect of x
ARy = 0.5 #autoregressive effect of y
Niter = 1:500 #number of samples per condition

conditions = crossing(IxIy, VIx, VIy, CLxy, ARx, ARy, Niter) #all combinations of conditions

#data-generating function ----
riclpm_gen = function(IxIy, VIx, VIy, CLxy, ARx, ARy, Niter) {
  
  #(residual) means
  mu = c(0, #intercept mean (x) 
         0, #intercept mean (y)
         3, 3, 3, 3, #residual means (x)
         4, 4, 4, 4) #residual means (y)
  
  #(residuals) correlation matrix
  r = matrix(c(
    1.0, IxIy,0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
    IxIy,1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
    0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.3, 0.0, 0.0, 0.0, 
    0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.2, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.2, 0.0, 
    0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.2, 
    0.0, 0.0, 0.3, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 
    0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 
    0.0, 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 1.0, 0.0, 
    0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 1.0
  ),
  10, 10)
  
  #(residual) variances
  v = diag(c(VIx^0.5, VIy^0.5, #intercept variances
             20^0.5, 20^0.5, 20^0.5, 20^0.5, #residual variances
             30^0.5, 30^0.5, 30^0.5, 30^0.5))
  
  #(residual) variance-covariance matrix
  s = v%*%r%*%v
  
  rownames(s) = c("Ix", "Iy",
                  "Vx1", "Rx2", "Rx3", "Rx4",
                  "Vy1", "Ry2", "Ry3", "Ry4")
  
  colnames(s) = c("Ix", "Iy",
                  "Vx1", "Rx2", "Rx3", "Rx4",
                  "Vy1", "Ry2", "Ry3", "Ry4")
  
  res = as.data.frame(mvrnorm(1000, mu, s, tol = 1e-7, empirical = FALSE)) #generate from a multivariate normal distribution
  
  riclpm_data = res %>%
    
    mutate(x1 = Ix + Vx1) %>% #total variance of x1 made of intercept variance and residual variance
    mutate(y1 = Iy + Vy1) %>% #total variance of y1 made of intercept variance and residual variance
    
    mutate(Dx2 = ARx*(Vx1 - 3) + 0.08*(Vy1 - 4) + Rx2) %>% #deviation term + residual error
    mutate(Dy2 = ARy*(Vy1 - 4) + CLxy*(Vx1 - 3) + Ry2) %>%
    
    mutate(x2 = Ix + Dx2) %>% #total variance of x2 made of intercept variance + the variance of the deviation term + residual error
    mutate(y2 = Iy + Dy2) %>%
    
    mutate(Dx3 = ARx*(Dx2 - 3) + 0.08*(Dy2 - 4) + Rx3) %>%
    mutate(Dy3 = ARy*(Dy2 - 4) + CLxy*(Dx2 - 3) + Ry3) %>%
    
    mutate(x3 = Ix + Dx3) %>%
    mutate(y3 = Iy + Dy3) %>%
    
    mutate(Dx4 = ARx*(Dx3 - 3) + 0.08*(Dy3 - 4) + Rx4) %>%
    mutate(Dy4 = ARy*(Dy3 - 4) + CLxy*(Dx3 - 3) + Ry4) %>%
    
    mutate(x4 = Ix + Dx4) %>%
    mutate(y4 = Iy + Dy4) %>%
    
    subset(select = c(x1, x2, x3, x4, y1, y2, y3, y4))
  
  return(riclpm_data)
}

#riclpm data ----
set.seed(1) #to ensure replicability

riclpm_data = conditions %>%
  mutate(dataset = pmap(list(IxIy, VIx, VIy, CLxy, ARx, ARy, Niter), 
                        possibly(riclpm_gen, NA))) #generate and store a list of datasets for each condition

riclpm_data$index = 1:nrow(riclpm_data) #index each simulated data set

#paralellise fitting ----
source(here("0_models_syntax.R")) #define models
source(here("0_functions_extract-info_fit-models.R")) #define functions (extracting info and model fitting)
models_list = mget(c("clpm", "riclpm", "lgcmsr")) #loaded previously
ds = riclpm_data[ , c("index", "dataset")] #assign dataset values to the variable used in the functions
riclpm_data = dplyr::select(riclpm_data, -dataset) #remove datasets from initial data frame to save memory

cores = makeCluster(detectCores()-1)
registerDoParallel(cores)
clusterExport(cores, list('fit', 'ds', 'extract.info', 'models_list')) #export needed data and functions

#model fitting ----
fit_list = parLapply(cores, seq_along(models_list), fun = fit) #fit all models to all datasets and extract fit info
riclpm_log = as.data.frame(bind_rows(fit_list)) 
riclpm_log[ , c(2, 4:14)] = as.numeric(unlist(riclpm_log[ , c(2, 4:14)])) 

#save files ----
write.csv(riclpm_log, "1_log_riclpm.csv") #save model fitting log
write.csv(riclpm_data, "1_conditions_riclpm.csv") #save conditions data frame