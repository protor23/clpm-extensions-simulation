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
SxSy = c(0.1, 0.5) #slopes correlation
VSx = c(2.44, 9.76) #slope variance (x)
VSy = c(3.22, 12.88) #slope variance (y)
CLxy = c(0.08, 0.15) #cross-lagged effect of x on y
ARx = 0.5 #autoregressive effect of x
ARy = 0.5 #autoregressive effect of y
Niter = 1:500 #number of samples per condition

conditions = crossing(SxSy, VSx, VSy, CLxy, ARx, ARy, Niter)

#data-generating function ----
lgcmsr_gen = function(SxSy, VSx, VSy, CLxy, ARx, ARy, Niter) {
  
  #(residual) means
  mu = c(50.36, #intercept (x)
         3.32,  #slope (x)
         50.35, #intercept (y)
         3.27,  #slope (y)
         0, 0,   0,  0, #residual means
         0,  0,  0,  0)
  
  #(residuals) correlation matrix
  r = matrix(c(
    1.0, 0.3, 0.5, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
    0.3, 1.0, 0.1,SxSy, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
    0.5, 0.1, 1.0, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
    0.1,SxSy, 0.3, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
    0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 
    0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.2, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.2, 0.0, 
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.2, 
    0.0, 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 
    0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 1.0, 0.0, 
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 1.0
  ),
  12, 12)
  
  #(residual) variances
  v = diag(c(72.08^0.5, #intercept (x)
             VSx^0.5,   #slope (x)
             78.90^0.5, #intercept (y)
             VSy^0.5,   #slope (y)
             25^0.5, 25^0.5, 25^0.5, 25^0.5, #residual variances
             28^0.5, 28^0.5, 28^0.5, 28^0.5))
  
  #(residual) variance-covariance matrix
  s = v%*%r%*%v
  
  rownames(s) = c("Ix", "Sx", "Iy", "Sy",
                  "Vx1", "Rx2", "Rx3", "Rx4",
                  "Vy1", "Ry2", "Ry3", "Ry4")
  
  colnames(s) = c("Ix", "Sx", "Iy", "Sy",
                  "Vx1", "Rx2", "Rx3", "Rx4",
                  "Vy1", "Ry2", "Ry3", "Ry4")
  
  res = as.data.frame(mvrnorm(1000, mu, s, tol = 1e-6, empirical = FALSE)) #generate from a multivariate normal distribution
  
  lgcmsr_data = res %>%
    
    mutate(x1 = Ix + Vx1) %>% #total variance of x1 made of intercept and residual variance
    mutate(y1 = Iy + Vy1) %>%
    
    mutate(Dx2 = ARx*Vx1 + 0.08*Vy1 + Rx2) %>% #deviation term + residual error
    mutate(Dy2 = ARy*Vy1 + CLxy*Vx1 + Ry2) %>%
    
    mutate(x2 = Ix + Sx + Dx2) %>% #total variance of x2 made of intercept variance + the variance of the deviation term +residual error
    mutate(y2 = Iy + Sy + Dy2) %>%
    
    mutate(Dx3 = ARx*Dx2 + 0.08*Dy2 + Rx3) %>%
    mutate(Dy3 = ARy*Dy2 + CLxy*Dx2 + Ry3) %>%
    
    mutate(x3 = Ix + 2.2*Sx+ Dx3) %>%
    mutate(y3 = Iy + 2.3*Sy +Dy3) %>%
    
    mutate(Dx4 = ARx*Dx3 + 0.08*Dy3 + Rx4) %>%
    mutate(Dy4 = ARy*Dy3 + CLxy*Dx3 + Ry4) %>%
    
    mutate(x4 = Ix + 2.86*Sx + Dx4) %>%
    mutate(y4 = Iy + 3.57*Sy + Dy4) %>%
    
    subset(select = c(x1, x2, x3, x4, y1, y2, y3, y4))
  
  return(lgcmsr_data)
}

#lgcmsr data ----
set.seed(1) #to ensure replicability

lgcmsr_data = conditions %>%
  mutate(dataset = pmap(list(SxSy, VSx, VSy, CLxy, ARx, ARy, Niter), 
                        possibly(lgcmsr_gen, NA))) #generate and store a list of datasets for each condition

lgcmsr_data$index = 1:nrow(lgcmsr_data) #index each simualted data set

#paralellise fitting ----
source(here("0_models_syntax.R")) #define models
source(here("0_functions_extract-info_fit-models.R")) #define functions (extracting info and model fitting)
models_list = mget(c("lgcmsr", "clpm", "riclpm")) #loaded previously
ds = lgcmsr_data[ , c("index", "dataset")] #assign dataset values to the variable used in the functions
lgcmsr_data = dplyr::select(lgcmsr_data, -dataset) #remove datasets from initial data frame to save memory

cores = makeCluster(detectCores()-1)
registerDoParallel(cores)
clusterExport(cores, list('fit', 'ds', 'extract.info', 'models_list')) #export needed data and functions

#model fitting ----
fit_list = parLapply(cores, seq_along(models_list), fun = fit) #fit all models to all datasets and extract fit info
lgcmsr_log = as.data.frame(bind_rows(fit_list))
lgcmsr_log[ , c(2, 4:14)] = as.numeric(unlist(lgcmsr_log[ , c(2, 4:14)]))

#save files ----
write.csv(lgcmsr_log, "3_log_lgcmsr.csv") #save model fitting log
write.csv(lgcmsr_data, "3_conditions_lgcmsr.csv") #save conditions data frame
