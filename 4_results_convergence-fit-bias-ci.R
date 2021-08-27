#libraries ----
library(here)
library(dplyr)
library(ggplot2)

#lgcmsr ----

#load dataframes
lgcmsr_log = read.csv(file.path(here("simulations") ,"3_log_lgcmsr.csv"))
lgcmsr_data = read.csv(file.path(here("simulations") ,"3_conditions_lgcmsr.csv"))

#convergence and improper solutions
lgcmsr_sol = lgcmsr_log %>%
  group_by(model, warning) %>%
  summarise(no = n()) #number of models resulting in warnings or not converging

#model fit indices
lgcmsr_fit = lgcmsr_log %>%
  filter(warning == "W: ") %>% #base calculations only on admissible solutions
  group_by(model) %>%
  summarize(mBIC = mean(BIC),
            sdBIC = sd(BIC),
            mRMSEA = mean(RMSEA),
            sdRMSEA = sd(RMSEA), 
            mCFI = mean(CFI),
            sdCFI = sd(CFI)) #means and standard deviations

#bias 
lgcmsr_bias = lgcmsr_log %>%
  filter(warning == "W: ") %>% #base calculations only on admissible solutions
  group_by(model) %>%
  left_join(lgcmsr_data[ , c("CLxy", "ARx", "ARy", "index")], 
            by = c("sample" = "index")) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            sdCLxy = sd((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08),
            sdCLyx = sd((CLyx - 0.08)/0.08)) #mean relative bias in cross-lagged parameters (and standard deviation)

#confidence intervals
lgcmsr_ci = lgcmsr_log %>%
  filter(warning == "W: ") %>% #base calculations only on admissible solutions
  group_by(model) %>%
  left_join(lgcmsr_data[ , c("CLxy", "ARx", "ARy", "index")], 
            by = c("sample" = "index")) %>%
  mutate(CLxy_in_ci = if((CLxy.y <= CLxy.x + 1.96*CLxy.se) && (CLxy.y >= CLxy.x - 1.96*CLxy.se)) TRUE else NA, #does the CI include the true parameter?
         CLyx_in_ci = if((0.08 <= CLyx + 1.96*CLyx.se) && (0.08 >= CLyx - 1.96*CLyx.se)) TRUE else NA) %>%
  summarize(Nxy = sum(CLxy_in_ci),
            Nyx = sum(CLyx_in_ci)) #frequency of cases in which CI includes the true parameter

#substantive conclusions re causal dominance - WTF
lgcmsr_conc = lgcmsr_log %>%
  filter(warning == "W: ") %>% #base calculations only on admissible solutions
  group_by(model) %>%
  left_join(lgcmsr_data[ , c("CLxy", "ARx", "ARy", "index")], 
            by = c("sample" = "index")) %>%
  mutate(dom_true = ifelse(CLxy.y == 0.08, "non-dominance", "dominance"),
         dom_est = ifelse(abs(CLxy.x) < abs(CLyx) + 0.02 && abs(CLxy.x) > abs(CLyx) - 0.02, "non-dominance",
                          ifelse(abs(CLxy.x) >= abs(CLyx) + 0.02, "dominance", "reversed dominance"))) %>%
  summarise(dom = sum(dom_true == dom_est))

lgcmsr_conc

#riclpm ----

#load dataframes
riclpm_log = read.csv(file.path(here("simulations") ,"2_log_riclpm.csv"))
riclpm_data = read.csv(file.path(here("simulations") ,"2_conditions_riclpm.csv"))

#convergence and improper solutions
riclpm_sol = riclpm_log %>%
  group_by(model, warning) %>%
  summarise(no = n())

#model fit indices
riclpm_fit = riclpm_log %>%
  filter(warning == "W: ") %>%
  group_by(model) %>%
  summarize(mBIC = mean(BIC),
            sdBIC = sd(BIC),
            mRMSEA = mean(RMSEA),
            sdRMSEA = sd(RMSEA), 
            mCFI = mean(CFI),
            sdCFI = sd(CFI))

#bias
riclpm_bias = riclpm_log %>%
  filter(warning == "W: ") %>%
  group_by(model) %>%
  left_join(riclpm_data[ , c("CLxy", "ARx", "ARy", "index")], 
            by = c("sample" = "index")) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            sdCLxy = sd((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08),
            sdCLyx = sd((CLyx - 0.08)/0.08))

#confidence intervals
riclpm_ci = riclpm_log %>%
  filter(warning == "W: ") %>%
  group_by(model) %>%
  left_join(riclpm_data[ , c("CLxy", "ARx", "ARy", "index")], 
            by = c("sample" = "index")) %>%
  mutate(CLxy_in_ci = if((CLxy.y <= CLxy.x + 1.96*CLxy.se) && (CLxy.y >= CLxy.x - 1.96*CLxy.se)) TRUE else NA,
         CLyx_in_ci = if((0.08 <= CLyx + 1.96*CLyx.se) && (0.08 >= CLyx - 1.96*CLyx.se)) TRUE else NA) %>%
  summarize(Nxy = sum(CLxy_in_ci),
            Nyx = sum(CLyx_in_ci))

#substantive conclusions re causal dominance - WTF AGAIN

#clpm ----

#load dataframes
clpm_log = read.csv(file.path(here("simulations") ,"1_log_clpm.csv"))
clpm_data = read.csv(file.path(here("simulations") ,"1_conditions_clpm.csv"))

#convergence and improper solutions
clpm_sol = clpm_log %>%
  group_by(model, warning) %>%
  summarise(no = n())

#model fit indices
clpm_fit = clpm_log %>%
  filter(warning == "W: ") %>%
  group_by(model) %>%
  summarize(mBIC = mean(BIC),
            sdBIC = sd(BIC),
            mRMSEA = mean(RMSEA),
            sdRMSEA = sd(RMSEA), 
            mCFI = mean(CFI),
            sdCFI = sd(CFI))

#bias
clpm_bias = clpm_log %>%
  filter(warning == "W: ") %>%
  group_by(model) %>%
  left_join(clpm_data[ , c("CLxy", "ARx", "ARy", "index")], 
            by = c("sample" = "index")) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            sdCLxy = sd((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08),
            sdCLyx = sd((CLyx - 0.08)/0.08))

#confidence intervals
clpm_ci = clpm_log %>%
  filter(warning == "W: ") %>%
  group_by(model) %>%
  left_join(clpm_data[ , c("CLxy", "ARx", "ARy", "index")], 
            by = c("sample" = "index")) %>%
  mutate(CLxy_in_ci = if((CLxy.y <= CLxy.x + 1.96*CLxy.se) && (CLxy.y >= CLxy.x - 1.96*CLxy.se)) TRUE else NA,
         CLyx_in_ci = if((0.08 <= CLyx + 1.96*CLyx.se) && (0.08 >= CLyx - 1.96*CLyx.se)) TRUE else NA) %>%
  summarize(Nxy = sum(CLxy_in_ci),
            Nyx = sum(CLyx_in_ci))
