#libraries ----
library(here)
library(dplyr)
library(ggplot2)

#clpm ----
clpm_log = read.csv(here("1_log_clpm.csv"))
clpm_data = read.csv(here("1_conditions_clpm.csv"))

#group by dom and plot bias
clpm_bias_cond = clpm_log %>%
  filter(warning == "W: ") %>% #only include admissible solutions
  left_join(clpm_data[ , c("CLxy", "index")], 
            by = c("sample" = "index")) %>%
  mutate(dom = ifelse(CLxy.y == 0.08, "dominance", "non-dominance")) %>% #label causal dominance conditions
  group_by(model, cond) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            sdCLxy = sd((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08),
            sdCLyx = sd((CLyx - 0.08)/0.08)) #bias by dominance condition

ggplot(data = clpm_bias_cond, 
       aes(x = dom, y = bCLxy, group = model, colour = model)) +
  geom_line() + 
  geom_point() +
  ggtitle("Mean Relative Bias in CLxy when Fitting to CLPM Data") +
  xlab("Dominance Condition") + 
  ylab("Mean Relative Bias") +
  theme(axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -3))

ggplot(data = clpm_bias_cond, 
       aes(x = dom, y = bCLyx, group = model, colour = model)) +
  geom_line() + 
  geom_point() +
  ggtitle("Mean Relative Bias in CLyx when Fitting to CLPM Data") +
  xlab("Dominance Condition") + 
  ylab("Mean Relative Bias") +
  theme(axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -3))

#riclpm ----
riclpm_log = read.csv(here("2_log_riclpm.csv"))
riclpm_data = read.csv(here("2_conditions_riclpm.csv"))

riclpm_cond = riclpm_log %>%
  filter(warning == "W: ") %>%
  left_join(riclpm_data[ , c("CLxy", "IxIy", "VIx", "VIy", "index")], 
            by = c("sample" = "index")) %>%
  group_by(model) %>%
  mutate(dom = ifelse(CLxy.y == 0.08, "non-dominance", "dominance"),
         int_cov = ifelse(IxIy == 0.1, "Low Intercept Covariance", "High Intercept Covariance"),
         x.int_var = ifelse(VIx == 2.44, "Low Intercept Variance (x)", "High Intercept Variance (x)"),
         y.int_var = ifelse(VIy == 3.22, "Low Intercept Variance (y)", "High Intercept Variance (y)"))

#group by dom and plot bias
riclpm_cond_bias = riclpm_cond %>%
  group_by(model, dom) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08))

ggplot(data = riclpm_cond_bias, 
       aes(x = dom, y = bCLxy, group = model, colour = model)) +
  geom_line() + 
  geom_point() +
  ggtitle("Mean Relative Bias in CLxy when Fitting to RICLPM Data") +
  xlab("Dominance Condition") + 
  ylab("Mean Relative Bias") + 
  theme(axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -3))

ggplot(data = riclpm_cond_bias, 
       aes(x = dom, y = bCLyx, group = model, colour = model)) +
  geom_line() + 
  geom_point() +
  ggtitle("Mean Relative Bias in CLyx when Fitting to RICLPM Data") +
  xlab("Dominance Condition") + 
  ylab("Mean Relative Bias") +
  theme(axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -3))

#group by int_cov and dom and plot bias
riclpm_cond_bias = riclpm_cond %>%
  group_by(model, dom, int_cov) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08))

ggplot(data = riclpm_cond_bias, 
       aes(x = dom, y = bCLxy, group = model, colour = model)) +
  geom_line() + 
  geom_point() +
  ggtitle("Mean Relative Bias in CLxy when Fitting to RICLPM Data") +
  xlab("Dominance Condition") + 
  ylab("Mean Relative Bias") +
  facet_wrap( ~int_cov) + 
  theme(axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -3))

#group by intercept covariances, dominance, and intercept variances and plot bias
riclpm_cond_bias = riclpm_cond %>%
  group_by(model, dom, int_cov, x.int_var, y.int_var) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08))

ggplot(data = riclpm_cond_bias, 
       aes(x = dom, y = bCLxy, group = model, colour = model)) +
  geom_line() + 
  geom_point() +
  ggtitle("Mean Relative Bias in CLxy when Fitting to RICLPM Data") +
  xlab("Dominance Condition") + 
  ylab("Mean Relative Bias") +
  facet_grid(int_cov~x.int_var + y.int_var) + 
  theme(axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -3))

#lgcmsr ----
lgcmsr_log = read.csv(here("3_lgcmsr_log.csv"))
lgcmsr_data = read.csv(here("3_lgcmsr_conditions.csv"))

lgcmsr_cond = lgcmsr_log %>%
  filter(warning == "W: ") %>%
  left_join(lgcmsr_data[ , c("CLxy", "SxSy", "VSx", "VSy", "index")], 
            by = c("sample" = "index")) %>%
  group_by(model) %>%
  mutate(dom = ifelse(CLxy.y == 0.08, "non-dominance", "dominance"),
         slp_cov = ifelse(SxSy == 0.1, "Low Slope Covariance", "High Slope Covariance"),
         x.slp_var = ifelse(VSx == 2.44, "Low Slope Variance (x)", "High Slope Variance (x)"),
         y.slp_var = ifelse(VSy == 3.22, "Low Slope Variance (y)", "High Slope Variance (y)"))

#group by dominance and plot bias
lgcmsr_cond_bias = lgcmsr_cond %>%
  group_by(model, dom) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08))

ggplot(data = lgcmsr_cond_bias, 
       aes(x = dom, y = bCLxy, group = model, colour = model)) +
  geom_line() + 
  geom_point() +
  ggtitle("Mean Relative Bias in CLxy when Fitting to LGCMSR Data") +
  xlab("Dominance Condition") + 
  ylab("Mean Relative Bias") + 
  theme(axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -3))

ggplot(data = lgcmsr_cond_bias, 
       aes(x = dom, y = bCLyx, group = model, colour = model)) +
  geom_line() + 
  geom_point() +
  ggtitle("Mean Relative Bias in CLyx when Fitting to LGCMSR Data") +
  xlab("Dominance Condition") + 
  ylab("Mean Relative Bias") +
  theme(axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -3))

##group by slope covariance and dominance and plot bias
lgcmsr_cond_bias = lgcmsr_cond %>%
  group_by(model, dom, slp_cov) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08))

ggplot(data = lgcmsr_cond_bias, 
       aes(x = dom, y = bCLxy, group = model, colour = model)) +
  geom_line() + 
  geom_point() +
  ggtitle("Mean Relative Bias in CLxy when Fitting to LGCMSR Data") +
  xlab("Dominance Condition") + 
  ylab("Mean Relative Bias") +
  facet_wrap( ~slp_cov) + 
  theme(axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -3))

#group by slope covariance, dominance, and slope variances and plot bias
lgcmsr_cond_bias = lgcmsr_cond %>%
  group_by(model, dom, slp_cov, x.slp_var, y.slp_var) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08))

ggplot(data = lgcmsr_cond_bias, 
       aes(x = dom, y = bCLxy, group = model, colour = model)) +
  geom_line() + 
  geom_point() +
  ggtitle("Mean Relative Bias in CLxy when Fitting to RICLPM Data") +
  xlab("Dominance Condition") + 
  ylab("Mean Relative Bias") +
  facet_grid(slp_cov~x.slp_var + y.slp_var) + 
  theme(axis.text.x = element_text(vjust = -2),
        axis.title.x = element_text(vjust = -3))