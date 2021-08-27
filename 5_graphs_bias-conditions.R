#libraries ----
library(here)
library(dplyr)
library(ggplot2)
library(gridExtra)

#clpm ----

#load data frames
clpm_log = read.csv(file.path(here("simulations") ,"1_log_clpm.csv"))
clpm_data = read.csv(file.path(here("simulations") ,"1_conditions_clpm.csv"))

#bias by dominance condition
clpm_bias_cond = clpm_log %>%
  filter(warning == "W: ") %>% #only include admissible solutions
  left_join(clpm_data[ , c("CLxy", "index")], 
            by = c("sample" = "index")) %>%
  mutate(dom = ifelse(CLxy.y == 0.08, "dominance", "non-dominance")) %>% #label causal dominance conditions
  group_by(model, dom) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            sdCLxy = sd((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08),
            sdCLyx = sd((CLyx - 0.08)/0.08)) #bias by dominance condition

plot_list = list(
  ggplot(data = clpm_bias_cond, 
       aes(x = dom, y = bCLxy, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    scale_y_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, 0.1),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLxy (CLPM Data) by Dominance Condition") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3)), 

  ggplot(data = clpm_bias_cond, 
         aes(x = dom, y = bCLyx, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    scale_y_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, 0.1),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLyx (CLPM Data) by Dominance Condition") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3))
)

ggsave(filename = "1_bias_by-dominance_clpm.pdf",
       path = file.path(here("graphs")),
       plot = marrangeGrob(plot_list, nrow = 2, ncol = 1),
       width = 7, 
       height = 15)


#riclpm ----
riclpm_log = read.csv(file.path(here("simulations") ,"2_log_riclpm.csv"))
riclpm_data = read.csv(file.path(here("simulations") ,"2_conditions_riclpm.csv"))

riclpm_cond = riclpm_log %>%
  filter(warning == "W: ") %>%
  left_join(riclpm_data[ , c("CLxy", "IxIy", "VIx", "VIy", "index")], 
            by = c("sample" = "index")) %>%
  group_by(model) %>%
  mutate(dom = ifelse(CLxy.y == 0.08, "non-dominance", "dominance"),
         int_cov = ifelse(IxIy == 0.1, "Low Intercept Covariance", "High Intercept Covariance"),
         x.int_var = ifelse(VIx == 2.44, "Low Intercept Variance (x)", "High Intercept Variance (x)"),
         y.int_var = ifelse(VIy == 3.22, "Low Intercept Variance (y)", "High Intercept Variance (y)"))

#bias by dominance condition
riclpm_cond_bias = riclpm_cond %>%
  group_by(model, dom) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08))

plot_list = list(
  ggplot(data = riclpm_cond_bias, 
         aes(x = dom, y = bCLxy, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    scale_y_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, 0.1),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLxy (RICLPM Data) by Dominance Condition") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3)),

  ggplot(data = riclpm_cond_bias, 
         aes(x = dom, y = bCLyx, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    scale_y_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, 0.1),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLyx (RICLPM Data) by Dominance Condition") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3))
)

ggsave(filename = "2_bias_by-dominance_riclpm.pdf",
       path = file.path(here("graphs")),
       plot = marrangeGrob(plot_list, nrow = 2, ncol = 1),
       width = 7, 
       height = 15)

#bias by dominance and intercept covariance
riclpm_cond_bias = riclpm_cond %>%
  group_by(model, dom, int_cov) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08))

plot_list = list(
  ggplot(data = riclpm_cond_bias, 
         aes(x = dom, y = bCLxy, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    facet_grid(~int_cov) +
    scale_y_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, 0.1),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLxy (RICLPM Data) by Dominance Condition and Intercept Covariance") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3)),
  
  ggplot(data = riclpm_cond_bias, 
         aes(x = dom, y = bCLyx, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    facet_grid(~int_cov) +
    scale_y_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, 0.1),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLyx (RICLPM Data) by Dominance Condition and Intercept Covariance") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 1,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 1,
                                            linetype = 3))
)

ggsave(filename = "2_bias_by-dominance-covariance_riclpm.pdf",
       path = file.path(here("graphs")),
       plot = marrangeGrob(plot_list, nrow = 2, ncol = 1),
       width = 15, 
       height = 15)

#bias by dominance, intercept covariance, and intercept variances conditions
riclpm_cond_bias = riclpm_cond %>%
  group_by(model, dom, int_cov, x.int_var, y.int_var) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08))

plot_list = list(
  ggplot(data = riclpm_cond_bias, 
         aes(x = dom, y = bCLxy, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    facet_grid(int_cov~x.int_var + y.int_var) +
    scale_y_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, 0.1),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLxy (RICLPM Data) by Dominance Condition, Intercept Covariance, and Intercept Variances") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3)),
  ggplot(data = riclpm_cond_bias, 
         aes(x = dom, y = bCLyx, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    facet_grid(int_cov~x.int_var + y.int_var) +
    scale_y_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, 0.1),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLyx (RICLPM Data) by Dominance Condition, Intercept Covariance, and Intercept Variances") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3))
)

ggsave(filename = "2_bias_by-dominance-covariance-variance_riclpm.pdf",
       path = file.path(here("graphs")),
       plot = marrangeGrob(plot_list, nrow = 2, ncol = 1),
       width = 15, 
       height = 15)

#lgcmsr ----
lgcmsr_log = read.csv(file.path(here("simulations") ,"3_log_lgcmsr.csv"))
lgcmsr_data = read.csv(file.path(here("simulations") ,"3_conditions_lgcmsr.csv"))

lgcmsr_cond = lgcmsr_log %>%
  filter(warning == "W: ") %>%
  left_join(lgcmsr_data[ , c("CLxy", "SxSy", "VSx", "VSy", "index")], 
            by = c("sample" = "index")) %>%
  group_by(model) %>%
  mutate(dom = ifelse(CLxy.y == 0.08, "non-dominance", "dominance"),
         slp_cov = ifelse(SxSy == 0.1, "Low Slope Covariance", "High Slope Covariance"),
         x.slp_var = ifelse(VSx == 2.44, "Low Slope Variance (x)", "High Slope Variance (x)"),
         y.slp_var = ifelse(VSy == 3.22, "Low Slope Variance (y)", "High Slope Variance (y)"))

#bias by dominance
lgcmsr_cond_bias = lgcmsr_cond %>%
  group_by(model, dom) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08))

plot_list = list(
  ggplot(data = lgcmsr_cond_bias, 
         aes(x = dom, y = bCLxy, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    scale_y_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, 0.1),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLxy (LGCMSR Data) by Dominance Condition") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3)),

  ggplot(data = lgcmsr_cond_bias, 
         aes(x = dom, y = bCLyx, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    scale_y_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, 0.1),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLyx (LGCMSR Data) by Dominance Condition") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3))
)

ggsave(filename = "3_bias_by-dominance_lgcmsr.pdf",
       path = file.path(here("graphs")),
       plot = marrangeGrob(plot_list, nrow = 2, ncol = 1),
       width = 7, 
       height = 15)

##bias by dominance and slope covariance
lgcmsr_cond_bias = lgcmsr_cond %>%
  group_by(model, dom, slp_cov) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08))

plot_list = list(
  ggplot(data = lgcmsr_cond_bias, 
         aes(x = dom, y = bCLxy, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    facet_grid(~slp_cov) +
    scale_y_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, 0.1),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLxy (LGCMSR Data) by Dominance Condition and Slope Covariance") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3)), 
  
  ggplot(data = lgcmsr_cond_bias, 
         aes(x = dom, y = bCLyx, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    facet_grid(~slp_cov) +
    scale_y_continuous(limits = c(-0.6, 0.6),
                       breaks = seq(-0.6, 0.6, 0.1),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLyx (LGCMSR Data) by Dominance Condition and Slope Covariance") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3))
)
  
ggsave(filename = "3_bias_by-dominance-covariance_lgcmsr.pdf",
       path = file.path(here("graphs")),
       plot = marrangeGrob(plot_list, nrow = 2, ncol = 1),
       width = 15, 
       height = 15)

#bias by dominance, slope covariance and variances
lgcmsr_cond_bias = lgcmsr_cond %>%
  group_by(model, dom, slp_cov, x.slp_var, y.slp_var) %>%
  summarise(bCLxy = mean((CLxy.x - CLxy.y)/CLxy.y),
            bCLyx = mean((CLyx - 0.08)/0.08))

print(lgcmsr_cond_bias, n = 50)

plot_list = list(
  ggplot(data = lgcmsr_cond_bias, 
         aes(x = dom, y = bCLxy, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    facet_grid(slp_cov~x.slp_var + y.slp_var) + 
    scale_y_continuous(limits = c(-1.5, 1.5),
                       breaks = seq(-1.5, 1.5, 0.3),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLxy (LGCMSR Data) by Dominance Condition, Slope Covariance and Slope Variances") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3)),
  
  ggplot(data = lgcmsr_cond_bias, 
         aes(x = dom, y = bCLyx, group = model, colour = model)) +
    geom_line() + 
    geom_point() +
    facet_grid(slp_cov~x.slp_var + y.slp_var) + 
    scale_y_continuous(limits = c(-1.5, 1.5),
                       breaks = seq(-1.5, 1.5, 0.3),
                       labels = scales::number_format(accuracy = 0.01)) +
    scale_x_discrete(expand = c(-0.8, 1)) +
    ggtitle("\nBias in CLyx (LGCMSR Data) by Dominance Condition, Slope Covariance and Slope Variances") +
    xlab("Dominance Condition") + 
    ylab("Mean Relative Bias") +
    theme(axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2),
          axis.title.y = element_text(vjust = 3),
          panel.background = element_rect(fill = "white",
                                          colour = "black"),
          panel.grid.major.x = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3),
          panel.grid.minor.y = element_line(colour = "grey",
                                            size = 0.5,
                                            linetype = 3))
)
  
ggsave(filename = "3_bias_by-dominance-covariance-variances_lgcmsr.pdf",
       path = file.path(here("graphs")),
       plot = marrangeGrob(plot_list, nrow = 2, ncol = 1),
       width = 15, 
       height = 15)
    