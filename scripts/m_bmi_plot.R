# Goal: Compare the distribution of mother's BMI before pregnancy vs. during different weeks of pregnancy to see how long it is similar
# The "WomenFirst" study has mother's measurements before pregnancy

library(tidyverse)
library(synapser)
library(viridis)
options(tibble.width = Inf)
synapser::synLogin(rememberMe = TRUE)

dat <- read_rds(path = "data/derived_data/women_first_bmi.rds")

before_repeated <- dat %>%
  filter(gagedays < 0) %>%
  group_by(uid) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  summarise_at(.vars = "m_bmi",c("mean", "sd","median", "max", "min")) 


before_repeated %>%
  ggplot(aes(x =  sd))+
  geom_histogram(color = "white") +
  theme_bw() +
  labs(x = "Standard deviation of repeated pre-pregnancy measures", y = "Count", 
       title = "With Mother BMI Standard Deviation of pre-pregnancy measurements")

before = dat %>%
  filter(gagedays < 0) %>%
  group_by(uid) %>%
  summarise_at(.vars = "m_bmi",c("mean", "sd","median", "max", "min")) %>%
  ungroup() %>%
  left_join(dat %>%
  filter(gagedays < 0) %>%
  group_by(uid) %>%
  summarise(last = m_bmi[gagedays == max(gagedays)], n_before = n()) )

f14w <- dat %>%
  group_by(uid) %>%
  mutate(n_total = n()) %>%
  ungroup() %>%
  # filter(gagedays > 0, gagewks < 14) %>%
  # group_by(uid) %>%
  # mutate(n_f14 = n()) %>%
  # ungroup() %>%
  left_join(before)

# looks like the first first 15 shows little weight change. But after 7 weeks the variability increases.
  
dat %>%
  left_join(before) %>% 
  filter(gagedays > 0, gagedays < 300) %>%
  ggplot(aes(x = factor(gagewks), y = m_bmi - last)) +
  geom_jitter(height = .1, width = .25, color = "lightgrey") +
  geom_boxplot(outlier.color = NA, fill = NA) +
  coord_cartesian(ylim = c(-5, 14)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(-6,20, by = 2)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
  labs(x = "Gestational age (weeks)", 
       y = "BMI difference from last BMI before pregnancy\n(pregnancy bmi - pre bmi)",
       title = "BMI difference from baseline by gestational week", 
       subtitle = "First 15 weeks has median of 0. First 7 weeks keeps more than 50% of BMI differences within 1.")

ggsave("data/plots/BMI.png", height = 5, width = 9)


dat %>%
  left_join(before) %>% 
  mutate(bmi_group = case_when(
    last < 18.5 ~ "Under",
    last < 25 ~ "Normal",
    last < 30 ~ "Overweight",
    last >= 30 ~ "Obese",
    TRUE ~ "missing")) %>%
  filter(gagedays > 0, gagedays < 300, bmi_group != "missing") %>%
  mutate( bmi_group = factor(bmi_group, levels = c("Under", "Normal", "Overweight", "Obese"))) %>%
  ggplot(aes(x = factor(gagewks), y = m_bmi - last)) +
  geom_jitter(height = .1, width = .25, color = "lightgrey") +
  geom_boxplot(outlier.color = NA, fill = NA) +
  geom_vline(xintercept = 7.5, aes(color = "Change in\nvariability")) +
  geom_vline(xintercept = 7.5, aes(color = "Change in\nvariability")) +
  coord_cartesian(ylim = c(-5, 10)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(-6,20, by = 2)) +
  facet_wrap(~bmi_group, ncol = 1) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
  labs(x = "Gestational age (weeks)", 
       y = "BMI difference from last BMI before pregnancy\n(pregnancy bmi - pre bmi)",
       title = "BMI difference from baseline by gestational week", 
       subtitle = "First 7 weeks keeps BMI differences within 1 for all BMI groups.")

ggsave("data/plots/BMI_bywieghtgroups.png", height = 9, width = 9)





dat %>%
  ggplot(aes(y = m_bmi, x = factor(gagewks, levels = names(table(dat$gagewks))))) +
  geom_point() +
  geom_line(aes(group = uid))

  