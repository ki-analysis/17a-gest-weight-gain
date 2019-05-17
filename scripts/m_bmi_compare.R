# Goal: Compare the distribution of mother's BMI before pregnancy vs. during different weeks of pregnancy to see how long it is similar
# The "WomenFirst" study has mother's measurements before pregnancy

source("scripts/_fns.R")

library(tidyverse)
library(synapser)
library(viridis)
options(tibble.width = Inf)
synapser::synLogin(rememberMe = TRUE)


rmote::start_rmote()

d <- read_data(path)
data_spec <- read_data_spec()

wf <- filter(d, studyid == "WomenFirst")

length(which(is.na(wf$m_bmi))) / nrow(wf)
length(which(is.na(wf$m_htcm))) / nrow(wf)
length(which(is.na(wf$m_wtkg))) / nrow(wf)
# 80% is missing!

table(is.na(wf$gagedays))

# Are missing and non-missing happening at different ages?
na_bmi_dist <- wf %>%
  filter(!is.na(gagedays)) %>%
  mutate(na_bmi = ifelse(is.na(m_bmi), "missing BMI", "has BMI")) %>%
  group_by(na_bmi) %>%
  summarise(
    q = list(ppoints(100)),
    d = list(quantile(gagedays, ppoints(100)))
  ) %>%
  unnest()

ggplot(na_bmi_dist, aes(q, d, color = na_bmi)) +
  geom_point() +
  # facet_wrap(~ na_bmi) +
  theme_bw()
# the distributions are close enough to the same

# Why are there NA gagedays?
table(is.na(wf2$gagedays))
length(which(is.na(d$agedays)))

d %>%
  filter(d, uid == 12078) %>%
  select(uid, agedays, gagedays) %>%
  head() %>%
  knitr::kable(format = "markdown")

# Just ignore NA gagedays for now...
wf2 <- wf %>%
  filter(!is.na(gagedays)) %>%
  select(uid, m_bmi, agedays, gagedays) %>%
  filter(!is.na(m_bmi)) %>%
  mutate(gagewks = floor(gagedays / 7))

# Use this dataset to answer question

write_rds(wf2, path = "data/derived_data/women_first_bmi.rds")
# See m_bmi_plot.R for visualization
