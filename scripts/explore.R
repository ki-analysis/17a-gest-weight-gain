# git clone https://user.name@git.ghap.io/stash/scm/hbgd/rally-17.git

library(tidyverse)

rmote::start_rmote()

# Read and format data
d <- readr::read_csv("/data/git/hbgd/rally-17/Sprint\ A/adam/rally_17.csv")
names(d) <- tolower(names(d))
# get rid of ki... prefixes in study IDs
d$studyid <- gsub("^ki[0-9]+\\-(.*)", "\\1", d$studyid)
d$studyid <- gsub("kiGH5241\\-", "", d$studyid)

## How many observations in each study
study_nobs <- d %>%
  filter(!is.na(m_wtkg)) %>%
  select(studyid) %>%
  group_by(studyid) %>%
  tally() %>%
  mutate(studyid = fct_reorder(factor(studyid), n))

rmote::rmote_device(width = 6 * 72, height = 6 * 72)
ggplot(study_nobs, aes(studyid, n)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# How many subjects do we have data for
# with non-NA mother's weight before birth
d %>%
  filter(!is.na(m_wtkg) & agedays <= 0) %>%
  group_by(studyid, subjid) %>%
  tally() %>%
  nrow()
# 43,608 subjects

# Count how many subjects have how many non-NA
# recorded weights before birth.
# Lump all over 10 into "10+".
momweight_count <- d %>%
  filter(!is.na(m_wtkg) & agedays <= 0) %>%
  group_by(studyid, subjid) %>%
  tally() %>%
  mutate(n = ifelse(n > 10, 10, n)) %>%
  rename(n_meas = n) %>%
  ungroup() %>%
  group_by(studyid, n_meas) %>%
  tally()
momweight_count$n_meas <- factor(momweight_count$n_meas)
levels(momweight_count$n_meas)[10] <- "10+"

rmote::rmote_device(width = 16 * 72, height = 6 * 72)
ggplot(momweight_count, aes(n_meas, n)) +
  geom_col() +
  facet_wrap(~ studyid, scales = "free_x", nrow = 1) +
  coord_flip() +
  theme_bw() +
  labs(y = "Number of subjects",
    x = "Number of non-NA mother's weight observations")

# Cumulative number of subjects that have n or more observations
momweight_ccount <- d %>%
  filter(!is.na(m_wtkg) & agedays <= 0) %>%
  group_by(uid) %>%
  tally() %>%
  rename(n_meas = n) %>%
  ungroup() %>%
  group_by(n_meas) %>%
  tally() %>%
  arrange(-n_meas) %>%
  mutate(cumn = cumsum(n))

rmote::rmote_device(width = 12 * 72, height = 6 * 72)
ggplot(momweight_ccount, aes(n_meas, cumn)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Number of non-NA mother's weight observations",
    y = "Number of subjects",
    title = "Number of subjects having at least x mother's weight obserations")

tableau <- ggthemes::scale_color_tableau()$palette(4)

rmote::rmote_device(width = 18 * 72, height = 4 * 72)
d %>%
  filter(!is.na(m_wtkg) & agedays <= 0 & studyid != "WomenFirst" & gagedays > 0) %>%
  ggplot(aes(gagedays / 7)) +
    annotate("rect",
      xmin = 0, xmax = 13, ymin = -Inf, ymax = Inf,
      fill = tableau[1], alpha = 0.3) +
    annotate("rect",
      xmin = 13, xmax = 26, ymin = -Inf, ymax = Inf,
      fill = tableau[2], alpha = 0.3) +
    annotate("rect",
      xmin = 26, xmax = 45, ymin = -Inf, ymax = Inf,
      fill = tableau[4], alpha = 0.3) +
    geom_histogram(breaks = c(0:44)) +
    facet_wrap(~ studyid, nrow = 1, scales = "free_y") +
    theme_bw() +
    labs(x = "gestational age (weeks)",
      title = "Number of measurements of mother's weight for each gestational week, by study")

rmote::rmote_device(width = 8 * 72, height = 4 * 72)
d %>%
  filter(!is.na(m_wtkg) & agedays <= 0 & studyid == "WomenFirst") %>%
  ggplot(aes(gagedays / 7)) +
    geom_histogram(bins = 10) +
    facet_wrap(~ studyid, nrow = 1) +
    theme_bw() +
    labs(x = "gestational age (weeks)",
      title = "Number of measurements of mother's weight for each gestational week")

# What percentage of subjects are from JiVitA-3
d %>%
  filter(!is.na(m_wtkg) & agedays <= 0 & studyid == "JiVitA-3") %>%
  group_by(uid) %>%
  tally() %>%
  nrow()
# 28434 / 43608 = 65%
