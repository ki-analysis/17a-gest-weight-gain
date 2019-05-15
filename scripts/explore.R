# git clone https://user.name@git.ghap.io/stash/scm/hbgd/rally-17.git

source("scripts/_fns.R")

library(tidyverse)

rmote::start_rmote()
colorout::noColorOut()

d <- read_data(path)
data_spec <- read_data_spec()

# birth weight for GA (INTERGROWTH)
tmp <- d %>%
  group_by(uid, studyid) %>%
  summarise(
    igbwaz = igbwaz[max(which(!is.na(igbwaz))[1], 1)],
    birthwt = birthwt[1],
    birthlen = birthlen[1])

1 - length(unique(d$uid[!is.na(d$igbwaz)])) / length(unique(d$uid))
# 0.4622714
filter(d, uid == 8757) %>% select(agedays, igbwaz, studyid)

length(which(is.na(tmp$igbwaz))) / nrow(tmp)
# 0.9997978

1 - length(unique(d$uid[!is.na(d$birthwt)])) / length(unique(d$uid))


length(which(is.na(tmp$birthwt))) / nrow(tmp)
# 0.07882792

length(which(is.na(tmp$birthlen))) / nrow(tmp)
# 0.1263988

tmp <- d %>%
  group_by(uid) %>%
  summarise(m_htcm = m_htcm[1])
nrow(tmp)
length(which(is.na(tmp$m_htcm)))
# 17% missing mother's height

tmp <- d %>%
  group_by(uid) %>%
  summarise(m_bmi = m_bmi[1])
nrow(tmp)
length(which(is.na(tmp$m_bmi)))
# 17% missing mother's bmi

tmp2 <- d %>%
  group_by(uid, studyid) %>%
  summarise(m_bmi = m_bmi[max(which(!is.na(m_bmi))[1], 1)]) %>%
  ungroup() %>%
  mutate(has_bmi = !is.na(m_bmi)) %>%
  group_by(studyid, has_bmi) %>%
  tally() %>%
  spread(has_bmi, n, fill = 0, sep = "_")

tmp2 %>%
  knitr::kable(format = "markdown")

# |studyid        | has_bmi_FALSE| has_bmi_TRUE|
# |:--------------|-------------:|------------:|
# |HealthyStart   |          1404|            6|
# |iLiNS-DYAD-G   |          1299|           21|
# |iLiNS-DYAD-M   |           175|         1071|
# |INTERBIO-21    |             0|         2846|
# |INTERGROWTH-21 |             0|         4500|
# |JiVitA-3       |           229|        28282|
# |St-Johns       |          2001|            0|
# |WomenFirst     |          2668|            0|

1 - length(unique(d$uid[!is.na(d$m_bmi)])) / length(unique(d$uid))


# How many subjects have mother's weight in first 14 weeks
#   and mother's weight within 2 weeks of birth
d %>%
  select(studyid, uid, gagebrth, gagedays, agedays, m_wtkg) %>%
  filter(!is.na(m_wtkg) & agedays <= 0 & gagedays > 0) %>%
  group_by(uid) %>%
  filter(min(gagedays) < 14 * 7 & min(gagedays - gagebrth) < 14)

meas_timing <- d %>%
  filter(!is.na(m_wtkg) & agedays <= 0 & gagedays > 0
   & gagedays <= gagebrth) %>%
  group_by(uid, studyid) %>%
  summarise(
    from_concep = min(gagedays),
    from_birth = gagebrth[1] - max(gagedays)
  ) %>%
  ungroup()

ggplot(meas_timing, aes(from_concep)) + geom_histogram() + theme_bw()
# How come so many zeros? How would they know to measure mom's weight on day of conception?

# Look into this:
meas_timing %>%
  mutate(day1 = from_concep == 1) %>%
  group_by(studyid, day1) %>%
  tally() %>%
  spread(day1, n, fill = 0, sep = "_") %>%
  knitr::kable(format = "markdown")

# |studyid        | day1_FALSE| day1_TRUE|
# |:--------------|----------:|---------:|
# |HealthyStart   |       1332|         0|
# |iLiNS-DYAD-G   |          1|      1223|
# |iLiNS-DYAD-M   |       1243|         0|
# |INTERBIO-21    |       2259|         0|
# |INTERGROWTH-21 |       4415|         0|
# |JiVitA-3       |      20290|         0|
# |St-Johns       |       1991|         0|
# |WomenFirst     |       1994|        24|

# iLiNS-DYAD-G has all subjects but one with mother's weight measured day after conception...



# For measurements on day of delivery, do they show indication of before or after delivery?

meas_timing_q <- tibble(
  q = ppoints(100),
  from_concep = quantile(meas_timing$from_concep, ppoints(100)),
  from_birth = quantile(meas_timing$from_birth, ppoints(100)),
)
ggplot(meas_timing_q, aes(q, from_concep / 7)) + geom_point() + theme_bw()
ggplot(meas_timing_q, aes(q, from_birth / 7)) + geom_point() + theme_bw()

median(meas_timing$from_concep)
# median is 73 days or 10.4 weeks
quantile(meas_timing$from_concep, 0.75)

last_meas <- d %>%
  filter(!is.na(m_wtkg) & agedays <= 0 & gagedays > 0) %>%
  group_by(uid) %>%
  summarise(days_from_birth = gagebrth[1] - max(gagedays))

ggplot(last_meas, aes(days_from_birth)) + geom_histogram() + theme_bw()
# median is 40 days or 5.7 weeks

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
  filter(!is.na(m_wtkg) & agedays <= 0 & gagedays > 0) %>%
  group_by(uid) %>%
  tally() %>%
  rename(n_meas = n) %>%
  ungroup() %>%
  group_by(n_meas) %>%
  tally() %>%
  arrange(-n_meas) %>%
  mutate(cumn = cumsum(n))

# Additional criteria: number of measurements after 14 weeks who are also measured before 14 weeks
momweight_ccount2 <- d %>%
  filter(!is.na(m_wtkg) & agedays <= 0 & gagedays > 0) %>%
  mutate(before14 = gagedays <= 14 * 7) %>%
  group_by(uid, before14) %>%
  tally() %>%
  ungroup() %>%
  spread(before14, n, sep = "_") %>%
  filter(before14_TRUE >= 1) %>%
  rename(n_meas = before14_FALSE) %>%
  group_by(n_meas) %>%
  tally() %>%
  arrange(-n_meas) %>%
  mutate(cumn = cumsum(n))

rmote::rmote_device(width = 12 * 72, height = 6 * 72)
filter(momweight_ccount2, n_meas <= 15) %>%
ggplot(aes(n_meas, cumn)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Number of non-NA mother's weight observations after 14 weeks",
    y = "Number of subjects",
    title = "Number of subjects having at least x mother's weight observations after 14 weeks",
    subtitle = "Given that there is at least one mother's weight observation before 14 weeks")


momweight_study_ccount <- d %>%
  filter(!is.na(m_wtkg) & agedays <= 0 & gagedays > 0) %>%
  mutate(before14 = gagedays <= 14 * 7) %>%
  group_by(uid, before14, studyid) %>%
  tally() %>%
  ungroup() %>%
  spread(before14, n, sep = "_") %>%
  filter(before14_TRUE >= 1) %>%
  rename(n_meas = before14_FALSE) %>%
  group_by(n_meas, studyid) %>%
  tally() %>%
  ungroup() %>%
  arrange(studyid, -n_meas) %>%
  group_by(studyid) %>%
  mutate(cumn = cumsum(n))


tmp <- d %>%
  group_by(subjid) %>%
  summarise(m_htcm = m_htcm[1])
length(which(is.na(tmp$m_htcm)))


tableau <- ggthemes::scale_color_tableau()$palette(4)

rmote::rmote_device(width = 18 * 72, height = 4 * 72)
d %>%
  filter(!is.na(m_wtkg) & agedays <= 0 & gagedays > 0) %>%
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







# Per-subject number of unique values for each variable
nu <- function(x) length(unique(x[!is.na(x)]))
usubj <- d %>%
  group_by(uid) %>%
  summarise_all(nu)

usubj

vars <- unlist(lapply(usubj, function(x) any(x > 1)))
lvars <- names(vars)[vars]
svars <- names(vars)[!vars]

data_spec$longitudinal[84] <- TRUE
data_spec$longitudinal[18] <- FALSE

lvars2 <- data_spec$varnam[data_spec$longitudinal]
svars2 <- data_spec$varnam[!data_spec$longitudinal]

setdiff(lvars, lvars2) # longitudinal but shouldn't be
setdiff(lvars2, lvars) # specified as longitudinal but constant

table(usubj$bicycle)
filter(usubj, bicycle == 2)
filter(d, uid == 14746) %>% select(bicycle) %>% data.frame()


filter(data_spec, is.na(longitudinal))
