# install.packages("synapser", repos=c("http://ran.synapse.org", "http://cran.fhcrc.org"))

library(tidyverse)
library(synapser)
library(viridis)
synapser::synLogin(rememberMe = TRUE)
options(tibble.width = Inf)

rmote::start_rmote()

# Read and format data
# d <- readr::read_csv("/data/git/hbgd/rally-17/Sprint\ A/adam/rally_17.csv")

d <- readr::read_csv("data/raw_data/rally_17.csv")

names(d) <- tolower(names(d))
# get rid of ki... prefixes in study IDs
d$studyid <- gsub("^ki[0-9]+\\-(.*)", "\\1", d$studyid)
d$studyid <- gsub("kiGH5241\\-", "", d$studyid)

# Read and format data spec
f <- synapser::synGet("syn18671277")
data_spec <- readxl::read_xlsx(f$path)
names(data_spec) <- tolower(names(data_spec))
names(data_spec)[6] <- "longitudinal"
data_spec <- data_spec %>%
  mutate(
    varnam = tolower(varnam),
    longitudinal = ifelse(longitudinal == "Yes", TRUE, FALSE)
  )

table(data_spec$longitudinal)
# FALSE  TRUE 
#    65    55 

# For all of the plots below, just look at mothers with non-na m_wtkg and agedays <= 0
d %>%
  filter(!is.na(m_wtkg) & agedays <= 0) # %>% ...


# TODO: Plot of number of non-NA values for each subject-level variable, by study
# do free scale for count axis

### I did this by study and number of mother measures (see dat_vars_by_number_women_prebirth_measures.R)

# TODO: Heatmap of number of non-NA subject-level variables, by study
# maybe broken down into categories etc.



# TODO: Plot of number of subjects with x non-NA subject-level variables
# Detail: for each subject, select just subject-level variables
# Then count the number of non-NA values
# Then tally this number and plot the frequency of this number
# e.g. how many subjects have non-NA values for all 65 subject-level variables?
#      how many have non-NA values for 64 subject-level variables
#      etc.

# TODO: To get at which variables have the highest co-occurance, look at the most frequently-occuring combinations of subject-level variables
# This could get combinatorically complex but worth a try - maybe look at top 50 combinations of non-NA variables and how many subjects that covers

# TODO: ways to visualize missingness of longitudinal variables

