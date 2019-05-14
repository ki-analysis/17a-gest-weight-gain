library(tidyverse)
library(synapser)
options(tibble.width = Inf)
synapser::synLogin(rememberMe = TRUE)


# Read and format data
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



# For all of the plots below, just look at mothers with non-na m_wtkg and agedays <= 0
dnna <- d %>%
  filter(!is.na(m_wtkg) & agedays <= 0 & gagedays >= 0) %>%
  mutate(before14 = gagedays <= 14 * 7) %>%
  select(studyid:citytown, before14, everything())

## From Ryan email.
# d %>%
#   filter(!is.na(m_wtkg) & agedays <= 0 & gagedays > 0) %>%
#   mutate(before14 = gagedays <= 14 * 7) %>%
#   group_by(studyid, uid, before14) %>%
#   tally() %>%
#   ungroup() %>%
#   spread(before14, n, sep="_") %>%
#   filter(before14_TRUE >= 1 & before14_FALSE >= 3)


# TODO: Plot of number of non-NA values for each subject-level variable, by study
# do free scale for count axis

dnna_long <- dnna %>% select(one_of(c("subjid", "studyid", "before14", filter(data_spec, longitudinal == TRUE) %>% .$varnam)))

# assumes that data is ordered by subject gagedays
dnna_subj <- dnna %>% 
  select(one_of(c("subjid", "studyid", "before14", filter(data_spec, longitudinal == FALSE) %>% .$varnam))) %>%
  mutate(uid = str_c(subjid, studyid, sep = "_")) %>%
  filter(!duplicated(uid))


count_no_na <- function(x){
  as.numeric(!is.na(x))
}




# This data shows how many mother weight measurements during gestation are available at the subject level.
(d_varcount <- dnna %>%
    count(subjid, studyid, before14) %>%
    spread(before14, n, sep="_", fill = 0) %>%
    rename(n_mweight = before14_FALSE, before14w = before14_TRUE) %>%
    mutate(before14w = before14w > 0))


# this recreates ryan's count barchart
d_count <- d_varcount %>% 
  count(studyid, n_mweight, before14w) %>%
  rename(n_subjects = n) 

perc_obs <- function(x, nn = n){
  x / nn
}

d_obs <- dnna_subj %>%
  filter(before14 == TRUE) %>%
  mutate_at(vars(ctrycd:tv),"count_no_na" ) %>%
  left_join(d_varcount) %>%
  group_by(studyid, n_mweight) %>%
  summarise_at(vars(ctrycd:tv),"sum") %>%
  ungroup() %>%
  left_join(d_count) %>%
  select(studyid, n_mweight, n_subjects, everything()) %>%
  mutate_at(vars(ctrycd:tv), perc_obs, nn = .$n_subjects ) %>%
  filter(n_mweight > 0)

write_rds(d_obs, path = "data/derived_data/d_obs.rds")




#### Stuff for longitudinal measures ####

# 
# # now build the missing by variable data to merge into counts above
# 
# d_tabulate <- function(dat = d_varcount, var1 = quo(studyid), var2 = quo(uid), var3 = quo(delivery) ){
#   dat %>% 
#     group_by(!!var1, !!var2, !!var3) %>%
#     summarise(n = n()) %>% 
#     filter(!!var3 == 0) %>%
#     select(!!var1, !!var2, n) %>%
#     rename(!!quo_name(var3) := n)
# }
# 
# perc_good <- function(x, nn = n){
#   (nn - x) / nn
# }
# 
# # colnames(d) %>% str_c(collapse = "))) %>%\n") %>% cat()
# 
# d_obs <- d_obs %>% 
#   left_join(d_tabulate(var3 = quo(ctrycd))) %>%
#   left_join(d_tabulate(var3 = quo(country))) %>%
#   left_join(d_tabulate(var3 = quo(citytown))) %>%
#   left_join(d_tabulate(var3 = quo(sex))) %>%
#   left_join(d_tabulate(var3 = quo(arm))) %>%
#   left_join(d_tabulate(var3 = quo(armcd))) %>%
#   left_join(d_tabulate(var3 = quo(gagebrth))) %>%
#   left_join(d_tabulate(var3 = quo(gagecm))) %>%
#   left_join(d_tabulate(var3 = quo(gagecrl))) %>%
#   left_join(d_tabulate(var3 = quo(gageus))) %>%
#   left_join(d_tabulate(var3 = quo(gagelmp))) %>%
#   left_join(d_tabulate(var3 = quo(gagerw))) %>%
#   left_join(d_tabulate(var3 = quo(gagerwsp))) %>%
#   left_join(d_tabulate(var3 = quo(pregout))) %>%
#   left_join(d_tabulate(var3 = quo(dead))) %>%
#   left_join(d_tabulate(var3 = quo(agedth))) %>%
#   left_join(d_tabulate(var3 = quo(brthyr))) %>%
#   left_join(d_tabulate(var3 = quo(brthweek))) %>%
#   left_join(d_tabulate(var3 = quo(birthwt))) %>%
#   left_join(d_tabulate(var3 = quo(birthlen))) %>%
#   left_join(d_tabulate(var3 = quo(birthhc))) %>%
#   left_join(d_tabulate(var3 = quo(delivery))) %>%
#   left_join(d_tabulate(var3 = quo(apgar1))) %>%
#   left_join(d_tabulate(var3 = quo(apgar5))) %>%
#   left_join(d_tabulate(var3 = quo(multbrth))) %>%
#   left_join(d_tabulate(var3 = quo(agedays))) %>%
#   left_join(d_tabulate(var3 = quo(ageimpfl))) %>%
#   left_join(d_tabulate(var3 = quo(ageimpcm))) %>%
#   left_join(d_tabulate(var3 = quo(gagedays))) %>%
#   left_join(d_tabulate(var3 = quo(epochn))) %>%
#   left_join(d_tabulate(var3 = quo(epoch))) %>%
#   left_join(d_tabulate(var3 = quo(visitnum))) %>%
#   left_join(d_tabulate(var3 = quo(visit))) %>%
#   left_join(d_tabulate(var3 = quo(antptnum))) %>%
#   left_join(d_tabulate(var3 = quo(antpt))) %>%
#   left_join(d_tabulate(var3 = quo(wtkg))) %>%
#   left_join(d_tabulate(var3 = quo(htcm))) %>%
#   left_join(d_tabulate(var3 = quo(lencm))) %>%
#   left_join(d_tabulate(var3 = quo(bmi))) %>%
#   left_join(d_tabulate(var3 = quo(hcircm))) %>%
#   left_join(d_tabulate(var3 = quo(muaccm))) %>%
#   left_join(d_tabulate(var3 = quo(tsftmm))) %>%
#   left_join(d_tabulate(var3 = quo(ssftmm))) %>%
#   left_join(d_tabulate(var3 = quo(waz))) %>%
#   left_join(d_tabulate(var3 = quo(haz))) %>%
#   left_join(d_tabulate(var3 = quo(baz))) %>%
#   left_join(d_tabulate(var3 = quo(hcaz))) %>%
#   left_join(d_tabulate(var3 = quo(whz))) %>%
#   left_join(d_tabulate(var3 = quo(muaz))) %>%
#   left_join(d_tabulate(var3 = quo(r_haz))) %>%
#   left_join(d_tabulate(var3 = quo(r_hcaz))) %>%
#   left_join(d_tabulate(var3 = quo(igbwaz))) %>%
#   left_join(d_tabulate(var3 = quo(igbhaz))) %>%
#   left_join(d_tabulate(var3 = quo(igbhcaz))) %>%
#   left_join(d_tabulate(var3 = quo(wlr))) %>%
#   left_join(d_tabulate(var3 = quo(igbwlraz))) %>%
#   left_join(d_tabulate(var3 = quo(gagedctr))) %>%
#   left_join(d_tabulate(var3 = quo(gagesctr))) %>%
#   left_join(d_tabulate(var3 = quo(abcircm))) %>%
#   left_join(d_tabulate(var3 = quo(acaz))) %>%
#   left_join(d_tabulate(var3 = quo(chcircm))) %>%
#   left_join(d_tabulate(var3 = quo(bpdcm))) %>%
#   left_join(d_tabulate(var3 = quo(bpdaz))) %>%
#   left_join(d_tabulate(var3 = quo(femurcm))) %>%
#   left_join(d_tabulate(var3 = quo(flaz))) %>%
#   left_join(d_tabulate(var3 = quo(ctordcm))) %>%
#   left_join(d_tabulate(var3 = quo(ctoraz))) %>%
#   left_join(d_tabulate(var3 = quo(m_htcm))) %>%
#   left_join(d_tabulate(var3 = quo(m_bmi))) %>%
#   left_join(d_tabulate(var3 = quo(m_muaccm))) %>%
#   left_join(d_tabulate(var3 = quo(m_ssftmm))) %>%
#   left_join(d_tabulate(var3 = quo(m_tsftmm))) %>%
#   left_join(d_tabulate(var3 = quo(sysbp))) %>%
#   left_join(d_tabulate(var3 = quo(diabp))) %>%
#   left_join(d_tabulate(var3 = quo(m_sysbp))) %>%
#   left_join(d_tabulate(var3 = quo(m_diabp))) %>%
#   left_join(d_tabulate(var3 = quo(hgb))) %>%
#   left_join(d_tabulate(var3 = quo(m_hgb))) %>%
#   left_join(d_tabulate(var3 = quo(m_prot_c))) %>%
#   left_join(d_tabulate(var3 = quo(m_prot_n))) %>%
#   left_join(d_tabulate(var3 = quo(pregnum))) %>%
#   left_join(d_tabulate(var3 = quo(mscr))) %>%
#   left_join(d_tabulate(var3 = quo(stlbrth))) %>%
#   left_join(d_tabulate(var3 = quo(lvbrth))) %>%
#   left_join(d_tabulate(var3 = quo(smoked))) %>%
#   left_join(d_tabulate(var3 = quo(parity))) %>%
#   left_join(d_tabulate(var3 = quo(gravida))) %>%
#   left_join(d_tabulate(var3 = quo(nlivbrth))) %>%
#   left_join(d_tabulate(var3 = quo(dsppreg))) %>%
#   left_join(d_tabulate(var3 = quo(dlvloc))) %>%
#   left_join(d_tabulate(var3 = quo(dlvpsn))) %>%
#   left_join(d_tabulate(var3 = quo(gdm))) %>%
#   left_join(d_tabulate(var3 = quo(ghtn))) %>%
#   left_join(d_tabulate(var3 = quo(preeclmp))) %>%
#   left_join(d_tabulate(var3 = quo(eclmp))) %>%
#   left_join(d_tabulate(var3 = quo(anemia))) %>%
#   left_join(d_tabulate(var3 = quo(comprisk))) %>%
#   left_join(d_tabulate(var3 = quo(mage))) %>%
#   left_join(d_tabulate(var3 = quo(meducyrs))) %>%
#   left_join(d_tabulate(var3 = quo(mmarit))) %>%
#   left_join(d_tabulate(var3 = quo(mwork))) %>%
#   left_join(d_tabulate(var3 = quo(feducyrs))) %>%
#   left_join(d_tabulate(var3 = quo(bicycle))) %>%
#   left_join(d_tabulate(var3 = quo(chicken))) %>%
#   left_join(d_tabulate(var3 = quo(elec))) %>%
#   left_join(d_tabulate(var3 = quo(goat))) %>%
#   left_join(d_tabulate(var3 = quo(h2osrcp))) %>%
#   left_join(d_tabulate(var3 = quo(inctot))) %>%
#   left_join(d_tabulate(var3 = quo(mcycle))) %>%
#   left_join(d_tabulate(var3 = quo(mobile))) %>%
#   left_join(d_tabulate(var3 = quo(nchldlt5))) %>%
#   left_join(d_tabulate(var3 = quo(nperson))) %>%
#   left_join(d_tabulate(var3 = quo(nrooms))) %>%
#   left_join(d_tabulate(var3 = quo(roof))) %>%
#   left_join(d_tabulate(var3 = quo(sanitatn))) %>%
#   left_join(d_tabulate(var3 = quo(tv))) %>%
#   rename(m_wtkg_count = uid) %>%
#   replace(is.na(.), 0) %>%
#   mutate_at(vars(ctrycd:tv), perc_good, nn = .$n)
# 
# 
