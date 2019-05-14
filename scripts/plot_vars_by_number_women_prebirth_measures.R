library(viridis)
library(tidyverse)
library(synapser)
options(tibble.width = Inf)
synapser::synLogin(rememberMe = TRUE)

d_obs <- read_rds(path = "data/derived_data/d_obs.rds")

# Read and format data spec
f <- synapser::synGet("syn18671277")
data_spec <- readxl::read_xlsx(f$path)
names(data_spec) <- tolower(names(data_spec))
names(data_spec)[6] <- "longitudinal"
var_names <- data_spec %>%
  mutate(
    varnam = tolower(varnam),
    longitudinal = ifelse(longitudinal == "Yes", TRUE, FALSE)
  ) %>%
  select(variable = varnam, varlabel) 


d_plot <- d_obs %>%
  gather(key = "variable", value = "percent_observed", ctrycd:tv) %>%
  left_join(var_names)

variable_plot <- function(study = "St-Johns"){
  d_plot %>%
    mutate(count_group = ifelse(n_mweight >= 10, ">10", str_pad(n_mweight,width = 2, side = "left"))) %>%
    group_by(studyid, variable, count_group) %>%
    summarise(percent_observed = weighted.mean(percent_observed, n_subjects),n_subjects = sum(n_subjects)) %>%
    ungroup() %>%
    rename(n_mweight = count_group) %>%
    mutate(facet_var = str_c(n_mweight, " m_measures", " (n=", n_subjects, ")")) %>%
    filter(studyid == study, percent_observed >= 0.000001) %>%
    ggplot(aes(x = fct_reorder(variable,percent_observed), y = percent_observed, fill = n_subjects)) +
    geom_col() +
    scale_fill_gradientn(trans = "log10", colours = viridis_pal()(50), limits=c(1, 25000)) +
    # scale_fill_viridis(trans = "log10", breaks = scales::trans_breaks("log10", function(x) 10^x),
    #                    labels = scales::trans_format("log10", scales::math_format(10^.x)), 
    #                    direction = -1, discrete = FALSE) +
    scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "50", "100")) +
    facet_wrap(~facet_var, nrow = 1) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none", legend.key.width = unit(.65, "inches"), 
          legend.key.height = unit(.25, "inches"),
          panel.grid.major.y = element_blank()) +
    labs(y = "Observed variable percentage that has mother weight", x = "Study Variable (0% variables not shown)", 
         fill = "Number of mothers with weight measure", title = study,
         subtitle = "Summary of available variables that have mother weight measurements (m_measure) during pregnency")
  
  
}


bob <- unique(d_plot$studyid) %>% map(~(variable_plot(.x)))

map2(unique(d_plot$studyid), bob, ~ ggsave(filename = str_c("data/plots/",.x, "_variable_percent.png"), plot = .y,
                                           width = 20, height = 8))
##### Only look at greater than 3.

  d_plot %>%
    mutate(count_group = ifelse(n_mweight >= 3, ">3", str_pad(n_mweight,width = 2, side = "left"))) %>%
    filter(count_group == ">3", !variable %in% c("ctrycd", "country")) %>%
    group_by(studyid, variable, count_group, varlabel) %>%
    summarise(percent_observed = weighted.mean(percent_observed, n_subjects),n_subjects = sum(n_subjects)) %>%
    ungroup() %>%
    rename(n_mweight = count_group) %>%
    mutate(facet_var = str_c(varlabel, " (", variable, ")"),
           facet_var = fct_reorder(facet_var, percent_observed, sum, .desc = TRUE),
           study_var = str_c(studyid, " (n = ", format(n_subjects, big.mark = ","), ")"), 
           study_var = fct_reorder(study_var, percent_observed, sum, .desc = TRUE)) %>%
    filter(percent_observed >= 0.000001) %>%
    ggplot(aes(x = study_var, y = percent_observed), fill = "darkgrey") +
    geom_col() +
    scale_fill_viridis(option = "B") +
    scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "50", "100")) +
    facet_wrap(~facet_var, nrow = 9) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major.y = element_blank()) +
    labs(y = "Variable percentage with of 3 or more mother weights", x = "Study", 
         title = "Variable availability by study", 
         subtitle = "At least one weight measurement before 14 weeks and 3 or more measurements before birth")
  
ggsave(filename = "data/plots/allstudies_gt3measures_percent.png",height = 20, width = 28)  


## function plot for other options beyond 3

pnumber <- function(x){
  
  x_char = as.character(x)
  
  d_plot %>%
    mutate(count_group = ifelse(n_mweight >= x, str_c(">", x_char), str_pad(n_mweight,width = 2, side = "left"))) %>%
    filter(count_group == str_c(">",x_char), !variable %in% c("ctrycd", "country")) %>%
    group_by(studyid, variable, count_group, varlabel) %>%
    summarise(percent_observed = weighted.mean(percent_observed, n_subjects),n_subjects = sum(n_subjects)) %>%
    ungroup() %>%
    rename(n_mweight = count_group) %>%
    mutate(facet_var = str_c(varlabel, " (", variable, ")"),
           facet_var = fct_reorder(facet_var, percent_observed, sum, .desc = TRUE),
           study_var = str_c(studyid, " (n = ", format(n_subjects, big.mark = ","), ")"), 
           study_var = fct_reorder(study_var, percent_observed, sum, .desc = TRUE)) %>%
    filter(percent_observed >= 0.000001) %>%
    ggplot(aes(x = study_var, y = percent_observed), fill = "darkgrey") +
    geom_col() +
    scale_fill_viridis(option = "B") +
    scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "50", "100")) +
    facet_wrap(~facet_var, nrow = 9) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major.y = element_blank()) +
    labs(y = str_c("Variable percentage with of ", x_char , " or more mother weights"), x = "Study", 
         title = str_c("Variable availability by study (", x_char, " or more measurements before birth)" ), 
         subtitle = str_c("At least one weight measurement before 14 weeks (", x +1, "total)"))
  
  ggsave(filename = str_c("data/plots/allstudies_gt", x_char, "measures_percent.png"),height = 20, width = 28)  
  
  
}

pnumber(2); pnumber(3); pnumber(4); pnumber(5); pnumber(6)

