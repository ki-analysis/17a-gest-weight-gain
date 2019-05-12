d_obs <- read_rds(path = "data/derived_data/d_obs.rds")

#scale_fill_gradientn(trans = "log10", colours = viridis_pal()(50), limits=c(1, 25000))

d_plot <- d_obs %>%
  gather(key = "variable", value = "percent_observed", ctrycd:tv)

variable_plot <- function(study = "St-Johns"){
  d_plot %>%
    mutate(count_group = ifelse(m_wtkg_count >= 10, ">10", str_pad(m_wtkg_count,width = 2, side = "left"))) %>%
    group_by(studyid, variable, count_group) %>%
    summarise(percent_observed = weighted.mean(percent_observed, n),n = sum(n)) %>%
    ungroup() %>%
    rename(m_wtkg_count = count_group) %>%
    mutate(facet_var = str_c(m_wtkg_count, " m_measures", " (n=", n, ")")) %>%
    filter(studyid == study, percent_observed >= 0.000001) %>%
    ggplot(aes(x = fct_reorder(variable,percent_observed), y = percent_observed, fill = n)) +
    geom_col() +
    scale_fill_gradientn(trans = "log10", colours = viridis_pal()(50), limits=c(1, 25000)) +
    # scale_fill_viridis(trans = "log10", breaks = scales::trans_breaks("log10", function(x) 10^x),
    #                    labels = scales::trans_format("log10", scales::math_format(10^.x)), 
    #                    direction = -1, discrete = FALSE) +
    scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "1/2", "1")) +
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

count_p <- function(x) 1/sum(x> 0)

  d_plot %>%
    mutate(count_group = ifelse(m_wtkg_count >= 3, ">3", str_pad(m_wtkg_count,width = 2, side = "left"))) %>%
    filter(count_group == ">3", variable %in% c("anemia", "agpar1", "agpar5", "birthhc", "birthlen", "birthwt",
                                                "bpdaz", "bpdcm", "birthweek", "birthyr", "ctordcm", "ctoraz", 
                                                "dead", "delivery", "eclmp", "femurcm", "flaz", "gagebrth", 
                                                "gagedays", "goat", "lvbrth", "m_bmi", "m_diabp", "m_hgb", 
                                                "m_bmi", "m_muaccm", "m_ssftmm", "m_sysbp", "m_tsftmm", 
                                                "mage", "meducyrs", "nlivbrth", "mwork", "preeclmp", "pregnum")) %>%
    group_by(studyid, variable, count_group) %>%
    summarise(percent_observed = weighted.mean(percent_observed, n),n = sum(n)) %>%
    ungroup() %>%
    rename(m_wtkg_count = count_group) %>%
    mutate(facet_var = fct_reorder(variable, percent_observed, count_p)) %>%
    filter(percent_observed >= 0.000001) %>%
    ggplot(aes(x = str_c(studyid, "\nn = ", format(n, big.mark = ",")), y = percent_observed, fill = n)) +
    geom_col() +
    scale_fill_viridis(option = "B") +
    scale_y_continuous(breaks = c(0, .5, 1), labels = c("0", "1/2", "1")) +
    facet_wrap(~facet_var, ncol = 5) +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major.y = element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(y = "Variable percentage with of >3 mother weights", x = "Study", 
         title = "Variable availability with >3 mother weight measurements before birth", subtitle = "Selected variables shown")
  
ggsave(filename = "data/plots/allstudies_gt3measures_percent.png",width = 20, height = 8)  

