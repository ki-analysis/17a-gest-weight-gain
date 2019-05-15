
# Read and format rally dataset
read_data <- function(path) {
  d <- readr::read_csv("data/raw_data/rally_17.csv")

  names(d) <- tolower(names(d))
  # get rid of ki... prefixes in study IDs
  d$studyid <- gsub("^ki[0-9]+\\-(.*)", "\\1", d$studyid)
  d$studyid <- gsub("kiGH5241\\-", "", d$studyid)
  d
}

# Read and format data spec
read_data_spec <- function() {
  f <- synapser::synGet("syn18671277")
  data_spec <- readxl::read_xlsx(f$path)
  names(data_spec) <- tolower(names(data_spec))
  names(data_spec)[6] <- "longitudinal"
  data_spec %>%
    mutate(
      varnam = tolower(varnam),
      longitudinal = ifelse(longitudinal == "Yes", TRUE, FALSE)
    )
}


