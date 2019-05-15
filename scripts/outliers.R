source("scripts/_fns.R")

# remotes::install_github("ki-tools/growthstandards")
library(tidyverse)
library(growthstandards)

rmote::start_rmote()

d <- read_data(path)
