library(yaml)
library(DBI)
library(tidyverse)
library(rvest)
library(blastula)
library(glue)
library(reticulate)
library(tidytext)
library(praise)


yaml::write_yaml(list(a = list(1, 2), b = "X"), "config.yaml")
config <- read_yaml("config.yaml")
config[["a"]]
