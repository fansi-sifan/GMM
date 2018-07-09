
pkgs <- c('readxl','dplyr', 'ggplot2', 'scales')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

library('readxl')
library('dplyr')
# master <- read_xlsx("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/GMM17 database v4.xlsx")
# MENA <- read_xlsx("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/01182018/Original data/MENA database v2.xlsx")
# save(master, file = 'master.Rdata')


group <- read.csv("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/01182018/Sifan/source/GMM17 groups.csv") %>% 
  select(country, region, incomegroup) %>% 
  unique

worldmap <- read_xlsx("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/index/GMM17_index_50_50_bothperiods_coordinates.xlsx")
MENAmap <- read_xlsx("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/pockets/GMM17MENA_country_metro_comparison_2014_2016.xlsx")
