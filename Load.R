library('readxl')


setwd("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/01182018/Sifan")

master <- read_xlsx("../Original data/GMM17 database v3.xlsx")
group <- read.csv("source/GMM17 groups.csv")

MENA <- read_xlsx("../Original data/MENA database v2.xlsx")

#China <- filter(master, Country == "China")
