library('readxl')


master <- read_xlsx("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/GMM17 database v4.xlsx")
group <- read.csv("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/01182018/Sifan/source/GMM17 groups.csv") %>% 
  select(country, region, incomegroup) %>% 
  unique

MENA <- read_xlsx("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/01182018/Original data/MENA database v2.xlsx")

#China <- filter(master, Country == "China")
save(master, file = 'master.Rdata')
