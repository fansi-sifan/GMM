# Author: Sifan Liu
# Date: Thu May 17 11:25:11 2018
# --------------
pkgs <- c('dplyr','ggplot2')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 



# prepare data ------------------------------------------------------------

load("master.Rdata")
load("Func.R")
GCItype <- read.csv("China GCI type.csv")
# merge Factory China and others

China <- master %>% 
  filter(Country == "China") %>% 
  left_join(GCItype[1:2],by = c("Metro" = "metro")) %>%
  # filter(IsMetro == 1) %>%
  group_by(GCI.type, Type) %>%
  summarise_if(is.numeric, sum)

China_total <- matrix(rep(t(China[13:16,5:22]),3),nrow = 12,ncol = 18,byrow = TRUE)  

China_share <- bind_cols(China[1:12,1:4],China[1:12,5:22]/China_total) %>% mutate(var = "share")
China_indexed <- indexed_matrix(China[1:12,]) %>% mutate(var = "index")

China_blogdata <- bind_rows(China_share, China_indexed)
# write.csv(China_blogdata, "China_blogdata.csv")

China_long <- China_blogdata %>% 
  select(-IsMetro, - Units) %>%
  filter(Type == "RGDP"| Type == "EMPL") %>%
  tidyr::gather(year, value, `2000`:`2017`, factor_key = TRUE) %>%
  mutate(year = as.integer(as.character(year)))


# Plots -------------------------------------------------------------------

ggplot(data = China_long , aes(x = year, y = value, color = GCI.type)) +
  geom_point()+
  geom_line()+
  facet_wrap(var ~ Type, scales = 'free')+
  theme_classic()
