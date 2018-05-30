# Author: Sifan Liu
# Date: Thu May 17 11:25:11 2018
# --------------
pkgs <- c('dplyr','ggplot2', "caret", "car", "cluster", "factoextra")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 



# prepare data ------------------------------------------------------------

load("master_wide.Rdata")
source("load.R")
source("Func.R")
GCItype <- read.csv("source/China GCI type.csv", colClasses = "factor")

levels(GCItype$modified_cat) <- c("Giants (n = 2)",
                                  "Anchor cities (n = 15)",
                                  "Rust belt (n = 6)",
                                  "Service-driven (n = 24)",
                                  "Industrial-driven (n = 56)")


ChinaInd <- read.csv("source/China_ind share.csv")
ChinaHI <- read.csv("source/China_ht park.csv") %>% group_by(name_final) %>% summarise(count = n())

test <- left_join(GCItype,ChinaHI, by = c("metro" = "name_final"))

China_wide <- wide %>% 
  filter(country == "China") %>%
  filter(ismetro == 1) 
summary(China_wide)

features <- China_wide %>% 
  select(metro,contains("2016")) %>%
  left_join(ChinaInd, by = c("metro" = "name_EN")) %>%
  select(metro, contains("2016"), ind_city, sev_city) %>%
  select(-gdpusc_2016)



# summary data by clusters ------------------------------------------------

load("master.Rdata")
China <- master %>% 
  filter(Country == "China") 

China_type <- China %>% 
  left_join(features[c("metro","cat")],by = c("Metro" = "metro")) %>%
  left_join(GCItype,by = c("Metro" = "metro")) %>%
  # group_by(cat,Type)%>%
  group_by(modified_cat,Type) %>%
  summarise_if(is.numeric, sum)

China_gdppk <- bind_cols((China_type %>% filter(Type =="RGDP"))[,1:4],
                         ((China_type %>% filter(Type =="RGDP"))[,5:22]/(China_type%>%filter(Type=="POP"))[,5:22])) %>%
  mutate(Type = "GDPPK")

China_type <- bind_rows(China_type, China_gdppk) %>% arrange(modified_cat)


# calculate matrix
n_cluster <- 5
n_features <- length(unique(China_type$Type))

China_total <- matrix(rep(t(China_type[(n_features*n_cluster+1):(n_features*n_cluster+n_features),5:22]),n_cluster),
                      nrow = 5*n_cluster,ncol = 18,byrow = TRUE)

China_share <- bind_cols(China_type[1:(n_features*n_cluster),1:4],
                         China_type[1:(n_features*n_cluster),5:22]/China_total) %>% mutate(var = "share")

China_indexed <- indexed_matrix(China_type[1:(n_features*n_cluster),]) %>% mutate(var = "index")

China_blogdata <- bind_rows(China_share, China_indexed)
write.csv(China_blogdata, "results/China_blogdata.csv")
write.csv(China_type, "results/China_type.csv")

China_long <- China_blogdata %>% 
  select(-IsMetro, - Units) %>%
  tidyr::gather(year, value, `2000`:`2017`, factor_key = TRUE) %>%
  mutate(year = as.integer(as.character(year))) 

# Plots -------------------------------------------------------------------

# Indexed =================================================

summary(GCItype$modified_cat)

data_index <- China_long %>% 
  filter(var == "index") %>%
  filter(Type %in% c("EMPL", "GDPPK"))

ggplot(data = data_index, aes(x = year, y = value, color = modified_cat)) +
  geom_point()+
  geom_line()+
  scale_color_discrete(name = "Type of Chinese metros")+
  facet_wrap(~ Type, scales = 'free')+
  theme_classic()

# Share ===================================================
data_share <- China_long %>% 
  filter(var == "share") %>%
  filter(Type %in% c("EMPL", "RGDP"))

ggplot(data = data_share, aes(x = year, y = value, fill = modified_cat, position = "dodge")) +
  geom_area()+
  scale_fill_discrete(name = "Type of Chinese metros")+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~ Type)+
  theme_classic()

# map ---------------------------------------------------------------------
China_coord <- worldmap %>%
  filter(country =="China") %>%
  select(metro, gdpppp_2016, Latitude, Longitude, metrofinalname, regionrankglobalmetro2014_2016) %>%
  left_join(GCItype, by = "metro")


pkgs <- c('ggmap', "ggrepel", "ggalt")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

map.China <- map_data("world") %>% filter(region %in% c("China","Taiwan"))

GMM_theme <- theme(panel.background = element_blank(),
                   plot.background = element_blank(),
                   panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank())


ggplot() +
  geom_polygon(data = map.China , aes(x = long, y = lat, group = group), fill = "#bdbdbd", color = "white") +
  geom_point(data = China_coord, aes(x = Longitude, y = Latitude, color = modified_cat, size = gdpppp_2016/1000), alpha = 0.8) + 
  geom_text_repel(data = filter( China_coord, gdpppp_2016 >= mean(gdpppp_2016)), aes(x = Longitude, y = Latitude, label = metrofinalname), size = 3, nudge_x = 2)+
  # geom_text(data = filter(China_coord, (modified_cat == 1) & (Capital != 1)), 
  #                 aes(x = Longitude, y = Latitude, label = metrofinalname), nudge_x = 2, color = "#636363", size = 3) +
  # geom_text(data = filter(China_coord, (modified_cat == 1) & (Capital == 1)), 
  #                 aes(x = Longitude, y = Latitude, label = metrofinalname), nudge_y = 0.5, color = "#636363", size = 4, fontface = "bold") +
  scale_size_continuous(labels = scales::comma, name = "Nominal GDP(Blns $, PPP rates), 2016") +
  scale_color_discrete(guide = FALSE) +
  facet_wrap(~ modified_cat)+
  # coord_quickmap(xlim = c(90,145), ylim = c(20, 51)) +
  GMM_theme %+%
  theme(legend.position = c(0.8,0.2))


# rank --------------------------------------------------------------------

ggplot(China_coord, aes(modified_cat, regionrankglobalmetro2014_2016, color = modified_cat, size = gdpppp_2016/1000))+
  scale_y_continuous(name = "Economic Performance Index, 2014 - 2016 \nRank (1 - 103)",
                     trans = 'reverse')+
  scale_x_discrete(name = element_blank())+
  scale_color_discrete(guide = FALSE)+
  scale_size_continuous(labels = scales::comma, name = "2016 Nominal GDP \n(Blns $, PPP rates)") +
  geom_jitter(width = 0.1)+
  theme_classic()
