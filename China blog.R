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
GCItype <- read.csv("source/China GCI type.csv")
coast_p <- c("Liaoning", "Hebei", "Shandong", "Jiangsu", "Anhui", "Zhejiang", "Fujian","Guangdong", "Guangxi")

GCItype <- GCItype %>% 
  mutate(coastal = case_when(
    grepl(paste(coast_p,collapse = '|'),GCItype$metro) ~ 1,
    TRUE ~ 0
  ))  %>% 
  mutate(cat_3 = case_when(
    (cat_2 > 3) & (coastal == 1) ~ 4,
    (cat_2 > 3) & (coastal == 0) ~ 5,
    TRUE ~ as.numeric(cat_2)
  )) 

GCItype[] <- lapply(GCItype, factor)

levels(GCItype$cat_1) <- c("Giants (n = 2)",
                           "Anchor cities (n = 14)",
                            "Rust belt (n = 6)",
                            "Service cities (n = 24)",
                            "Industry cities (n = 57)")

levels(GCItype$cat_2) <- c("Giants (n = 2)",
                         "Anchor cities (n = 14)",
                         "Rust belt (n = 6)",
                         "manufacturing (n = 28)",
                         "others (n = 53)")

levels(GCItype$cat_3) <- c("Giants (n = 2)",
                           "Anchor cities (n = 14)",
                           "Rust belt (n = 6)",
                           "Coastal (n = 53)",
                           "Inland (n = 28)")


features <- wide %>%
  filter(country == "China") %>%
  filter(ismetro == 1) %>%
  select(metro,contains("2016")) 


# summary data by clusters ------------------------------------------------

load("master.Rdata")


China_type <- master %>% 
  filter(Country == "China") %>%
  left_join(GCItype,by = c("Metro" = "metro")) %>%
  group_by(cat_1,Type) %>%
  rename(modified = cat_1)%>%
  summarise_if(is.numeric, sum)

China_gdppk <- bind_cols((China_type %>% filter(Type =="RGDP"))[,1:4],
                         ((China_type %>% filter(Type =="RGDP"))[,5:22]/(China_type%>%filter(Type=="POP"))[,5:22])) %>%
  mutate(Type = "GDPPK")

China_type <- bind_rows(China_type, China_gdppk) %>% arrange(modified)


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
# write.csv(China_type, "results/China_type.csv")

China_long <- China_blogdata %>% 
  select(-IsMetro, - Units) %>%
  tidyr::gather(year, value, `2000`:`2017`, factor_key = TRUE) %>%
  mutate(year = as.integer(as.character(year))) 

# Plots -------------------------------------------------------------------

# Indexed =================================================



data_index <- China_long %>% 
  filter(var == "index") %>%
  filter(Type %in% c("EMPL", "GDPPK"))

ggplot(data = data_index, aes(x = year, y = value, color = modified)) +
  geom_point()+
  geom_line()+
  scale_color_discrete(name = "Type of Chinese metros")+
  facet_wrap(~ Type, scales = 'free')+
  theme_classic()

# Share ===================================================
data_share <- China_long %>% 
  filter(var == "share") %>%
  filter(Type %in% c("EMPL", "RGDP"))

ggplot(data = data_share, aes(x = year, y = value, fill = modified, position = "dodge")) +
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
  geom_polygon(data = map.China , aes(x = long, y = lat, group = group), fill = "#cccccc", color = "white") +
  GMM_theme

ggplot() +
  geom_polygon(data = map.China , aes(x = long, y = lat, group = group), fill = "#cccccc", color = "white") +
  geom_point(data = China_coord, aes(x = Longitude, y = Latitude, color = cat_1, size = gdpppp_2016/1000), alpha = 0.5) + 
  # geom_text_repel(data = filter( China_coord, gdpppp_2016 >= median(gdpppp_2016)), 
  #                 aes(x = Longitude, y = Latitude, label = metrofinalname), size = 3)+
  geom_text_repel(data = filter(China_coord, Capital == 1),
                  aes(x = Longitude, y = Latitude, label = metrofinalname), color = "#636363", size = 4) +
  scale_size_continuous(range = c(1,8),
                        labels = scales::comma, name = "Nominal GDP(Blns $, PPP rates), 2016" ) +
  scale_color_discrete(name = "Five types of Chinese metros") +
  # facet_wrap(~ cat_3) +
  GMM_theme %+%
  theme(legend.position = c(0.2,0.2),
        legend.key = element_rect(colour = NA, fill = NA))


# pearl delta

ggplot() +
  geom_polygon(data = map.China , aes(x = long, y = lat, group = group), fill = "#cccccc", color = "white") +
  geom_point(data = China_coord, aes(x = Longitude, y = Latitude, color = cat_1, size = gdpppp_2016/1000), alpha = 0.8) + 
  # geom_text_repel(data = filter( China_coord, gdpppp_2016 >= median(gdpppp_2016)), 
  #                 aes(x = Longitude, y = Latitude, label = metrofinalname), size = 3)+
  geom_text(data = China_coord,
                  aes(x = Longitude, y = Latitude, label = paste0(metrofinalname,"(",regionrankglobalmetro2014_2016,")")), 
            color = "#636363", size = 4, nudge_y = 0.05) +
  scale_size_continuous(range = c(1,8),
                        labels = scales::comma, name = "Nominal GDP(Blns $, PPP rates), 2016" ) +
  scale_color_discrete(name = "Five types of Chinese metros") +
  coord_quickmap(xlim = c(112,115), ylim = c(20, 24)) +
  GMM_theme %+%
  theme(legend.position = c(0.2,0.2),
        legend.key = element_rect(colour = NA, fill = NA))

# rank --------------------------------------------------------------------

ggplot(China_coord, aes(cat_1, regionrankglobalmetro2014_2016, color = cat_1, size = gdpppp_2016/1000))+
  scale_y_continuous(name = "Economic Performance Index, 2014 - 2016 \nRank (1 - 103)",
                     trans = 'reverse')+
  scale_x_discrete(name = element_blank())+
  scale_color_discrete(guide = FALSE)+
  scale_size_continuous(labels = scales::comma, name = "2016 Nominal GDP \n(Blns $, PPP rates)") +
  geom_jitter(width = 0.1)+
  theme_classic()

China_coord %>% group_by(cat_1) %>% summarise(avg = mean(regionrankglobalmetro2014_2016))
