# Author: Sifan Liu
# Date: Thu Apr 19 09:12:51 2018
# --------------
pkgs <- c('tidyverse', 'ggmap', "ggrepel", "ggalt")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 


source("load.R")

# Get World Map -----------------------------------------------------------
# data to map
worldmap$istop_q_14_16 <- as.factor(ifelse(worldmap$q_regionrankglobalmetro2014_2016 == 1, 1, 0))
worldmap <- worldmap[order(-worldmap$q_regionrankglobalmetro2014_2016),]

# base map
map.world <- map_data("world")
GMM_theme <- theme(panel.background = element_blank(),
                   plot.background = element_blank(),
                   panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank())

GMM_wrapper <- ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#bdbdbd", color = "white") +
  geom_point(data = worldmap %>% filter(istop_q_14_16 == 0), aes(x = Longitude, y = Latitude, color = "#1a9850", size = gdpppp_2016/1000),alpha = 0.8) +
  geom_point(data = worldmap %>% filter(istop_q_14_16 == 1), aes(x = Longitude, y = Latitude, color = "#f46d43", size = gdpppp_2016/1000),alpha = 0.8) +
  scale_size_continuous(labels = scales::comma, name = "Nominal GDP(Blns $, PPP rates), 2016") +
  scale_color_discrete(name = "Economic performance index (Regional rank)",label = c("All others","Top 20 %"))+
  

GMM_label_top <- geom_text(data = filter(worldmap,istop_q_14_16 == 1), aes(x = Longitude, y = Latitude, label = metrofinalname), 
                           nudge_y = 0.5, color = "#636363", size = 3)

GMM_label_large <- geom_text(data = filter(worldmap,gdpppp_2016 > 200000), aes(x = Longitude, y = Latitude, label = metrofinalname), 
                             nudge_y = 0.5, color = "#636363", size = 3)

# Plot the map ------------------------------------------------------------

# world map ================================================

GMM_wrapper   # geom_text_repel(data = filter(worldmap,(country!= "China") & (istop_q_14_16 == 1 | gdpppp_2016 > 500000)), aes(x = Longitude, y = Latitude, label = metrofinalname),
  #           nudge_y = 1.5, color = "#636363", size = 3) +
  # geom_text_repel(data = filter(worldmap,(country == "China") & (istop_q_14_16 == 1 & gdpppp_2016 > 500000)), aes(x = Longitude, y = Latitude, label = metrofinalname),
  #           nudge_y = 1.5, color = "#636363", size = 3) +
  coord_map("mercator",xlim = c(-180,180), ylim = c(-60, 65)) +
  GMM_theme 

# ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Draft/charts/Fig 10_with labels.pdf", width = 20, height = 12)
ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Draft/charts/Fig 10_no labels.pdf", width = 20, height = 12)   

#ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/plots/worldmap.png", width = 20, height = 12)   
#ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/plots/worldmap.pdf", width = 20, height = 12)  


# China ================================================
GMM_wrapper +
  # GMM_label_top +
  # GMM_label_large +
  coord_quickmap(xlim = c(100,145), ylim = c(20, 45)) +
  GMM_theme 

# ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Draft/charts/Fig 10_China_with labels.pdf", width = 20, height = 12)
ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Draft/charts/Fig 10_China_no labels.pdf", width = 10, height = 6)   

#ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/plots/China.png", width = 10, height = 6)  



# China worst performer ---------------------------------------------------

chinamap <- worldmap %>%
  filter(country == "China") %>%
  mutate(top_worst = as.factor(ifelse(regionrankglobalmetro2014_2016 > 10, ifelse(regionrankglobalmetro2014_2016 > 93,3,2),1)))

ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#bdbdbd", color = "white") +
  geom_point(data = chinamap, aes(x = Longitude, y = Latitude, color = top_worst, size = gdpppp_2016), alpha = 0.8) + 
  geom_text(data = filter(chinamap,top_worst != 2), aes(x = Longitude, y = Latitude, label = metrofinalname), 
            nudge_y = 0.5, color = "#636363", size = 3) + 
  scale_size_continuous(labels = scales::comma, name = "Nominal GDP(PPP), 2016") +
  scale_color_manual(values = c("#b2182b", "#bdbdbd","#2166ac"), 
                     labels = c("Top 10", "","Bottom 10"), 
                     name = 'Performance on Economic Index') +
  coord_quickmap(xlim = c(100,145), ylim = c(20, 50)) +
  GMM_theme 

ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Draft/charts/China_blog.png", width = 10, height = 6)   


# US ================================================
GMM_wrapper +
  # GMM_label_top +
  # GMM_label_large +
  coord_quickmap(xlim = c(-125,-60), ylim = c(18, 53)) +
  GMM_theme 


# ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Draft/charts/Fig 10_US_with labels.pdf", width = 10, height = 6)   
ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Draft/charts/Fig 10_US_no labels.pdf", width = 10, height = 6)   

# Europe ================================================
GMM_wrapper  +
  # GMM_label_top +
  # GMM_label_large +
  coord_quickmap(xlim = c(-10,22), ylim = c(40, 60)) +
  GMM_theme 

# ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Draft/charts/Fig 10_EU_with labels.pdf", width = 15, height = 8)
ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Draft/charts/Fig 10_EU_no labels.pdf", width = 8, height = 4)  

#ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/plots/Europe.png", width = 10, height = 6)   


# MENA pockets of growth --------------------------------------------------

# take out Jerusalem
MENAmap <- MENAmap %>% filter(metro != "Jerusalem")

ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#bdbdbd", color = "white") +
  geom_point(data = MENAmap, aes(x = Longitude, y = Latitude, color = as.factor(pockets_growth2014_2016), size = gdpppp_2016/1000), alpha = 0.8) + 
  scale_size_continuous(labels = scales::comma, name ="Nominal GDP(Blns $, PPP rates), 2016") +
  scale_color_manual(values = c("#b2182b", "#2166ac"), labels = c("All others","Pockets of growth"), name = "Growth Status") +
  # geom_text_repel(data = MENAmap, aes(x = Longitude, y = Latitude, label = metro),
  #                 nudge_y = 0.5, color = "#636363", size = 2) +
  coord_quickmap(xlim = c(-12,62), ylim = c(10, 40)) +
  GMM_theme 

# ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/plots/MENA.png", width = 10, height = 6)  
ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Draft/charts/Fig 14_no labels.pdf", width = 10, height = 6)
ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Draft/charts/Fig 14_with labels.pdf", width = 10, height = 6)  


# Slide map ---------------------------------------------------------------

ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#bdbdbd", color = "white")+
  geom_point(data = worldmap, aes(x = Longitude, y = Latitude), color = "#d95f0e", alpha = 0.8, size = 5) +
  coord_map("mercator",xlim = c(-180,180), ylim = c(-60, 65)) +
  GMM_theme

ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Presentation/figures/map.png", width = 20, height = 12, bg = "transparent")



# Distribution ------------------------------------------------------------

viz_data <- GMM17 %>% filter(ismetro == 1)
viz_data$gdppk_2016 <- viz_data$gdpppp_2016/viz_data$poptott_2016

GGHist <- theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(colour = "white", size = 24))

ggplot(data = viz_data, aes(viz_data$emptot_2016)) + 
  geom_histogram(binwidth = 100,
                 col = "white",
                 fill = "white",
                 alpha = 0.8) + 
  geom_vline(aes(xintercept = mean(viz_data$emptot_2016)),col = "#2c7fb8",size = 2) +
  GGHist
ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Presentation/figures/emp.png", width = 24, height = 4, bg = "transparent")

ggplot(data = viz_data, aes(viz_data$gdppk_2016)) + 
  geom_histogram(binwidth = 1,
                 col = "white",
                 fill = "white",
                 alpha = 0.8) + 
  geom_vline(aes(xintercept = sum(viz_data$gdpppp_2016)/sum(viz_data$poptott_2016)),col = "#2c7fb8",size = 2) +
  GGHist

ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Presentation/figures/gdppk.png", width = 24, height = 4, bg = "transparent")


# For deck
# US
ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#f0f0f0", color = "white") +
  geom_point(data = worldmap, aes(x = Longitude, y = Latitude, color = istop_q_14_16, size = `2016`), alpha = 0.8) + 
  scale_size_continuous(range = c(1,20),labels = scales::comma, name = "Nominal GDP(PPP), 2016") +
  scale_color_manual(values = c("#f46d43", "#2c7bb6"), 
                     labels = c("All others","Best performers by region (top 20 percent)"), 
                     name = 'Performance on Economic Index')+
  geom_label_repel(data = filter(worldmap,istop_q_14_16 == 1 & region_adj == "North America"), 
                   aes(x = Longitude, y = Latitude, label = metrofinalname), 
                   nudge_y = 0.5, color = "white", size = 10, fill = "#2c7bb6") +
  coord_quickmap(xlim = c(-125,-60), ylim = c(18, 53)) +
  GMM_theme %+% theme(legend.position = "none")

# Europe
ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#f0f0f0", color = "white") +
  geom_point(data = worldmap, aes(x = Longitude, y = Latitude, color = istop_q_14_16, size = `2016`), alpha = 0.8) + 
  scale_size_continuous(range = c(1,20),labels = scales::comma, name = "Nominal GDP(PPP), 2016") +
  scale_color_manual(values = c("#f46d43", "#2c7bb6"), 
                     labels = c("All others","Best performers by region (top 20 percent)"), 
                     name = 'Performance on Economic Index')+
  geom_label_repel(data = filter(worldmap,istop_q_14_16 == 1 & region_adj == "Western Europe"), 
                   aes(x = Longitude, y = Latitude, label = metrofinalname), 
                   nudge_y = 0.5, color = "white", size = 10, fill = "#2c7bb6") +
  coord_quickmap(xlim = c(-10,25), ylim = c(40, 60)) +
  GMM_theme %+% theme(legend.position = "none")

ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Presentation/figures/Europe.png", width = 20, height = 12,bg = "transparent")   

# China
ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#f0f0f0", color = "white") +
  geom_point(data = worldmap, aes(x = Longitude, y = Latitude, color = istop_q_14_16, size = `2016`), alpha = 0.8) + 
  scale_size_continuous(range = c(1,20),labels = scales::comma, name = "Nominal GDP(PPP), 2016") +
  scale_color_manual(values = c("#f46d43", "#2c7bb6"), 
                     labels = c("All others","Best performers by region (top 20 percent)"), 
                     name = 'Performance on Economic Index')+
  geom_label_repel(data = filter(worldmap,istop_q_14_16 == 1 & region_adj == "China"), 
                   aes(x = Longitude, y = Latitude, label = metrofinalname), 
                   nudge_y = 0.5, color = "white", size = 10, fill = "#2c7bb6") +
  coord_quickmap(xlim = c(100,145), ylim = c(20, 45)) +
  GMM_theme %+% theme(legend.position = "none")

ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Presentation/figures/China.png", width = 20, height = 12,bg = "transparent")   



