# Author: Sifan Liu
# Date: Thu Apr 19 09:12:51 2018
# --------------
pkgs <- c('tidyverse', 'ggmap', "ggrepel")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 

load("master.Rdata")
source("load.R")

# Get World Map -----------------------------------------------------------


map.world <- map_data("world")

worldmap$istop_q_14_16 <- as.factor(ifelse(worldmap$q_regionrankglobalmetro2014_2016 == 1, 1, 0))
worldmap <- worldmap %>% left_join(master %>% filter(IsMetro == 1 & Type == "NGDP") %>% select(`Location code`, `2016`), 
                      by = c("locationcode" = "Location code"))


# Plot the map ------------------------------------------------------------

ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#f0f0f0", color = "white") +
  geom_point(data = worldmap, aes(x = Longitude, y = Latitude, color = istop_q_14_16, size = `2016`), alpha = 0.8) + 
  scale_size_continuous(labels = scales::comma, name = "Nominal GDP(PPP), 2016") +
  scale_color_manual(values = c("#f46d43", "#1a9850"), labels = c("All others","Top quintile"), name = "Performance Status") +
  geom_text(data = worldmap, aes(x = Longitude, y = Latitude, label = metrofinalname),
            check_overlap = TRUE, nudge_y = 1.5, color = "#636363", size = 2) +
  # geom_text(data = filter(worldmap,istop_q_14_16 == 1), aes(x = Longitude, y = Latitude, label = finalname), 
  #           nudge_y = 1.5, color = "#636363", size = 2) +
  coord_map("mercator",xlim = c(-180,180), ylim = c(-60, 65)) +
  
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1,0),
        legend.position = c(0.2,0.3))

ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/plots/worldmap_random.png", width = 20, height = 12)   
#ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/plots/worldmap_q1.png", width = 20, height = 12)   


#ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/plots/worldmap.pdf", width = 20, height = 12)  

# MENA pockets of growth --------------------------------------------------

ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#f0f0f0", color = "white") +
  geom_point(data = MENAmap, aes(x = Longitude, y = Latitude, color = as.factor(pockets_growth2014_2016), size = gdpppp_2016), alpha = 0.8) + 
  scale_size_continuous(labels = scales::comma, name = "Nominal GDP(PPP), 2016") +
  scale_color_manual(values = c("#b2182b", "#2166ac"), labels = c("All others","Pockets of growth"), name = "Growth Status") +
  geom_text_repel(data = MENAmap, aes(x = Longitude, y = Latitude, label = metro), 
            nudge_y = 0.5, color = "#636363", size = 2) +
  coord_quickmap(xlim = c(-12,62), ylim = c(10, 40)) +
  
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/plots/MENA.png", width = 10, height = 6)  


# China -------------------------------------------------------------------

ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#f0f0f0", color = "white") +
  geom_point(data = worldmap, aes(x = Longitude, y = Latitude, color = istop_q_14_16, size = `2016`), alpha = 0.8) + 
  scale_size_continuous(labels = scales::comma, name = "Nominal GDP(PPP), 2016") +
  scale_color_manual(values = c("#f46d43", "#1a9850"), labels = c("All others","Top quintile"), name = element_blank()) +
  geom_text(data = filter(worldmap,istop_q_14_16 == 1), aes(x = Longitude, y = Latitude, label = metrofinalname), 
            nudge_y = 0.5, color = "#636363", size = 2) +
  coord_map("mercator",xlim = c(100,145), ylim = c(20, 45)) +
  
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/plots/China.png", width = 10, height = 6)  


# US ----------------------------------------------------------------------
ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#f0f0f0", color = "white") +
  geom_point(data = worldmap, aes(x = Longitude, y = Latitude, color = istop_q_14_16, size = `2016`), alpha = 0.8) + 
  scale_size_continuous(labels = scales::comma, name = "Nominal GDP(PPP), 2016") +
  scale_color_manual(values = c("#f46d43", "#1a9850"), labels = c("All others","Top quintile"), name = element_blank()) +
  geom_text(data = filter(worldmap,istop_q_14_16 == 1), aes(x = Longitude, y = Latitude, label = metrofinalname), 
            nudge_y = 0.5, color = "#636363", size = 3) +
  coord_quickmap(xlim = c(-125,-60), ylim = c(18, 52)) +
  
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/plots/NorthAmerica.png", width = 8, height = 6)   


# Europe ----------------------------------------------------------------------
ggplot() +
  geom_polygon(data = map.world, aes(x = long, y = lat, group = group), fill = "#f0f0f0", color = "white") +
  geom_point(data = worldmap, aes(x = Longitude, y = Latitude, color = istop_q_14_16, size = `2016`), alpha = 0.8) + 
  scale_size_continuous(labels = scales::comma, name = "Nominal GDP(PPP), 2016") +
  scale_color_manual(values = c("#f46d43", "#1a9850"), labels = c("All others","Top quintile"), name = element_blank()) +
  geom_text(data = filter(worldmap,istop_q_14_16 == 1), aes(x = Longitude, y = Latitude, label = metrofinalname), 
            nudge_y = 0.5, color = "#636363", size = 3) +
  coord_quickmap(xlim = c(-10,25), ylim = c(40, 60)) +
  
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

ggsave("V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/plots/Europe.png", width = 10, height = 6)   

