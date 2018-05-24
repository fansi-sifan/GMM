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



# Prepare data ------------------------------------------------------------

China_wide <- wide %>% 
  filter(country == "China") %>%
  filter(ismetro ==1)
summary(China_wide)

features <- China_wide %>% 
  select(metro,contains("2016")) 


# scale
log_features = cbind(log(features[,2:6]),log(features[7:14]+1))
log_features2 = log(features[7:14]+1)

scatterplotMatrix(log_features2)
str(log_features)
# PCA ---------------------------------------------------------------------

data_res = log_features

pca.vis <- prcomp(data_res,center = TRUE, scale. = TRUE)
plot(pca.vis, type = "lines")
summary(pca.vis)
pca.vis

log_features.pca <- preProcess(data_res, method = c("pca"))
PC <- predict(log_features.pca, data_res)



# compare methods -----------------------------------------------------
set.seed(2)
dist.PC <- dist(PC, method = "euclidean")
index_method <- "silhouette"

# kmeans
fviz_nbclust(PC, kmeans, method = index_method)
# Hierarchical clustering
h_method <- "ward.D"
fviz_nbclust(PC, hcut, method = index_method, hc_method = h_method)
# pam
fviz_nbclust(PC, pam, method = index_method)


# Kmeans ------------------------------------------------------------------

n_cluster_k <- 2

clusters <- kmeans(PC,n_cluster_k)
features$cat <- as.factor(clusters$cluster)

# manual cluster ----------------------------------------------------------



# hierachical clustering --------------------------------------------------

n_cluster_h <- 3

h_clusters <- hclust(dist.PC,method = h_method)
plot(h_clusters)
rect.hclust(h_clusters, n_cluster_h)

features$cat <- as.factor(cutree(h_clusters,n_cluster_h))

write.csv(features,"results/China_cat.csv")

# merge Factory China and others


load("master.Rdata")
China <- master %>% 
  filter(Country == "China") 


China_type <- China %>% 
  left_join(features[c("metro","cat")],by = c("Metro" = "metro")) %>%
  left_join(GCItype,by = c("Metro" = "metro")) %>%
  group_by(modified_cat,Type) %>%
  summarise_if(is.numeric, sum)

China_gdppk <- bind_cols((China_type %>% filter(Type =="RGDP"))[,1:4],
                         ((China_type %>% filter(Type =="RGDP"))[,5:22]/(China_type%>%filter(Type=="POP"))[,5:22])) %>%
  mutate(Type = "GDPPK")

China_type <- bind_rows(China_type, China_gdppk) %>% arrange(modified_cat)

# calculate matrix
n_cluster <- n_cluster_h
n_features <- length(unique(China_type$Type))

China_total <- matrix(rep(t(China_type[(n_features*n_cluster+1):(n_features*n_cluster+n_features),5:22]),n_cluster),
                      nrow = 5*n_cluster,ncol = 18,byrow = TRUE)

China_share <- bind_cols(China_type[1:(n_features*n_cluster),1:4],
                         China_type[1:(n_features*n_cluster),5:22]/China_total) %>% mutate(var = "share")

China_indexed <- indexed_matrix(China_type[1:(n_features*n_cluster),]) %>% mutate(var = "index")

China_blogdata <- bind_rows(China_share, China_indexed)
# write.csv(China_blogdata, "results/China_blogdata.csv")

China_long <- China_blogdata %>% 
  select(-IsMetro, - Units) %>%
  filter(Type != "NGDP") %>%
  tidyr::gather(year, value, `2000`:`2017`, factor_key = TRUE) %>%
  mutate(year = as.integer(as.character(year))) 

# Plots -------------------------------------------------------------------

# Indexed =================================================

summary(GCItype$modified_cat)

ggplot(data = China_long %>% filter(var == "index") , aes(x = year, y = value, color = modified_cat)) +
  geom_point()+
  geom_line()+
  scale_color_discrete(label = c("Large anchors (n = 17)",
                                 "Rising stars (n = 80)",
                                 "Rust belt (n = 6)"))+
  facet_wrap(~ Type, scales = 'free')+
  theme_classic()

# Share ===================================================

ggplot(data = China_long %>% filter(var == "share") %>% filter(Type != "GDPPK"), 
       aes(x = year, y = value, fill = modified_cat, position = "dodge")) +
  geom_area()+
  scale_fill_discrete(label = c("Large anchors (n = 17)",
                                 "Rising stars (n = 80)",
                                 "Rust belt (n = 6)"))+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~ Type)+
  theme_classic()


# map ---------------------------------------------------------------------
China_coord <- worldmap %>%
  filter(country =="China") %>%
  select(metro, gdpppp_2016, Latitude, Longitude, metrofinalname) %>%
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
  geom_text(data = filter(China_coord, (modified_cat == 1) & (Capital != 1)), 
                  aes(x = Longitude, y = Latitude, label = metrofinalname), nudge_x = 2, color = "#636363", size = 3) +
  geom_text(data = filter(China_coord, (modified_cat == 1) & (Capital == 1)), 
                  aes(x = Longitude, y = Latitude, label = metrofinalname), nudge_y = 0.5, color = "#636363", size = 4, fontface = "bold") +
  scale_size_continuous(labels = scales::comma, name = "Nominal GDP(Blns $, PPP rates), 2016") +
  scale_color_discrete(label = c("Large anchors (n = 17)",
                                 "Rising stars (n = 80)",
                                 "Rust belt (n = 6)"),
                       name = "Type of Chinese metros") +
  # coord_quickmap(xlim = c(90,145), ylim = c(20, 51)) +
  GMM_theme 

