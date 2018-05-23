# Author: Sifan Liu
# Date: Thu May 17 11:25:11 2018
# --------------
pkgs <- c('dplyr','ggplot2', "caret", "car")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 



# prepare data ------------------------------------------------------------

load("master_wide.Rdata")
source("Func.R")
GCItype <- read.csv("China GCI type.csv")



# Prepare data ------------------------------------------------------------


China_wide <- wide %>% 
  filter(country == "China") %>%
  filter(ismetro ==1)
summary(China_wide)

features <- China_wide %>% 
  select(metro,contains("2016")) 



log_features = cbind(log(features[,2:6]),log(features[7:14]+1))
scatterplotMatrix(log_features)
str(log_features)
# PCA ---------------------------------------------------------------------

log_features.pca <- preProcess(log_features, method = c("center","scale","pca"))
PC <- predict(log_features.pca, log_features)

# Kmeans ------------------------------------------------------------------

set.seed(2)

n_cluster = 4

clusters <- kmeans(PC,n_cluster)
features$cat <- as.factor(clusters$cluster)

str(clusters)




# manual cluster ----------------------------------------------------------


# merge Factory China and others
load("master.Rdata")
China <- master %>% 
  filter(Country == "China") 

China_type <- China %>% 
  left_join(features[,-(2:10)],by = c("Metro" = "metro")) %>%
  # filter(IsMetro == 1) %>%
  group_by(cat,Type) %>%
  summarise_if(is.numeric, sum)

China_total <- matrix(rep(t(China_type[(4*n_cluster+1):(4*n_cluster+4),5:22]),n_cluster),nrow = 4*n_cluster,ncol = 18,byrow = TRUE)

China_share <- bind_cols(China_type[1:(4*n_cluster),1:4],China_type[1:(4*n_cluster),5:22]/China_total) %>% mutate(var = "share")
China_indexed <- indexed_matrix(China_type[1:(4*n_cluster),]) %>% mutate(var = "index")

China_blogdata <- bind_rows(China_share, China_indexed)
# write.csv(China_blogdata, "China_blogdata.csv")

China_long <- China_blogdata %>% 
  select(-IsMetro, - Units) %>%
  # filter(Type == "RGDP"| Type == "EMPL") %>%
  tidyr::gather(year, value, `2000`:`2017`, factor_key = TRUE) %>%
  mutate(year = as.integer(as.character(year)))


# Plots -------------------------------------------------------------------

# Indexed
ggplot(data = China_long %>% filter(var == "index") , aes(x = year, y = value, color = reorder(cat, desc(cat)))) +
  geom_point()+
  geom_line()+
  # scale_color_discrete(label = c("Rising stars (n = 91)",
  #                                "Emerging Gateways (n = 10)",
  #                                "Asian Anchor (n = 2)"))+
  facet_wrap(~ Type, scales = 'free')+
  theme_classic()

str(clusters)

ggplot(data = China_long %>% filter(var == "share") , aes(x = year, y = value, fill = reorder(cat, desc(cat)), position = "dodge")) +
  geom_area()+
  # scale_fill_discrete(label = c("Rising stars (n = 91)",
  #                                "Emerging Gateways (n = 10)",
  #                                "Asian Anchor (n = 2)"))+
  facet_wrap(~ Type)+
  theme_classic()
