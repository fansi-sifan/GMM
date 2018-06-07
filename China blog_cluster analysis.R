# Author: Sifan Liu
# Date: Wed May 30 08:55:03 2018
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


China_wide <- wide %>% 
  filter(country == "China") %>%
  filter(ismetro == 1) 

summary(China_wide)

features <- China_wide %>% 
  select(metro,contains("2016")) %>%
  left_join(China_ind_share %>% filter(isurban == "total"), by = c("metro" = "name_EN")) %>%
  select(-gdpusc_2016, -ismetro, -isurban)

# scale
# log_features = cbind(log(features[,2:5]),log(features[,6:13]+1))
# log_features = cbind(log_features, log(features[,14:32]*100+0.0001))
# 
# # scatterplotMatrix(log_features)
# str(log_features)
# PCA ---------------------------------------------------------------------

data_res = features[c(-1)]

pca.vis <- prcomp(na.omit(data_res),center = TRUE, scale. = TRUE)
plot(pca.vis, type = "lines")
summary(pca.vis)


features.pca <- preProcess(data_res, method = c("pca"))
PC <- predict(features.pca, data_res)



# compare methods -----------------------------------------------------
set.seed(2)
dist.PC <- dist(PC, method = "euclidean")
index_method <- "silhouette"

# kmeans
fviz_nbclust(PC, kmeans, method = index_method)
# Hierarchical clustering
h_method <- "ward.D2"
fviz_nbclust(PC, hcut, method = index_method, hc_method = h_method)
# pam
fviz_nbclust(PC, pam, method = index_method)


# Kmeans ------------------------------------------------------------------

n_cluster_k <- 5

clusters <- kmeans(PC,n_cluster_k)
features$cat <- as.factor(clusters$cluster)



# hierachical clustering --------------------------------------------------

n_cluster_h <- 5

h_clusters <- hclust(dist.PC,method = h_method)
plot(h_clusters)
rect.hclust(h_clusters, n_cluster_h)

features$cat <- as.factor(cutree(h_clusters,n_cluster_h))

write.csv(features,"results/China_cat.csv")

ss <- silhouette(as.numeric(GCItype$modified_cat), dist.PC)
s <- silhouette(as.numeric(features$cat), dist.PC)

plot(ss)