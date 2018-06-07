# Author: Sifan Liu
# Date: Tue Jun 05 11:56:17 2018
# --------------
pkgs <- c('tidyr',"stringr")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 


China_ind <- read.csv("source/China_ind.csv", encoding = "UTF-8")

China_ind_long <- China_ind %>% 
  gather(industry, employment, total_11:urban_third, factor_key = TRUE) %>%
  mutate(employment = as.numeric(employment)) %>% 
  separate(industry, into = c("isurban", "industry")) %>%
  mutate(ismetro = as.factor(ismetro),
         isurban = as.factor(isurban),
         industry = as.factor(industry)) %>%
  replace(is.na(.), 0)

head(China_ind_long)
summary(China_ind_long)

China_ind_wide <- China_ind_long %>%
  spread(industry, employment) %>%
  mutate(sum = rowSums(.[8:26]))

China_ind_share <- cbind(China_ind_wide[c("name_EN", "ismetro", "isurban")],(China_ind_wide[,8:26]/China_ind_wide$sum)) %>%
  filter(ismetro == 1 )

