
source("Func.R")

# MENA dataset ------------------------------------------------------------

test <- reshape_MENA(MENA)
MENA_long <- test[[1]]
MENA_wide <- test[[2]]
var.list <- test[[3]]

write.csv(MENA_long, "../data for analysis/MENA_long.csv")

var.list <- c("GDPUSC", "GDPPPP", "EMPTOT", "POPTOTT", "GDPUSC_PK")

temp <- MENA_wide

for (var in var.list){
  tryCatch({
    temp <- YOY(var, temp)
    temp <- CAGR(temp, var, 2000, 2016)
    temp <- CAGR(temp, var, 2009, 2016)
    temp <- CAGR(temp, var, 2014, 2016)
  },
  error = function(e) e
  )
}
write.csv(temp, "../data for analysis/MENA_wide.csv")


# World master spreadsheet -------------------------------------------------------------------

Master_wide <- reshape(master)
temp <- Master_wide[[1]]
var.list <- Master_wide[[2]]

temp <- select(temp, -contains("EMPEPRIV"), - contains("EMPETOT"))

for (var in var.list){
  tryCatch({
    temp <- YOY(var, temp)
    temp <- CAGR(temp, var, 2000, 2016)
    temp <- CAGR(temp, var, 2009, 2016)
    temp <- CAGR(temp, var, 2014, 2016)
    },
  error = function(e) e
  )
}

summary(temp)

write.csv(temp, "World_wide.csv")


# Within region comparison =====================================================

world_group <- read.csv("source/GMM17_world_wide_groups.csv")
region_compare <- group_summary(world_group, "country", "ismetro", "region", "incomegroup")
region_compare$gdpusc_pk_2014 <- region_compare$gdpusc_2014/region_compare$poptott_2014
region_compare$gdpusc_pk_2016 <- region_compare$gdpusc_2016/region_compare$poptott_2016

for (var in c("emptot","gdpusc","gdpusc_pk", "poptott")){
  region_compare <- CAGR(region_compare, var , 2014, 2016)
}

region_compare_wide <- dcast(setDT(region_compare), country  ~ ismetro, 
                            value.var = c("CAGR_emptot_2014_2016","CAGR_gdpusc_pk_2014_2016","count"))

region_compare$gdpusc_2014_2016 <- region_compare$gdpusc_2016 - region_compare$gdpusc_2014
region_compare$emptot_2014_2016 <- region_compare$emptot_2016 - region_compare$emptot_2014

country_equality <- dcast(setDT(region_compare), country + region + incomegroup ~ ismetro, 
                          value.var = c("gdpusc_2014", "gdpusc_2016", "emptot_2014","emptot_2016",
                                        "gdpusc_2014_2016","emptot_2014_2016","poptott_2016"))

region_equality <- city_region(country_equality, "region")
income_equality <- city_region(country_equality, "incomegroup")

# create other regional cuts
# China, US, Europe, MENA

adj_region_equality <- country_equality %>%
  mutate(region_adj = ifelse(country == "China"|country == "United States", as.character(country),""))%>%
  mutate(region_adj = ifelse(grepl("Europe", region),"Europe",region_adj ))

adj_region_equality <- city_region(adj_region_equality, "region_adj")

inequality <- bind_rows(region_equality, income_equality, adj_region_equality)

write.csv(inequality, "result/inequality.csv")

region_compare_wide <- left_join(region_compare_wide, country_equality, by = "country")
region_compare_wide <- select(region_compare_wide,-count_0)

write.csv(region_compare_wide, "result/region_compare_wide.csv")
write.csv(region_compare, "result/region_compare_long.csv")


# inequality


# group summary===========================================================
world_cities <- filter(world_group, ismetro == 1)

# Data #######################################################

allcities_summary <- group_summary(world_cities, "region" ,"incomegroup")
allregion_summary <- group_summary(world_cities, "region")
allincome_summary <- group_summary(world_cities, "incomegroup")
all_summary <- group_summary(world_cities)

regional_summary <- bind_rows(allcities_summary, allregion_summary, allincome_summary, all_summary)
regional_summary$gdpusc_pk_2014 <- regional_summary$gdpusc_2014/regional_summary$poptott_2014
regional_summary$gdpusc_pk_2016 <- regional_summary$gdpusc_2016/regional_summary$poptott_2016

# CAGR ###################################################
temp <- regional_summary
for (var in c("emptot","gdpusc","gdpusc_pk", "poptott")){
  temp <- CAGR(temp, var , 2014, 2016)
}


# merge to world #################################################

regional_summary <- temp
names(regional_summary) <- tolower(names(regional_summary))

world <- world_group %>% 
  filter(country == "World") %>% 
  mutate(region = "World", count = 1) %>%
  select(names(regional_summary))

summary <- bind_rows(regional_summary, world)
write.csv(summary, 'result/summary.csv')




# Sandbox -----------------------------------------------------------------


# Asian Megacities --------------------------------------------------------------

library('dplyr')

EMGAsian_cities <- world_group %>% 
  filter(region == "Emerging Asia-Pacific") %>% 
  filter(ismetro == 1) %>%
  mutate (megacity = ifelse(poptott_2017 > 10000, 1, 0))

EMGAsian_countries <- world_group %>% 
  filter(region == "Emerging Asia-Pacific") %>% 
  filter(ismetro == 0)

EMGAsian <- bind_rows(EMGAsian_cities, EMGAsian_countries)

EMGAsian_summary <- EMGAsian %>%
  group_by(megacity, country) %>%
  summarise_if(is.numeric, sum) %>%
  select( megacity, country, contains("2000" ), contains( "2007"  ), contains("2014"), contains("2017"), 
          -contains("cagr"), -contains('yoy'), -contains('pk'), -contains('erp')) %>%
  mutate(gdpusc_short = gdpusc_2017 - gdpusc_2014,
         gdpusc_med = gdpusc_2017 - gdpusc_2007,
         gdpusc_long = gdpusc_2017 - gdpusc_2000)

# Mid-sized?

library('ggplot2')

world_cities <- filter(world_group, ismetro == 1)

ggplot(data = world_cities, mapping = aes(x = poptott_2016, color = incomegroup)) +
  geom_freqpoly()

ggplot(data = world_cities, mapping = aes(x = region, y = poptott_2016)) + 
  geom_boxplot()

ggplot(data = world_cities, mapping = aes(x = region, y = gdpusc_2016)) + 
  geom_boxplot()


# GDP growth share =============================================

#2000 - 2016
share <- temp %>%
  filter(POPTOTT_2016 > POPTOTT_2000) %>%
  filter(GDPUSC_PK_2000 < GDPUSC_PK_2016) %>%
  filter(GDPUSC_2016 > GDPUSC_2000)%>%
  mutate(Share_PK_2000_2016 = ((GDPUSC_2016 / GDPUSC_2000)-(GDPUSC_PK_2016 / GDPUSC_PK_2000)))%>%
  select(Location.code, Share_PK_2000_2016)

world_analysis <- left_join(temp, share, by = "Location.code")

#2009 - 2016
share <- temp %>%
  filter(POPTOTT_2016 > POPTOTT_2009) %>%
  filter(GDPUSC_PK_2009 < GDPUSC_PK_2016) %>%
  filter(GDPUSC_2016 > GDPUSC_2009)%>%
  mutate(Share_PK_2009_2016 = ((GDPUSC_2016 / GDPUSC_2009)-(GDPUSC_PK_2016 / GDPUSC_PK_2009)))%>%
  select(Location.code, Share_PK_2009_2016)

world_analysis <- left_join(world_analysis, share, by = "Location.code")




