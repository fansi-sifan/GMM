# Author: Sifan Liu
# Date: Fri Apr 13 16:41:41 2018
# --------------
source("Load.R")
source("Func.R")

# match to groups ---------------------------------------------------------
master_long <- left_join(master, group, by = c("Country"= "country"))

# check for NAs
# summary(master[c("region", "incomegroup")])
# filter(master, is.na(master$region))

# manually code Macau to high income, Advanced Asia Pacific
summary(master_long[c("region", "incomegroup")])

master_long <- select(master_long, region, incomegroup, everything())

write.csv(master_long, "V:/MetroMonitor/Global Monitor/Global Monitor V/Data/04132018/data for analysis/GMM17_World_long_groups.csv")




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



