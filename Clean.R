library('dplyr')
library('tidyr')
library('reshape2')
library('data.table')

# Reshape data, create new variables
reshape <- function(dataframe){
  names(dataframe) <- gsub(" ", ".", names(dataframe))
  var.list <- unique(dataframe$Variable.code)
  #  var.list <- append(var.list, "EPratio")
  #  var.list <- append(var.list, "GDPUSC_PK")
  var.list <- var.list[2:length(var.list)]
  long <- gather(dataframe, Year, Value, '2000':'2017', factor_key = TRUE)
  wide <- spread(select(long, -Series.mnemonic, -Type, -Variable.name, - Major, - Units, - Currency), 
                 Variable.code, Value)
  # For Chinese cities, use sum of private emp and tot emp as EMPTOT
  wide$EMPTOT <- ifelse(is.na(wide$EMPTOT), wide$EMPEPRIV + wide$EMPETOT, wide$EMPTOT)
  wide$EPratio <- wide$EMPTOT/wide$POPTOTT
  wide$GDPUSC_PK <- wide$GDPUSC/wide$POPTOTT
  WIDE <- dcast(setDT(wide), Location.code + IsMetro + Metro + Country ~ Year, 
                value.var = var.list)
  output <- list(WIDE,var.list)
  return(output)
}

# Reshape data, create new variables
reshape_MENA <- function(dataframe){
  names(dataframe) <- gsub(" ", ".", names(dataframe))
  var.list <- unique(dataframe$Variable.code)
  var.list <- append(var.list, "GDPUSC_PK")
  var.list <- var.list[2:length(var.list)]
  long <- gather(dataframe, Year, Value, '2000':'2017', factor_key = TRUE)
  wide <- spread(select(long, -Series.mnemonic, -Type, -Variable.name, - Major, - Units, - Currency), 
                 Variable.code, Value)
  wide$GDPUSC_PK <- wide$GDPUSC/wide$POPTOTT
  WIDE <- dcast(setDT(wide), Location.code + IsMetro + Metro + Country ~ Year, 
                value.var = var.list)
  output <- list(wide, WIDE, var.list)
  return(output)
}