
# Reshape data, create new variables
reshape <- function(dataframe){
  names(dataframe) <- gsub(" ", ".", names(dataframe))
  var.list <- unique(dataframe$Variable.code)
  var.list <- append(var.list, "EPratio")
  var.list <- append(var.list, "GDPUSC_PK")
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



# generate YOY growth rate
YOY <- function(variable, dataframe){
  i <- grep(variable, names(dataframe))[[1]]
  temp <- dataframe[,(i+1):(i+17)]/dataframe[,i:(i+16)]-1
  names(temp) <- paste0("YOY_",names(temp)) 
  dataframe <- cbind(dataframe, temp)
}


# generate CAGR for 3 time periods

CAGR <- function(dataframe, variable, start, end){
  start_var <- paste0(variable, "_",start)
  end_var <- paste0(variable, "_",end)
  dataframe$CAGR <- ((dataframe[[end_var]]/ dataframe[[start_var]]))^(1/(end-start))-1
  names(dataframe)[length(dataframe)] <- paste("CAGR",variable, start, end, sep = "_")
  return(dataframe)
}


# generate regional summary
group_summary <- function(dataframe, ...) dataframe %>% 
  group_by_(...) %>% mutate(count = 1)%>%
  summarise_at(c("emptot_2014","gdpppp_2014","gdpusc_2014","poptott_2014",
                 "emptot_2016","gdpppp_2016","gdpusc_2016","poptott_2016","count"), sum,na.rm = TRUE)


# generate city-region comparison -----------------------------------------

city_region <- function(dt, ...) dt%>%
  group_by_(...) %>%
  summarise_if(is.numeric,sum) %>%
  mutate(gdpusc_pk_16_0 = gdpusc_2016_0/poptott_2016_0,
         gdpusc_pk_16_1 = gdpusc_2016_1/poptott_2016_1,
         share_gdpusc_14_16 = gdpusc_2014_2016_1/gdpusc_2014_2016_0,
         share_emptot_14_16 = emptot_2014_2016_1/emptot_2014_2016_0,
         share_poptott_16 = poptott_2016_1/poptott_2016_0)
