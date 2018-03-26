library('dplyr')
library('reshape2')
library('data.table')


comprss <- function(tx){ 
  div <- findInterval(as.numeric(gsub("\\,", "", abs(tx))),
                      c(1, 1e3, 1e6, 1e9, 1e12) )
  paste(sign(tx)*round(as.numeric(gsub("\\,","",abs(tx)))/10^(3*(div-1)), 1), 
        c("","K","M","B","T")[div])
}

# Indicators ----------------------------------------------------------------

# generate YOY growth rate
YOY <- function(variable, dataframe){
  i <- grep(variable, names(dataframe))[[1]]
  temp <- dataframe[,(i+1):(i+17)]/dataframe[,i:(i+16)]-1
  names(temp) <- paste0("YOY_",names(temp)) 
  dataframe <- cbind(dataframe, temp)
}

# generage GDP per capita

GDPPK <- function(dataframe){
  gdp <- grep('gdpusc', names(dataframe))[[1]]
  pop <- grep('poptott', names(dataframe))[[1]]
  len <- length(grep('gdpusc', names(dataframe)))
  temp <- dataframe[,gdp:(gdp + len-1)]/dataframe[,pop:(pop + len-1)]
  names(temp) <- gsub("gdpusc","gdppk",names(temp)) 
  dataframe <- cbind(dataframe, temp)
  return(dataframe)
}

# generate CAGR for different time periods

CAGR <- function(dataframe, variable, start, end){
  start_var <- paste0(variable, "_",start)
  end_var <- paste0(variable, "_",end)
  dataframe$CAGR <- ((dataframe[[end_var]]/ dataframe[[start_var]]))^(1/(end-start))-1
  names(dataframe)[length(dataframe)] <- paste("CAGR",variable, start, end, sep = "_")
  return(dataframe)
}

# generate absolute growth for different time periods
VG <- function(dataframe, variable, start, end){
  start_var <- paste0(variable, "_",start)
  end_var <- paste0(variable, "_",end)
  dataframe$VG <- dataframe[[end_var]]- dataframe[[start_var]]
  names(dataframe)[length(dataframe)] <- paste("VG",variable, start, end, sep = "_")
  return(dataframe)
}



# generate regional summary
group_summary <- function(dataframe, metro, ...) {
  metro <- enquo(metro)
  dataframe %>% 
    filter(ismetro == !!metro) %>%
    group_by_(...) %>% 
    mutate(count = 1) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
}


# generate city-region comparison -----------------------------------------

city_region <- function(dt, ...) dt%>%
  group_by_(...) %>%
  summarise_if(is.numeric,sum) %>%
  mutate(gdpusc_pk_16_0 = gdpusc_2016_0/poptott_2016_0,
         gdpusc_pk_16_1 = gdpusc_2016_1/poptott_2016_1,
         share_gdpusc_14_16 = gdpusc_2014_2016_1/gdpusc_2014_2016_0,
         share_emptot_14_16 = emptot_2014_2016_1/emptot_2014_2016_0,
         share_poptott_16 = poptott_2016_1/poptott_2016_0)
