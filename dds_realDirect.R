
#'
#' This is the RealDirect Data Strategy problem from 
#' Doing Data Science, page 49. 
#' 

data_folder <- '~/Documents/data/doing_data_science-master 2/dds_datasets/dds_ch2_rollingsales/'
data_file <- 'rollingsales_manhattan.xls'

LoadLibraries <- function(){
  library(gdata)
  library(zipcode)
  library(ggplot2)
  library(plyr)
  library(dplyr)
}

LoadData <- function(folder = data_folder, file = data_file){
  df <- read.xls(paste(folder, file, sep = '/'), 
                 pattern = 'BOROUGH')
  df$BOROUGH <- as.factor(df$BOROUGH)
  df$ZIP.CODE <- as.factor(clean.zipcodes(df$ZIP.CODE))
  df$ADDRESS <- as.character(df$ADDRESS)
  df$APART.MENT.NUMBER <- as.character(df$APART.MENT.NUMBER)
  df$RESIDENTIAL.UNITS <- as.integer(df$RESIDENTIAL.UNITS)
  df$TOTAL.UNITS <- as.integer(df$TOTAL.UNITS)
  df$LAND.SQUARE.FEET <- as.integer(gsub('[^[:digit:]]', '', df$LAND.SQUARE.FEET))
  df$GROSS.SQUARE.FEET <- as.integer(gsub('[^[:digit:]]', '', df$GROSS.SQUARE.FEET))
  
  df$YEAR.BUILT <- as.numeric(as.character(df$YEAR.BUILT))
  df[df$YEAR.BUILT < 1800, 'YEAR.BUILT'] <- NA
  
  df$TAX.CLASS.AT.TIME.OF.SALE <- as.factor(df$TAX.CLASS.AT.TIME.OF.SALE)
  df$SALE.PRICE.N <- as.numeric(gsub('[^[:digit:]]', '', df$SALE.PRICE))
  df$SALE.DATE <- as.Date(df$SALE.DATE)
  df$BUILDING.CLASS.CATEGORY <- as.factor(trim(df$BUILDING.CLASS.CATEGORY))
  ## Should BLOCK and LOT be factors? Not sure what they are... 
  df$BLOCK <- as.factor(df$BLOCK)
  df$LOT <- as.factor(df$LOT)
  
  return(df)
}

DoExploration <- function(df){
  
  qplot(df$NEIGHBORHOOD)
  qplot(df$BUILDING.CLASS.CATEGORY) 
  ggplot(data = df, aes(x = BUILDING.CLASS.CATEGORY)) + 
    geom_histogram() +
    theme(axis.text.x = element_text(angle = 90) )
  
  qplot(df$TAX.CLASS.AT.PRESENT)
  ## Not sure what these are, so ignoring them for now (many, many levels)
#   qplot(df$BLOCK)
#   qplot(df$LOT)
  
  qplot(df$BUILDING.CLASS.AT.PRESENT)
  qplot(df$ZIP.CODE)
  qplot(df$RESIDENTIAL.UNITS, binwidth = 1)
  qplot(df$COMMERCIAL.UNITS, binwidth = 1)
  qplot(df$TOTAL.UNITS, binwidth = 1)
  qplot(df$LAND.SQUARE.FEET)
  qplot(df$YEAR.BUILT, binwidth = 10)

  qplot(df$TAX.CLASS.AT.TIME.OF.SALE)
  qplot(df$BUILDING.CLASS.AT.TIME.OF.SALE)
  qplot(df$SALE.DATE)
  qplot(df[df$SALE.PRICE.N < 1000000, 'SALE.PRICE.N'], binwidth = 10000)  
  qplot(df[df$SALE.PRICE.N >= 1000000, 'SALE.PRICE.N'], binwidth = 1000000)  
}

DoNeighborhoodComparisons <- function(df){
  
}
