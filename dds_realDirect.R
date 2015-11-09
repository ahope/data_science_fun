
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
  
  print(qplot(df$NEIGHBORHOOD))
  print(qplot(df$BUILDING.CLASS.CATEGORY) )
  print( ggplot(data = df, aes(x = BUILDING.CLASS.CATEGORY)) + 
    geom_histogram() +
    theme(axis.text.x = element_text(angle = 90) ))
  
  print(qplot(df$TAX.CLASS.AT.PRESENT))
  ## Not sure what these are, so ignoring them for now (many, many levels)
#   qplot(df$BLOCK)
#   qplot(df$LOT)
  
  print(qplot(df$BUILDING.CLASS.AT.PRESENT))
  print(qplot(df$ZIP.CODE))
  print(qplot(df$RESIDENTIAL.UNITS, binwidth = 1))
  print(qplot(df$COMMERCIAL.UNITS, binwidth = 1))
  print(qplot(df$TOTAL.UNITS, binwidth = 1))
  print(qplot(df$LAND.SQUARE.FEET))
  print(qplot(df$YEAR.BUILT, binwidth = 10))
  
  print(qplot(df$TAX.CLASS.AT.TIME.OF.SALE))
  print(qplot(df$BUILDING.CLASS.AT.TIME.OF.SALE))
  print(qplot(df$SALE.DATE))
  print(qplot(df[df$SALE.PRICE.N < 1000000, 'SALE.PRICE.N'], binwidth = 10000)  )
  print(qplot(df[df$SALE.PRICE.N >= 1000000, 'SALE.PRICE.N'], binwidth = 1000000)  )
}

DoNeighborhoodComparisons <- function(df){
  
  
  nbhd_df <- ddply(df, .(NEIGHBORHOOD), summarize, 
                   mean = mean(SALE.PRICE.N),
                   median = median(SALE.PRICE.N), 
                   max = max(SALE.PRICE.N), 
                   min = min(SALE.PRICE.N))
  
  ggplot(data = df[df$SALE.PRICE.N < 500000000,], 
         aes(x = NEIGHBORHOOD, y = SALE.PRICE.N)) +
    geom_point(alpha = 0.7, position = 'jitter') 
  
  ggplot(data = df[df$SALE.PRICE.N < 500000000,], 
         aes(x = NEIGHBORHOOD, y = YEAR.BUILT)) +
    geom_point(alpha = 0.7, position = 'jitter') 
  
  ggplot(data = df[df$SALE.PRICE.N < 500000000,], 
         aes(x = NEIGHBORHOOD, y = TAX.CLASS.AT.TIME.OF.SALE)) +
    geom_point(alpha = 0.7, position = 'jitter') 
  
  ggplot(data = df[df$SALE.PRICE.N < 500000000,], 
         aes(x = NEIGHBORHOOD, y = RESIDENTIAL.UNITS)) +
    geom_point(alpha = 0.7, position = 'jitter') 
  
  ggplot(data = df[df$COMMERCIAL.UNITS < 600,], 
         aes(x = NEIGHBORHOOD, y = COMMERCIAL.UNITS)) +
    geom_point(alpha = 0.7, position = 'jitter') 
  
  ggplot(data = df, 
         aes(x = NEIGHBORHOOD, y = LAND.SQUARE.FEET)) +
    geom_point(alpha = 0.7, position = 'jitter') 
  
  ggplot(data = df, 
         aes(x = NEIGHBORHOOD, y = GROSS.SQUARE.FEET)) +
    geom_point(alpha = 0.7, position = 'jitter') 
  
  ggplot(data = df, 
         aes(x = NEIGHBORHOOD, y = BUILDING.CLASS.CATEGORY)) +
    geom_point(alpha = 0.5, position = 'jitter')  +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5) )
  
  ggplot(data = df, 
         aes(x = SALE.DATE, y = SALE.PRICE.N, colour = NEIGHBORHOOD)) +
    geom_point(alpha = 0.7, position = 'jitter') + 
    facet_grid(. ~ BUILDING.CLASS.CATEGORY)
  
  nbhd_df <- ddply(df, .(SALE.DATE, NEIGHBORHOOD), summarize, 
                   mean = mean(SALE.PRICE.N))
  
  ggplot(data = nbhd_df, 
         aes(x = SALE.DATE, y = mean)) + 
    geom_line() +
    facet_grid(. ~ NEIGHBORHOOD)
  
  
  
  
  ggplot(data = df[df$SALE.PRICE.N > 0, ], 
         aes(x = GROSS.SQUARE.FEET, y = SALE.PRICE.N)) +
    geom_point(alpha = 0.7)
  
  bldgClassToKeep <- rownames(as.data.frame(sort(table(df$BUILDING.CLASS.CATEGORY),
                                                 decreasing = TRUE))[1:7,])
  bldgClassToKeep <- c("13  CONDOS - ELEVATOR APARTMENTS", 
                       "10  COOPS - ELEVATOR APARTMENTS", 
                       "25  LUXURY HOTELS", "17  CONDOPS",
                       "07  RENTALS - WALKUP APARTMENTS", 
                       "28  COMMERCIAL CONDOS")
  
  df2 <- df[df$BUILDING.CLASS.CATEGORY %in% bldgClassToKeep, ]
  df2 <- df2[df2$SALE.PRICE.N > 5, ]
  
  
  ggplot(data = df2[df2$SALE.PRICE.N > 0, ], 
         aes(x = GROSS.SQUARE.FEET, y = SALE.PRICE.N)) +
    geom_point(alpha = 0.7)
  
  
  
  ## Mean sale price each week over each building class 
  
  df2$SALE.DATE.WK <- as.numeric(df2$SALE.DATE - min(df2$SALE.DATE)) %/% 7  #dweek <- as.numeric(dvec-dvec[1]) %/% 7

  df2.grouped <- ddply(df2, .(SALE.DATE.WK, BUILDING.CLASS.CATEGORY), summarize, 
                   meanSalePrice = mean(SALE.PRICE.N))
  
  
  ggplot(data = df2.grouped[df2.grouped$meanSalePrice > 50,], 
         aes(x = SALE.DATE.WK, y = meanSalePrice)) + 
    geom_line(aes(group = BUILDING.CLASS.CATEGORY, 
                  colour = BUILDING.CLASS.CATEGORY)) + 
     # ylim(0, 10000000) + 
    theme(legend.position = 'bottom') + guides(col = guide_legend(ncol = 3))
  
  ## ###################
  ## Mean sale price versus sq ft for each neighborhood
  
  df2.grouped <- ddply(df2, .(GROSS.SQUARE.FEET, NEIGHBORHOOD), summarize, 
                       meanSalePrice = mean(SALE.PRICE.N))
  
  
  ggplot(data = df2.grouped[df2.grouped$GROSS.SQUARE.FEET < 10000,],  
         aes(x = GROSS.SQUARE.FEET, y = meanSalePrice)) + 
    geom_line(aes(group = NEIGHBORHOOD, 
                  colour = NEIGHBORHOOD)) + 
    # ylim(0, 10000000) + 
    theme(legend.position = 'bottom') + guides(col = guide_legend(ncol = 3))
  
  
  
}

DoReport <- function(df){
  bldgClassToKeep  = c("01  ONE FAMILY HOMES", 
                       "02  TWO FAMILY HOMES", 
                       "03  THREE FAMILY HOMES", 
                       "04  TAX CLASS 1 CONDOS", 
                       "07  RENTALS - WALKUP APARTMENTS", 
                       "08  RENTALS - ELEVATOR APARTMENTS", 
                       "09  COOPS - WALKUP APARTMENTS", 
                       "10  COOPS - ELEVATOR APARTMENTS", 
                       "11A CONDO-RENTALS", 
                       "12  CONDOS - WALKUP APARTMENTS", 
                       "13  CONDOS - ELEVATOR APARTMENTS", 
                       "14  RENTALS - 4-10 UNIT", 
                       "15  CONDOS - 2-10 UNIT RESIDENTIAL", 
                       "16  CONDOS - 2-10 UNIT WITH COMMERCIAL UNIT", 
                       "17  CONDOPS"
  )
  
  df2 <- df[df$BUILDING.CLASS.CATEGORY %in% bldgClassToKeep, ]
  
  
  
}


