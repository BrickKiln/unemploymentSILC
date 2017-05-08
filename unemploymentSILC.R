# LOAD STUFF
library(data.table) # for import function fread

setwd("/Users/SupremeLeader/Documents/Central European University/Applied Policy Project/Cross-sectional Data")
################################################

#########################
## PREDEFINE FUNCTIONS ##
#########################

#----------------------------------------------#
### IMPORT DATA
# Have not included conditions, works best when used with at least one variable
# from each of D, H, and P file.
# IDs of both households and persons are ALWAYS included.
# Need to verify accuracy of merging.
# Sample code: test <- importData(countries = c("AT", "HU"), 2007, 
# c = "DB090", "HX090", "PY090G", "PY200G")

importData <- function(countries, year, vars){
  
  # Simplify year
  year2 <- substr(as.character(year), 3, 4)
  # Separate vars, force include country and ID
  H <- c("HB020", "HB030", grep("H+", vars, value = TRUE))
  D <- c("DB020", "DB030", grep("D+", vars, value = TRUE))
  P <- c("PB020", "PB030", grep("P+", vars, value = TRUE))
  
  # Import to data
  householdData <- merge.data.frame(fread(paste("UDB_c", year2, "D.csv", sep = ""),
                                          select = D),
                                    fread(paste("UDB_c", year2, "H.csv", sep = ""),
                                          select = H),
                                    by.x = c("DB020", "DB030"), 
                                    by.y = c("HB020", "HB030"))
  # Reduce to countries of interest
  householdData <- householdData[householdData$DB020 %in% countries, ]
  
  personData <- fread(paste("UDB_c", year2, "P.csv", sep = ""), select = P)
  # Reduce to countries of interest
  personData <- personData[(personData$PB020 %in% countries), ]
  
  # Merge data
  personData$DB030 <- substr(as.character(personData$PB030), 1,
                             nchar(personData$PB030) - 2)
  allTogetherNow <- merge.data.frame(householdData, personData, 
                                     by.x = c("DB020", "DB030"),
                                     by.y = c("PB020", "DB030"))
} # end function importData


#----------------------------------------------#
### EVALUATING QUANTILES (WEIGHTS)
# Quantiles with weights (used for median as the default)
# To be used in conjunction with pov1

quanW <- function(values, weights, p = 0.5){
  
  # The sorted list of weights  --  cumulative totals
  sortedWeights <-  cumsum(weights[sort.list(values)])
  
  # The quantile of the cumulative weights
  quanWeights <- p * sortedWeights[length(values)]
  
  # The order of the quantile
  order <- sum(sortedWeights < quanWeights)
  
  # The corresponding values of values and weights
  qvc <- c(min(values), sort(values))[order+seq(2)]
  qwe <- c(0, sortedWeights)[order+seq(2)]
  
  # Their linear combination
  sum(qvc * (quanWeights - rev(qwe))*c(-1,1) ) / (qwe[2]-qwe[1])
}  #  end function quanW


#----------------------------------------------#
### CALCULATE POVERTY RATES
# To be used in conjunction with pov1

povRates <- function(data, weights, thresholds, median){
  ### Poverty rates for a range of thresholds -- function used in pov
  # Initialize rates
  rates <- c()
  
  # Rate for each threshold
  for (threshold in thresholds){
    rates <- c(rates, sum(weights * (data[, "HX090"] < threshold * median)) )
  }
  
  names(rates) <- 100 * thresholds
  
  # Conversion to percentages
  100 * rates / sum(weights)
} # end function povRates

#----------------------------------------------#
### CALCULATE POVERTY RATES FOR A RANGE OF THRESHOLDS FOR WHOLE COUNTRY
# Only works with 1 country for now!

pov1 <- function(data, thresholds = 60)
{
  ##  Check on the thresholds  --  have to be fractions
  while (max(thresholds) > 1.5){
    thresholds <- thresholds/100
  } 
  
  ##  The median equivalised income  --  households
  MDh <- quanW(data[, "HX090"], data[, "DB090"])
  
  ##  The poverty rates for the households
  RTh <- povRates(data, data[, "DB090"], thresholds, MDh)
}  ##  end function pov1
