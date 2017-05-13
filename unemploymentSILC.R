# LOAD STUFF
library(data.table) # for import function fread

setwd("/Users/SupremeLeader/Documents/Central European University/Applied Policy Project/Cross-sectional Data")
################################################

#########################
## PREDEFINE FUNCTIONS ##
#########################

#----------------------------------------------#
### IMPORT DATA
# Make sure to flatten directory with all files in the format "UDB_c[year][type].csv"
# Have not included conditions, works best when used with at least one variable
# from each of D, H, and P file.
# IDs of both households and persons are ALWAYS included.
# Need to verify accuracy of merging.
# Sample code: test <- importData(countries = c("AT", "HU"), 2007, 
# vars = c("DB090", "HX090", "PY090G", "PY200G"))

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
                                    by.y = c("HB020", "HB030")) # y replaced by x
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
# Sample code: pov1(country = "HU", year = 2012, thresholds = seq(30, 80, 1))

pov1 <- function(country, year, thresholds = 60)
{
  ##  Check on the thresholds  --  have to be fractions
  while (max(thresholds) > 1.5){
    thresholds <- thresholds/100
  } 
  
  data <- importData(countries = country, year = year, vars = c("DB090", "HX090"))
  
  ## The median equivalized income -- persons
  medianPerson <- quanW(data[, "HX090"], data[, "DB090"])
  
  ## Poverty rates for persons
  povRatePerson <- povRates(data, data[, "DB090"], thresholds, medianPerson)
  
  ## Reduce data to household level
  data <- data[!duplicated(data[, "DB030"]),]
  
  ##  The median equivalized income  --  households 
  
  medianHouse <- quanW(data[, "HX090"], data[, "DB090"])
  
  ##  The poverty rates for the households 
  povRateHouse <- povRates(data, data[, "DB090"], thresholds, medianHouse)
  
  rbind(person = povRatePerson, household = povRateHouse)
}  ##  end function pov1

#----------------------------------------------#
### PLOT SHITS

##  Application
par(mfrow=c(1,1), mar=c(4,4,1,1), mgp=0.75*c(3,1,0), lab=c(3,3,1))

##  Diagram for Austria 2010 
EUpov2(AT410b[[7]])

EUpov2 <- function(res, rws=c(1,2))
{
  ###   Individual- and household-level poverty rates  
  ##  res    The result object (matrix)
  ##  rws    The rows to plot
  
  ##  The valuation points 
  pts <- as.numeric(colnames(res))
  npt <- length(pts)
  
  ##  Empty shell of the plot
  plot(range(pts), range(res), type="n", 
       xlab="Poverty threshold (%)", ylab="Poverty rate (%)", 
       cex.lab=0.9, cex.axis=0.9)
  
  ##  No smoothing 
  for (rw in rws)
    lines(pts, res[rw, ], lty=rw, lwd=0.1)
  
  ##  Neighbourhood smoothing 
  for (rw in rws)
    lines(pts, SMOO(res[rw, ], 0.25), lty=rw, lwd=0.4)
  
  ##  Normal kernel smoothing  -- preferred
  for (rw in rws)
    lines(pts, KernSq(res[rw, ], npt/60), lty=rw, lwd=1.1, col="gray50")
  
  ##  The conventional threshold
  abline(v=60, lty=3, lwd=0.9)
  
  ##  Legend at the bottom-right hand corner
  legend(sum(range(pts)*c(0.07, 0.93)), sum(range(res)*c(0.87,0.13)), 
         lty=rws, substring(rownames(res)[rws], 1, 1), cex=0.7)
  
  "DONE"
}  ##  End of function EUpov2

a <-pov1(test[which(test$DB020 == "HU"),], 60)


a <- pov1(country = c("HU"), year = 2012, thresholds = seq(30, 50, 1))
