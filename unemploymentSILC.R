# LOAD STUFF
library(data.table) # for import function fread
library(ggplot2)

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
# CALCULATE INCOME BEFORE UNEMPLOYMENT BENEFITS

rawIncome <- function(dat){
  dat$rawIncome <- NA
  for (household in unique(dat$DB030)){
    dat$rawIncome[which(dat$DB030 == household)] <- (
      dat$HX090[which(dat$DB030 == household)] - 
        sum(dat$PY090G[which(dat$DB030 == household)])
    )
  }
  return(dat)
} # end function rawIncome

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
  
  data <- importData(countries = country, year = year, 
                     vars = c("DB090", "HX090", "PY090G"))
  
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
  
  data.frame(country = country, threshold = thresholds * 100, personRate = povRatePerson, 
             householdRate = povRateHouse)
}  ##  end function pov1

#----------------------------------------------#
### PLOT SHITS - UNUSED

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


##  Application
par(mfrow=c(1,1), mar=c(4,4,1,1), mgp=0.75*c(3,1,0), lab=c(3,3,1))


HU10b <- pov1("HU", 2010, thresholds = seq(30, 80, 1))
HU12b <- pov1("HU", 2012, thresholds = seq(30, 80,1))

##  Initialisation for the annual results
HU1012b <- list()

##  Evaluation for a set of years 
for (yr in seq(2010, 2012)){
  HU1012b[[as.character(yr)]] <- pov1("HU", yr, thresholds=seq(30, 80, 0.5))
}
##  Diagram for Austria 2010 
EUpov2(HU1012b[[1]])


##  Smoothing functions  SMOO,  KernSq
SMOO <- function(vec, sm=0.25)
{
  ###  Function to smooth vector  vec  with coefficient of smoothing  sm
  
  ##  From percentage to fraction
  while (sm > 1) sm <- sm/100
  
  ##  The length of the vector
  nve <- length(vec)
  
  ##  The smoothing
  c(vec[1], vec[-seq(2)]*sm+vec[-c(1,nve)]*(1-2*sm)+vec[-(nve-c(0,1))]*sm, vec[nve])
}  ##  End of function  SMOO


KernSq <- function(ysq, sde)
{
  ###  Kernel smoothing for a sequence  (see Section 7.4.1)
  
  ##  ysq    The sequence
  ##  sde    The standard deviation of the normal kernel
  
  ##  The length of the sequence 
  n <- length(ysq)
  
  ##  The weights
  smw <- dnorm(seq(n)-1, 0, sde)
  
  ##  Initialisation for the smoothed values
  ysm <- c()
  
  for (k in seq(n))
  {
    wei <- smw[1+abs(k - seq(n))]
    ysm <- c(ysm, sum(ysq * wei) / sum(wei))
  }
}  ##  End of function  KernSq

## UNUSED 

#----------------------------------------------#
# TEST STUFF

hu1 <- pov1("HU", 2014, seq(30, 80, 1))
lv1 <- pov1("LV", 2014, seq(30, 80, 1))
cz1 <- pov1("CZ", 2014, seq(30, 80, 1))
b <- rbind(hu1, lv1, cz1)
plot2 <- ggplot(data = b, aes(threshold, personRate, 
                          colour = country)) + 
       geom_line() + geom_vline(xintercept = 60) + theme_classic() +
       ggtitle("Poverty Rates for Different Thresholds, Person 2014")

hu <- pov1("HU", 2012, seq(30, 80, 1))
lv <- pov1("LV", 2012, seq(30, 80, 1))
cz <- pov1("CZ", 2012, seq(30, 80, 1))
a <- rbind(hu, lv, cz)




plot1 <- ggplot(data = a, aes(threshold, personRate, 
                          colour = country)) + 
       geom_line() + geom_vline(xintercept = 60) + theme_classic() +
       ggtitle("Poverty Rates for Different Thresholds, Person 2012")

plot3 <- ggplot(data = a, aes(threshold, householdRate, 
                                   colour = country)) + 
                geom_line() + geom_vline(xintercept = 60) + theme_classic() +
                ggtitle("Poverty Rates for Different Thresholds, Household 2012")

plot4 <- ggplot(data = b, aes(threshold, householdRate, 
                                   colour = country)) + 
                geom_line() + geom_vline(xintercept = 60) + theme_classic() +
                ggtitle("Poverty Rates for Different Thresholds, Household 2014")


library(gridExtra)
myPlotList = list(plot1, plot2, plot3, plot4)
do.call(grid.arrange,  myPlotList)
grid.arrange(plot1, plot2, plot3, plot4)

a$year <- "2012"
b$year <- "2014"
c <- rbind(a[which(a$country == "CZ"),], b[which(a$country == "CZ"),])
plot(ggplot(data =c, aes(threshold, personRate, colour = year)) +
       geom_line())






