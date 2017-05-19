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
# Added incomeVar as input to distinguish total income and total income before
# benefits. Takes either "HX090" or "rawIncome"

povRates <- function(data, incomeVar, weights, thresholds, median){
  ### Poverty rates for a range of thresholds -- function used in pov
  # Initialize rates
  rates <- c()
  
  # Rate for each threshold
  for (threshold in thresholds){
    rates <- c(rates, sum(weights * (data[, incomeVar] < threshold * median)) )
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
  
  ## Import data
  data <- importData(countries = country, year = year, 
                     vars = c("DB090", "HX090", "PY090G"))
  
  ## Calculate income before unemployment benefits
  data <- rawIncome(data)
  
  ## The median equivalized income -- persons
  medianPerson <- quanW(data[, "HX090"], data[, "DB090"])
  
  ## Poverty rates for persons
  povRatePerson <- povRates(data, "HX090", data[, "DB090"], 
                            thresholds, medianPerson)
  
  ## Poverty rates for persons -- raw income
  povRatePersonRaw <- povRates(data, "rawIncome", data[, "DB090"], 
                               thresholds, medianPerson)
  
  ## Reduce data to household level
  data <- data[!duplicated(data[, "DB030"]),]
  
  ## The median equivalized income  --  households 
  medianHouse <- quanW(data[, "HX090"], data[, "DB090"])
  
  ## The poverty rates for households 
  povRateHouse <- povRates(data, "HX090", data[, "DB090"], 
                           thresholds, medianHouse)
  
  ## Poverty rates for households -- raw income
  povRateHouseRaw <- povRates(data, "rawIncome", data[, "DB090"], 
                              thresholds, medianHouse)
  
  data.frame(country = country, year = year, threshold = thresholds * 100, 
             personRate = povRatePerson, householdRate = povRateHouse, 
             personRateNoBen = povRatePersonRaw,
             householdRateNoBen = povRateHouseRaw)
}  ##  end function pov1



#----------------------------------------------#
# TEST STUFF
# Pov rate plot for countries 
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

# Put in grid 
library(gridExtra)
myPlotList = list(plot1, plot2, plot3, plot4)
do.call(grid.arrange,  myPlotList)
grid.arrange(plot1, plot2, plot3, plot4)

# tested some shit here I don't know what
a$year <- "2012"
b$year <- "2014"
c <- rbind(a[which(a$country == "CZ"),], b[which(a$country == "CZ"),])
plot(ggplot(data =c, aes(threshold, personRate, colour = year)) +
       geom_line())



beta <- melt(hu1[, -c(1)], measure.vars = c("personRate", "personRateNoBen", "householdRate", "householdRateNoBen"))
head(beta)
plot(ggplot(data = beta, aes(threshold, value, colour = variable)) +
       geom_line() + theme_classic())
beta
hu1
write.csv(hu1, "hu.csv")



#-----------------------------------------------------------------------------------
# Generate massive dataset for pov rates for all countries all years
listCountry <- c("HU", "LV", "CZ", "SK")
listYear <- c(2005:2014)

endMyLifeFam <- data.frame(country = character(), year = integer(), 
                          threshold = integer(), 
                          personRate = double(), householdRate = double(),
                          personRateNoBen = double(), householdRateNoBen = double())
for (country in listCountry){
  for (year in listYear){
    endMyLifeFam <- rbind(endMyLifeFam, pov1(country, year, seq(30, 80, 1)))
  }
}

endMyLifeFam[which(endMyLifeFam$country == "HU" & endMyLifeFam$year == 2005),]
write.csv(endMyLifeFam, "povRates.csv")

# Data for pov rate in comparison to HUNGARY
fuckMe <- data.frame(country = character(), year = integer(), 
                     threshold = integer(), 
                     personRate = double(), householdRate = double(),
                     personRateNoBen = double(), householdRateNoBen = double())

fuckMe <- endMyLifeFam[which(endMyLifeFam$country == "HU"),]
fuckMe <- rbind(fuckMe, fuckMe, fuckMe, fuckMe)

diff <- endMyLifeFam[, 4:7] - fuckMe[, 4:7]
diff <- cbind(endMyLifeFam[, 1:3], diff)
write.csv(diff, "diffPovRates.csv")

json <- toJSON(diff)
write(json, "deathEmbrace.json")

#------------------------------------------------------------------------------------




# graph for difference between pov rate before and after benefits 


omega <- melt(lv1[, -c(1)], measure.vars = c("personRate", "personRateNoBen", "householdRate", "householdRateNoBen"))
head(beta)
plot(ggplot(data = omega, aes(threshold, value, colour = variable)) +
       geom_line() + theme_classic())

lv1_1 <- lv1[, c("country" ,"threshold", "householdRate", "householdRateNoBen")]
lv1_1$difference <- abs(lv1_1$householdRate - lv1_1$householdRateNoBen)
lv1_1 <- melt(lv1_1, measure.vars = c("householdRate", "householdRateNoBen"))
plot(ggplot(data = lv1_1, aes(threshold, difference)) +
       geom_line() + theme_classic())


hu1_1 <- hu1[, c("country" ,"threshold", "householdRate", "householdRateNoBen")]
hu1_1$difference <- abs(hu1_1$householdRate - hu1_1$householdRateNoBen)
hu1_1 <- melt(hu1_1, measure.vars = c("householdRate", "householdRateNoBen"))
plot(ggplot(data = hu1_1, aes(threshold, difference)) +
       geom_line() + theme_classic())


cz1_1 <- cz1[, c("country" , "threshold", "householdRate", "householdRateNoBen")]
cz1_1$difference <- abs(cz1_1$householdRate - cz1_1$householdRateNoBen)
cz1_1 <- melt(cz1_1, measure.vars = c("householdRate", "householdRateNoBen"))
plot(ggplot(data = cz1_1, aes(threshold, difference)) +
       geom_line() + theme_classic())

#################################################################################
# Prepare data for D3
# eternalVoid = data, totalDespair = difference to Hungary
HU <- data.frame(country = character(), year = integer(), 
                 threshold = integer(), 
                 personRate = double(), householdRate = double(),
                 personRateNoBen = double(), householdRateNoBen = double())
for (year in listYear){
  HU <- rbind(HU, pov1("HU", year, seq(30, 80, 1)))
}
names(HU) <- c("country", "year", "threshold", "personHU", "householdHU",
               "personNoBenHU", "householdNoBenHU")
  
LV <- data.frame(country = character(), year = integer(), 
                 threshold = integer(), 
                 personRate = double(), householdRate = double(),
                 personRateNoBen = double(), householdRateNoBen = double())

for (year in listYear){
  LV <- rbind(LV, pov1("LV", year, seq(30, 80, 1)))
}  
names(LV) <- c("country", "year", "threshold", "personLV", "householdLV",
               "personNoBenLV", "householdNoBenLV")

CZ <- data.frame(country = character(), year = integer(), 
                 threshold = integer(), 
                 personRate = double(), householdRate = double(),
                 personRateNoBen = double(), householdRateNoBen = double())
for (year in listYear){
  CZ <- rbind(CZ, pov1("CZ", year, seq(30, 80, 1)))
}
names(CZ) <- c("country", "year", "threshold", "personCZ", "householdCZ",
               "personNoBenCZ", "householdNoBenCZ")

SK <- data.frame(country = character(), year = integer(), 
                 threshold = integer(), 
                 personRate = double(), householdRate = double(),
                 personRateNoBen = double(), householdRateNoBen = double())
for (year in listYear){
  SK <- rbind(SK, pov1("SK", year, seq(30, 80, 1)))
}
names(SK) <- c("country", "year", "threshold", "personSK", "householdSK",
               "personNoBenSK", "householdNoBenSK")


eternalVoid <- cbind(HU[, -c(1)], LV[, -c(1,2,3)], CZ[, -c(1,2,3)], SK[, -c(1,2,3)])
totalDespair <- eternalVoid
totalDespair$personLV <- totalDespair$personLV - totalDespair$personHU
totalDespair$personCZ <- totalDespair$personCZ - totalDespair$personHU
totalDespair$personSK <- totalDespair$personSK - totalDespair$personHU
totalDespair$householdLV <- totalDespair$householdLV - totalDespair$householdHU
totalDespair$householdCZ <- totalDespair$householdCZ - totalDespair$householdHU
totalDespair$householdSK <- totalDespair$householdSK - totalDespair$householdHU
totalDespair$personNoBenLV <- totalDespair$personNoBenLV - totalDespair$personNoBenHU
totalDespair$personNoBenCZ <- totalDespair$personNoBenCZ - totalDespair$personNoBenHU
totalDespair$personNoBenSK <- totalDespair$personNoBenSK - totalDespair$personNoBenHU
totalDespair$householdNoBenLV <- totalDespair$householdNoBenLV - totalDespair$householdNoBenHU
totalDespair$householdNoBenCZ <- totalDespair$householdNoBenCZ - totalDespair$householdNoBenHU
totalDespair$householdNoBenSK <- totalDespair$householdNoBenSK - totalDespair$householdNoBenHU
totalDespair$personHU <- 0
totalDespair$householdHU <- 0
totalDespair$personNoBenHU <- 0
totalDespair$householdNoBenHU <- 0

write.csv(totalDespair, "d3data.csv")

############################################################################
# Plot some shit. Difference in poverty compare to HUNGARY

plot(ggplot(diff[which(diff$year == 2014),],
            aes(threshold, personRate, colour = country)) + geom_line() +
       theme_classic() + scale_y_continuous(limits = c(-15, 15)) +
       geom_vline(xintercept = 60))



