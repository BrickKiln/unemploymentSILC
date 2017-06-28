# LOAD LIBRARIES
library(data.table) # for import function fread
library(ggplot2)
library(directlabels) # use in conjunction with ggplot2
library(gridExtra) # for combinePlot function

# Set directory to SILC Cross-sectional
setwd("/Users/SupremeLeader/Documents/Central European University/Applied Policy Project/Cross-sectional Data")
################################################

#########################
## PREDEFINE FUNCTIONS ##
#########################

#----------------------------------------------#
#################
## IMPORT DATA ##
#################
# Make sure to flatten directory with all files in the format "UDB_c[year][type].csv"
# This can be done using bash/command line

# Have not included conditions, works best when used with at least one variable
# from each of D, H, and P file.
# IDs of both households and persons are ALWAYS included.
# Need to verify accuracy of merging.

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

# EXAMPLE 
test <- importData(countries = c("AT", "HU"), 2007, 
                   vars = c("DB090", "HX090", "PY090G", "PY200G"))


#----------------------------------------------#
#########################################
## INCOME BEFORE UNEMPLOYMENT BENEFITS ##
#########################################

rawIncome <- function(dat){
  dat$rawIncome <- NA
  for (household in unique(dat$DB030)){
    dat$rawIncome[which(dat$DB030 == household)] <- (
      (dat$HY020[which(dat$DB030 == household)] - 
        sum(dat$PY090G[which(dat$DB030 == household)]))/
        dat$HX050[which(dat$DB030 == household)]
    )
  }
  return(dat)
} # end function rawIncome


#----------------------------------------------#
####################################
## EVALUATING QUANTILES (WEIGHTS) ##
####################################

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
#############################
## CALCULATE POVERTY RATES ##
#############################
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
                     vars = c("DB090", "HX090", "PY090G", "HX050", "HY020"))
  
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


#-----------------------------------------------------------------------------------
#######################################################################
# GENERATE MASSIVE DATASET FOR POV RATES FOR ALL 4 COUNTRIES ALL YEARS#
#######################################################################

# Create lists of countries and years for for loop 
listCountry <- c("HU", "LV", "CZ", "SK")
listYear <- c(2005:2014)

# Create empty dataframe as placeholder for for loop output
povRatesOut <- data.frame(country = character(), year = integer(), 
                          threshold = integer(), 
                          personRate = double(), householdRate = double(),
                          personRateNoBen = double(), householdRateNoBen = double())

# For loop calculating poverty rate for each country each year
for (country in listCountry){
  for (year in listYear){
    povRatesOut <- rbind(povRatesOut, pov1(country, year, seq(30, 80, 1)))
  }
}

# Save to csv
write.csv(povRatesOut, "povRatesOut.csv", row.names = FALSE)

#############################################
# DATA FOR POV RATE IN COMPARISON TO HUNGARY#
#############################################
# Create a dataframe of base poverty rates (Hungarian rates). Output is the
# difference between povRatesOut and base

# Create empty dataframe, identical to previous dataframe for povRatesOut
baseRates <- data.frame(country = character(), year = integer(), 
                     threshold = integer(), 
                     personRate = double(), householdRate = double(),
                     personRateNoBen = double(), householdRateNoBen = double())

# Fill dataframe with data from povRatesOut, filtered to include only Hungary
baseRates <- povRatesOut[which(povRatesOut$country == "HU"),]

# Row bind 4 times to match the 4 countries in povRatesOut 
baseRates <- rbind(baseRates, baseRates, baseRates, baseRates)

# Get poverty rates in comparison to Hungary by subtracting base from povRatesOut
diffPovRatesOut <- povRatesOut[, 4:7] - baseRates[, 4:7]
diffPovRatesOut <- cbind(povRatesOut[, 1:3], diffPovRatesOut)
write.csv(diffPovRatesOut, "diffPovRatesOut.csv", row.names = FALSE)

#------------------------------------------------------------------------------------


####################
## PLOT FUNCTIONS ##
####################
# Load data
povRatesOut <- read.csv("povRatesOut.csv")
diffPovRatesOut <- read.csv("diffPovRatesOut.csv")

# Create a color-blind-friendly pallette
# add this to ggplot: scale_colour_manual(values = cbPalette)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# USAGE: FUNCTIONS GENERALLY TAKE ARGUMENTS YEAR, RATE, COUNTRY, TITLE, OF WHICH:
# YEAR: in numeric
# RATE: in string, either "personRate" or "householdRate"
# COUNTRY: in string, abbreviation
# TITLE: in string, whatever the graph's name should be

######################################
# Plot compare rates among countries #
######################################
# Not used

plotCompare <- function(year, rate, title){
  ggplot(data = povRatesOut[which(povRatesOut$year == year),], 
         aes_string("threshold", rate, colour = "country")) + geom_line() +
    geom_vline(xintercept = 60, linetype = "dotted") + theme_classic() +
    ggtitle(title) + theme(legend.position = "none") + 
    scale_colour_manual(values = cbPalette) +
    scale_y_continuous(limits = c(0, 42)) +
    theme(plot.title = element_text(family = "Trebuchet MS", size = 22, 
                                    color="#666666", face="bold")) +
    geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), 
                                                cex = 0.8))

} # end of function plotCompare

# EXAMPLE
plotCompare(2014, "householdRate", "Poverty Rates for Households in 2014")

############################################
# Plot rates of countries using HU as base #
############################################

plotDiff <- function(year, rate, title){
  ggplot(data = diffPovRatesOut[which(diffPovRatesOut$year == year),], 
         aes_string("threshold", rate, colour = "country")) + geom_line() +
    geom_vline(xintercept = 60, linetype = "dotted") + theme_classic() +
    ggtitle(title) + theme(legend.position = "none", 
                           plot.title = element_text(family = "Trebuchet MS", size = 22,
                                                     color="#666666", face="bold")) + 
    scale_colour_manual(values = cbPalette) +
    scale_y_continuous(name = "poverty rate (%)", limits = c(-8, 18)) +
    geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), 
                                                cex = 0.8))
} # end of function plotDiff

# EXAMPLE
plotDiff(2014, "householdRate", "Poverty Rates for Household in 2014 \nin Comparison to Hungary")

##############################################
# Plot difference of curves within a country #
##############################################
# Not used. Hard to differentiate lines, not recommended. Use the next function instead
plotCountry <- function(country, year, title){
  temp <- melt(povRatesOut[which(povRatesOut$year == year & povRatesOut$country == country), c(-1)],
               measure.vars = c("personRate", "personRateNoBen", "householdRate",
                                "householdRateNoBen"))
  ggplot(data = temp, aes(threshold, value, colour = variable)) + geom_line() + 
    geom_vline(xintercept = 60, linetype = "dotted") + theme_classic() + ggtitle(title) + 
    theme(legend.position = c(0.2, 0.8), legend.title = element_blank(),
                           plot.title = element_text(family = "Trebuchet MS", size = 22,
                                                     color="#666666", face="bold")) +
    scale_colour_manual(values = cbPalette) + 
    scale_y_continuous(name = "poverty rate (%)", limits = c(0, 42))
} # end of function plotCountry

# EXAMPLE
plot(plotCountry("HU", 2011, "Hungary, 2011"))


##############################
# Recommended by Alexis Diamond
# Plot difference between ben and no ben in a country for 2 years on same graph
# Good for comparing effects of benefits on poverty rates over time
diffBenNoBen <- function(country, rate, year1, year2){
  povRatesOut$diffPerson <- povRatesOut$personRateNoBen - povRatesOut$personRate
  povRatesOut$diffHouse <- povRatesOut$householdRateNoBen - povRatesOut$householdRate
  compare <- povRatesOut[which(povRatesOut$country == country),]
  compare <- compare[which(compare$year == year1 | compare$year == year2),]
  compare$year <- as.factor(compare$year)
  if(rate == "personRate"){
    ggplot(data = compare, aes_string("threshold", "diffPerson", colour = "year")) + 
      geom_line() + theme_classic() + 
      geom_vline(xintercept = 60, linetype = "dotted") +
      ggtitle(paste(country, ", ", year1, " & ", year2, sep = "")) +
      theme(legend.position = "none") +
      scale_colour_manual(values = cbPalette) + 
      theme(plot.title = element_text(family = "Trebuchet MS", size = 22, 
                                      color = "#666666", face = "bold")) + labs(y = "change (% point)") + 
      geom_dl(aes(label = year), method = list(dl.combine("first.points", "last.points"),
                                               cex = 0.8))  
  }
  else{
    ggplot(data = compare, aes_string("threshold", "diffHouse", colour = "year")) + 
      geom_line() + theme_classic() + 
      geom_vline(xintercept = 60, linetype = "dotted") +
      ggtitle(paste(country, ", ", year1, " & ", year2, sep = "")) + 
      theme(legend.position = "none") +
      scale_colour_manual(values = cbPalette) + 
      theme(plot.title = element_text(family = "Trebuchet MS", size = 22, 
                                      color = "#666666", face = "bold")) + labs(y = "change (% point)") + 
      geom_dl(aes(label = year), method = list(dl.combine("first.points", "last.points"),
                                               cex = 0.8))
  }
  
} # end of function diffBenNoBen

# EXAMPLE
diffBenNoBen("HU", "householdRate", 2011, 2014)

#################
# Combine Plots #
#################

combinePlot <- function(plot1, plot2){
  temp = list(plot1, plot2)
  do.call(grid.arrange, temp)
  grid.arrange(plot1, plot2, ncol = 2, nrow = 1)
} # end of function combinePlot

#--------------------------------------------------------------------

##################
# Plot poverty rates in relation in Hungary 2011 and 2014
hu11 <- plotDiff(2011, "householdRate", "2011")
hu14 <- plotDiff(2014, "householdRate", "2014")
combinePlot(hu11, hu14)

# Plot poverty rates in relation to Hungary in 2011 and 2014, no ben
hu11NoBen <- plotDiff(2011, "householdRateNoBen", "2011")
hu14NoBen <- plotDiff(2014, "householdRateNoBen", "2014")
combinePlot(hu11NoBen, hu14NoBen)

# Plot country rates for Hungary
hu06 <- plotCountry("HU", 2006, "HU 2006")
hu11 <- plotCountry("HU", 2011, "HU 2011")
hu13 <- plotCountry("HU", 2013, "HU 2013")
hu14 <- plotCountry("HU", 2014, "HU 2014")
combinePlot(hu11, hu14)
# This should be replaced by
diffBenNoBen("HU", "householdRate", 2011, 2014)

# for Latvia 2010 and 2014
lv10 <- plotCountry("LV", 2011, "LV 2011")
lv14 <- plotCountry("LV", 2014, "LV 2014")
combinePlot(lv10, lv14)

#------------------------------------------------------

###################################################
# Graph for Armine's data                         #  
# Include graphs for public expenditures on ALMPs #
###################################################
setwd("/Users/SupremeLeader/Documents/Central European University/Applied Policy Project/codes/unemploymentSILC")

# Graph for expenditure on training vs job creation
graphStuff <- read.csv("Data-for-graphs.csv")
graphStuff$Country <- gsub("Hungary", "HU", graphStuff$Country)
graphStuff$Country <- gsub("Czech Reuplic", "CZ", graphStuff$Country)
graphStuff$Country <- gsub("Latvia", "LV", graphStuff$Country)
graphStuff$Country <- gsub("Slovakia", "SK", graphStuff$Country)
graphStuff$Train_Perc_GDP <- as.numeric(graphStuff$Train_Perc_GDP)
graphStuff$PW_Perc_GDP <- as.numeric(graphStuff$PW_Perc_GDP)

graph1 <- graphStuff[-which(graphStuff$Country == "EU"), c(1:4)]
graph1 <- graph1[-c(17:19),]
graph1$Train_Perc_GDP

plot1<- ggplot(data = graph1, 
               aes(Year, Train_Perc_GDP, colour = Country)) + geom_line() +
  theme_classic() +
  ggtitle("Public Expenditure on Training") + theme(legend.position = "none") + scale_colour_manual(values=cbPalette) +
  theme(plot.title = element_text(family = "Trebuchet MS", size = 22, 
                                  color="#666666", face="bold")) +
  labs(y = "Expenditure (% of GDP)") +
  geom_dl(aes(label = Country), method = list(dl.combine("first.points", "last.points"), 
                                              cex = 0.8))

plot2<- ggplot(data = graph1, 
               aes(Year, PW_Perc_GDP, colour = Country)) + geom_line() +
  theme_classic() +
  ggtitle("Public Expenditure on \nDirect Job Creation") + theme(legend.position = "none") + scale_colour_manual(values=cbPalette) +
  theme(plot.title = element_text(family = "Trebuchet MS", size = 22, 
                                  color="#666666", face="bold")) +
  labs(y = "Expenditure (% of GDP)") +
  geom_dl(aes(label = Country), method = list(dl.combine("first.points", "last.points"), 
                                              cex = 0.8))
combinePlot(plot2, plot1)

# Graph for unemployment rates in Hungary with and without public work
graph2 <- graphStuff[which(graphStuff$Country == "HU"), c(2, 5, 6)]
graph2 <- graph2[-(1:4),]
graph2 <- melt(graph2, id.vars = c("Year"))
graph2$variable <- gsub("Unemployment", "Unemployment Rate", graph2$variable)
graph2$variable <- gsub("Unemp_and_PW", "Unemployment Rate Including Public Work", graph2$variable)

plot(ggplot(data = graph2, 
            aes(Year, value, colour = variable)) + geom_line() +
       theme_classic() + theme(legend.position = "none") + scale_colour_manual(values=cbPalette) +
       theme(plot.title = element_text(family = "Trebuchet MS", size = 22, 
                                       color="#666666", face="bold"),
             legend.position = c(0.2, 0.2), legend.title = element_blank(),
             legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
             legend.text = element_text(size = 13)) +
       labs(y = "Rate (%)"))

# Graph for expenditure in Hungary vs EU
graph3 <- graphStuff[which(graphStuff$Country == "HU" | graphStuff$Country == "EU"), c(1,2, 7:11)]
graph3 <- graph3[-which(graph3$Year == 2016),]

graph3 <- read.csv("graph3.csv")
graph3$Year <- as.factor(graph3$Year)
names(graph3) <- c("Country", "Year", "Training", "Employment Incentives", "Supported Employment & Rehabilitation", 
                   "Direct Job Creation", "Start-up Incentive")
graph3HU <- graph3[which(graph3$Country == "Hungary"), c(2:7)]
graph3HU <- melt(graph3HU, id.vars = c("Year"))
graph3EU <- graph3[which(graph3$Country == "EU"), c(2:7)]
graph3EU <- melt(graph3EU, id.vars = c("Year"))


plot3<- ggplot(data = graph3HU, 
               aes(Year, value, fill = variable)) + geom_bar(stat = 'identity') + ggtitle("  HU") +
  theme_classic() + theme(legend.position = "none") + scale_fill_manual(values=cbPalette) +
  theme(plot.title = element_text(family = "Trebuchet MS", size = 22, 
                                  color="#666666", face="bold"),
        legend.position = c(0.35, 0.85), legend.title = element_blank(),
        legend.text = element_text(size = 10)) + 
  labs(y = "Million Euro")

plot4 <- ggplot(data = graph3EU, 
                aes(Year, value, fill = variable)) + geom_bar(stat = 'identity') + ggtitle("  EU") +
  theme_classic() + theme(legend.position = "none") + scale_fill_manual(values=cbPalette) +
  theme(plot.title = element_text(family = "Trebuchet MS", size = 22, 
                                  color="#666666", face="bold"),
        legend.position = "none", legend.title = element_blank(),
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
        legend.text = element_text(size = 13)) + 
  labs(y = "Million Euro")
combinePlot(plot3, plot4)



