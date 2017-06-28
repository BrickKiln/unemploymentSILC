# LOAD STUFF
library(data.table) # for import function fread
library(ggplot2)
library(directlabels)
library(gridExtra) # for combinePlot function

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

write.csv(endMyLifeFam, "massiveData.csv")

# Data for pov rate in comparison to HUNGARY
fuckMe <- data.frame(country = character(), year = integer(), 
                     threshold = integer(), 
                     personRate = double(), householdRate = double(),
                     personRateNoBen = double(), householdRateNoBen = double())

fuckMe <- endMyLifeFam[which(endMyLifeFam$country == "HU"),]
fuckMe <- rbind(fuckMe, fuckMe, fuckMe, fuckMe)

diff <- endMyLifeFam[, 4:7] - fuckMe[, 4:7]
diff <- cbind(endMyLifeFam[, 1:3], diff)
write.csv(diff, "diffPovRates.csv", row.names = FALSE)

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
write.csv(eternalVoid, "eternalVoid.csv")

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


#----------------------------------------------------------

####################
## PLOT FUNCTIONS ##
####################
# Load data
massiveData <- read.csv("massiveData.csv")
diff <- read.csv("diffPovRates.csv")

# Color-blind- friendly pallette
# add this to ggplot: scale_colour_manual(values=cbPalette)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Plot compare rates among countries
plotCompare <- function(year, rate, title){
  ggplot(data = massiveData[which(massiveData$year == year),], 
         aes_string("threshold", rate, colour = "country")) + geom_line() +
    geom_vline(xintercept = 60, linetype = "dotted") + theme_classic() +
    ggtitle(title) + theme(legend.position = "none") + scale_colour_manual(values=cbPalette) +
    scale_y_continuous(limits = c(0, 42)) +
    theme(plot.title = element_text(family = "Trebuchet MS", size = 22, 
                                    color="#666666", face="bold")) +
    geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), 
                                                cex = 0.8))

} # end of function plotCompare

# Plot rates of countries using HU as base 
plotDiff <- function(year, rate, title){
  ggplot(data = diff[which(diff$year == year),], 
         aes_string("threshold", rate, colour = "country")) + geom_line() +
    geom_vline(xintercept = 60, linetype = "dotted") + theme_classic() +
    ggtitle(title) + theme(legend.position = "none", 
                           plot.title = element_text(family = "Trebuchet MS", size = 22,
                                                     color="#666666", face="bold")) + 
    scale_colour_manual(values=cbPalette) +
    scale_y_continuous(name = "poverty rate (%)", limits = c(-8, 18)) +
    geom_dl(aes(label = country), method = list(dl.combine("first.points", "last.points"), 
                                                cex = 0.8))
} # end of function plotDiff

# Plot difference of curves within a country
plotCountry <- function(country, year, title){
  temp <- melt(massiveData[which(massiveData$year == year & massiveData$country == country), c(-1)],
               measure.vars = c("personRate", "personRateNoBen", "householdRate",
                                "householdRateNoBen"))
  ggplot(data = temp, aes(threshold, value, colour = variable)) + geom_line() + 
    geom_vline(xintercept = 60, linetype = "dotted") + theme_classic() +
    ggtitle(title) + theme(legend.position = c(0.2, 0.8), legend.title = element_blank(),
                           plot.title = element_text(family = "Trebuchet MS", size = 22,
                                                     color="#666666", face="bold")) +
    scale_colour_manual(values=cbPalette) + scale_y_continuous(name = "poverty rate (%)", limits = c(0, 42))
} # end of function plotCountry

# Combine Plots
combinePlot <- function(plot1, plot2){
  temp = list(plot1, plot2)
  do.call(grid.arrange, temp)
  grid.arrange(plot1, plot2, ncol = 2, nrow = 1)
} # end of function combinePlot

##################
# Plot poverty rates in relation in Hungary 2010 and 2014
test <- plotDiff(2011, "householdRate", "2011")
test1 <- plotDiff(2014, "householdRate", "2014")
combinePlot(test, test1)
# Plot poverty rates in relation to Hungary in 2010 and 2014, no ben
random <- plotDiff(2011, "householdRateNoBen", "2011")
random1 <- plotDiff(2014, "householdRateNoBen", "2014")
combinePlot(random, random1)

# Plot country rates for Hungary
hu06 <- plotCountry("HU", 2006, "HU 2006")
hu11 <- plotCountry("HU", 2011, "HU 2011")
hu13 <- plotCountry("HU", 2013, "HU 2013")
hu14 <- plotCountry("HU", 2014, "HU 2014")
combinePlot(hu11, hu14)
# for Latvia 2010 and 2014
lv10 <- plotCountry("LV", 2011, "LV 2011")
lv14 <- plotCountry("LV", 2014, "LV 2014")
combinePlot(lv10, lv14)

lv10all <- importData(countries = "LV", year = 2010, 
                      vars = c("DB090", "HX090", "PY090G", "HX050", "HY020"))
lv14all <- importData(countries = "LV", year = 2014, 
                      vars = c("DB090", "HX090", "PY090G", "HX050", "HY020"))
lv07all <- importData(countries = "LV", year = 2008, 
                      vars = c("DB090", "HX090", "PY090G", "HX050", "HY020"))
summary(lv10all$PY090G)
summary(lv14all$PY090G)
summary(lv07all$py090G)


test <- importData("HU", 2015, vars = c("DB090", "HX090", "PY090G", "HX050", "HY020"))
summary(test$HX090)



setwd("/Users/SupremeLeader/Documents/Central European University/Applied Policy Project/Cross-sectional Data")
euromod <- read.table("HU_2012_a5.txt")
hudata12 <- importData("HU", 2012, vars = c("DB090", "HX090", "PY090G", "HX050", "HY020"))
(setdiff(sort(unique(euromod$idhh)), sort(unique(hudata12$DB030))))
(setdiff(sort(unique(hudata12$DB030)), sort(unique(euromod$idhh))))

endMyLifeFam <- endMyLifeFam[-which(endMyLifeFam$year == 2006),]
endMyLifeFam <- read.csv("endMyLifeFam.csv")
endMyLifeFam <- endMyLifeFam[, -c(1)]
abc <- melt(endMyLifeFam, id.vars = c("country", "year", "threshold"))
abc$variable <- paste(abc$variable, abc$country, sep = "")
write.csv(abc, "final.csv")

setwd("/Users/SupremeLeader/Documents/Coding Stuff/test")
endMyLifeFam <- read.csv("endMyLifeFam.csv")
endMyLifeFam <- endMyLifeFam[, -c(1)]
write.csv(endMyLifeFam, "endMyLIfeFam.csv", row.names = FALSE)







###################
# Graph for Armine's data
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
  ggtitle("Public Expenditure on \n Direct Job Creation") + theme(legend.position = "none") + scale_colour_manual(values=cbPalette) +
  theme(plot.title = element_text(family = "Trebuchet MS", size = 22, 
                                  color="#666666", face="bold")) +
  labs(y = "Expenditure (% of GDP)") +
  geom_dl(aes(label = Country), method = list(dl.combine("first.points", "last.points"), 
                                              cex = 0.8))
combinePlot(plot2, plot1)

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


##############################
# Fix according to Alexis
# Plot difference between ben and no ben for 2 years on same graph
massiveData$diffPerson <- massiveData$personRateNoBen - massiveData$personRate
massiveData$diffHouse <-massiveData$householdRateNoBen - massiveData$householdRate
totona <- massiveData[which(massiveData$country == "HU"), c(2, 3, 4, 10)]
totona <- totona[which(totona$year == 2014 | totona$year == 2011), -c(1)]
totona$year <- as.factor(totona$year)
totona = melt(totona, id.vars = c("threshold", "year"))

plot(ggplot(data = totona, 
       aes(threshold, diffHouse, colour = year)) + geom_line() +
  theme_classic() + geom_vline(xintercept = 60, linetype = "dotted") +
  ggtitle("Hungary, 2011 & 2014") + theme(legend.position = "none") + scale_colour_manual(values=cbPalette) +
  theme(plot.title = element_text(family = "Trebuchet MS", size = 22, 
                                  color="#666666", face="bold")) +
  labs(y = "change (% point)") +
  geom_dl(aes(label = year), method = list(dl.combine("first.points", "last.points"), 
                                              cex = 0.8)))

plot(plotCountry("HU", 2011, "Hungary, 2011"))

