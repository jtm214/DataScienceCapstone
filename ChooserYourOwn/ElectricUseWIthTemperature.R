##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

# Setting up the environment

#The packages and libraries used throughout the analysis need to be loaded on startup. 
#The analysis was a series of trial and error events so some of these libraries may no 
#longer be used in the final analysis but were need as part of the discovery process.

# Installing required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo");
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages(("kableExtra"))
if(!require(tidyr)) install.packages("tidyr");
if(!require(ggplot2)) install.packages("ggplot2");
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(summarytools)) install.packages("summarytools");
if(!require(RRF)) install.packages("RRF");
if(!require(kernlab)) install.packages("kernlab");
if(!require(LiblineaR)) install.packages("LiblineaR");
if(!require(purrr)) install.packages("purrr");
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("string");

# Loading required packages
library(tidyverse)
library(zoo)
library(caret)
library(kableExtra)
library(ggplot2)
library(knitr)
library(gridExtra)
library(dplyr)
library(summarytools)
library(RRF)
library(kernlab)
library(LiblineaR)
library(purrr)
library(data.table)
library(stringr)

#---------------Helper Functions-------------------------------------------
#This set of functions is here just to make it easier to calculate the 
#results of the predictions.
# The RMSE function that will be used for calculations
RMSE <- function(testData = NULL, predictedData = NULL) {
  sqrt(mean((testData - predictedData)^2))
}

#Mean square error
MSE <- function(testData = NULL, predictedData = NULL) {
  mean((testData-predictedData)^2)
}

#Mean absolute percent error
MAPE <- function(testData = NULL, predictedData = NULL) {
  mean(abs((testData-predictedData)/testData))*100
}

#Bias calcultion
BIAS <- function(testData = NULL, predictedData = NULL) {
  mean(testData-predictedData)
}

#Mean absolute error
MAE <- function(testData = NULL, predictedData = NULL) {
  mean(abs(testData-predictedData))
}

results <- function(testData = NULL, predictedData = NULL) {
  c(BIAS(testData, predictedData), RMSE(testData, predictedData), MSE(testData, predictedData),
    MAE(testData, predictedData), MAPE(testData, predictedData))
}
#---------------End Helper Functions---------------------------------------

# ---------------Begin Introduction/Overview section-----------------------
# Energy use dataset:
# https://www.kaggle.com/datasets/pythonafroz/price-of-electricity-and-the-renewable-energy?resource=download
# Temperature dataset:
# www.noaa.org  TODO - FIX ME - PUt exact url

#Import the data from github.  The files are in the git repo at the same folder level
#as this .R file.  So please make sure that you've pulled the full repo or that you've
#got all the files in the proper location before running the script.
options(timeout = 120)


#energyUse <- read.csv2("energyData.csv", header = TRUE, sep = ";", dec = ".")
energyUse <- read.csv2("https://raw.githubusercontent.com/jtm214/DataScienceCapstone/master/ChooserYourOwn/energyData.csv", 
                        header = TRUE, sep = ";", dec = ".")
#tempData <- read.csv("tempData.csv");
tempData <- read.csv("https://raw.githubusercontent.com/jtm214/DataScienceCapstone/master/ChooserYourOwn/tempData.csv")
#Determine how many unique temperature stations are available in the dataset
numOfStations <- length(unique(tempData$STATION));
numOfStations

stationNames <- unique(tempData$NAME);
stationNames

#The temperature data encompasses a number of stations.  I'm going to just use the madrid readings
#since it's fairly centralized within the country and I have limited time to work on this.  I'm also
#only going to use the daily max temperature since the average isn't available in a lot of the data sets.
#NOTE: The data is in Fahrenheit even though it's form Spain, that's just how i pulled the data from the
#noaa website
madridData <- tempData %>%
  filter(str_detect(NAME, "MADRID"))

dailyMeanTemps <- madridData %>% group_by(DATE) %>% 
                  summarize(tMax=mean(TMAX),
                            tMin=mean(TMIN))

#The data is in european format where the thousands are separated by a . and the decimal
#is delineated by the a comma so i'm going to strip out the . and replace the commas with a decimal.
#I'm really only interested in the total energy use so this is all happening with just one column.

#The energy data is tracked in 4 hour intervals so I'm just going to average it for the day since I don't 
#have quite that resolution in the temperature data.
dailyEnergyAverage <- energyUse %>% group_by(date) %>%
                      summarize(eDaily=sum(as.numeric(gsub(",",".",gsub("\\.","",energy.total..MWh.)))))

#There's a few NA's in the data so I'm just going to replace the value with the previous day's readings since the
#average temperature should be relatively consistent from day to day
dailyEnergyAverage <- na.locf(dailyEnergyAverage)
dailyMeanTemps <- na.locf(dailyMeanTemps)

#Next we're going to align the dates, first find the first/last date for both datasets

dailyMeanTemps[1,1]
dailyMeanTemps[dim(dailyMeanTemps)[1],1]

dailyEnergyAverage[1,1]
dailyEnergyAverage[dim(dailyEnergyAverage)[1],1]

#there's more temperature data than energy use so we're going to bracket 
#the analysis on the energy use data
dailyMeanTemps <- dailyMeanTemps %>% 
                  filter(DATE >= dailyEnergyAverage$date[1] & DATE <= dailyEnergyAverage$date[dim(dailyEnergyAverage)[1]])

dailyMeanTemps <- dailyMeanTemps %>% rename(date = DATE)
datum <- merge(dailyMeanTemps, dailyEnergyAverage, by = "date")

#Generate the test and train datasets now before we do any kind of date exploration or analysis
# Final hold-out test set will be 10% of data set
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = datum$eDaily, times = 1, p = 0.1, list = FALSE)
trainData <- datum[-test_index,]
testData <- datum[test_index,]

# Make sure tMax, tMin, and eDaily are also in final hold-out test set are also in edx set
final_holdout_test <- testData %>% 
  semi_join(datum, by = "tMax") %>%
  semi_join(datum, by = "tMin") %>% 
  semi_join(datum, by = "eDaily")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(testData, final_holdout_test)
trainData <- rbind(trainData, removed)

rm(datum, dailyEnergyAverage, dailyMeanTemps, energyUse, tempData, madridData, removed)

# -------------Exploratory Data Analysis ----------------------------------
#Here's a glimpse of the data set
trainData %>% as_tibble()

#In an effort to get a feel for the data I'm going to take a few basic statistics
tMaxStats <- trainData %>% summarize(measurements=n(), avg=mean(tMax), med=median(tMax),
                        stdev=sd(tMax), high=max(tMax), low=min(tMax))
tMinStats <- trainData %>% summarize(measurements=n(), avg=mean(tMin), med=median(tMin),
                                          stdev=sd(tMin), high=max(tMin), low=min(tMin))
eDailyStats <- trainData %>% summarize(measurements=n(), avg=mean(eDaily), med=median(eDaily),
                                          stdev=sd(eDaily), high=max(eDaily), low=min(eDaily))

tab <- rbind(tMaxStats, tMinStats, eDailyStats);
rownames(tab) <- c("Max Temp", "Min Temp", "Energy Use MWH")
kable(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 12,
                full_width = TRUE)

#The stats didn't provide much useful information to me so I'm going to plot some data
#plot raw data
temperatureDataPlot <-  trainData %>% gather(key,value, tMax, tMin) %>%
                    ggplot(aes(x=date,y=value, color=key,group=1)) + 
                    geom_line(linewidth=1) +
                    scale_x_discrete(labels=trainData$date[seq(1, dim(trainData)[1], by=30)],breaks=trainData$date[seq(1,dim(trainData)[1],by=30)]) +
                    labs(title = "Daily Min/Max Temperatures",
                            x = "Date",
                            y = "Temperature (F)"
                         ) +
                    theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

energyDataPlot <-  trainData %>% 
                    ggplot(aes(x=date,y=eDaily,  group=1)) + 
                    geom_line(linewidth=1) +
                    scale_x_discrete(labels=trainData$date[seq(1, dim(trainData)[1], by=30)],breaks=trainData$date[seq(1,dim(trainData)[1],by=30)]) +
                    labs(title = "Daily Energy Usage",
                         x = "Date",
                         y = "MWh (F)"
                    ) +
                    theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

grid.arrange(temperatureDataPlot, energyDataPlot, nrow = 2);

#The raw data doesn't give us a whole lot detail other than the daily temperature/energy use fluctuates throughout the
#year.
#Next we're going to plot the smoothed data to see if there's anything interesting there
temperatureDataPlotSmooth <-  trainData %>% 
                              ggplot(aes(x=date,group=1)) + 
                              geom_smooth(aes(y=tMax, color = "tMax"), method="loess", span=.3) +
                              geom_smooth(aes(y=tMin, color = "tMin"), method="loess", span=.3) +
                              scale_x_discrete(labels=trainData$date[seq(1, dim(trainData)[1], by=30)],breaks=trainData$date[seq(1,dim(trainData)[1],by=30)]) +
                              labs(title = "Daily Min/Max Temperatures",
                                   x = "Date",
                                   y = "Temperature (F)"
                              ) +
                              theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

energyDataPlotSmooth <-       trainData %>% 
                              ggplot(aes(x=date, group=1)) + 
                              geom_smooth(aes(y=eDaily, method="loess", span=.3)) + 
                              scale_x_discrete(labels=trainData$date[seq(1, dim(trainData)[1], by=30)],breaks=trainData$date[seq(1,dim(trainData)[1],by=30)]) +
                              labs(title = "Daily Energy Usage",
                                   x = "Date",
                                   y = "MWh (F)"
                              ) +
                              theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

grid.arrange(temperatureDataPlotSmooth, energyDataPlotSmooth, nrow = 2);

#The next plot is a combination of the one above to see the data energy and temperature data on the
#same time scale.
temperatureEnergyPlotSmooth <-  trainData %>% 
  ggplot(aes(x=date,group=1)) + 
  geom_smooth(aes(y=tMax, color = "tMax"), method="loess", span=.3) +
  geom_smooth(aes(y=tMin, color = "tMin"), method="loess", span=.3) +
  geom_smooth(aes(y=eDaily/50000, color = "eDaily"), method="loess", span=.3) +
  scale_y_continuous(
    name = "Temperature (F)",
    sec.axis = sec_axis(~.*50000, name="MWH")
  ) +
  labs(title = "Daily Temperatures and Energy Use",
       x = "Date",
       y = "Temperature (F)/Energy Use"
  ) +
  scale_x_discrete(labels=trainData$date[seq(1, dim(trainData)[1], by=30)],breaks=trainData$date[seq(1,dim(trainData)[1],by=30)]) +
  theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))
temperatureEnergyPlotSmooth

#This plot is the temperature vs. energy.  The data is smoothed to get rid of the high frequency variations.
temperaturevsEnergy <-  trainData %>% 
  ggplot(aes(x=eDaily,group=1)) + 
  geom_smooth(aes(y=tMax, color = "tMax"), method="loess", span=.3) +
  geom_smooth(aes(y=tMin, color = "tMin"), method="loess", span=.3) +
  labs(title = "Daily Min/Max Temperatures vs. Energy",
       x = "Energy Use (MWh)",
       y = "Temperature (F)"
  ) +
  theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))
temperaturevsEnergy

#Now we're going to order the data by energy use and the plot the smoothed max/min
#temperatures
orderedTrainData <- trainData %>% arrange(desc(eDaily));

temperaturevsEnergyOrdered <-  orderedTrainData %>% 
  ggplot(aes(x=eDaily, group = 1)) + 
  geom_smooth(aes(y=tMax, color = "tMax"), method="loess", span=.3) +
  geom_smooth(aes(y=tMin, color = "tMin"), method="loess", span=.3) + 
  labs(title = "Daily Min/Max Temperatures vs. Energy",
       x = "Energy Use (MWh)",
       y = "Temperature (F)"
  ) +
  theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))
temperaturevsEnergyOrdered

#Plot the correlation between the variables just to see if there's anything interesting
r1 <- round(cor(trainData$tMax, trainData$eDaily), 2)
p1 <- cor.test(trainData$tMax, trainData$eDaily)$p.value
tMaxCor <- ggplot(trainData, aes(y=tMax,x=eDaily)) + 
    geom_point() + 
    geom_smooth(method="lm", col="black") +
    annotate("text",x=4750000, y=90, label=paste0("r = ", r1), hjust=0) +
    annotate("text",x=4750000, y=84, label=paste0("p = ", round(p1,3)), hjust=0) + 
    labs(title = "Correlation (Daily Max Temp, Daily Energy Use)",
         x = "Energy Use (MWh)",
         y = "Daily High Temp (F)"
    ) +  
    theme_classic()

r2 <- round(cor(trainData$tMin, trainData$eDaily), 2)
p2 <- cor.test(trainData$tMin, trainData$eDaily)$p.value
tMinCor <- ggplot(trainData, aes(y=tMin,x=eDaily)) + 
    geom_point() + 
    geom_smooth(method="lm", col="black") +
    annotate("text", x=4750000, y=67, label=paste0("r = ", r2), hjust=0) +
    annotate("text", x=4750000, y=62, label=paste0("p = ", round(p2,3)), hjust=0) + 
    labs(title = "Correlation (Daily Min Temp, Daily Energy Use)",
         x = "Energy Use (MWh)",
         y = "Daily Low Temp (F)"
    ) +  
    theme_classic()

#So the actual correletion was not that interesting and actually gave a negative value which i wasn't expecting
#so i'm going to try the delta between the max and min  and see if that has any different values
delta <- trainData$tMax - trainData$tMin;
r3 <- round(cor(delta, trainData$eDaily),2)
p3 <- cor.test(delta, trainData$eDaily)$p.value
deltaCor <- ggplot(trainData, aes(y=delta, x=eDaily)) +
    geom_point() + 
    geom_smooth(method="lm", col="black") +
    annotate("text", x=4750000, y=42, label=paste0("r = ", r3), hjust=0) +
    annotate("text", x=4750000, y=39, label=paste0("p = ", round(p3,3)), hjust=0) +
    labs(title = "Correlation (Daily Delta Temp, Daily Energy Use)",
         x = "Energy Use (MWh)",
         y = "Daily Max-Min Temp (F)"
    ) +  
    theme_classic()

trainData$tAVE <- mean(trainData$tMax-trainData$tMin) + trainData$tMin;
r4 <- round(cor(trainData$tAVE, trainData$eDaily),2)
p4 <- cor.test(trainData$tAVE, trainData$eDaily)$p.value
tAveCor <- ggplot(trainData, aes(y=tAVE, x=eDaily)) +
  geom_point() + 
  geom_smooth(method="lm", col="black") +
  annotate("text", x=4750000, y=80, label=paste0("r = ", r4), hjust=0) +
  annotate("text", x=4750000, y=75, label=paste0("p = ", round(p4,3)), hjust=0) +
  labs(title = "Correlation (Daily Ave Temp, Daily Energy Use)",
       x = "Energy Use (MWh)",
       y = "Daily Max-Min Temp (F)"
  ) +  
  theme_classic()

grid.arrange(tMaxCor, tMinCor, nrow = 2); 
grid.arrange(tAveCor, deltaCor, nrow = 2);

#The delta didn't seem to help much either so I'm not going to bother with it on any other analysis


# Going to take the histograms of the data to see if there's anything useful
tMaxHist <- trainData %>%
  group_by(date) %>%
  ggplot(aes(tMax)) +
  geom_histogram(bins=20, fill = "blue") +
  labs(title = "TMax Distribution",
       x = "Temperature (F)",
       y = "Frequency"
  )+
  theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

tMinHist <- trainData %>%
  group_by(date) %>%
  ggplot(aes(tMin)) +
  geom_histogram(bins=20, fill = "blue") +
  labs(title = "TMin Distribution",
       x = "Temperature (F)",
       y = "Frequency"
  )+
  theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

eDailyHist <- trainData %>%
  group_by(date) %>%
  ggplot(aes(eDaily)) +
  geom_histogram(bins=20, fill = "blue") +
  labs(title = "Energy Usage Distribution",
       x = "MWh",
       y = "Frequency"
  )+
  theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))

grid.arrange(tMaxHist, tMinHist, eDailyHist)

# --------------- Analysis section ----------------------------------------
# Since I bounded the analysis early on to a couple of variables I'm going to try several
# different algorithms to see if there's much of a difference here.

#Simple linear model
lmFit <- lm(eDaily ~ tMax + tMin, data = trainData)
lmHat <- predict(lmFit, testData)
lmResults <- results(lmHat, testData$eDaily)
lmResults

#generalized linear model
glmFit <- train(eDaily ~ tMax + tMin, method = "glm", data = trainData);
glmHat <- predict(glmFit, testData)

#Random forest
rfFit <- train(eDaily ~ tMax + tMin, method = "rf", data = trainData);
rfHat <- predict(rfFit, testData)

#Regularized Random forest
rrfFit <- train(eDaily ~ tMax + tMin, method = "RRF", data = trainData);
rrfHat <- predict(rrfFit, testData)

#SVM with linear kernel
svmFit <- train(eDaily ~ tMax + tMin, method = "svmLinear", data = trainData);
svmHat <- predict(svmFit, testData)

#Least squares Support Vector Machine
svm3Fit <- train(eDaily ~ tMax + tMin, method = "svmLinear3", data = trainData);
svm3Hat <- predict(svm3Fit, testData)

#KNN
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
knnFit <- train(eDaily ~ tMax + tMin, method = "knn", data = trainData, tuneGrid = expand.grid(k=seq(3,101,2)))
#Going to try to find the value of k that minimizes there error
minIdx <- which.min(knnFit$results$RMSE)
#So after finding the minimum i realized that the fit has a bestTune variable that gives it without needed to
#do any other calculations.  So it goes.
knnHat <- predict(knnFit, testData, type = "raw")
neighborPlot <- ggplot(knnFit, highlight=TRUE)
neighborPlot
# Results section ---------------------------------------------------------
# Here's the results of the above algorithms but just from looking at the data
# it appears the limitation is not the algorithm but the limited data set I started
# with.
#Summarize the results
lmResults <- results(lmHat, testData$eDaily)
glmResults <- results(glmHat, testData$eDaily)
knnResults <- results(knnHat, testData$eDaily)
rfResults <- results(rfHat, testData$eDaily)
rrfResults <- results(rrfHat, testData$eDaily)
svmResults <- results(svmHat, testData$eDaily)
svm3Results <- results(svm3Hat, testData$eDaily)

generalResults <- as.data.frame(rbind(lmResults, glmResults, knnResults, rfResults, rrfResults, svmResults, svm3Results))

colnames(generalResults) <- c("Bias","RMSE","MSE","MAE","MAPE")
rownames(generalResults) <- c("Linear Model", "Generalized Linear Model", "KNN", 
                              "Random Forest", "Regularized Random Forest", "SVM", "Least Squares SVM")

# Put out a table of the results just to see where we are
kable(generalResults) %>% 
  kable_material(c("striped", "hover", "condensed", "responsive"),
                 position = "center",
                 font_size = 12)


#Create a table of the predicted and actual values
data <- data.frame(actual= testData$eDaily, predicted=knnHat)

#Plot the predicted vs actual values for a sanity check of the data
errorPlot <- data %>%
  ggplot(aes(x=actual,y=predicted)) +
  geom_point() +
  geom_abline() +
  labs(title = "Predicted vs. Actual Energy Use (MWh)",
       x = "Actual Values",
       y = "Predicted Values"
  )+
  theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5))
errorPlot




