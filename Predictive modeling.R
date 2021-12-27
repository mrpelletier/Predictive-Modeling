#Loading in packages and our file

library(ISLR)
library(knitr)
library(printr)
library(dplyr)
library(broom)
library(car)
library(MASS)
library(dvmisc)
library(leaps)
library(glmnet)
library(pls)
library(splines)
library(gam)
library(akima)

baseball <-  read.csv("C:\\Users\\18046\\Downloads\\MachineLearning1\\buad5122-m2-moneyball-training.csv")
summary(baseball) #initial look at our data

#Data Preparation/Cleaning

#Making sure our data is is correct form
baseball$INDEX <- as.factor(baseball$INDEX)
baseball$TARGET_WINS <- as.numeric(baseball$TARGET_WINS)
baseball$TEAM_BATTING_H <-as.numeric(baseball$TEAM_BATTING_H)
baseball$TEAM_BATTING_2B <- as.numeric(baseball$TEAM_BATTING_2B)
baseball$TEAM_BATTING_3B <- as.numeric(baseball$TEAM_BATTING_3B)
baseball$TEAM_BATTING_HR <- as.numeric(baseball$TEAM_BATTING_HR)
baseball$TEAM_BATTING_BB <- as.numeric(baseball$TEAM_BATTING_BB)
baseball$TEAM_BATTING_SO <- as.numeric(baseball$TEAM_BATTING_SO)
baseball$TEAM_BASERUN_SB <- as.numeric(baseball$TEAM_BASERUN_SB)
baseball$TEAM_BASERUN_CS <- as.numeric(baseball$TEAM_BASERUN_CS)
baseball$TEAM_BATTING_HBP <- as.numeric(baseball$TEAM_BATTING_HBP)
baseball$TEAM_PITCHING_H <- as.numeric(baseball$TEAM_PITCHING_H)
baseball$TEAM_PITCHING_HR <- as.numeric(baseball$TEAM_PITCHING_HR)
baseball$TEAM_PITCHING_BB <- as.numeric(baseball$TEAM_PITCHING_BB)
baseball$TEAM_PITCHING_SO <- as.numeric(baseball$TEAM_PITCHING_SO)
baseball$TEAM_FIELDING_E <- as.numeric(baseball$TEAM_FIELDING_E)
baseball$TEAM_FIELDING_DP <- as.numeric(baseball$TEAM_FIELDING_DP)

#Fixing NAs to means
baseball$TEAM_BATTING_SO.NA <- is.factor(ifelse(is.na(baseball$TEAM_BATTING_SO), 1, 0))
baseball$TEAM_BATTING_SO[is.na(baseball$TEAM_BATTING_SO)] = median(baseball$TEAM_BATTING_SO, na.rm = TRUE)

baseball$TEAM_BASERUN_SB.NA <- is.factor(ifelse(is.na(baseball$TEAM_BASERUN_SB), 1, 0))
baseball$TEAM_BASERUN_SB[is.na(baseball$TEAM_BASERUN_SB)] = median(baseball$TEAM_BASERUN_SB, na.rm = TRUE)

baseball$TEAM_BASERUN_CS.NA <- is.factor(ifelse(is.na(baseball$TEAM_BASERUN_CS), 1, 0))
baseball$TEAM_BASERUN_CS[is.na(baseball$TEAM_BASERUN_CS)] = median(baseball$TEAM_BASERUN_CS, na.rm = TRUE)

baseball$TEAM_BATTING_HBP.NA <- is.factor(ifelse(is.na(baseball$TEAM_BATTING_HBP), 1, 0))
baseball$TEAM_BATTING_HBP[is.na(baseball$TEAM_BATTING_HBP)] = median(baseball$TEAM_BATTING_HBP, na.rm = TRUE)

baseball$TEAM_PITCHING_SO.NA <- is.factor(ifelse(is.na(baseball$TEAM_PITCHING_SO), 1, 0))
baseball$TEAM_PITCHING_SO[is.na(baseball$TEAM_PITCHING_SO)] = median(baseball$TEAM_PITCHING_SO, na.rm = TRUE)

baseball$TEAM_FIELDING_DP.NA <- is.factor(ifelse(is.na(baseball$TEAM_FIELDING_DP), 1, 0))
baseball$TEAM_FIELDING_DP[is.na(baseball$TEAM_FIELDING_DP)] = median(baseball$TEAM_FIELDING_DP, na.rm = TRUE)

#Now that NAs are taken care of lets find our upper and lower bounds if needed to exclude outliers

#Target Wins 
hist(baseball$TARGET_WINS, col = "#09ADAD", xlab = "Doubles", main = "Histogram of Wins")
TARGET_WINSBP = boxplot(baseball$TARGET_WINS, col = "#09ADAD", main = "Boxplot of Wins")

lbW = 21 #lowest ever wins and highest ever in single szn
ubW = 123

baseball$TARGET_WINS.Flag <- as.factor(ifelse(baseball$TARGET_WINS < lbW, 0, 1)) 
baseball$TARGET_WINS <- as.numeric(ifelse(baseball$TARGET_WINS.Flag == 0, lbW, baseball$TARGET_WINS))

baseball$TARGET_WINS.max <- as.factor(ifelse(baseball$TARGET_WINS > ubW, 0, 1))
baseball$TARGET_WINS <- as.numeric(ifelse(baseball$TARGET_WINS.max == 0, ubW, baseball$TARGET_WINS))

# Hits and Doubles

par(mfrow=c(2,2))
hist(baseball$TEAM_BATTING_H, col = "#A71930", xlab = "Team_Batting_H", main = "Histogram of Hits")
hist(baseball$TEAM_BATTING_2B, col = "#09ADAD", xlab = "Doubles", main = "Histogram of Doubles")
BATTING_HBP = boxplot(baseball$TEAM_BATTING_H, col = "#A71930", main = "Boxplot of Hits")
BATTING_2BBP = boxplot(baseball$TEAM_BATTING_2B, col = "#09ADAD", main = "Boxplot of Doubles")
par(mfrow=c(1,1))

lbHits = BATTING_HBP$stats[1,]
ubHits = BATTING_HBP$stats[5,]

lb2B = BATTING_2BBP$stats[1,]
ub2B = BATTING_2BBP$stats[5,]
#hits
baseball$TEAM_BATTING_H.Flag <- as.factor(ifelse(baseball$TEAM_BATTING_H < lbHits, 0, 1)) 
baseball$TEAM_BATTING_H <- as.numeric(ifelse(baseball$TEAM_BATTING_H.Flag == 0, lbHits, baseball$TEAM_BATTING_H))

baseball$TEAM_BATTING_H.max <- as.factor(ifelse(baseball$TEAM_BATTING_H > ubHits, 0, 1))
baseball$TEAM_BATTING_H <- as.numeric(ifelse(baseball$TEAM_BATTING_H.max == 0, ubHits, baseball$TEAM_BATTING_H))
#2B
baseball$TEAM_BATTING_2B.Flag <- as.factor(ifelse(baseball$TEAM_BATTING_2B < lb2B, 0, 1)) 
baseball$TEAM_BATTING_2B <- as.numeric(ifelse(baseball$TEAM_BATTING_2B.Flag == 0, lb2B, baseball$TEAM_BATTING_2B))

baseball$TEAM_BATTING_2B.max <- as.factor(ifelse(baseball$TEAM_BATTING_2B > ub2B, 0, 1))
baseball$TEAM_BATTING_2B <- as.numeric(ifelse(baseball$TEAM_BATTING_2B.max == 0, ub2B, baseball$TEAM_BATTING_2B))
summary(baseball$TEAM_BATTING_2B)

# Triples and Home Runs

par(mfrow=c(2,2))
hist(baseball$TEAM_BATTING_3B, col = "#A71930", xlab = "Triples", main = "Histogram of Triples")
hist(baseball$TEAM_BATTING_HR, col = "#DBCEAC", xlab = "Home Runs", main = "Histogram of Home Runs")
BATTING_3BBP = boxplot(baseball$TEAM_BATTING_3B, col = "#A71930", main = "Boxplot of Triples")
TEAM_BATTING_HRBP = boxplot(baseball$TEAM_BATTING_HR, col = "#DBCEAC", main = "Boxplot of Home Runs")
par(mfrow=c(1,1))

lb3B = BATTING_3BBP$stats[1,]
ub3B = BATTING_3BBP$stats[5,]
#3B
baseball$TEAM_BATTING_3B.Flag <- as.factor(ifelse(baseball$TEAM_BATTING_3B < lb3B, 0, 1)) 
baseball$TEAM_BATTING_3B <- as.numeric(ifelse(baseball$TEAM_BATTING_3B.Flag == 0, lb3B, baseball$TEAM_BATTING_3B))

baseball$TEAM_BATTING_3B.max <- as.factor(ifelse(baseball$TEAM_BATTING_3B > ub3B, 0, 1))
baseball$TEAM_BATTING_3B <- as.numeric(ifelse(baseball$TEAM_BATTING_3B.max == 0, ub3B, baseball$TEAM_BATTING_3B))
summary(baseball$TEAM_BATTING_3B)

# Walks, Strikeouts, HBP
par(mfrow=c(2,3))
hist(baseball$TEAM_BATTING_BB, col = "#A71930", xlab = "Walks", main = "Histogram of Walks")
hist(baseball$TEAM_BATTING_SO, col = "#09ADAD", xlab = "Strikeouts", main = "Histogram of Strikeouts")
hist(baseball$TEAM_BATTING_HBP, col = "#DBCEAC", xlab = "Hit By Pitches", main = "Histogram of HBP")
BATTING_BBBP = boxplot(baseball$TEAM_BATTING_BB, col = "#A71930", main = "Boxplot of Walks")
BATTING_SOBP = boxplot(baseball$TEAM_BATTING_SO, col = "#09ADAD", main = "Boxplot of Strikeouts")
boxplot(baseball$TEAM_BATTING_HBP, col = "#DBCEAC", main = "Boxplot of HBP")
par(mfrow=c(1,1))

lbBB = BATTING_BBBP$stats[1,]
ubBB = BATTING_BBBP$stats[5,]

lbSO = BATTING_SOBP$stats[1,]
ubSO = BATTING_SOBP$stats[5,]

#BB
baseball$TEAM_BATTING_BB.Flag <- as.factor(ifelse(baseball$TEAM_BATTING_BB < lbBB, 0, 1)) 
baseball$TEAM_BATTING_BB <- as.numeric(ifelse(baseball$TEAM_BATTING_BB.Flag == 0, lbBB, baseball$TEAM_BATTING_BB))

baseball$TEAM_BATTING_BB.max <- as.factor(ifelse(baseball$TEAM_BATTING_BB > ubBB, 0, 1))
baseball$TEAM_BATTING_BB <- as.numeric(ifelse(baseball$TEAM_BATTING_BB.max == 0, ubBB, baseball$TEAM_BATTING_BB))
#SO
baseball$TEAM_BATTING_SO.Flag <- as.factor(ifelse(baseball$TEAM_BATTING_SO < lbSO, 0, 1)) 
baseball$TEAM_BATTING_SO <- as.numeric(ifelse(baseball$TEAM_BATTING_SO.Flag == 0, lbSO, baseball$TEAM_BATTING_SO))

baseball$TEAM_BATTING_SO.max <- as.factor(ifelse(baseball$TEAM_BATTING_SO > ubSO, 0, 1))
baseball$TEAM_BATTING_SO <- as.numeric(ifelse(baseball$TEAM_BATTING_SO.max == 0, ubSO, baseball$TEAM_BATTING_SO))

# Stolen Bases and Caught Stealing
par(mfrow=c(2,2))
hist(baseball$TEAM_BASERUN_SB, col = "#A71930", xlab = "Stolen Bases", main = "Histogram of Steals")
hist(baseball$TEAM_BASERUN_CS, col = "#DBCEAC", xlab = "Caught Stealing", main = "Histogram of CS")
BASERUN_SBBP = boxplot(baseball$TEAM_BASERUN_SB, col = "#A71930", main = "Boxplot of Steals")
BASERUN_CSBP = boxplot(baseball$TEAM_BASERUN_CS, col = "#DBCEAC", main = "Boxplot of CS")
par(mfrow=c(1,1))

lbSB = BASERUN_SBBP$stats[1,]
ubSB = BASERUN_SBBP$stats[5,]

lbCS = BASERUN_CSBP$stats[1,]
ubCS = BASERUN_CSBP$stats[5,]

#SB
baseball$TEAM_BASERUN_SB.Flag <- as.factor(ifelse(baseball$TEAM_BASERUN_SB < lbSB, 0, 1)) 
baseball$TEAM_BASERUN_SB <- as.numeric(ifelse(baseball$TEAM_BASERUN_SB.Flag == 0, lbSB, baseball$TEAM_BASERUN_SB))

baseball$TEAM_BASERUN_SB.max <- as.factor(ifelse(baseball$TEAM_BASERUN_SB > ubSB, 0, 1))
baseball$TEAM_BASERUN_SB <- as.numeric(ifelse(baseball$TEAM_BASERUN_SB.max == 0, ubSB, baseball$TEAM_BASERUN_SB))

#CS
baseball$TEAM_BASERUN_CS.Flag <- as.factor(ifelse(baseball$TEAM_BASERUN_CS < lbCS, 0, 1)) 
baseball$TEAM_BASERUN_CS <- as.numeric(ifelse(baseball$TEAM_BASERUN_CS.Flag == 0, lbCS, baseball$TEAM_BASERUN_CS))

baseball$TEAM_BASERUN_CS.max <- as.factor(ifelse(baseball$TEAM_BASERUN_CS > ubCS, 0, 1))
baseball$TEAM_BASERUN_CS <- as.numeric(ifelse(baseball$TEAM_BASERUN_CS.max == 0, ubCS, baseball$TEAM_BASERUN_CS))

#Pitching hits and HR allowed
par(mfrow=c(2,2))
hist(baseball$TEAM_PITCHING_H, col = "#A71930", xlab = "Hits Against", main = "Histogram of Hits Against")
hist(baseball$TEAM_PITCHING_HR, col = "#09ADAD", xlab = "Home Runs Against", main = "Histograms of HR Against")
PITCHING_HBP = boxplot(baseball$TEAM_PITCHING_H, col = "#A71930", main = "Boxplot of Hits Against")
PITCHING_HRBP = boxplot(baseball$TEAM_PITCHING_HR, col = "#09ADAD", main = "Boxplot of HR Against")
par(mfrow=c(1,1))

lbPH = PITCHING_HBP$stats[1,]
ubPH = PITCHING_HBP$stats[5,]

lbPHR = PITCHING_HRBP$stats[1,]
ubPHR = PITCHING_HRBP$stats[5,]
#H
baseball$TEAM_PITCHING_H.Flag <- as.factor(ifelse(baseball$TEAM_PITCHING_H < lbPH, 0, 1)) 
baseball$TEAM_PITCHING_H <- as.numeric(ifelse(baseball$TEAM_PITCHING_H.Flag == 0, lbPH, baseball$TEAM_PITCHING_H))

baseball$TEAM_PITCHING_H.max <- as.factor(ifelse(baseball$TEAM_PITCHING_H > ubPH, 0, 1))
baseball$TEAM_PITCHING_H <- as.numeric(ifelse(baseball$TEAM_PITCHING_H.max == 0, ubPH, baseball$TEAM_PITCHING_H))
#HR
baseball$TEAM_PITCHING_HR.Flag <- as.factor(ifelse(baseball$TEAM_PITCHING_HR < lbPHR, 0, 1)) 
baseball$TEAM_PITCHING_HR <- as.numeric(ifelse(baseball$TEAM_PITCHING_HR.Flag == 0, lbPHR, baseball$TEAM_PITCHING_HR))

baseball$TEAM_PITCHING_HR.max <- as.factor(ifelse(baseball$TEAM_PITCHING_HR > ubPHR, 0, 1))
baseball$TEAM_PITCHING_HR <- as.numeric(ifelse(baseball$TEAM_PITCHING_HR.max == 0, ubPHR, baseball$TEAM_PITCHING_HR))

# Walks and Strikeouts Pitched
par(mfrow=c(2,2))
hist(baseball$TEAM_PITCHING_BB, col = "#A71930", xlab = "Walks Allowed", main = "Histogram of Walks Allowed")
hist(baseball$TEAM_PITCHING_SO, col = "#DBCEAC", xlab = "Strikeouts", main = "Histograms of Strikeouts")
PITCHING_BBBP = boxplot(baseball$TEAM_PITCHING_BB, col = "#A71930", main = "Boxplot of Walks Allowed")
PITCHING_SOBP = boxplot(baseball$TEAM_PITCHING_SO, col = "#DBCEAC", main = "Boxplot of Strikeouts")
par(mfrow=c(1,1))

lbPBB = PITCHING_BBBP$stats[1,]
ubPBB = PITCHING_BBBP$stats[5,]

lbPSO = PITCHING_SOBP$stats[1,]
ubPSO = PITCHING_SOBP$stats[5,]

baseball$TEAM_PITCHING_BB.Flag <- as.factor(ifelse(baseball$TEAM_PITCHING_BB < lbPBB, 0, 1)) 
baseball$TEAM_PITCHING_BB <- as.numeric(ifelse(baseball$TEAM_PITCHING_BB.Flag == 0, lbPBB, baseball$TEAM_PITCHING_BB))

baseball$TEAM_PITCHING_BB.max <- as.factor(ifelse(baseball$TEAM_PITCHING_BB > ubPBB, 0, 1))
baseball$TEAM_PITCHING_BB <- as.numeric(ifelse(baseball$TEAM_PITCHING_BB.max == 0, ubPBB, baseball$TEAM_PITCHING_BB))

baseball$TEAM_PITCHING_SO.Flag <- as.factor(ifelse(baseball$TEAM_PITCHING_SO < lbPSO, 0, 1)) 
baseball$TEAM_PITCHING_SO <- as.numeric(ifelse(baseball$TEAM_PITCHING_SO.Flag == 0, lbPSO, baseball$TEAM_PITCHING_SO))

baseball$TEAM_PITCHING_SO.max <- as.factor(ifelse(baseball$TEAM_PITCHING_SO > ubPSO, 0, 1))
baseball$TEAM_PITCHING_SO <- as.numeric(ifelse(baseball$TEAM_PITCHING_SO.max == 0, ubPSO, baseball$TEAM_PITCHING_SO))

# Double Plays and Errors 
par(mfrow=c(2,2))
hist(baseball$TEAM_FIELDING_DP, col = "#A71930", xlab = "Double Plays", main = "Histogram of Double Plays")
hist(baseball$TEAM_FIELDING_E, col = "#09ADAD", xlab = "Errors Committed", main = "Histogram of Errors Committed")
FIELDING_DPBP = boxplot(baseball$TEAM_FIELDING_DP, col = "#A71930", main = "Boxplot of Double Plays")
FIELDING_EBP = boxplot(baseball$TEAM_FIELDING_E, col = "#09ADAD", main = "Boxplot of Errors Committed")
par(mfrow=c(1,1))

lbDP = FIELDING_DPBP$stats[1,]
ubDP = FIELDING_DPBP$stats[5,]

lbEB = FIELDING_EBP$stats[1,]
ubEB = FIELDING_EBP$stats[5,]

baseball$TEAM_FIELDING_DP.Flag <- as.factor(ifelse(baseball$TEAM_FIELDING_DP < lbDP, 0, 1)) 
baseball$TEAM_FIELDING_DP <- as.numeric(ifelse(baseball$TEAM_FIELDING_DP.Flag == 0, lbDP, baseball$TEAM_FIELDING_DP))

baseball$TEAM_FIELDING_DP.max <- as.factor(ifelse(baseball$TEAM_FIELDING_DP > ubDP, 0, 1))
baseball$TEAM_FIELDING_DP <- as.numeric(ifelse(baseball$TEAM_FIELDING_DP.max == 0, ubDP, baseball$TEAM_FIELDING_DP))

baseball$TEAM_FIELDING_E.Flag <- as.factor(ifelse(baseball$TEAM_FIELDING_E < lbEB, 0, 1)) 
baseball$TEAM_FIELDING_E <- as.numeric(ifelse(baseball$TEAM_FIELDING_E.Flag == 0, lbEB, baseball$TEAM_FIELDING_E))

baseball$TEAM_FIELDING_E.max <- as.factor(ifelse(baseball$TEAM_FIELDING_E > ubEB, 0, 1))
baseball$TEAM_FIELDING_E <- as.numeric(ifelse(baseball$TEAM_FIELDING_E.max == 0, ubEB, baseball$TEAM_FIELDING_E))

#Now that our data is prepped lets create our models

#splitting data
baseball[, c("TEAM_BATTING_3B.Flag","TEAM_BATTING_SO.max","TEAM_BASERUN_SB.Flag","TEAM_PITCHING_H.Flag","TEAM_PITCHING_HR.Flag","TEAM_FIELDING_E.Flag")] = list(NULL) #getting rid of 1 factor data

library(caret)
set.seed(22)
index <- createDataPartition(baseball$TARGET_WINS, p=.8, list = FALSE, times =1)
train_baseball = baseball[index,]
test_baseball = baseball[-index,]

#Ridge
library(glmnet)
lam.array <- seq(from = 0.01, to = 100, by = 0.01)
x= as.matrix(train_baseball[,3:17])
y = train_baseball[,2]
xtest = as.matrix(test_baseball[,3:17])
  
ridge <- glmnet(x,y, alpha = 0, lamdba = lam.array)
ridgemodel = predict(ridge, s = min(lam.array), newx= xtest)
mean(ridgemodel) #not bad

ridgemodelcoef = predict(ridge, s = min(lam.array), newx= xtest, type = "coefficients")
#Lasso
set.seed(22)
ctrl <- trainControl(method = "cv", number = 10, savePredictions = "all")
lambda_vec = 10^seq(5,-5, length=100)
lasso = train(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_3B + TEAM_BATTING_BB +
                TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_H + TEAM_PITCHING_BB +
                TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP, data =train_baseball, preProcess=c("center","scale"), method = "glmnet", tuneGrid=expand.grid(alpha=1, lambda=lambda_vec), trControl=ctrl)

coef(lasso$finalModel, lasso$bestTune$lambda)
lassomodel = 80.8232711   +  5.2048272*train_baseball$TEAM_BATTING_H  +    3.0515823*train_baseball$TEAM_BATTING_3B   +  3.7151742*train_baseball$TEAM_BATTING_BB  - 0.2395115*train_baseball$TEAM_BATTING_SO   + 2.8980360*train_baseball$TEAM_BASERUN_SB   - 1.2241661*train_baseball$TEAM_BASERUN_CS + 1.7880880*train_baseball$TEAM_PITCHING_H -    1.3990783*train_baseball$TEAM_PITCHING_BB   - 6.9858771*train_baseball$TEAM_FIELDING_E  - 2.5526362*train_baseball$TEAM_FIELDING_DP 
mean(lassomodel)#not even close


#Bootstrap 
set.seed(22)
boot =function(z,index)
  +return(coef(lm(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_3B + TEAM_BATTING_BB +
                    TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_H + TEAM_PITCHING_BB +
                    TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP, data = train_baseball, subset=index)))
boot(train_baseball,sample(1593,1593,replace=TRUE))

bootmodel = 30.054214578   +  0.026401394*train_baseball$TEAM_BATTING_H  +    0.180267354*train_baseball$TEAM_BATTING_3B   +  0.041762796*train_baseball$TEAM_BATTING_BB  - 0.001175119*train_baseball$TEAM_BATTING_SO   + 0.047636338*train_baseball$TEAM_BASERUN_SB   - 0.124595415*train_baseball$TEAM_BASERUN_CS + 0.014395547*train_baseball$TEAM_PITCHING_H -    0.013827847*train_baseball$TEAM_PITCHING_BB    +  0.001051585*train_baseball$TEAM_PITCHING_SO -0.080413742*train_baseball$TEAM_FIELDING_E  - 0.114168748*train_baseball$TEAM_FIELDING_DP 
mean(bootmodel) #not bad

#Ridge and Bootstrap models both performed well, I'll go with the ridge model.
#Loading Test data
tbaseball <-  read.csv("C:\\Users\\18046\\Downloads\\MachineLearning1\\buad5122-m2-moneyball-test.csv")

#Data Preparation/Cleaning

#Making sure our data is is correct form
tbaseball$INDEX <- as.factor(tbaseball$INDEX)
tbaseball$TEAM_BATTING_H <-as.numeric(tbaseball$TEAM_BATTING_H)
tbaseball$TEAM_BATTING_2B <- as.numeric(tbaseball$TEAM_BATTING_2B)
tbaseball$TEAM_BATTING_3B <- as.numeric(tbaseball$TEAM_BATTING_3B)
tbaseball$TEAM_BATTING_HR <- as.numeric(tbaseball$TEAM_BATTING_HR)
tbaseball$TEAM_BATTING_BB <- as.numeric(tbaseball$TEAM_BATTING_BB)
tbaseball$TEAM_BATTING_SO <- as.numeric(tbaseball$TEAM_BATTING_SO)
tbaseball$TEAM_BASERUN_SB <- as.numeric(tbaseball$TEAM_BASERUN_SB)
tbaseball$TEAM_BASERUN_CS <- as.numeric(tbaseball$TEAM_BASERUN_CS)
tbaseball$TEAM_BATTING_HBP <- as.numeric(tbaseball$TEAM_BATTING_HBP)
tbaseball$TEAM_PITCHING_H <- as.numeric(tbaseball$TEAM_PITCHING_H)
tbaseball$TEAM_PITCHING_HR <- as.numeric(tbaseball$TEAM_PITCHING_HR)
tbaseball$TEAM_PITCHING_BB <- as.numeric(tbaseball$TEAM_PITCHING_BB)
tbaseball$TEAM_PITCHING_SO <- as.numeric(tbaseball$TEAM_PITCHING_SO)
tbaseball$TEAM_FIELDING_E <- as.numeric(tbaseball$TEAM_FIELDING_E)
tbaseball$TEAM_FIELDING_DP <- as.numeric(tbaseball$TEAM_FIELDING_DP)

#Fixing NAs to means
tbaseball$TEAM_BATTING_SO.NA <- is.factor(ifelse(is.na(tbaseball$TEAM_BATTING_SO), 1, 0))
tbaseball$TEAM_BATTING_SO[is.na(tbaseball$TEAM_BATTING_SO)] = median(baseball$TEAM_BATTING_SO, na.rm = TRUE)

tbaseball$TEAM_BASERUN_SB.NA <- is.factor(ifelse(is.na(tbaseball$TEAM_BASERUN_SB), 1, 0))
tbaseball$TEAM_BASERUN_SB[is.na(tbaseball$TEAM_BASERUN_SB)] = median(baseball$TEAM_BASERUN_SB, na.rm = TRUE)

tbaseball$TEAM_BASERUN_CS.NA <- is.factor(ifelse(is.na(tbaseball$TEAM_BASERUN_CS), 1, 0))
tbaseball$TEAM_BASERUN_CS[is.na(tbaseball$TEAM_BASERUN_CS)] = median(baseball$TEAM_BASERUN_CS, na.rm = TRUE)

tbaseball$TEAM_BATTING_HBP.NA <- is.factor(ifelse(is.na(tbaseball$TEAM_BATTING_HBP), 1, 0))
tbaseball$TEAM_BATTING_HBP[is.na(tbaseball$TEAM_BATTING_HBP)] = median(baseball$TEAM_BATTING_HBP, na.rm = TRUE)

tbaseball$TEAM_PITCHING_SO.NA <- is.factor(ifelse(is.na(tbaseball$TEAM_PITCHING_SO), 1, 0))
tbaseball$TEAM_PITCHING_SO[is.na(tbaseball$TEAM_PITCHING_SO)] = median(baseball$TEAM_PITCHING_SO, na.rm = TRUE)

tbaseball$TEAM_FIELDING_DP.NA <- is.factor(ifelse(is.na(tbaseball$TEAM_FIELDING_DP), 1, 0))
tbaseball$TEAM_FIELDING_DP[is.na(tbaseball$TEAM_FIELDING_DP)] = median(baseball$TEAM_FIELDING_DP, na.rm = TRUE)

#adjusting outliers used in formula
tbaseball$TARGET_WINS.Flag <- as.factor(ifelse(tbaseball$TARGET_WINS < lbW, 0, 1)) 
tbaseball$TARGET_WINS <- as.numeric(ifelse(tbaseball$TARGET_WINS.Flag == 0, lbW, tbaseball$TARGET_WINS))

tbaseball$TARGET_WINS.max <- as.factor(ifelse(tbaseball$TARGET_WINS > ubW, 0, 1))
tbaseball$TARGET_WINS <- as.numeric(ifelse(tbaseball$TARGET_WINS.max == 0, ubW, tbaseball$TARGET_WINS))

#hits
tbaseball$TEAM_BATTING_H.Flag <- as.factor(ifelse(tbaseball$TEAM_BATTING_H < lbHits, 0, 1)) 
tbaseball$TEAM_BATTING_H <- as.numeric(ifelse(tbaseball$TEAM_BATTING_H.Flag == 0, lbHits, tbaseball$TEAM_BATTING_H))

tbaseball$TEAM_BATTING_H.max <- as.factor(ifelse(tbaseball$TEAM_BATTING_H > ubHits, 0, 1))
tbaseball$TEAM_BATTING_H <- as.numeric(ifelse(tbaseball$TEAM_BATTING_H.max == 0, ubHits, tbaseball$TEAM_BATTING_H))

tbaseball$TEAM_BATTING_3B.Flag <- as.factor(ifelse(tbaseball$TEAM_BATTING_3B < lb3B, 0, 1)) 
tbaseball$TEAM_BATTING_3B <- as.numeric(ifelse(tbaseball$TEAM_BATTING_3B.Flag == 0, lb3B, tbaseball$TEAM_BATTING_3B))

tbaseball$TEAM_BATTING_3B.max <- as.factor(ifelse(tbaseball$TEAM_BATTING_3B > ub3B, 0, 1))
tbaseball$TEAM_BATTING_3B <- as.numeric(ifelse(tbaseball$TEAM_BATTING_3B.max == 0, ub3B, tbaseball$TEAM_BATTING_3B))

#BB
tbaseball$TEAM_BATTING_BB.Flag <- as.factor(ifelse(tbaseball$TEAM_BATTING_BB < lbBB, 0, 1)) 
tbaseball$TEAM_BATTING_BB <- as.numeric(ifelse(tbaseball$TEAM_BATTING_BB.Flag == 0, lbBB, tbaseball$TEAM_BATTING_BB))

tbaseball$TEAM_BATTING_BB.max <- as.factor(ifelse(tbaseball$TEAM_BATTING_BB > ubBB, 0, 1))
tbaseball$TEAM_BATTING_BB <- as.numeric(ifelse(tbaseball$TEAM_BATTING_BB.max == 0, ubBB, tbaseball$TEAM_BATTING_BB))
#SO
tbaseball$TEAM_BATTING_SO.Flag <- as.factor(ifelse(baseball$TEAM_BATTING_SO < lbSO, 0, 1)) 
tbaseball$TEAM_BATTING_SO <- as.numeric(ifelse(baseball$TEAM_BATTING_SO.Flag == 0, lbSO, baseball$TEAM_BATTING_SO))

tbaseball$TEAM_BATTING_SO.max <- as.factor(ifelse(tbaseball$TEAM_BATTING_SO > ubSO, 0, 1))
tbaseball$TEAM_BATTING_SO <- as.numeric(ifelse(tbaseball$TEAM_BATTING_SO.max == 0, ubSO, tbaseball$TEAM_BATTING_SO))

#SB
tbaseball$TEAM_BASERUN_SB.Flag <- as.factor(ifelse(tbaseball$TEAM_BASERUN_SB < lbSB, 0, 1)) 
tbaseball$TEAM_BASERUN_SB <- as.numeric(ifelse(tbaseball$TEAM_BASERUN_SB.Flag == 0, lbSB, tbaseball$TEAM_BASERUN_SB))

tbaseball$TEAM_BASERUN_SB.max <- as.factor(ifelse(tbaseball$TEAM_BASERUN_SB > ubSB, 0, 1))
tbaseball$TEAM_BASERUN_SB <- as.numeric(ifelse(tbaseball$TEAM_BASERUN_SB.max == 0, ubSB, tbaseball$TEAM_BASERUN_SB))

#CS
tbaseball$TEAM_BASERUN_CS.Flag <- as.factor(ifelse(tbaseball$TEAM_BASERUN_CS < lbCS, 0, 1)) 
tbaseball$TEAM_BASERUN_CS <- as.numeric(ifelse(tbaseball$TEAM_BASERUN_CS.Flag == 0, lbCS, tbaseball$TEAM_BASERUN_CS))

tbaseball$TEAM_BASERUN_CS.max <- as.factor(ifelse(tbaseball$TEAM_BASERUN_CS > ubCS, 0, 1))
tbaseball$TEAM_BASERUN_CS <- as.numeric(ifelse(tbaseball$TEAM_BASERUN_CS.max == 0, ubCS, tbaseball$TEAM_BASERUN_CS))

#H
tbaseball$TEAM_PITCHING_H.Flag <- as.factor(ifelse(tbaseball$TEAM_PITCHING_H < lbPH, 0, 1)) 
tbaseball$TEAM_PITCHING_H <- as.numeric(ifelse(tbaseball$TEAM_PITCHING_H.Flag == 0, lbPH, tbaseball$TEAM_PITCHING_H))

tbaseball$TEAM_PITCHING_H.max <- as.factor(ifelse(tbaseball$TEAM_PITCHING_H > ubPH, 0, 1))
tbaseball$TEAM_PITCHING_H <- as.numeric(ifelse(tbaseball$TEAM_PITCHING_H.max == 0, ubPH, tbaseball$TEAM_PITCHING_H))

tbaseball$TEAM_PITCHING_BB.Flag <- as.factor(ifelse(tbaseball$TEAM_PITCHING_BB < lbPBB, 0, 1)) 
tbaseball$TEAM_PITCHING_BB <- as.numeric(ifelse(tbaseball$TEAM_PITCHING_BB.Flag == 0, lbPBB, tbaseball$TEAM_PITCHING_BB))

tbaseball$TEAM_PITCHING_BB.max <- as.factor(ifelse(tbaseball$TEAM_PITCHING_BB > ubPBB, 0, 1))
tbaseball$TEAM_PITCHING_BB <- as.numeric(ifelse(tbaseball$TEAM_PITCHING_BB.max == 0, ubPBB, tbaseball$TEAM_PITCHING_BB))

tbaseball$TEAM_PITCHING_SO.Flag <- as.factor(ifelse(tbaseball$TEAM_PITCHING_SO < lbPSO, 0, 1)) 
tbaseball$TEAM_PITCHING_SO <- as.numeric(ifelse(tbaseball$TEAM_PITCHING_SO.Flag == 0, lbPSO, tbaseball$TEAM_PITCHING_SO))

tbaseball$TEAM_PITCHING_SO.max <- as.factor(ifelse(tbaseball$TEAM_PITCHING_SO > ubPSO, 0, 1))
tbaseball$TEAM_PITCHING_SO <- as.numeric(ifelse(tbaseball$TEAM_PITCHING_SO.max == 0, ubPSO, tbaseball$TEAM_PITCHING_SO))

tbaseball$TEAM_FIELDING_DP.Flag <- as.factor(ifelse(tbaseball$TEAM_FIELDING_DP < lbDP, 0, 1)) 
tbaseball$TEAM_FIELDING_DP <- as.numeric(ifelse(tbaseball$TEAM_FIELDING_DP.Flag == 0, lbDP, tbaseball$TEAM_FIELDING_DP))

tbaseball$TEAM_FIELDING_DP.max <- as.factor(ifelse(tbaseball$TEAM_FIELDING_DP > ubDP, 0, 1))
tbaseball$TEAM_FIELDING_DP <- as.numeric(ifelse(tbaseball$TEAM_FIELDING_DP.max == 0, ubDP, tbaseball$TEAM_FIELDING_DP))

tbaseball$TEAM_FIELDING_E.Flag <- as.factor(ifelse(tbaseball$TEAM_FIELDING_E < lbEB, 0, 1)) 
tbaseball$TEAM_FIELDING_E <- as.numeric(ifelse(tbaseball$TEAM_FIELDING_E.Flag == 0, lbEB, tbaseball$TEAM_FIELDING_E))

tbaseball$TEAM_FIELDING_E.max <- as.factor(ifelse(tbaseball$TEAM_FIELDING_E > ubEB, 0, 1))
tbaseball$TEAM_FIELDING_E <- as.numeric(ifelse(tbaseball$TEAM_FIELDING_E.max == 0, ubEB, tbaseball$TEAM_FIELDING_E))
summary(tbaseball)

#Champion Model
tbaseball$wins_predictions <- 30.377186082 + tbaseball$TEAM_BATTING_H*0.035036235 - tbaseball$TEAM_BATTING_2B*0.004508004 + tbaseball$TEAM_BATTING_3B*0.128012816 + tbaseball$TEAM_BATTING_HR*0.019957597 + tbaseball$TEAM_BATTING_BB*0.028375773 - tbaseball$TEAM_BATTING_SO*0.004811718 + tbaseball$TEAM_BASERUN_SB*0.040842698 - tbaseball$TEAM_BASERUN_CS*0.063479156 + tbaseball$TEAM_BATTING_HBP*0.039104054 + tbaseball$TEAM_PITCHING_H*0.004768459 + tbaseball$TEAM_PITCHING_HR*0.036489633 - tbaseball$TEAM_PITCHING_BB*0.010193046 - tbaseball$TEAM_PITCHING_SO*0.002454098 - tbaseball$TEAM_FIELDING_E*0.048464596 - tbaseball$TEAM_FIELDING_DP*0.119285368
mean(tbaseball$wins_predictions)

#Sending to csv
results = tbaseball[c("INDEX","wins_predictions")]
write.csv(results, file = "C:\\Users\\18046\\Downloads\\MachineLearning1\\m4results.csv")
