## ref: https://stackoverflow.com/questions/5758084/loop-in-r-to-read-many-files
fi <- list.files('../data/',full.names=T)
dat <- lapply(fi,read.csv)

## ref: https://stackoverflow.com/questions/2851327/combine-a-list-of-data-frames-into-one-data-frame-by-row
library(dplyr)
players <- bind_rows(dat, .id = "column_label")
players$WL <- factor(players$WL)
players$WhiteWL <- factor(players$WhiteWL)
players <- players[,-c(1,2,11)]


## unscaled lin reg ##
set.seed(123)
library(caret)
library(car)

## mean_cp
training.samples <- players$Mean_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- players[training.samples, ]
test.data <- players[-training.samples, ]

players.lm1 <- lm(Mean_CP~.,data=train.data)
summary(players.lm1) # Age, Time, Std_CP, Elo, WL, WhiteWL significant
vif(players.lm1) #age, time, WL, WhiteWL need to be removed

players.lm2 <- lm(Mean_CP~Std_CP + Elo,data=train.data)
summary(players.lm2) # Std_CP, Elo significant
vif(players.lm2) #looks good now

players.lm3 <- lm(Mean_CP~(Std_CP + Elo)^2,
                  data=train.data)
summary(players.lm3) ## no interactions are significant

predictions1 <- players.lm2 %>% predict(test.data)
p1=data.frame(
        RMSE = RMSE(predictions1, test.data$Mean_CP),
        R2 = R2(predictions1, test.data$Mean_CP)
)

predictions2 <- players.lm3 %>% predict(test.data)
p2=data.frame(
        RMSE = RMSE(predictions2, test.data$Mean_CP),
        R2 = R2(predictions2, test.data$Mean_CP)
)

p1 ##best model (though, marginally)
p2

AIC(players.lm2,players.lm3)


## std_cp
training.samples <- players$Std_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- players[training.samples, ]
test.data <- players[-training.samples, ]

players.lm4 <- lm(Std_CP~.,data=train.data)
summary(players.lm4) ##only mean_cp significant
vif(players.lm4) ##age, time, WL, WhiteWL need to be removed

players.lm5 <- lm(Std_CP~Mean_CP + Elo + OppElo,data=train.data)
summary(players.lm5) ##only mean_cp significant
vif(players.lm5) ##looks good now

players.lm6 <- lm(Std_CP~(Mean_CP + Elo + OppElo)^2,data=train.data)
summary(players.lm6) ##mean_cp, elo, oppelo, elo*oppelo significant

players.lm7 <- lm(Std_CP~Mean_CP + Elo + OppElo + Elo*OppElo,data=train.data)
summary(players.lm7) ##mean_cp, elo, oppelo, elo*oppelo significant

players.lm8 <- lm(Std_CP~Mean_CP,data=train.data)
summary(players.lm8)


predictions3 <- players.lm5 %>% predict(test.data)
p3=data.frame(
        RMSE = RMSE(predictions3, test.data$Std_CP),
        R2 = R2(predictions3, test.data$Std_CP)
)
predictions4 <- players.lm6 %>% predict(test.data)
p4=data.frame(
        RMSE = RMSE(predictions4, test.data$Std_CP),
        R2 = R2(predictions4, test.data$Std_CP)
)
predictions5 <- players.lm7 %>% predict(test.data)
p5=data.frame(
        RMSE = RMSE(predictions5, test.data$Std_CP),
        R2 = R2(predictions5, test.data$Std_CP)
)
predictions6 <- players.lm8 %>% predict(test.data)
p6=data.frame(
        RMSE = RMSE(predictions6, test.data$Std_CP),
        R2 = R2(predictions6, test.data$Std_CP)
)

p3
p4 ## best model
p5
p6

AIC(players.lm5,players.lm6,players.lm7,players.lm8)


## remove outliers ##
Q <- quantile(players$Mean_CP, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(players$Mean_CP)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
mean_eliminated <- subset(players, players$Mean_CP > (Q[1] - 1.5*iqr) & players$Mean_CP < (Q[2]+1.5*iqr))

Q <- quantile(mean_eliminated$Std_CP, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(mean_eliminated$Std_CP)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
players_no_outliers <- subset(mean_eliminated, mean_eliminated$Std_CP > (Q[1] - 1.5*iqr) & mean_eliminated$Std_CP < (Q[2]+1.5*iqr))

## mean_cp
training.samples <- players_no_outliers$Mean_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- players_no_outliers[training.samples, ]
test.data <- players_no_outliers[-training.samples, ]

players_no_outliers.lm1 <- lm(Mean_CP~.,data=train.data)
summary(players_no_outliers.lm1) # Age, Time, Std_CP, Elo, WL, WhiteWL significant
vif(players_no_outliers.lm1) #age, time need to be removed

players_no_outliers.lm2 <- lm(Mean_CP~Std_CP + Elo + WL + WhiteWL,data=train.data)
summary(players_no_outliers.lm2) # Std_CP, Elo, WL, WhiteWL significant
vif(players_no_outliers.lm2) #WL, WhiteWL need to be removed

players_no_outliers.lm3 <- lm(Mean_CP~Std_CP + Elo,
                              data=train.data)
summary(players_no_outliers.lm3) # Std_CP, Elo significant
vif(players_no_outliers.lm3) # looks good now

players_no_outliers.lm4 <- lm(Mean_CP~(Std_CP + Elo)^2,
                  data=train.data)
summary(players_no_outliers.lm4) ## no interactions are significant

predictions1 <- players_no_outliers.lm3 %>% predict(test.data)
p1=data.frame(
        RMSE = RMSE(predictions1, test.data$Mean_CP),
        R2 = R2(predictions1, test.data$Mean_CP)
)

predictions2 <- players_no_outliers.lm4 %>% predict(test.data)
p2=data.frame(
        RMSE = RMSE(predictions2, test.data$Mean_CP),
        R2 = R2(predictions2, test.data$Mean_CP)
)

p1 
p2 ##best model (though, marginally)

AIC(players_no_outliers.lm3,players_no_outliers.lm4)


## std_cp
training.samples <- players_no_outliers$Std_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- players_no_outliers[training.samples, ]
test.data <- players_no_outliers[-training.samples, ]

players_no_outliers.lm5 <- lm(Std_CP~.,data=train.data)
summary(players_no_outliers.lm5) #mean_cp, oppelo significant
vif(players_no_outliers.lm5) #age, time need to be removed

players_no_outliers.lm6 <- lm(Std_CP~Mean_CP + Elo + OppElo + WL + WhiteWL,data=train.data)
summary(players_no_outliers.lm6) #only mean_cp, oppelo significant
vif(players_no_outliers.lm6) #WL, WhiteWL needs to be removed

players_no_outliers.lm7 <- lm(Std_CP~Mean_CP + Elo + OppElo,data=train.data)
summary(players_no_outliers.lm7) #only mean_cp significant
vif(players_no_outliers.lm7) #looks good now

players_no_outliers.lm8 <- lm(Std_CP~(Mean_CP + Elo + OppElo)^2,data=train.data)
summary(players_no_outliers.lm8) #mean_cp, elo, oppelo, mean_cp*oppelo, elo*oppelo significant

players_no_outliers.lm9 <- lm(Std_CP~Mean_CP + Elo + OppElo + Mean_CP*OppElo +
                                      Elo*OppElo,data=train.data)
summary(players_no_outliers.lm9) #mean_cp, elo, oppelo, mean_cp*oppelo, elo*oppelo significant

players_no_outliers.lm10 <- lm(Std_CP~Mean_CP,data=train.data)
summary(players_no_outliers.lm10)


predictions3 <- players_no_outliers.lm7 %>% predict(test.data)
p3=data.frame(
        RMSE = RMSE(predictions3, test.data$Std_CP),
        R2 = R2(predictions3, test.data$Std_CP)
)
predictions4 <- players_no_outliers.lm8 %>% predict(test.data)
p4=data.frame(
        RMSE = RMSE(predictions4, test.data$Std_CP),
        R2 = R2(predictions4, test.data$Std_CP)
)
predictions5 <- players_no_outliers.lm9 %>% predict(test.data)
p5=data.frame(
        RMSE = RMSE(predictions5, test.data$Std_CP),
        R2 = R2(predictions5, test.data$Std_CP)
)
predictions6 <- players_no_outliers.lm10 %>% predict(test.data)
p6=data.frame(
        RMSE = RMSE(predictions6, test.data$Std_CP),
        R2 = R2(predictions6, test.data$Std_CP)
)

p3 ## best model
p4
p5
p6

AIC(players_no_outliers.lm7,players_no_outliers.lm8,players_no_outliers.lm9,players_no_outliers.lm10)



## normalized lin reg ##
players.norm <- players
players.norm$Mean_CP <- log(players$Mean_CP)
players.norm$Std_CP <- log(players$Std_CP)

## mean_cp
training.samples <- players.norm$Mean_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- players.norm[training.samples, ]
test.data <- players.norm[-training.samples, ]

players.norm.lm1 <- lm(Mean_CP~.,data=train.data)
summary(players.norm.lm1) # Std_CP, Elo, WL, WhiteWL significant
vif(players.norm.lm1) #age, time need to be removed

players.norm.lm2 <- lm(Mean_CP~Std_CP + Elo + WL + WhiteWL,data=train.data)
summary(players.norm.lm2) # Std_CP, Elo, WL, WhiteWL significant
vif(players.norm.lm2) #looks good now

players.norm.lm3 <- lm(Mean_CP~(Std_CP + Elo + WL + WhiteWL)^2,
                  data=train.data)
summary(players.norm.lm3) # WL and WhiteWL seem to be causing issues, so remove

players.norm.lm4 <- lm(Mean_CP~(Std_CP + Elo)^2,
                       data=train.data)
summary(players.norm.lm4) # std_cp, elo, std_cp*elo significant

players.norm.lm5 <- lm(Mean_CP~Std_CP + Elo + Std_CP*Elo,
                       data=train.data)
summary(players.norm.lm5) # std_cp, elo, std_cp*elo significant

predictions1 <- players.norm.lm2 %>% predict(test.data)
p1=data.frame(
        RMSE = RMSE(predictions1, test.data$Mean_CP),
        R2 = R2(predictions1, test.data$Mean_CP)
)

predictions2 <- players.norm.lm4 %>% predict(test.data)
p2=data.frame(
        RMSE = RMSE(predictions2, test.data$Mean_CP),
        R2 = R2(predictions2, test.data$Mean_CP)
)

predictions3 <- players.norm.lm5 %>% predict(test.data)
p3=data.frame(
        RMSE = RMSE(predictions3, test.data$Mean_CP),
        R2 = R2(predictions3, test.data$Mean_CP)
)

p1 ##best model (though, marginally)
p2 
p3

AIC(players.norm.lm2,players.norm.lm4,players.norm.lm5)


## std_cp
training.samples <- players.norm$Std_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- players.norm[training.samples, ]
test.data <- players.norm[-training.samples, ]

players.norm.lm6 <- lm(Std_CP~.,data=train.data)
summary(players.norm.lm6) ##only mean_cp, elo, WhiteWL significant
vif(players.norm.lm6) ##age, time need to be removed

players.norm.lm7 <- lm(Std_CP~Mean_CP + Elo + WhiteWL,data=train.data)
summary(players.norm.lm7) ##mean_cp, Elo significant
vif(players.norm.lm7) ##looks good now

players.norm.lm8 <- lm(Std_CP~(Mean_CP + Elo + WhiteWL)^2,data=train.data)
summary(players.norm.lm8) ##mean_cp, WhiteWL, Mean_CP*WhiteWL significant

players.norm.lm9 <- lm(Std_CP~Mean_CP + WhiteWL + Mean_CP*WhiteWL,
                  data=train.data)
summary(players.norm.lm9) ##mean_cp, WhiteWL, mean_CP*WhiteWL significant

predictions4 <- players.norm.lm7 %>% predict(test.data)
p4=data.frame(
        RMSE = RMSE(predictions4, test.data$Std_CP),
        R2 = R2(predictions4, test.data$Std_CP)
)
predictions5 <- players.norm.lm8 %>% predict(test.data)
p5=data.frame(
        RMSE = RMSE(predictions5, test.data$Std_CP),
        R2 = R2(predictions5, test.data$Std_CP)
)
predictions6 <- players.norm.lm9 %>% predict(test.data)
p6=data.frame(
        RMSE = RMSE(predictions6, test.data$Std_CP),
        R2 = R2(predictions6, test.data$Std_CP)
)

p4 
p5 ## best model (but could be overfitting)
p6

AIC(players.norm.lm7,players.norm.lm8,players.norm.lm9)


## normalized + remove outliers ##
Q <- quantile(players.norm$Mean_CP, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(players.norm$Mean_CP)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
mean_eliminated <- subset(players.norm, players.norm$Mean_CP > (Q[1] - 1.5*iqr) & players.norm$Mean_CP < (Q[2]+1.5*iqr))

Q <- quantile(mean_eliminated$Std_CP, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(mean_eliminated$Std_CP)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
players_no_outliers_norm <- subset(mean_eliminated, mean_eliminated$Std_CP > (Q[1] - 1.5*iqr) & mean_eliminated$Std_CP < (Q[2]+1.5*iqr))

## mean_cp
training.samples <- players_no_outliers_norm$Mean_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- players_no_outliers_norm[training.samples, ]
test.data <- players_no_outliers_norm[-training.samples, ]

players_no_outliers_norm.lm1 <- lm(Mean_CP~.,data=train.data)
summary(players_no_outliers_norm.lm1) # Std_CP, Elo, WL, WhiteWL significant
vif(players_no_outliers_norm.lm1) #age, time, WL, WhiteWL need to be removed

players_no_outliers_norm.lm2 <- lm(Mean_CP~Std_CP + Elo,data=train.data)
summary(players_no_outliers_norm.lm2) # Std_CP, Elo
vif(players_no_outliers_norm.lm2) #looks good now

players_no_outliers_norm.lm3 <- lm(Mean_CP~(Std_CP + Elo)^2,
                              data=train.data)
summary(players_no_outliers_norm.lm3) # Std_CP, Elo, Std_CP*Elo significant

players_no_outliers_norm.lm4 <- lm(Mean_CP~Std_CP + Elo + Std_CP*Elo,
                              data=train.data)
summary(players_no_outliers_norm.lm4) # Std_CP, Elo, Std_CP*Elo significant

predictions1 <- players_no_outliers_norm.lm2 %>% predict(test.data)
p1=data.frame(
        RMSE = RMSE(predictions1, test.data$Mean_CP),
        R2 = R2(predictions1, test.data$Mean_CP)
)

predictions2 <- players_no_outliers_norm.lm3 %>% predict(test.data)
p2=data.frame(
        RMSE = RMSE(predictions2, test.data$Mean_CP),
        R2 = R2(predictions2, test.data$Mean_CP)
)

predictions3 <- players_no_outliers_norm.lm4 %>% predict(test.data)
p3=data.frame(
        RMSE = RMSE(predictions3, test.data$Mean_CP),
        R2 = R2(predictions3, test.data$Mean_CP)
)

p1 
p2 ##best model (though, marginally)
p3 ## same as p2

AIC(players_no_outliers_norm.lm2,players_no_outliers_norm.lm3,players_no_outliers_norm.lm4)


## std_cp
training.samples <- players_no_outliers_norm$Std_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- players_no_outliers_norm[training.samples, ]
test.data <- players_no_outliers_norm[-training.samples, ]

players_no_outliers_norm.lm5 <- lm(Std_CP~.,data=train.data)
summary(players_no_outliers_norm.lm5) #mean_cp, elo, WhiteWL significant
vif(players_no_outliers_norm.lm5) #age, time need to be removed

players_no_outliers_norm.lm6 <- lm(Std_CP~Mean_CP + Elo + OppElo + WhiteWL,data=train.data)
summary(players_no_outliers_norm.lm6) #only mean_cp, elo significant
vif(players_no_outliers_norm.lm6) #looks good now

players_no_outliers_norm.lm7 <- lm(Std_CP~(Mean_CP + Elo + OppElo + WhiteWL)^2,data=train.data)
summary(players_no_outliers_norm.lm7) #mean_cp, elo, oppelo, elo*oppelo significant

players_no_outliers_norm.lm8 <- lm(Std_CP~Mean_CP + Elo + OppElo + Elo*OppElo,data=train.data)
summary(players_no_outliers_norm.lm8) #mean_cp, elo, oppelo, elo*oppelo significant

players_no_outliers_norm.lm9 <- lm(Std_CP~Mean_CP + Elo,data=train.data)
summary(players_no_outliers_norm.lm9) #mean_cp, elo significant

players_no_outliers_norm.lm10 <- lm(Std_CP~(Mean_CP + Elo)^2,data=train.data)
summary(players_no_outliers_norm.lm10) #no interaction significant

predictions4 <- players_no_outliers_norm.lm6 %>% predict(test.data)
p4=data.frame(
        RMSE = RMSE(predictions4, test.data$Std_CP),
        R2 = R2(predictions4, test.data$Std_CP)
)
predictions5 <- players_no_outliers_norm.lm7 %>% predict(test.data)
p5=data.frame(
        RMSE = RMSE(predictions5, test.data$Std_CP),
        R2 = R2(predictions5, test.data$Std_CP)
)
predictions6 <- players_no_outliers_norm.lm8 %>% predict(test.data)
p6=data.frame(
        RMSE = RMSE(predictions6, test.data$Std_CP),
        R2 = R2(predictions6, test.data$Std_CP)
)
predictions7 <- players_no_outliers_norm.lm9 %>% predict(test.data)
p7=data.frame(
        RMSE = RMSE(predictions7, test.data$Std_CP),
        R2 = R2(predictions7, test.data$Std_CP)
)

p4 
p5 ## best model (but could be overfitting)
p6
p7

AIC(players_no_outliers_norm.lm6,players_no_outliers_norm.lm7,players_no_outliers_norm.lm8,players_no_outliers_norm.lm9)



