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


## scaled lin reg ##
players.scaled <- players
players.scaled$Mean_CP <- scale(players$Mean_CP)
players.scaled$Std_CP <- scale(players$Std_CP)

## mean_cp
training.samples <- players.scaled$Mean_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- players.scaled[training.samples, ]
test.data <- players.scaled[-training.samples, ]

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

p1
p2 ##best model (though, marginally)


## std_cp
training.samples <- players$Std_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- players[training.samples, ]
test.data <- players[-training.samples, ]

players.lm4 <- lm(Std_CP~.,data=train.data)
summary(players.lm4) ##only mean_cp significant
vif(players.lm4) ##age, time, WL, WhiteWL need to be removed

players.lm5 <- lm(Std_CP~Mean_CP + Elo + OppElo,data=train.data)
summary(players.lm5) ##mean_cp, Elo significant
vif(players.lm5) ##looks good now

players.lm6 <- lm(Std_CP~(Mean_CP + Elo + OppElo)^2,data=train.data)
summary(players.lm6) ##mean_cp, elo, oppelo, mean_cp*elo, elo*oppelo significant

players.lm7 <- lm(Std_CP~Mean_CP + Elo + OppElo + Mean_CP*Elo + Elo*OppElo,
                  data=train.data)
summary(players.lm7) ##elo, oppelo, elo*oppelo significant

players.lm8 <- lm(Std_CP~Elo + OppElo + Elo*OppElo,
                  data=train.data)
summary(players.lm8) ##elo, oppelo, elo*oppelo significant

players.lm9 <- lm(Std_CP~Mean_CP,data=train.data)
summary(players.lm9)

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
predictions7 <- players.lm9 %>% predict(test.data)
p7=data.frame(
        RMSE = RMSE(predictions7, test.data$Std_CP),
        R2 = R2(predictions7, test.data$Std_CP)
)

p3
p4 
p5 ## best model 
p6
p7
