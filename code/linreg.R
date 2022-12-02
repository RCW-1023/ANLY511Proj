## ref: https://stackoverflow.com/questions/5758084/loop-in-r-to-read-many-files
fi <- list.files('../data/',full.names=T)
dat <- lapply(fi,read.csv)

## ref: https://stackoverflow.com/questions/2851327/combine-a-list-of-data-frames-into-one-data-frame-by-row
library(dplyr)
players <- bind_rows(dat, .id = "column_label")
players$WL <- factor(players$WL)
players$WhiteWL <- factor(players$WhiteWL)
players <- players[,-c(1,2,3,12)]

## normalize ##
players$Mean_CP <- log(players$Mean_CP)
players$Std_CP <- log(players$Std_CP)

## lin reg ##
library(olsrr)
model1 <- lm(Std_CP ~ Age + Mean_CP + Elo + OppElo, data = players) #best model is 3: Mean_CP, Elo, OppElo
vif(model1)
ols_step_all_possible(model1)
ols_step_best_subset(model1)

model2 <- lm(Mean_CP ~ Age + Std_CP + Elo + OppElo, data = players) #best model is 3: Std_CP, Elo, OppElo
vif(model2)
ols_step_all_possible(model2)
ols_step_best_subset(model2)

model3 <- lm(Std_CP ~ Time + Mean_CP + Elo + OppElo, data = players) #best model is 3: Mean_CP, Elo, OppElo
vif(model3)
ols_step_all_possible(model3)
ols_step_best_subset(model3)

model4 <- lm(Mean_CP ~ Time + Std_CP + Elo + OppElo, data = players) #best model is 3: Std_CP, Elo, OppElo
vif(model4)
ols_step_all_possible(model4)
ols_step_best_subset(model4)

model5 <- lm(Std_CP ~ (Time + Mean_CP + Elo + OppElo)^2, data = players) #best model is 6: Time Mean_CP OppElo Time:Mean_CP Mean_CP:Elo Elo:OppElo 
ols_step_all_possible(model5)
ols_step_best_subset(model5)

model6 <- lm(Mean_CP ~ (Time + Std_CP + Elo + OppElo)^2, data = players) #best model is 6: Time Elo Time:Std_CP Std_CP:Elo Std_CP:OppElo Elo:OppElo
ols_step_all_possible(model6)
ols_step_best_subset(model6)

model7 <- lm(Std_CP ~ (Age + Mean_CP + Elo + OppElo)^2, data = players) #best model is 7: Age Mean_CP OppElo Age:Mean_CP Age:OppElo Mean_CP:Elo Elo:OppElo
ols_step_all_possible(model7)
ols_step_best_subset(model7)

model8 <- lm(Mean_CP ~ (Age + Std_CP + Elo + OppElo)^2, data = players) #best model is 5: Age Elo Age:Std_CP Age:OppElo Std_CP:Elo
ols_step_all_possible(model8)
ols_step_best_subset(model8)


## mean_cp ##
training.samples <- players$Mean_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- players[training.samples, ]
test.data <- players[-training.samples, ]

players.lm1 <- lm(Mean_CP~Std_CP+Elo+OppElo,data=train.data)
summary(players.lm1)

players.lm2 <- lm(Mean_CP~Time+Elo+Time*Std_CP+Std_CP*Elo+Std_CP*OppElo+Elo*OppElo,data=train.data)
summary(players.lm2)

players.lm3 <- lm(Mean_CP~Age+Elo+Age*Std_CP+Age*OppElo+Std_CP*Elo,data=train.data)
summary(players.lm3)

predictions1 <- players.lm1 %>% predict(test.data)
p1=data.frame(
        RMSE = RMSE(predictions1, test.data$Mean_CP),
        R2 = R2(predictions1, test.data$Mean_CP)
)
predictions2 <- players.lm2 %>% predict(test.data)
p2=data.frame(
        RMSE = RMSE(predictions2, test.data$Mean_CP),
        R2 = R2(predictions2, test.data$Mean_CP)
)
predictions3 <- players.lm3 %>% predict(test.data)
p3=data.frame(
        RMSE = RMSE(predictions3, test.data$Mean_CP),
        R2 = R2(predictions3, test.data$Mean_CP)
)

p1 #best model -- avoid overfitting
p2 
p3

AIC(players.lm1,players.lm2,players.lm3)

par(mfrow=c(2,2))
plot(players.lm1)

## std_cp ##
training.samples <- players.norm$Std_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- players.norm[training.samples, ]
test.data <- players.norm[-training.samples, ]

players.lm1 <- lm(Std_CP~Mean_CP+Elo+OppElo,data=train.data)
summary(players.lm1)

players.lm2 <- lm(Std_CP~Time+Mean_CP+OppElo+Time*Mean_CP+Mean_CP*Elo+Elo*OppElo,data=train.data)
summary(players.lm2)

players.lm3 <- lm(Std_CP~Age+Mean_CP+OppElo+Age*Mean_CP+Age*OppElo+Mean_CP*Elo+Elo*OppElo,data=train.data)
summary(players.lm3)

predictions1 <- players.lm1 %>% predict(test.data)
p1=data.frame(
        RMSE = RMSE(predictions1, test.data$Mean_CP),
        R2 = R2(predictions1, test.data$Mean_CP)
)
predictions2 <- players.lm2 %>% predict(test.data)
p2=data.frame(
        RMSE = RMSE(predictions2, test.data$Mean_CP),
        R2 = R2(predictions2, test.data$Mean_CP)
)
predictions3 <- players.lm3 %>% predict(test.data)
p3=data.frame(
        RMSE = RMSE(predictions3, test.data$Mean_CP),
        R2 = R2(predictions3, test.data$Mean_CP)
)

p1 #best model -- avoid overfitting
p2 
p3

AIC(players.lm1,players.lm2,players.lm3)


## w/o outliers ##
Q <- quantile(players$Mean_CP, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(players$Mean_CP)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
mean_eliminated <- subset(players, players$Mean_CP > (Q[1] - 1.5*iqr) & players$Mean_CP < (Q[2]+1.5*iqr))

Q <- quantile(mean_eliminated$Std_CP, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(mean_eliminated$Std_CP)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
no.outliers <- subset(mean_eliminated, mean_eliminated$Std_CP > (Q[1] - 1.5*iqr) & mean_eliminated$Std_CP < (Q[2]+1.5*iqr))

model1 <- lm(Std_CP ~ Age + Mean_CP + Elo + OppElo, data = no.outliers) #best model is 3: Mean_CP, Elo, OppElo
vif(model1)
ols_step_all_possible(model1)
ols_step_best_subset(model1)

model2 <- lm(Mean_CP ~ Age + Std_CP + Elo + OppElo, data = no.outliers) #best model is 3: Std_CP, Elo, OppElo
vif(model2)
ols_step_all_possible(model2)
ols_step_best_subset(model2)

model3 <- lm(Std_CP ~ Time + Mean_CP + Elo + OppElo, data = no.outliers) #best model is 3: Mean_CP, Elo, OppElo
vif(model3)
ols_step_all_possible(model3)
ols_step_best_subset(model3)

model4 <- lm(Mean_CP ~ Time + Std_CP + Elo + OppElo, data = no.outliers) #best model is 3: Std_CP, Elo, OppElo
vif(model4)
ols_step_all_possible(model4)
ols_step_best_subset(model4)

model5 <- lm(Std_CP ~ (Time + Mean_CP + Elo + OppElo)^2, data = no.outliers) #best model is 7: Time Mean_CP OppElo Time:Mean_CP Time:Elo Mean_CP:Elo Elo:OppElo 
ols_step_all_possible(model5)
ols_step_best_subset(model5)

model6 <- lm(Mean_CP ~ (Time + Std_CP + Elo + OppElo)^2, data = no.outliers) #best model is 5: Time Elo Time:Std_CP Time:OppElo Std_CP:Elo
ols_step_all_possible(model6)
ols_step_best_subset(model6)

model7 <- lm(Std_CP ~ (Age + Mean_CP + Elo + OppElo)^2, data = no.outliers) #best model is 8: Age Mean_CP OppElo Age:Mean_CP Age:Elo Mean_CP:Elo Mean_CP:OppElo Elo:OppElo
ols_step_all_possible(model7)
ols_step_best_subset(model7)

model8 <- lm(Mean_CP ~ (Age + Std_CP + Elo + OppElo)^2, data = no.outliers) #best model is 6: Age Std_CP Elo Age:Std_CP Age:OppElo Std_CP:Elo
ols_step_all_possible(model8)
ols_step_best_subset(model8)

## mean_cp ##
training.samples <- no.outliers$Mean_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- no.outliers[training.samples, ]
test.data <- no.outliers[-training.samples, ]

players.lm1 <- lm(Mean_CP~Std_CP+Elo+OppElo,data=train.data)
summary(players.lm1)

players.lm2 <- lm(Mean_CP~Time+Elo+Time*Std_CP+Time*OppElo+Std_CP*Elo,data=train.data)
summary(players.lm2)

players.lm3 <- lm(Mean_CP~Age+Std_CP+Elo+Age*Std_CP+Age*OppElo+Std_CP*Elo,data=train.data)
summary(players.lm3)

predictions1 <- players.lm1 %>% predict(test.data)
p1=data.frame(
        RMSE = RMSE(predictions1, test.data$Mean_CP),
        R2 = R2(predictions1, test.data$Mean_CP)
)
predictions2 <- players.lm2 %>% predict(test.data)
p2=data.frame(
        RMSE = RMSE(predictions2, test.data$Mean_CP),
        R2 = R2(predictions2, test.data$Mean_CP)
)
predictions3 <- players.lm3 %>% predict(test.data)
p3=data.frame(
        RMSE = RMSE(predictions3, test.data$Mean_CP),
        R2 = R2(predictions3, test.data$Mean_CP)
)

p1 #best model -- avoid overfitting
p2 
p3

AIC(players.lm1,players.lm2,players.lm3)


## std_cp ##
training.samples <- no.outliers$Std_CP %>%
        createDataPartition(p = 0.8, list = FALSE)
train.data  <- no.outliers[training.samples, ]
test.data <- no.outliers[-training.samples, ]

players.lm1 <- lm(Std_CP~Mean_CP+Elo+OppElo,data=train.data)
summary(players.lm1)

players.lm2 <- lm(Std_CP~Time+Mean_CP+OppElo+Time*Mean_CP+Time*Elo+Mean_CP*Elo+Elo*OppElo,data=train.data)
summary(players.lm2)

players.lm3 <- lm(Std_CP~Age+Mean_CP+OppElo+Age*Mean_CP+Age*Elo+Mean_CP*Elo+Mean_CP*OppElo+Elo*OppElo,data=train.data)
summary(players.lm3)

predictions1 <- players.lm1 %>% predict(test.data)
p1=data.frame(
        RMSE = RMSE(predictions1, test.data$Mean_CP),
        R2 = R2(predictions1, test.data$Mean_CP)
)
predictions2 <- players.lm2 %>% predict(test.data)
p2=data.frame(
        RMSE = RMSE(predictions2, test.data$Mean_CP),
        R2 = R2(predictions2, test.data$Mean_CP)
)
predictions3 <- players.lm3 %>% predict(test.data)
p3=data.frame(
        RMSE = RMSE(predictions3, test.data$Mean_CP),
        R2 = R2(predictions3, test.data$Mean_CP)
)

p1 #best model -- avoid overfitting
p2 
p3

AIC(players.lm1,players.lm2,players.lm3)
