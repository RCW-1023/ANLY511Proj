## ref: https://stackoverflow.com/questions/5758084/loop-in-r-to-read-many-files
fi <- list.files('../data/',full.names=T)
dat <- lapply(fi,read.csv)

## ref: https://stackoverflow.com/questions/2851327/combine-a-list-of-data-frames-into-one-data-frame-by-row
library(dplyr)
players <- bind_rows(dat, .id = "column_label")
players$WL <- factor(players$WL)
players$WhiteWL <- factor(players$WhiteWL)
players <- players[,-c(1,2,3,12)]
players$EloDiff <- players$Elo - players$OppElo

## ordinal reg
library(MASS)
library(RStata)
library(car)

# ref: https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/

## age
players.ord <- polr(WL ~ Age + EloDiff + Mean_CP + Std_CP, data=players)
summary(players.ord)
vif(players.ord)
(ctable <- coef(summary(players.ord)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p)) ## age, elodiff, mean_cp

players.ord2 <- polr(WhiteWL ~ Age + EloDiff + Mean_CP + Std_CP, data=players)
summary(players.ord2)
(ctable <- coef(summary(players.ord2)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p)) ## mean_cp

## time
players.ord3 <- polr(WL ~ Time + EloDiff + Mean_CP + Std_CP, data=players)
summary(players.ord3)
(ctable <- coef(summary(players.ord3)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p)) ## time, elodiff, mean_cp

players.ord4 <- polr(WhiteWL ~ Time + EloDiff + Mean_CP + Std_CP, data=players)
summary(players.ord4)
(ctable <- coef(summary(players.ord4)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p)) ## mean_cp

