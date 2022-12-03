## MAKE SURE TO SETWD TO INSIDE CODE FOLDER

## only over-the-board games
## mean_cp_less = beta_0 + beta_1 * age + beta_2 * time + beta_3 * elo + beta_4 * oppelo
## std_cp_less = beta_0 + beta_1 * age + beta_2 * time + beta_3 * elo + beta_4 * oppelo
## win vs. not win = beta_0 + beta_1 * blackelo + beta_2 * white_elo


## ref: https://stackoverflow.com/questions/5758084/loop-in-r-to-read-many-files
fi <- list.files('../data/',full.names=T)
dat <- lapply(fi,read.csv)

## ref: https://stackoverflow.com/questions/2851327/combine-a-list-of-data-frames-into-one-data-frame-by-row
library(dplyr)
library(ggplot2)
players <- bind_rows(dat, .id = "column_label")

player_names = unique(players$Name) #list of player names
growth = data_frame(player_names, elo_change = NA, age_change = NA, avg_elo_change = NA, num_games = NA, elo_change_pergame = NA, max_elo = NA) #define dataframe that will show the average elo change for each player
for (i in 1:31){ #for ech player index
  name = growth$player_names[i] #name equals the player at that index
  df = subset(players, players$Name == name) #subset the dataframe to include only games of that player
  first = df[1,] #first equals the players first recorded game
  elo_change = max(df$Elo) - first$Elo #change in elo, max elo minus first elo
  max_index = which.max(df$Elo) #index of the max elo 
  age_change = df$Age[max_index] - first$Age #age change = age at max elo minus first game age
  age_change = age_change +1  #add 1 to age to avoid 0
  growth$elo_change[i] = elo_change # add elo change to df
  growth$age_change[i] = age_change #add age change to df
  growth$avg_elo_change[i] = (elo_change/age_change) #add avg elo change to df
  growth$num_games[i] = max_index #add number of games it took to get to max elo
  growth$elo_change_pergame[i] = (elo_change/max_index)
  growth$max_elo[i] = max(df$Elo) #add max elo for player to datafrae
}

#Visualizations

#average change
ggplot(growth, aes(x = elo_change)) + geom_histogram(bins = 30) + geom_vline(xintercept = 229, color = "red") + labs(x = "Total Elo Growth", y = "Frequency", title = "Histogram of Total Elo Growth for Players") + theme_bw()



#Number of Games vs Elo Change Scatter Plot
ggplot(growth, aes(x = num_games, y = elo_change, label = player_names)) + geom_point(color = "blue")  + labs(x = "Number of Games", y = "Elo Change", title = "Elo Change by Number of Games") + geom_text(aes(label=ifelse(elo_change>40,as.character(player_names),'')),hjust = 1, vjust =-1) +theme_classic()

#Number of Games vs Average Elo Change Scatter Plot
ggplot(growth, aes(x = num_games, y = avg_elo_change, label = player_names)) + geom_point(color = "blue")  + labs(x = "Number of Games", y = "Average Elo Change per Year", title = "Average Elo Change by Number of Games") + geom_text(aes(label=ifelse(avg_elo_change>10,as.character(player_names),'')),hjust = 1, vjust =-1) +theme_classic()


# Bootstrapping
n = nrow(growth)
elochange.Boot = numeric(10000)
change_pergame.Boot = numeric(10000)
for (i in 1:10000){
  x = sample(growth$avg_elo_change, n, replace = T)
  y = sample(growth$elo_change_pergame, n , replace = T)
  elochange.Boot[i] = mean(x)
  change_pergame.Boot[i] = mean(y)
}

#bootstrap confidence intervals
quantile(elochange.Boot, c(0.025, 0.975))
quantile(change_pergame.Boot, c(0.025, 0.975))

#histogram of bootstrap average elo change per year
ggplot(data.frame(elochange.Boot), aes(elochange.Boot)) + geom_histogram(fill = "lightblue", bins = 100) + geom_vline(xintercept = 57.25, color = "red") + labs(x= "Average Elo Change per Year", y = "Frequency", title = "Histogram of Bootstrap Average Elo Change Per year")  + theme_bw()

#histogram of bootsrap average elo change per game
ggplot(data.frame(change_pergame.Boot), aes(change_pergame.Boot)) + geom_histogram(fill = "lightblue") + geom_vline(xintercept = 0.654, color = "red") + labs(x = "Average Elo Change Per Game", y = "Frequency", title = "Histogram of Bootstrap Average Elo Change per Game") + theme_bw()


#THEME 
g + theme(
  plot.background = element_rect(fill = "#E6EDf5"),
  panel.grid.major = element_line(color = "#626569", size = 0.2),
  # panel.grid.minor = element_line(color = "#626569"),
  strip.text.x = element_text(
    size = 8, color = "#271F7F"
  ),
  plot.tag.position = c(.95, .95),
  axis.text.y = element_text(
    size = 8, color = "#271F7F"
  ),
  axis.text.x = element_text(
    size = 8, color = "#271F7F"
  ),
  plot.caption = element_text(
    size = 10, color = "#271F7F"
  ),
  plot.title = element_text(
    size = 20, face = "bold", color = "#271F7F"
  ),
  axis.title.y = element_text(
    size = 15, color = "#271F7F"
  ),
  axis.title.x = element_text(
    size = 15, color = "#271F7F"
  ),
  legend.title = element_text(
    size = 12, color = "#271F7F"
  ),
  legend.text = element_text(
    size = 10, color = "#271F7F"
  )
)
