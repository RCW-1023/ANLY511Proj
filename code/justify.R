## ref: https://stackoverflow.com/questions/5758084/loop-in-r-to-read-many-files
fi <- list.files('../data/',full.names=T)
dat <- lapply(fi,read.csv)

## ref: https://stackoverflow.com/questions/2851327/combine-a-list-of-data-frames-into-one-data-frame-by-row
library(dplyr)
players <- bind_rows(dat, .id = "column_label")
players.scaled <- scale(players[,c(5,6)])

library(factoextra)
set.seed(123)

results1<-kmeans(players.scaled,2)
results2<-kmeans(players.scaled,3)
results3<-kmeans(players.scaled,4)

fviz_nbclust(players.scaled, kmeans, method = "wss") #+
        #geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(players.scaled, method = "silhouette", 
             FUN = hcut, k.max = 5)

fviz_cluster(results1, data = players.scaled,
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results1
fviz_cluster(results2, data = players.scaled,
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results2
fviz_cluster(results3, data = players.scaled,
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results3

## remove outliers + redo cluster
Q <- quantile(players$Mean_CP, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(players$Mean_CP)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
mean_eliminated <- subset(players, players$Mean_CP > (Q[1] - 1.5*iqr) & players$Mean_CP < (Q[2]+1.5*iqr))

Q <- quantile(players$Std_CP, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(players$Std_CP)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
players_no_outliers <- subset(players, players$Std_CP > (Q[1] - 1.5*iqr) & players$Std_CP < (Q[2]+1.5*iqr))

players.scaled <- scale(players_no_outliers[,c(5,6)])

results4<-kmeans(players.scaled,2)
results5<-kmeans(players.scaled,3)
results6<-kmeans(players.scaled,4)

fviz_nbclust(players.scaled, kmeans, method = "wss") #+
        #geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(players.scaled, method = "silhouette", 
             FUN = hcut, k.max = 5)

fviz_cluster(results4, data = players.scaled,
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results4
fviz_cluster(results5, data = players.scaled,
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results5
fviz_cluster(results6, data = players.scaled,
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results6
