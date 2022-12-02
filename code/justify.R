## ref: https://stackoverflow.com/questions/5758084/loop-in-r-to-read-many-files
fi <- list.files('../data/',full.names=T)
dat <- lapply(fi,read.csv)

## ref: https://stackoverflow.com/questions/2851327/combine-a-list-of-data-frames-into-one-data-frame-by-row
library(dplyr)
players <- bind_rows(dat, .id = "column_label")

library(factoextra)
set.seed(123)

results1<-kmeans(players[,c(5,6)],2)
results2<-kmeans(players[,c(5,6)],3)
results3<-kmeans(players[,c(5,6)],4)

fviz_nbclust(players[,c(5,6)], kmeans, method = "wss") #+
        #geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(players[,c(5,6)], method = "silhouette", 
             FUN = hcut, k.max = 5)

fviz_cluster(results1, data = players[,c(5,6)],
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results1
fviz_cluster(results2, data = players[,c(5,6)],
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results2
fviz_cluster(results3, data = players[,c(5,6)],
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

Q <- quantile(mean_eliminated$Std_CP, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(mean_eliminated$Std_CP)
up <-  Q[2]+1.5*iqr 
low<- Q[1]-1.5*iqr
players_no_outliers <- subset(mean_eliminated, mean_eliminated$Std_CP > (Q[1] - 1.5*iqr) & mean_eliminated$Std_CP < (Q[2]+1.5*iqr))

results4<-kmeans(players_no_outliers[,c(5,6)],2)
results5<-kmeans(players_no_outliers[,c(5,6)],3)
results6<-kmeans(players_no_outliers[,c(5,6)],4)

fviz_nbclust(players_no_outliers[,c(5,6)], kmeans, method = "wss") #+
        #geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(players_no_outliers[,c(5,6)], method = "silhouette", 
             FUN = hcut, k.max = 5)

fviz_cluster(results4, data = players_no_outliers[,c(5,6)],
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results4
fviz_cluster(results5, data = players_no_outliers[,c(5,6)],
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results5
fviz_cluster(results6, data = players_no_outliers[,c(5,6)],
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results6


## transform data w/ log to normalize (sort of)
hist(players$Mean_CP) ## before
players$Mean_CP <- log(players$Mean_CP)
hist(players$Mean_CP) ## after

hist(players$Std_CP) ## before
players$Std_CP <- log(players$Std_CP)
hist(players$Std_CP) ## after

results7<-kmeans(players[,c(5,6)],2)
results8<-kmeans(players[,c(5,6)],3)
results9<-kmeans(players[,c(5,6)],4)

fviz_nbclust(players[,c(5,6)], kmeans, method = "wss") #+
#geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(players[,c(5,6)], method = "silhouette", 
             FUN = hcut, k.max = 5)

fviz_cluster(results7, data = players[,c(5,6)],
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results7
fviz_cluster(results8, data = players[,c(5,6)],
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results8
fviz_cluster(results9, data = players[,c(5,6)],
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results9


## remove outliers + redo cluster
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

results10<-kmeans(players_no_outliers[,c(5,6)],2)
results11<-kmeans(players_no_outliers[,c(5,6)],3)
results12<-kmeans(players_no_outliers[,c(5,6)],4)

fviz_nbclust(players_no_outliers[,c(5,6)], kmeans, method = "wss") #+
#geom_vline(xintercept = 3, linetype = 2)
fviz_nbclust(players_no_outliers[,c(5,6)], method = "silhouette", 
             FUN = hcut, k.max = 5)

fviz_cluster(results10, data = players_no_outliers[,c(5,6)],
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results10
fviz_cluster(results11, data = players_no_outliers[,c(5,6)],
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results11
fviz_cluster(results12, data = players_no_outliers[,c(5,6)],
             ellipse.type = "convex",
             palette = "jco",
             axes = c(1, 3), 
             ggtheme = theme_minimal(),
             main='Cluster of Mean and Std CP')
results12
