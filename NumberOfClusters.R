install.packages("dplyr")
install.packages("knitr")
install.packages("gclus")
install.packages("fpc")
install.packages("cluster")
install.packages("vegan")
install.packages("mclust")
install.packages("apcluster")
install.packages("gclus")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("clValid")



library(dplyr)
library(knitr)
library(gclus)
library(fpc)
library(cluster)
library(vegan)
library(mclust)
library(apcluster)
library(ggplot2)
library(dplyr)
library(clValid)
library(kohonen)
library(ggplot2)
library(factoextra)
library(mlr)



mySom_60_1000

MySOM_60 <- mySom_60_1000
MySom_45 <- mySom_45_2500
summary(MySom_45)

#WSS
wss_plot <- fviz_nbclust(MySom_45$codes[[1]], kmeans, method = "wss", k.max = 15)
dev.new()
plot(wss_plot, cex.lab= 1.50, main = "WSS optimal number of clusters")

#SIllhouette
silhouette_clust <- fviz_nbclust(MySom_45$codes[[1]], kmeans, method = "silhouette", k.max = 15)
dev.new()
plot(silhouette_clust)
#Sillhouette
pamk.best2 <- pamk(MySom_45$codes[[1]])
pamk.best2$nc
#2

summary(MySOM_60)
#0.446

#Bootsrap
model_gapStat <- fviz_nbclust(MySom_45$codes[[1]],kmeans, method = "gap_stat", k.max = 10, nboot = 100)
dev.new()
model_gapStat

save.image()
#nboot = 500 = 4



#CALINSKY CRITERION
cal_fit2 <- cascadeKM(MySom_45$codes[[1]], 1, 10, iter = 1000)
dev.new()
plot(cal_fit2, sortg = TRUE, grpmts.plot = TRUE)
#2 clusters


#BAYESIAN INFORMATION CRITERION FOR EXPECTATION - MAXIMIZATION
d_clust2 <- Mclust(MySOM_60$codes[[1]], G=1:10)
?Mclust
m.best2 <- dim(d_clust2$z)[2]
save.image()
plot(d_clust2)
cat("model-based optimal number of clusters:", m.best2, "\n")
#2

#AFFINITY PROPAGATION CLUSTERING
d.apclus2 <- apcluster(negDistMat(r=2), MySOM_60$codes[[1]])
cat("affinity propogation optimal number of clusters:", length(d.apclus2@clusters), "\n")
heatmap(d.apclus2)


#Validation Tests

valid_test <- clValid(mySom_10_1500$codes[[1]], c(2:10),
                      clMethods = c("kmeans"),
                      validation = c("internal", "stability"))

summary(valid_test)
save.image()
cluster_stats_60 <- valid_test


