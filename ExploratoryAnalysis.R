#Exploratory Analysis
summary(MySOM_60)


#---------------------------SOM-STATS---------------------------------------
som_stats <- function(ads.model,noClust){
  coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
  
  set.seed(1)
  SOM_CLUSTER <- kmeans(ads.model$codes[[1]], noClust)
  dev.new()
  plot(ads.model,type = "mapping", pchs = 19, shape = "round")
  
  
  dev.new()
  par(cin =  c(1000,1000))
  plot(ads.model, type = "mapping", bgcol = rainbow(16)[SOM_CLUSTER$cluster], main = "Cluster Map")
  add.cluster.boundaries(ads.model, SOM_CLUSTER$cluster, lwd = 6)
  

  dev.new()
  plot(ads.model, type = "changes")
  
  dev.new()
  plot(ads.model, type = "counts", main = "Counts", palette.name = coolBlueHotRed, heatkeywidth = 0.8)
  

  dev.new()
  plot(ads.model, type = "dist.neighbours",heatkeywidth = 0.8)
  add.cluster.boundaries(ads.model, SOM_CLUSTER$cluster )


}

som_stats(MySOM_60,3)


set.seed(1)
SOM_CLUSTER <- kmeans(MySOM_60$codes[[1]], 2)

coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
for (i in 1:15){
  dev.new()
  plot(MySOM_60, type = "property", property = getCodes(MySOM_60)[,i], 
       main=colnames(getCodes(MySOM_60))[i], palette.name=coolBlueHotRed)
  add.cluster.boundaries(MySOM_60, SOM_CLUSTER$cluster)
}

test <- getCodes(MySOM_60)[,15]
test

length(getCodes(MySOM_60)[,15])
 