#SOM:
library(kohonen)
library(ggplot2)
library(factoextra)
library(mlr)


write("Koos", file = "Werk sdkoos",append = T)
matrix_for_SOM_backup =  matrix_for_SOM
df_for_SOM <- df_guns_dummy

str(df_guns_dummy)

df_normal <- normalizeFeatures(df_for_SOM,method = "range", range = c(0,1))

sum(apply(df_normal,2,max)) #the sum of the maximum values per column should be 1
sum(apply(df_normal,2,min)) #the sum of the minimum values per colum should be 0
#We scale due to the fact that we make use of a distance measure such as euclidean, so that
#each variable has the same weight and not depedning on the measured unit 1km vs 1000meters vs 100000cm


mySomOuts <- function(Grid_Name,GRID_DIM, ITERATIONS, inputMAt){
  ads.train <- as.matrix(inputMAt)
  ads.grid <- kohonen::somgrid(xdim = GRID_DIM, ydim = GRID_DIM, topo = "hexagonal", neighbourhood.fct = "gaussian", toroidal = F)
  ads.model <- som(ads.train, ads.grid, rlen =ITERATIONS, keep.data = TRUE, dist.fcts = "euclidean", mode = "pbatch", cores = -1)

  save.image()
  
  
  write(Grid_Name,Grid_Name,append = T)
  write(paste("Iterations: ", ITERATIONS), Grid_Name, append = T)
  capture.output(summary(ads.model),file = Grid_Name,append = T)
  
  
  
  #CLUSTERING
  coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
  
  set.seed(1)
  

  wss_plot <- fviz_nbclust(ads.model$codes[[1]], kmeans, method = "wss", k.max = 20)
  png(paste("SOM_",Grid_Name,"_WSS.png",sep = ""), width = 1000, height = 1000)
  plot(wss_plot)
  dev.off()
  

  
  silhouette_clust <- fviz_nbclust(ads.model$codes[[1]], kmeans, method = "silhouette", k.max = 25)
  png(paste("SOM_",Grid_Name,"_SILHOUETTE.png",sep = ""), width = 1000, height = 1000)
  plot(silhouette_clust)
  dev.off()
  
  
  top5ClusterSizes <- order(silhouette_clust$data[,2],decreasing = T)[1:5]
  top5ClusterSizes <- top5ClusterSizes[top5ClusterSizes < 20]
  top5ClusterSizes
  
  write("Total Number of optimal clusters:", Grid_Name, append = T)
  write(top5ClusterSizes, Grid_Name, append = T)
  

  savePlotImages <- function(SOM_Size,som_model,SOM_CLUSTER,clustSIZE)
  {
    #SOM_Size = String, dimentions of the som
    
    #Getting working directory
    Current_WorkingDr <- getwd()
    
    #Creating the folder name
    fileNAME = paste("SOM_DIM_",SOM_Size,"_CLUST_",clustSIZE, sep = '')
    #Creating folder
    dir.create(fileNAME)
    #Setting directory
    setwd(file.path(Current_WorkingDr,fileNAME))
    
    #--------------OUTPUT of CLUSTERS
    #DO KMEANS
    set.seed(652)
    SOM_CLUSTER <- kmeans(som_model$codes[[1]], clustSIZE)
    
    
    #plottng the clusters and observations in each cluster
    set.seed(652)
    png(paste("SOM_",SOM_Size,"_NoCLUST_",clustSIZE,"_fviz_CLUST.png",sep = ""),
        width = 1000, height = 1000)
    plot(fviz_cluster(SOM_CLUSTER,som_model$codes[[1]]))
    dev.off()
    
    #Plotting te MAPPING--------------------
    png(paste("SOM_",SOM_Size,"_NoCLUST_",clustSIZE,"_MAPPING_SOM.png",sep = ""),
        width = 1000, height = 1000)
    plot(som_model, type = "mapping", bgcol = rainbow(16)[SOM_CLUSTER$cluster], main = "Cluster Map")
    add.cluster.boundaries(som_model, SOM_CLUSTER$cluster)
    dev.off()
    
    #---------------------------SOM-STATS---------------------------------------
    #MAPPING
    png(paste("SOM_",SOM_Size,"_NoCLUST_",clustSIZE,"_MAPPING.png",sep = ""),
        width = 1000, height = 1000)
    plot(ads.model,type = "mapping", pchs = 19, shape = "round")
    dev.off()
    
    #CODES
    png(paste("SOM_",SOM_Size,"_NoCLUST_",clustSIZE,"_CODES.png",sep = ""),
        width = 1000, height = 1000)
    plot(ads.model, type = "codes", main = "Codes Plot", palette.name = rainbow)
    add.cluster.boundaries(som_model, SOM_CLUSTER$cluster)
    dev.off()
    
    #CHANGES
    png(paste("SOM_",SOM_Size,"_NoCLUST_",clustSIZE,"_Training.png",sep = ""),
        width = 1000, height = 1000)
    plot(ads.model, type = "changes")
    dev.off()
    
    #Counts
    png(paste("SOM_",SOM_Size,"_NoCLUST_",clustSIZE,"_counts.png",sep = ""),
        width = 1000, height = 1000)
    plot(ads.model, type = "counts", main = "Counts", palette.name = coolBlueHotRed)
    dev.off()
    
    #Dists
    png(paste("SOM_",SOM_Size,"_NoCLUST_",clustSIZE,"_NeigbourDISTS.png",sep = ""),
        width = 1000, height = 1000)
    plot(ads.model, type = "dist.neighbours")
    dev.off()
    
   
    png(paste("SOM_",SOM_Size,"_NoCLUST_",clustSIZE,"_NeigbourDISTS_CLUST.png",sep = ""),
        width = 1000, height = 1000)
    plot(ads.model, type = "dist.neighbours")
    add.cluster.boundaries(som_model, SOM_CLUSTER$cluster)
    dev.off()
    
    
    #-----------------------------SAVING THE ATTRIBUTES------------------------------
    #Extracting number of columns
    NUMBER_COL<- ncol(as.data.frame(som_model$data))
    #ColumnNames
    ModelAttNames <- colnames(as.data.frame(ads.model$data))
    
    #Colourpalette
    coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
    
    #Saving each feature
    for (i in 1:NUMBER_COL){
      png(paste("SOM_",SOM_Size,"_NoCLUST_",clustSIZE,"_",ModelAttNames[i],".png",sep = ""),
          width = 1000, height = 1000)
      
      plot(som_model, type = "property", property = getCodes(som_model)[,i], 
           main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)
      
      add.cluster.boundaries(som_model, SOM_CLUSTER$cluster)
      dev.off()
      graphics.off()
    }
    
    
    #Setting back to old directory
    setwd(Current_WorkingDr)
    
    
  }
  
  for(i in top5ClusterSizes)
    savePlotImages(Grid_Name,som_model = ads.model,clustSIZE =  i)
  
  write("\n\n",Grid_Name,append = T)
  return(ads.model)
}


mySom_85_1000 <- mySomOuts("85x85_1000",85,1000,df_normal)
save.image()
#
mySom_85_750 <- mySomOuts("85x85_750",85,750,df_normal)
save.image()
#

mySom_85_500 <- mySomOuts("85x85_500",85,500,df_normal)
save.image()
#

mySom_85_250 <- mySomOuts("85x85_250",85,250,df_normal)
save.image()
#

############SOM 70
mySom_70_1000 <- mySomOuts("70x70_1000",70,1000,df_normal)
save.image()
#
mySom_70_750 <- mySomOuts("70x70_750",70,750,df_normal)
save.image()
#

mySom_70_500 <- mySomOuts("70x70_500",70,500,df_normal)
save.image()
#

mySom_70_250 <- mySomOuts("70x70_250",70,250,df_normal)
save.image()
#


#############______60x60
mySom_45_2500 <- mySomOuts("45x45_2500",45,2500,df_normal)
save.image()
#
mySom_60_750 <- mySomOuts("60x60_750",60,750,df_normal)
save.image()
#

mySom_60_500 <- mySomOuts("60x60_500",60,500,df_normal)
save.image()
#

mySom_60_250 <- mySomOuts("60x60_250",60,250,df_normal)
save.image()
#

mySom_10_1500<- mySomOuts("10x10_5000",10,5000,df_normal)
save.image()
mySom_10_750<- mySomOuts("10x10_750",10,750,df_normal)
save.image()
mySom_10_500<- mySomOuts("10x10_500",10,500,df_normal)
save.image()
mySom_10_250<- mySomOuts("10x10_250",10,250,df_normal)
save.image()

#







