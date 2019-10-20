#install.packages("kohonen")
#install.packages("class")

library(kohonen)
library(ggplot2)
library(dummy)
library(factoextra)
library(mlr)

####------------------ Reading th data file-----------------------------

guns <- read.csv("gunKillingsInUSA.csv",header = T) 
guns.transf <- guns

str(guns)
length((guns.transf[1,]))
?apply
#Dataset characteristics
sum(na.omit(as.vector(apply((guns.transf== ""),1,sum) > 1)))
sum((guns.transf== "Yes"))


####--------------------DROPPING VARIABLES & REASONS ---------------------------------


#### ----------------------------- Variables to drop -------------------------------
#Dropping the victim name, source
guns.transf <- guns.transf[,-c(1,4,8)]
#Obvious variables to drop
#Date killed == there is simular columns already
#Victim name == it is a very sparsie field with only unique values
#Source of information, it is unique and has no effect on whetere the deaths occured or not
#Longitude and latitude, due to the unique ness and that the information is already captured within the states
#Dropping the year variable, all the data was observed over a year

####----------- Changing the column names---------------------------------

colNames<- c("City","State","Victim_Age","Victim_AgeGrp","Victim_Gender","Long","Lat","State_pop","BG_Checks",
             "LG_SP","LG_FR","LG_AL","LG_OL","LG_CP","LG_OC","LG_SLR","LG_NFAR","LG_PJ",
             "HG_SP","HG_FR","HG_AL","HG_OL","HG_CP","HG_OC","HG_SLR","HG_NFAR","HG_PJ",
             "Killed_year","Killed_month","Killed_day","Killed_DayOfweek")

names(guns.transf)<- colNames

####------------------END OF NAME CHANGES ---------------------------------


#Dropping the name
length(unique(guns.transf$Victim.Name))
7365/8306

#Dropping the source
length(unique(guns.transf$Source))
7190/8306
summary(guns.transf)
str(guns.transf)

#LONGITUDE AND LATETUDE
library(rworldmap)
newmap <- getMap(resolution = "high")

dev.new()
plot(newmap,xlim = c(min(guns.transf$Lat),max(guns.transf$Lat)), ylim = c(min(guns.transf$Long),max(guns.transf$Long)))
points(guns.transf$Lat,guns.transf$Long,
       main = "Location of deaths plot",
       xlab = "Latitude", ylab = "Longitude", 
       col = 'red', pch = 16, cex = 0.7)

length(unique(guns.transf$Long))
length(unique(guns.transf$Lat))

length(factor(guns.transf$State))

sort(summary(guns.transf$State),decreasing = T)
#Califonia == most deaths 945

df_California <- guns.transf[guns.transf$State == "California",]
#Sorting by Longitude
(df_California[order(df_California$City),])

hist(df_California$Lat)

nrow(unique(guns.transf[,c(6,7)]))
2703/8703

#---- Cities
df_stateCity <- guns.transf[,c(1,2)]
df_stateCityUnique = unique(df_stateCity)
nrow(df_stateCityUnique)

df_stateCityUnique[order(df_stateCityUnique$City),][seq(2000,2500),]
nrow(unique((df_stateCity )))


table(guns.transf$LG_OC)
3866/8307


length(colnames(guns.transf))
####------STATISTICS ABOUT THE DATA-----------------------------------

#City
length(unique(guns.transf$City))
sapply(unique(guns.transf$City), count)

dev.new()
barplot(table(guns.transf$City),las = 2)

#STATE
dev.new()
barplot(table(guns.transf$State),las = 2)


#Victim age
dev.new()
hist(guns.transf$Victim_Age, freq = F, main = c("Histogram of Victim Age"), xlab= 'Victim Age', ylim = c(0,0.04) )
lines(density(x = na.omit(guns.transf$Victim_Age)), col = 'blue', lwd = 2)

summary(guns.transf$Victim_Age)

#NA omitted




#AgeGroup
dev.new()
barplot(table(guns.transf$Victim_AgeGrp), freq =T, main = c("Barplot of Victim Age"), xlab= 'Victim Age Group')


#Victim gender
dev.new()
barplot(table(guns.transf$Victim_Gender), freq =T, main = c("Barplot of Victim Gender"), xlab= 'Victim Age Group')

# LONGITUDE AND LATITUDE DATA
#install.packages("rworldmap")


#Drop these variables

#State Population
dev.new()
hist(guns.transf$State_pop, freq = F, main = c("Histogram of State populations"), xlab= 'State populations')
lines(density(x = na.omit(guns.transf$State_pop)), col = 'blue', lwd = 2)
#do not drop, more pop, more people to be abled to die

#State Background checks
dev.new()
#par(mfrow = c(2,1))
hist(guns.transf$BG_Checks, freq = T, main = c("Histogram of Background checks"), xlab= 'Background checks', ylim = c(0,0.00013)
    , breaks = 100 )
lines(density(x = na.omit(guns.transf$BG_Checks)), col = 'blue', lwd = 2)
boxplot(guns.transf$BG_Checks)
?hist


#Plotting Gun laws
plot_laws <- function(from, to){
  for (i in from:to) {
    dev.new()
    barplot(table(guns.transf[,i]), main = colnames(guns.transf)[i], col = c("red","green","blue","orange"))
  }
}
plot_laws(10,28)


#Killed year
dev.new()
hist(guns.transf$Killed_year, freq = F, main = c("Histogram of year killed"), xlab= 'Killed Year',)
lines(density(x = na.omit(guns.transf$Killed_year)), col = 'blue', lwd = 2)
#unnessecary

#Killed month
install.packages("swfscMisc")
library(swfscMisc)
dev.new()
hist.out = hist(guns.transf$Killed_month, freq = F, main = c("Histogram of month killed"), xlab= 'Month',)
lines(density(x = na.omit(guns.transf$Killed_month)), col = 'blue', lwd = 2)


#unnessecary

#Killed Day
dev.new()
killed_hist = hist(guns.transf$Killed_day, freq =F , main = c("Histogram of day killed"), xlab= 'Day',)
lines(density(x = na.omit(guns.transf$Killed_day)), col = 'blue', lwd = 2)
#unifrom, except for the 31st becuase it not present each month

#Day of Week Killed
dev.new()
hist(guns.transf$Killed_week, freq = F, main = c("Histogram of day killed"), xlab= 'week')
lines(density(x = na.omit(guns.transf$Killed_week)), col = 'blue', lwd = 2)

length(unique(guns.transf$Killed_week))



#-------DROPPING FINAL VARIABLES
colnames(guns.transf)
#Dropping final variables
guns.transf <- guns.transf[,-c(1,4,6,7,28)]
colnames(guns.transf)


####------------------Function for dummies --------------------------


matwithDummy <- createDummyFeatures(guns.transf, cols = c("State","Victim_Gender","LG_SP","LG_FR","LG_AL",
                                          "LG_OL" ,"LG_CP","LG_OC","LG_SLR","LG_NFAR","LG_PJ","HG_SP","HG_PJ",
                                          "HG_FR","HG_AL","HG_OL","HG_CP","HG_OC","HG_SLR","HG_NFAR"))
matforSOG <- as.matrix(matwithDummy)
#Variables with "V1" in its name had missing values
str(matwithDummy)

colnames(matwithDummy)


#####---------Data processing for the SOM
ads.train <- as.matrix(scale(matforSOG))
ads.grid <- kohonen::somgrid(xdim = 30, ydim = 30, topo = "hexagonal")
ads.model <- supersom(ads.train, ads.grid, rlen =200, radius = 2.5, keep.data = TRUE, dist.fcts = "euclidean")


?somgrid

str(ads.model)

dev.new()
plot(ads.model)

ads.model$unit.classif

dev.new()
plot(ads.model,type = "mapping", pchs = 19, shape = "round")
dev.new()
plot(ads.model, type = "codes", main = "Codes Plot", palette.name = rainbow)
dev.new()
plot(ads.model, type = "changes")
dev.new()
plot(ads.model, type = "counts")
dev.new()
plot(ads.model, type = "dist.neighbours")

?plot

heatmap.som <- function(model){
  for (i in 1:15) {
    dev.new()
    plot(model, type = "property", property = getCodes(model)[,i], 
         main = colnames(getCodes(model))[i]) 
  }
}
heatmap.som(ads.model)



fviz_nbclust(ads.model$codes[[1]], kmeans, method = "wss")
clust <- kmeans(ads.model$codes[[1]], 7)

dev.new()
plot(ads.model, type = "codes", bgcol = rainbow(16)[clust$cluster], main = "Cluster Map")
add.cluster.boundaries(ads.model, clust$cluster)


###READ THE FOLLOWING ON CATEGORICAL###
#https://towardsdatascience.com/an-overview-of-categorical-input-handling-for-neural-networks-c172ba552dee
