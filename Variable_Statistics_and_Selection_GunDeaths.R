df_guns <- read.csv("gunKillingsInUSA.csv",header = T)

which(is.na(df_guns$Victim.Age))

df_guns <- df_guns[,-c(1,4,8)]

colNames<- c("City","State","Victim_Age","Victim_AgeGrp","Victim_Gender","Long","Lat","State_pop","BG_Checks",
             "LG_SP","LG_FR","LG_AL","LG_OL","LG_CP","LG_OC","LG_SLR","LG_NFAR","LG_PJ",
             "HG_SP","HG_FR","HG_AL","HG_OL","HG_CP","HG_OC","HG_SLR","HG_NFAR","HG_PJ",
             "Killed_year","Killed_month","Killed_day","Killed_DayOfweek")

colnames(df_guns) <- colNames

#Dropping 7.37% of the variables which is NA to avoid imputing
df_guns <- df_guns[!is.na(df_guns$Victim_Age),]

####-------------Transforming variables so that they are factor/categorical variables ---------------------------
str(df_guns)
#Month = Categorical
df_guns$Killed_month <- factor(df_guns$Killed_month)
#Day of week
df_guns$Killed_DayOfweek <- factor(df_guns$Killed_DayOfweek)

#Checking to see if succesful
str(df_guns)

####--------------- Renaming factor variables in R for descirptive attribute names---------------------
#Killed month
levels(df_guns$Killed_month) <- c("Jan","Feb","March","Apr","May","Jun","Jul","Aug","Sept","Dec")

#Killed day of week
levels(df_guns$Killed_DayOfweek) <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

#### ----GENDER
#Before
levels(df_guns$Victim_Gender)
#After
levels(df_guns$Victim_Gender)  <- c("Missing","Female","Male")
levels(df_guns$Victim_Gender)

#### ----LG_SP ----
#Before
levels(df_guns$LG_SP)
#After
levels(df_guns$LG_SP)  <- c("Missing","No","Yes")
levels(df_guns$LG_SP)

#### ----LG_AL ----
#Before
levels(df_guns$LG_AL)
#After
levels(df_guns$LG_AL)  <- c("Missing","No","Yes")
levels(df_guns$LG_AL)


#### ----LG_FR ----
#Before
levels(df_guns$LG_FR)
#After
levels(df_guns$LG_FR)  <- c("Missing","No","Yes")
levels(df_guns$LG_FR)


#### ----LG_OL ----
#Before
levels(df_guns$LG_OL)
#After
levels(df_guns$LG_OL)  <- c("Missing","No","Yes")
levels(df_guns$LG_OL)


#### ----LG_CP ----
#Before
levels(df_guns$LG_CP)
#After
levels(df_guns$LG_CP)  <- c("Missing","No","Yes")
levels(df_guns$LG_CP)



#### ----LG_OC ----
#Before
levels(df_guns$LG_OC)
#After
levels(df_guns$LG_OC)  <- c("Missing","No","Yes")
levels(df_guns$LG_OC)


#### ----LG_SLR ----
#Before
levels(df_guns$LG_SLR)
#After
levels(df_guns$LG_SLR)  <- c("Missing","No","Partial","Yes")
levels(df_guns$LG_SLR)


#### ----LG_NFAR ----
#Before
levels(df_guns$LG_NFAR)
#After
levels(df_guns$LG_NFAR)  <- c("Missing","No","Partial","Yes")
levels(df_guns$LG_NFAR)

#### ----LG_PJ ----
#Before
levels(df_guns$LG_PJ)
#After
levels(df_guns$LG_PJ)  <- c("Missing","No","Yes")
levels(df_guns$LG_PJ)


####------->>>>>>>>>> HAND GUNS <<<<<<<<<--------------------

#### ----HG_SP ----
#Before
levels(df_guns$HG_SP)
#After
levels(df_guns$HG_SP)  <- c("Missing","No","Partial","Yes")
levels(df_guns$HG_SP)

#### ----HG_AL ----
#Before
levels(df_guns$HG_AL)
#After
levels(df_guns$HG_AL)  <- c("Missing","No","Yes")
levels(df_guns$HG_AL)


#### ----HG_FR ----
#Before
levels(df_guns$HG_FR)
#After
levels(df_guns$HG_FR)  <- c("Missing","No","Partial","Yes")
levels(df_guns$HG_FR)


#### ----HG_OL ----
#Before
levels(df_guns$HG_OL)
#After
levels(df_guns$HG_OL)  <- c("Missing","No","Yes")
levels(df_guns$HG_OL)


#### ----HG_CP ----
#Before
levels(df_guns$HG_CP)
#After
levels(df_guns$HG_CP)  <- c("Missing","No","Shall-Issue","Yes")
levels(df_guns$HG_CP)



#### ----HG_OC ----
#Before
levels(df_guns$HG_OC)
#After
levels(df_guns$HG_OC)  <- c("Missing","No","Yes")
levels(df_guns$HG_OC)


#### ----HG_SLR ----
#Before
levels(df_guns$HG_SLR)
#After
levels(df_guns$HG_SLR)  <- c("Missing","No","Partial","Yes")
levels(df_guns$HG_SLR)


#### ----HG_NFAR ----
#Before
levels(df_guns$HG_NFAR)
#After
levels(df_guns$HG_NFAR)  <- c("Missing","No","Partial","Yes")
levels(df_guns$HG_NFAR)

#### ----HG_PJ ----
#Before
levels(df_guns$HG_PJ)
#After
levels(df_guns$HG_PJ)  <- c("Missing","No","Yes")
levels(df_guns$HG_PJ)


str(df_guns)






#------END OF FIXING MISSING VARIABLES ------------

#Extracting data statistics:
summary(df_guns)


#Investigating the STATE VARIABLE
length(unique(df_guns$State))
sum(is.na(df_guns$State))

#Droping varaibles:
colnames(df_guns)
#City
#Age group
#Longitude
#Latitude
#Year Killed
#Day Killed
df_guns <- df_guns[,-c(1,4,6,7,28,30)]



###-------------Quick dataset Stats -------------------------
ncol(df_guns)
#Number of dimentions: 25
nrow(df_guns)
#Number of rows: 8306
#Number of rows after ommit: 7692 ... ... ... + 614(missing) = 8306

###----- Removing Outliers--------
boxplot(na.omit(df_guns$Victim_Age))

summary(df_guns$Victim_Age)

quant_out <- quantile((df_guns$Victim_Age), probs = c(0.005,0.995),na.rm = T)

#Quantiles
quant1 <- quant_out[[1]]
quant2 <- quant_out[[2]]


#Extracting all the values that is larger than the 0.5% quantile
test_guns_age <-  df_guns$Victim_Age[df_guns$Victim_Age > quant1]
length(test_guns_age) 
#Remainder 7644

#Extracting all the values that is smaller than the 99.5% quantile from the values that is larger than the 0.5% quantile
test_guns_age <- test_guns_age[test_guns_age < quant2]
length(test_guns_age)
#Remainder 8216 values

#Drawing a histogram of the remved values compared to the original

draw_Age_hist <- function()
  {
    dev.new()
    par(mfrow = c(1,2))
    
    hist(df_guns$Victim_Age, freq = F, main = "Histogram before pre-processing", ylim = c(0,0.045))
    lines(density(na.omit(df_guns$Victim_Age)), col = "red", lwd = 3)
    
    hist(test_guns_age, freq = F, main = "Histogram Outliers Removed",ylim = c(0,0.045))
    lines(density(na.omit(test_guns_age)), col = "red", lwd = 3)
  }
draw_Age_hist()


#Removing outliers from the whole dataset

mean(na.omit(df_guns$Victim_Age))
#32.97803

df_guns <- df_guns[df_guns$Victim_Age > quant1,]
df_guns <- df_guns[df_guns$Victim_Age < quant2,]
mean(na.omit(df_guns$Victim_Age))

nrow(df_guns)
#Reduced to 8216
((8306-7602)/8306)*100
#1.08% reduction in the dataset



####---------------Creating the dummy variables-------------------------------
library(mlr)
colnames(df_guns)
df_guns_dummy <- createDummyFeatures(df_guns, 
                                     cols = c("State","Victim_Gender","LG_SP","LG_FR","LG_AL",
                                              "LG_OL" ,"LG_CP","LG_OC","LG_SLR","LG_NFAR","LG_PJ","HG_SP","HG_PJ",
                                              "HG_FR","HG_AL","HG_OL","HG_CP","HG_OC","HG_SLR","HG_NFAR","Killed_month",
                                              "Killed_DayOfweek"))
matrix_for_SOM <- as.matrix(df_guns_dummy)
colnames(df_guns_dummy)
ncol(df_guns_dummy) #135 Dimentions

