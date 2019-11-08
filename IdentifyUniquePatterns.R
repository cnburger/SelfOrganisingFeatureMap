#number of duplicated patterns
sum(duplicated(df_guns))
#464

#Number of unique patterns
sum(!duplicated(df_guns))
#7138

7138+464

#The number of neurons should be +- = to nuber of unque patterns
#7138 unique patterns... sqrt(7138) will give the grid dimentions
sqrt(7138)
#85*85

#minimim
#135
sqrt(135)
#12*12

install.packages("som")

?som

dev.new()
plot(ads.model, type = "quality")

mode <- c("online","batch","pbatch")
match.arg(mode)

install.packages("Smisc")
library(Smisc)


Lin_depRows <- findDepMat(as.matrix(df_normal), rows = TRUE, tol = 1e-10)



install.packages("plm")
library(plm)
?detect.lindep
ans <-t(as.matrix(df_normal))
linDep_rows <- detect.lindep(ans)
save.image()




