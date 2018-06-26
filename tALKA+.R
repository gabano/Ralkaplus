#########################
# 	tALKA+  	          #
#########################
# Author: João Gabriel Nogueira (Lab. Limnologia / UFRJ). 2016.
# Github: @gabano
# This file may be distributed and/or modified, provided that the author is credited. 
#
# To use this file, create a .csv (separated by ;) file containing the following header and information
# id samplev ph1 v1 ph2 v2 ph3 v3 correction_factor
# sample1 100 3.79 0.83 3.5 1.33 3.34 1.83 1.07793
# Where samplev is the sample volume in mL and v1, v2 and v3 are the instilled volumes of 0.01 N acidic solution.
# The corretion_fator is the correction factor for the 0,01 N acidic solution used in the titration. 

# Choose the .csv file


alcdata <- read.csv2(file.choose())
rm(AlcT)
rm(AlcTx)
alcdata <- na.omit(alcdata)
sample_v <- as.double(as.character(alcdata$samplev))
v1        <- as.double(as.character(alcdata$v1))
v2        <- as.double(as.character(alcdata$v2))
v3        <- as.double(as.character(alcdata$v3))

ph1       <- as.double(as.character(alcdata$ph1))
ph2       <- as.double(as.character(alcdata$ph2))
ph3       <- as.double(as.character(alcdata$ph3))
correction_factor <- as.double(as.character(alcdata$correction_factor))

for(i in 1:nrow(alcdata)) {

# F = VolumeTotal * 10^(-ph)
f1 <- (v1[i]+sample_v[i])*(10^(-ph1[i]))
f2 <- (v2[i]+sample_v[i])*(10^(-ph2[i]))
f3 <- (v3[i]+sample_v[i])*(10^(-ph3[i]))

vetorF <- c(f1, f2, f3)
vetorph <- c(ph1[i], ph2[i], ph3[i])
vetorV <- c(v1[i], v2[i], v3[i])

regressao <- lm(vetorV ~ vetorF)

veq <- round(as.numeric(regressao$coefficients[1]),5) 
veq 

AlcTx <-  ((veq*(10^-3))*0.01*correction_factor[i]*1000)*10000 

if (exists("AlcT")) {
  AlcT <- c(AlcT, AlcTx)
} else {
  AlcT <- as.vector(c(AlcTx))
}

}
new.df <- cbind(id = as.vector(alcdata$id), AlcT)

write.csv2(new.df, file.choose(), dec=",", row.names = F)
