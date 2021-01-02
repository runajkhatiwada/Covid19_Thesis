#rm(list=ls())
options(scipen=999)
# Load the library
library(surveillance)

# load over all data set  CSV File attached
covid.df <- read.csv("C:/Users/runajk/Documents/Study/Thesis/data.set.csv")

summary(covid.df)

# create a 42*21 matrix for all the ICU cases in the counties
matrix1 <- data.matrix(covid.df[covid.df$Region == "Blekinge",]$ICU.Cases, rownames.force = FALSE)
matrix2 <- data.matrix(covid.df[covid.df$Region == "Dalarna",]$ICU.Cases, rownames.force = FALSE)
matrix3 <- data.matrix(covid.df[covid.df$Region == "Gavleborg",]$ICU.Cases, rownames.force = FALSE)
matrix4 <- data.matrix(covid.df[covid.df$Region == "Gotland",]$ICU.Cases, rownames.force = FALSE)
matrix5 <- data.matrix(covid.df[covid.df$Region == "Halland",]$ICU.Cases, rownames.force = FALSE)
matrix6 <- data.matrix(covid.df[covid.df$Region == "Jamtland",]$ICU.Cases, rownames.force = FALSE)
matrix7 <- data.matrix(covid.df[covid.df$Region == "Jonkoping",]$ICU.Cases, rownames.force = FALSE)
matrix8 <- data.matrix(covid.df[covid.df$Region == "Kalmar",]$ICU.Cases, rownames.force = FALSE)
matrix9 <- data.matrix(covid.df[covid.df$Region == "Kronoberg",]$ICU.Cases, rownames.force = FALSE)
matrix10 <- data.matrix(covid.df[covid.df$Region == "Norrbotten",]$ICU.Cases, rownames.force = FALSE)
matrix11 <- data.matrix(covid.df[covid.df$Region == "Orebro",]$ICU.Cases, rownames.force = FALSE)
matrix12 <- data.matrix(covid.df[covid.df$Region == "Ostergotland",]$ICU.Cases, rownames.force = FALSE)
matrix13 <- data.matrix(covid.df[covid.df$Region == "Skane",]$ICU.Cases, rownames.force = FALSE)
matrix14 <- data.matrix(covid.df[covid.df$Region == "Sodermanland",]$ICU.Cases, rownames.force = FALSE)
matrix15 <- data.matrix(covid.df[covid.df$Region == "Stockholm",]$ICU.Cases, rownames.force = FALSE)
matrix16 <- data.matrix(covid.df[covid.df$Region == "Uppsala",]$ICU.Cases, rownames.force = FALSE)
matrix17 <- data.matrix(covid.df[covid.df$Region == "Varmland",]$ICU.Cases, rownames.force = FALSE)
matrix18 <- data.matrix(covid.df[covid.df$Region == "Vasterbotten",]$ICU.Cases, rownames.force = FALSE)
matrix19 <- data.matrix(covid.df[covid.df$Region == "Vasternorrland",]$ICU.Cases, rownames.force = FALSE)
matrix20 <- data.matrix(covid.df[covid.df$Region == "Vastmanland",]$ICU.Cases, rownames.force = FALSE)
matrix21 <- data.matrix(covid.df[covid.df$Region == "Vastra Gotaland",]$ICU.Cases, rownames.force = FALSE)


ICU.matrix = cbind(matrix1, matrix2, matrix3, matrix4, matrix5, matrix6, matrix7, matrix8, matrix9, matrix10, matrix11, matrix12, matrix13, matrix14, matrix15, matrix16, matrix17, matrix18, matrix19, matrix20, matrix21) 
colnames(ICU.matrix) = c('Blekinge','Dalarna','Gavleborg','Gotland','Halland','Jamtland','Jonkoping','Kalmar','Kronoberg','Norrbotten','Orebro','Ostergotland','Skane','Sodermanland','Stockholm','Uppsala','Varmland','Vasterbotten','Vasternorrland','Vastmanland','Vastra Gotaland')


# create a 42*21 matrix for populaiton of the counties
population <- data.matrix(unique(covid.df$Population.per.10000 * 10000), rownames.force = FALSE)
population <- t(population)

colnames(population) = c('Blekinge','Dalarna','Gavleborg','Gotland','Halland','Jamtland','Jonkoping','Kalmar','Kronoberg','Norrbotten','Orebro','Ostergotland','Skane','Sodermanland','Stockholm','Uppsala','Varmland','Vasterbotten','Vasternorrland','Vastmanland','Vastra Gotaland')
for (i in 1:41) {
  population <- rbind(population, population[1,])
}


# start date of data collection
start <- c(2020, 1)

# frequency weekly
frequency <- 52

# neighbourhood matrix .. csv file attached
mat.df <- read.csv("C:/Users/runajk/Documents/Study/Thesis/matrix.csv")
neighbourhood <- data.matrix(mat.df)

# rename the columns to match in all the matrices
colnames(ICU.matrix) = gsub(" ", ".",colnames(ICU.matrix))
colnames(population) = gsub(" ", ".",colnames(population))

# create a 42*21 matrix for the policy 1 in all counties
P1 = data.matrix(covid.df[covid.df$Region == "Blekinge",]$P1, rownames.force = FALSE)

for (i in 1:20) {
  P1 <- cbind(P1, P1[,1])
}
colnames(P1) = c('Blekinge','Dalarna','Gavleborg','Gotland','Halland','Jamtland','Jonkoping','Kalmar','Kronoberg','Norrbotten','Orebro','Ostergotland','Skane','Sodermanland','Stockholm','Uppsala','Varmland','Vasterbotten','Vasternorrland','Vastmanland','Vastra Gotaland')

# create a 36*21 matrix for the policy 2 in all counties
P2 = data.matrix(covid.df[covid.df$Region == "Blekinge",]$P2, rownames.force = FALSE)

for (i in 1:20) {
  P2 <- cbind(P2, P2[,1])
}
colnames(P2) = c('Blekinge','Dalarna','Gavleborg','Gotland','Halland','Jamtland','Jonkoping','Kalmar','Kronoberg','Norrbotten','Orebro','Ostergotland','Skane','Sodermanland','Stockholm','Uppsala','Varmland','Vasterbotten','Vasternorrland','Vastmanland','Vastra Gotaland')

# create a 36*21 matrix for the policy 3 in all counties
P3 = data.matrix(covid.df[covid.df$Region == "Blekinge",]$P3, rownames.force = FALSE)

for (i in 1:20) {
  P3 <- cbind(P3, P3[,1])
}
colnames(P3) = c('Blekinge','Dalarna','Gavleborg','Gotland','Halland','Jamtland','Jonkoping','Kalmar','Kronoberg','Norrbotten','Orebro','Ostergotland','Skane','Sodermanland','Stockholm','Uppsala','Varmland','Vasterbotten','Vasternorrland','Vastmanland','Vastra Gotaland')

# create a 36*21 matrix for the policy 4 in all counties

P4 = data.matrix(covid.df[covid.df$Region == "Blekinge",]$P4, rownames.force = FALSE)

for (i in 1:20) {
  P4 <- cbind(P4, P4[,1])
}
colnames(P4) = c('Blekinge','Dalarna','Gavleborg','Gotland','Halland','Jamtland','Jonkoping','Kalmar','Kronoberg','Norrbotten','Orebro','Ostergotland','Skane','Sodermanland','Stockholm','Uppsala','Varmland','Vasterbotten','Vasternorrland','Vastmanland','Vastra Gotaland')

# create a 36*21 matrix for the policy 5 in all counties

P5 = data.matrix(covid.df[covid.df$Region == "Blekinge",]$P5, rownames.force = FALSE)

for (i in 1:20) {
  P5 <- cbind(P5, P5[,1])
}
colnames(P5) = c('Blekinge','Dalarna','Gavleborg','Gotland','Halland','Jamtland','Jonkoping','Kalmar','Kronoberg','Norrbotten','Orebro','Ostergotland','Skane','Sodermanland','Stockholm','Uppsala','Varmland','Vasterbotten','Vasternorrland','Vastmanland','Vastra Gotaland')

# create a 36*21 matrix for the policy 6 in all counties

P6 = data.matrix(covid.df[covid.df$Region == "Blekinge",]$P6, rownames.force = FALSE)

for (i in 1:20) {
  P6 <- cbind(P6, P6[,1])
}
colnames(P6) = c('Blekinge','Dalarna','Gavleborg','Gotland','Halland','Jamtland','Jonkoping','Kalmar','Kronoberg','Norrbotten','Orebro','Ostergotland','Skane','Sodermanland','Stockholm','Uppsala','Varmland','Vasterbotten','Vasternorrland','Vastmanland','Vastra Gotaland')

# create a 36*21 matrix for the policy 7 in all counties

P7 = data.matrix(covid.df[covid.df$Region == "Blekinge",]$P7, rownames.force = FALSE)

for (i in 1:20) {
  P7 <- cbind(P7, P7[,1])
}
colnames(P7) = c('Blekinge','Dalarna','Gavleborg','Gotland','Halland','Jamtland','Jonkoping','Kalmar','Kronoberg','Norrbotten','Orebro','Ostergotland','Skane','Sodermanland','Stockholm','Uppsala','Varmland','Vasterbotten','Vasternorrland','Vastmanland','Vastra Gotaland')

# create a 36*21 matrix for the policy 8 in all counties

P8 = data.matrix(covid.df[covid.df$Region == "Blekinge",]$P8, rownames.force = FALSE)

for (i in 1:20) {
  P8 <- cbind(P8, P8[,1])
}
colnames(P8) = c('Blekinge','Dalarna','Gavleborg','Gotland','Halland','Jamtland','Jonkoping','Kalmar','Kronoberg','Norrbotten','Orebro','Ostergotland','Skane','Sodermanland','Stockholm','Uppsala','Varmland','Vasterbotten','Vasternorrland','Vastmanland','Vastra Gotaland')

# create a 36*21 matrix for the policy 9 in all counties

P9 = data.matrix(covid.df[covid.df$Region == "Blekinge",]$P9, rownames.force = FALSE)

for (i in 1:20) {
  P9 <- cbind(P9, P9[,1])
}
colnames(P9) = c('Blekinge','Dalarna','Gavleborg','Gotland','Halland','Jamtland','Jonkoping','Kalmar','Kronoberg','Norrbotten','Orebro','Ostergotland','Skane','Sodermanland','Stockholm','Uppsala','Varmland','Vasterbotten','Vasternorrland','Vastmanland','Vastra Gotaland')

Week = unique(covid.df$Week.Number)
week.matrix = data.matrix(Week)

matrix1 <- data.matrix(covid.df[covid.df$Region == "Blekinge",]$TempCat, rownames.force = FALSE)
matrix2 <- data.matrix(covid.df[covid.df$Region == "Dalarna",]$TempCat, rownames.force = FALSE)
matrix3 <- data.matrix(covid.df[covid.df$Region == "Gavleborg",]$TempCat, rownames.force = FALSE)
matrix4 <- data.matrix(covid.df[covid.df$Region == "Gotland",]$TempCat, rownames.force = FALSE)
matrix5 <- data.matrix(covid.df[covid.df$Region == "Halland",]$TempCat, rownames.force = FALSE)
matrix6 <- data.matrix(covid.df[covid.df$Region == "Jamtland",]$TempCat, rownames.force = FALSE)
matrix7 <- data.matrix(covid.df[covid.df$Region == "Jonkoping",]$TempCat, rownames.force = FALSE)
matrix8 <- data.matrix(covid.df[covid.df$Region == "Kalmar",]$TempCat, rownames.force = FALSE)
matrix9 <- data.matrix(covid.df[covid.df$Region == "Kronoberg",]$TempCat, rownames.force = FALSE)
matrix10 <- data.matrix(covid.df[covid.df$Region == "Norrbotten",]$TempCat, rownames.force = FALSE)
matrix11 <- data.matrix(covid.df[covid.df$Region == "Orebro",]$TempCat, rownames.force = FALSE)
matrix12 <- data.matrix(covid.df[covid.df$Region == "Ostergotland",]$TempCat, rownames.force = FALSE)
matrix13 <- data.matrix(covid.df[covid.df$Region == "Skane",]$TempCat, rownames.force = FALSE)
matrix14 <- data.matrix(covid.df[covid.df$Region == "Sodermanland",]$TempCat, rownames.force = FALSE)
matrix15 <- data.matrix(covid.df[covid.df$Region == "Stockholm",]$TempCat, rownames.force = FALSE)
matrix16 <- data.matrix(covid.df[covid.df$Region == "Uppsala",]$TempCat, rownames.force = FALSE)
matrix17 <- data.matrix(covid.df[covid.df$Region == "Varmland",]$TempCat, rownames.force = FALSE)
matrix18 <- data.matrix(covid.df[covid.df$Region == "Vasterbotten",]$TempCat, rownames.force = FALSE)
matrix19 <- data.matrix(covid.df[covid.df$Region == "Vasternorrland",]$TempCat, rownames.force = FALSE)
matrix20 <- data.matrix(covid.df[covid.df$Region == "Vastmanland",]$TempCat, rownames.force = FALSE)
matrix21 <- data.matrix(covid.df[covid.df$Region == "Vastra Gotaland",]$TempCat, rownames.force = FALSE)

temp.matrix = cbind(matrix1, matrix2, matrix3, matrix4, matrix5, matrix6, matrix7, matrix8, matrix9, matrix10, matrix11, matrix12, matrix13, matrix14, matrix15, matrix16, matrix17, matrix18, matrix19, matrix20, matrix21) 
colnames(temp.matrix) = c('Blekinge','Dalarna','Gavleborg','Gotland','Halland','Jamtland','Jonkoping','Kalmar','Kronoberg','Norrbotten','Orebro','Ostergotland','Skane','Sodermanland','Stockholm','Uppsala','Varmland','Vasterbotten','Vasternorrland','Vastmanland','Vastra Gotaland')


# construct sts object
ICUs.sts <- sts(
  observed = ICU.matrix, 
  start = start,
  frequency = frequency,
  population = population, 
  neighbourhood = neighbourhood
)
Temp <- temp.matrix

t = week.matrix
t2 = t^2

temp1 = ifelse(Temp == 1, 1, 0)
temp2 = ifelse(Temp >= 2, 1, 0)

# fit a HHH model
covid.fit.ICU <-  hhh4(
  stsObj = ICUs.sts, 
  control = list(
    ar = list(f = ~ 1),
    ne =  list(f = ~ 1, weights = neighbourhood(ICUs.sts) == 1),
    end = list(f = ~ t+t2+P1+P2+P3+P4+P5+P6+P7+P8+P9+temp1+temp2, offset = population(ICUs.sts)),
    family = "NegBin1"
  )
)

summary(covid.fit.ICU)


fitted <- plot(covid.fit.ICU, type = "fitted", total = TRUE, hide0s = TRUE, par.settings = NULL, legend = TRUE, ylab = 'ICU Case' ) 
fitted$Overall[12:20,]
colSums(fitted$Overall)[3:5] / sum(fitted$Overall[,1])

#subplot <- which(colSums(observed(ICUs.sts)) > 200)
subplot <- which(colnames(observed(ICUs.sts)) == 'Stockholm')
Stockholm <- plot(covid.fit.ICU, ylab = 'ICU Case', type = "fitted", units = subplot, hide0s = TRUE, legend = TRUE, par.settings = NULL)
subplot <- which(colnames(observed(ICUs.sts)) == 'Vastra.Gotaland')
Vastra.Gotaland <- plot(covid.fit.ICU, ylab = 'ICU Case', type = "fitted", units = subplot, hide0s = TRUE, legend = TRUE, par.settings = NULL)
subplot <- which(colnames(observed(ICUs.sts)) == 'Skane')
Skane <- plot(covid.fit.ICU, ylab = 'ICU Case', type = "fitted", units = subplot, hide0s = TRUE, legend = TRUE, par.settings = NULL)
subplot <- which(colnames(observed(ICUs.sts)) == 'Dalarna')
Dalarna <- plot(covid.fit.ICU, ylab = 'ICU Case', type = "fitted", units = subplot, hide0s = TRUE, legend = TRUE, par.settings = NULL)


colSums(fitted$Overall)[3:5] / sum(fitted$Overall[,1])
colSums(Stockholm$Stockholm)[3:5] / sum(Stockholm$Stockholm[,1])
colSums(Vastra.Gotaland$Vastra.Gotaland)[3:5] / sum(Vastra.Gotaland$Vastra.Gotaland[,1])
colSums(Skane$Skane)[3:5] / sum(Skane$Skane[,1])
colSums(Dalarna$Dalarna)[3:5] / sum(Dalarna$Dalarna[,1])


prediction <- predict(covid.fit.ICU) 


gen.predicted.data <- function(county) {
  predicted.case = as.integer(prediction[,county]) 
  df = data.frame(predicted.case)
  w <- seq.int(nrow(df)) + 42
  w <- ifelse(w > 52, w - 52, w)
  print (w)
  df$Week.Number <- w
  
  df <- df[,c(2,1)]
  return(df)  
}

blekinge = gen.predicted.data("Blekinge")
dalarna = gen.predicted.data("Dalarna")

stockholm = gen.predicted.data("Stockholm")


