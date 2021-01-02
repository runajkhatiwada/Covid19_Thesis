library(httr)
library(jsonlite)
library(dplyr)

url <- "https://opendata-download-metobs.smhi.se/api/version/latest/parameter/1.json"
GET(url, write_disk(temp.file <- tempfile(fileext = ".json")))
json.data <- fromJSON(temp.file)
all.stations <- json.data$station

stations <- data.frame(all.stations)


url <- "https://raw.githubusercontent.com/sphrak/svenska-stader/master/src/svenska-stader.csv"
df.cities <- read.csv(url, fileEncoding = "UTF-8")
names(df.cities)[names(df.cities) == "Locality"] <- "name"
View(df.cities)
df.stations.county <- merge(x = all.stations, y = df.cities, by = "name", all = FALSE)
df.stations.county <- subset(df.stations.county, select = c(id, County, name))
df.stations.county <- df.stations.county[order(df.stations.county$County),]

#install.packages("tidyverse")
library(tidyverse)
# df.stations.county <- data.frame(df.stations.county %>%
#                  mutate(County = df.stations.county$County) %>%
#                  group_by(County) %>%
#                  summarise_all("max"))
write.csv(df.stations.county,"E:\\Thesis\\Stations.csv", row.names = FALSE)
#temp <- df.stations.county[df.stations.county$County == 'Jönköping',]

url <- "https://opendata-download-metobs.smhi.se/api/version/1.0/parameter/1/station/64130/period/corrected-archive/data.csv"
GET(url, write_disk(temp.file <- tempfile(fileext = ".csv")))


url <- "https://opendata-download-metobs.smhi.se/api/version/1.0/parameter/2/station/105450/period/latest-months/data.csv"
GET(url, write_disk(temp.file <- tempfile(fileext = ".csv")))


#Derive missing temperature data for Vasterbotten county (for August)
df1 <- read.csv("E:\\Thesis\\Temperature Data\\Norrbotten.csv")
df1 <- df1[df1$Date >= '2020-08-01',]
df2 <- read.csv("E:\\Thesis\\Temperature Data\\Vasternorrland.csv")
df2 <- df2[df2$Date >= '2020-08-01',]
df1 <- select(df1, c(2,3))
df2 <- select(df2, c(2,3))
df3 <- merge(x = df1, y = df2, by = "Date", all = FALSE)
df3 <- transform(df3, Temperature = (Temperature.x+Temperature.y)/2)
df3 <- select(df3, c(1,4))
Vasterbotten <- read.csv("E:\\Thesis\\Temperature Data\\Vasterbotten.csv")
names(Vasterbotten)[names(Vasterbotten) == "ï..Region"] <- "Region"
Vasterbotten <- merge(x = Vasterbotten, y = df3, by = "Date", all = TRUE)
Vasterbotten <- transform(Vasterbotten, Temperature = ifelse(is.na(Temperature.x) == FALSE, Temperature.x,Temperature.y))
Vasterbotten <- select(Vasterbotten, c(2,1,5))
write.csv(Vasterbotten,"E:\\Thesis\\Temperature Data\\Vasterbotten.csv", row.names = FALSE)

#Derive missing temperature data for Gotland county (for August)
df1 <- read.csv("E:\\Thesis\\Temperature Data\\Kalmar.csv")
df1 <- df1[df1$Date >= '2020-08-01',]
df2 <- read.csv("E:\\Thesis\\Temperature Data\\Ostergotland.csv")
df2 <- df2[df2$Date >= '2020-08-01',]
df1 <- select(df1, c(2,3))
df2 <- select(df2, c(2,3))
df3 <- merge(x = df1, y = df2, by = "Date", all = FALSE)
df3 <- transform(df3, Temperature = (Temperature.x+Temperature.y)/2)
df3 <- select(df3, c(1,4))
Gotland <- read.csv("E:\\Thesis\\Temperature Data\\Gotland.csv")
names(Gotland)[names(Gotland) == "ï..Region"] <- "Region"
Gotland <- merge(x = Gotland, y = df3, by = "Date", all = TRUE)
Gotland <- transform(Gotland, Temperature = ifelse(is.na(Temperature.x) == FALSE, Temperature.x,Temperature.y))
Gotland <- select(Gotland, c(2,1,5))
write.csv(Gotland,"E:\\Thesis\\Temperature Data\\Gotland.csv", row.names = FALSE)

#Derive missing temperature data for Halland county (for August)
df1 <- read.csv("E:\\Thesis\\Temperature Data\\skane.csv")
df1 <- df1[df1$Date >= '2020-08-01',]
df2 <- read.csv("E:\\Thesis\\Temperature Data\\Vastra Gotaland.csv")
df2 <- df2[df2$Date >= '2020-08-01',]
df1 <- select(df1, c(2,3))
df2 <- select(df2, c(2,3))
df3 <- merge(x = df1, y = df2, by = "Date", all = FALSE)
df3 <- transform(df3, Temperature = (Temperature.x+Temperature.y)/2)
df3 <- select(df3, c(1,4))
Halland <- read.csv("E:\\Thesis\\Temperature Data\\Halland.csv")
names(Halland)[names(Halland) == "ï..Region"] <- "Region"
Halland <- merge(x = Halland, y = df3, by = "Date", all = TRUE)
Halland <- transform(Halland, Temperature = ifelse(is.na(Temperature.x) == FALSE, Temperature.x,Temperature.y))
Halland <- select(Halland, c(2,1,5))
write.csv(Halland,"E:\\Thesis\\Temperature Data\\Halland.csv", row.names = FALSE)

#Derive missing temperature data for Jamtland county (for August)
df1 <- read.csv("E:\\Thesis\\Temperature Data\\Dalarna.csv")
df1 <- df1[df1$Date >= '2020-08-01',]
df2 <- read.csv("E:\\Thesis\\Temperature Data\\Vasterbotten.csv")
df2 <- df2[df2$Date >= '2020-08-01',]
df1 <- select(df1, c(2,3))
df2 <- select(df2, c(2,3))
df3 <- merge(x = df1, y = df2, by = "Date", all = FALSE)
df3 <- transform(df3, Temperature = (Temperature.x+Temperature.y)/2)
df3 <- select(df3, c(1,4))
Jamtland <- read.csv("E:\\Thesis\\Temperature Data\\Jamtland.csv")
names(Jamtland)[names(Jamtland) == "ï..Region"] <- "Region"
Jamtland <- merge(x = Jamtland, y = df3, by = "Date", all = TRUE)
Jamtland <- transform(Jamtland, Temperature = ifelse(is.na(Temperature.x) == FALSE, Temperature.x,Temperature.y))
Jamtland <- select(Jamtland, c(2,1,5))
write.csv(Jamtland,"E:\\Thesis\\Temperature Data\\Jamtland.csv", row.names = FALSE)

#Derive missing temperature data for Vastmanland county (for August)
df1 <- read.csv("E:\\Thesis\\Temperature Data\\Dalarna.csv")
df1 <- df1[df1$Date >= '2020-08-01',]
df2 <- read.csv("E:\\Thesis\\Temperature Data\\Sodermanland.csv")
df2 <- df2[df2$Date >= '2020-08-01',]
df1 <- select(df1, c(2,3))
df2 <- select(df2, c(2,3))
df3 <- merge(x = df1, y = df2, by = "Date", all = FALSE)
df3 <- transform(df3, Temperature = (Temperature.x+Temperature.y)/2)
df3 <- select(df3, c(1,4))
Vastmanland <- read.csv("E:\\Thesis\\Temperature Data\\Vastmanland.csv")
names(Vastmanland)[names(Vastmanland) == "ï..Region"] <- "Region"
Vastmanland <- merge(x = Vastmanland, y = df3, by = "Date", all = TRUE)
Vastmanland <- transform(Vastmanland, Temperature = ifelse(is.na(Temperature.x) == FALSE, Temperature.x,Temperature.y))
Vastmanland <- select(Vastmanland, c(2,1,5))
write.csv(Vastmanland,"E:\\Thesis\\Temperature Data\\Vastmanland.csv", row.names = FALSE)


#Derive missing temperature data for Jonkoping county 
df1 <- read.csv("C:\\Users\\runajk\\Documents\\Study\\Thesis\\Temperature Data\\Kronoberg.csv")
df2 <- read.csv("C:\\Users\\runajk\\Documents\\Study\\Thesis\\Temperature Data\\Ostergotland.csv")
df3 <- read.csv("C:\\Users\\runajk\\Documents\\Study\\Thesis\\Temperature Data\\Vastra Gotaland.csv")
df1 <- select(df1, c(2,3))
df2 <- select(df2, c(2,3))
df3 <- select(df3, c(2,3))

jonkoping <- merge(x = df1, y = df2, by = "Date", all = FALSE)
jonkoping <- merge(x = jonkoping, y = df3, by = "Date", all = FALSE)
names(jonkoping)[names(jonkoping) == "Temperature"] <- "Temperature.z"
jonkoping <- transform(jonkoping, Temperature = round((Temperature.x+Temperature.y+Temperature.z)/3, 1))
jonkoping$Region <- "Jonkoping"
jonkoping <- select(jonkoping, c(6,1,5))
write.csv(jonkoping,"C:\\Users\\runajk\\Documents\\Study\\Thesis\\Temperature Data\\Jonkoping.csv", row.names = FALSE)


#Derive missing temperature data for Uppsala county

df1 <- read.csv("C:\\Users\\runajk\\Documents\\Study\\Thesis\\Temperature Data\\Gavleborg.csv")
df2 <- read.csv("C:\\Users\\runajk\\Documents\\Study\\Thesis\\Temperature Data\\Stockholm.csv")
df3 <- read.csv("C:\\Users\\runajk\\Documents\\Study\\Thesis\\Temperature Data\\Vastmanland.csv")
df1 <- select(df1, c(2,3))
df2 <- select(df2, c(2,3))
df3 <- select(df3, c(2,3))

Uppsala <- merge(x = df1, y = df2, by = "Date", all = FALSE)
Uppsala <- merge(x = Uppsala, y = df3, by = "Date", all = FALSE)
names(Uppsala)[names(Uppsala) == "Temperature"] <- "Temperature.z"
Uppsala <- transform(Uppsala, Temperature = round((Temperature.x+Temperature.y+Temperature.z)/3, 1))
Uppsala$Region <- "Uppsala"
Uppsala <- select(Uppsala, c(6,1,5))
write.csv(Uppsala,"C:\\Users\\runajk\\Documents\\Study\\Thesis\\Temperature Data\\Uppsala.csv", row.names = FALSE)



#install.packages("plyr")
library(plyr)
library(readr)
setwd("C:\\Users\\runajk\\Documents\\Study\\Thesis\\Temperature Data")
mydir = "C:\\Users\\runajk\\Documents\\Study\\Thesis\\Temperature Data"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles
df_weather = ldply(myfiles, read_csv)
con<-file('C:\\Users\\runajk\\Documents\\Study\\Thesis\\Temperature Data\\Temperature.csv')#,encoding="UTF-8")
write.csv(df_weather,con, row.names = FALSE)

library(aweek)
df_weather <- read.csv("C:\\Users\\runajk\\Documents\\Study\\Thesis\\Temperature Data\\Temperature.csv")
#df_weather$Date <- as.Date(df_weather$Date, "%Y-%m-%d")
#df_weather[is.na(df_weather)] <- 0
week <- date2week(df_weather$Date, floor_day = TRUE, numeric = TRUE)

df_weather$Week.Number <- week
df_weather <- select(df_weather, c(1,4,3))

library(tidyverse)
df_weather <- data.frame( 
  df_weather %>%
    mutate(df_weather$Week.Number, df_weather$Region) %>%
    group_by(df_weather$Week.Number, df_weather$Region) %>%
    summarise_all("mean")
)
unique(df_weather$df_weather.Region)
df_weather <- df_weather[order(df_weather$df_weather.Region),]
df_weather <- select(df_weather, c(2,4,5))
df_weather <- transform(df_weather, Temperature = round(Temperature, 1))
names(df_weather)[names(df_weather) == "df_weather.Region"] <- "Region"


df_weather[df_weather$Temperature < 5, "TempCat"] <- 0
df_weather[between(df_weather$Temperature, 5, 10), "TempCat"] <- 1
df_weather[between(df_weather$Temperature, 10, 15), "TempCat"] <- 2
df_weather[df_weather$Temperature > 15, "TempCat"] <- 3

View(df_weather)
summary(df_weather)
write.csv(df_weather,"C:\\Users\\runajk\\Documents\\Study\\Thesis\\weather.csv", row.names = FALSE)

dataper10000 <- read.csv("E:/Thesis/dataper10000.csv")
data.set <- read.csv("E:/Thesis/final.data.set.csv")

new_df <- merge(x = dataper10000, y = data.set, by = c("Week.Number","Region"), all = TRUE)
new_df <- new_df[order(new_df$Region,new_df$Week.Number),]
new_df[is.na(new_df)] <- 0
write.csv(new_df,"E:/Thesis/final.data.set.csv", row.names = FALSE)
