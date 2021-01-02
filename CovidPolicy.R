#Clear the environment
rm(list=ls())

#library to read excel file
library(readxl)
install.packages('readxl')
#library for GET function to retrieve the excel sheet from url
install.packages('httr')
library(httr)

#library to plot graph
install.packages('ggplot2')
library(ggplot2)

#url from public health agency website for data source
url <- "https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data"

#function to download the temp file from the url
GET(url, write_disk(temp_file <- tempfile(fileext = ".xlsx")))

#read the data from temp file and import to data frame
df_covid <- read_excel(temp_file, sheet = "Veckodata Region")

#data frame for the details of government policies in different dates
df_policy <- read_excel("C:\\Users\\runajk\\Documents\\Study\\Thesis\\DataDescription.xlsx", sheet = "Policy as Predictor")

#translate and rename the column name from Swedish to English 
names(df_covid)[names(df_covid) == "veckonummer"] <- "Week.Number"
names(df_covid)[names(df_covid) == "Antal_fall_vecka"] <- "No.of.cases"
names(df_covid)[names(df_covid) == "Kum_antal_fall"] <- "Cumulative.cases"
names(df_covid)[names(df_covid) == "Antal_intensivvårdade_vecka"] <- "ICU.Cases"
names(df_covid)[names(df_covid) == "Kum_antal_intensivvårdade"] <- "Cumulative.ICU.cases"
names(df_covid)[names(df_covid) == "Antal_avlidna_vecka"] <- "No.of.deaths"
names(df_covid)[names(df_covid) == "Kum_antal_avlidna"] <- "Cumulative.deaths"
names(df_covid)[names(df_covid) == "Antal_fall_100000inv_vecka"] <- "cases.per.100000"
names(df_covid)[names(df_covid) == "Kum_fall_100000inv"] <- "Cumulative.cases.per.100000"

 View(df_covid)
# View(df_policy)

#select policy and week number and make subset
df_policy_sub <- subset(df_policy, select = c(Policy,Week.Number))

#remove the duplicate columns
library(dplyr)
df_policy_sub <- df_policy_sub %>% distinct(Policy, Week.Number)

#add the columns for different policies made by government and add default value as 0
for (i in 1:nrow(df_policy_sub)) {
  column <- df_policy_sub[i,1]
  column <- as.character(column)
  df_covid[[column]]<-0
}

#rename the columns by replacing space by "."
colnames(df_covid) = gsub(" ", ".",colnames(df_covid))

#update the policy columns by 1 after the week of implementation
df_covid <- within(df_covid, P1[Week.Number >= 9] <- 1)
df_covid <- within(df_covid, P2[Week.Number >= 10] <- 1)
df_covid <- within(df_covid, P3[Week.Number >= 11] <- 1)
df_covid <- within(df_covid, P4[Week.Number >= 12] <- 1)
df_covid <- within(df_covid, P5[Week.Number >= 13] <- 1)
df_covid <- within(df_covid, P6[Week.Number >= 14] <- 1)
df_covid <- within(df_covid, P7[Week.Number >= 15] <- 1)
df_covid <- within(df_covid, P8[Week.Number >= 16] <- 1)
df_covid <- within(df_covid, P9[Week.Number >= 19] <- 1)
df_covid <- within(df_covid, P10[Week.Number >= 20] <- 1)
df_covid <- within(df_covid, P11[Week.Number >= 22] <- 1)
df_covid <- within(df_covid, P12[Week.Number >= 23] <- 1)
df_covid <- within(df_covid, P13[Week.Number >= 26] <- 1)
df_covid <- within(df_covid, P14[Week.Number >= 27] <- 1)
df_covid <- within(df_covid, P15[Week.Number >= 28] <- 1)


# df_covid[["County.Code"]]<-0
# df_covid <- within(df_covid, County.Code[Region >= "Blekinge"] <- 10)
# df_covid <- within(df_covid, County.Code[Region >= "Dalarna"] <- 20)
# df_covid <- within(df_covid, County.Code[Region >= "Gotland"] <- 09)
# df_covid <- within(df_covid, County.Code[Region >= "Gävleborg"] <- 21)
# df_covid <- within(df_covid, County.Code[Region >= "Halland"] <- 13)
# df_covid <- within(df_covid, County.Code[Region >= "Jämtland Härjedalen"] <- 23)
# df_covid <- within(df_covid, County.Code[Region >= "Jönköping"] <- 06)
# df_covid <- within(df_covid, County.Code[Region >= "Kalmar"] <- 08)
# df_covid <- within(df_covid, County.Code[Region >= "Kronoberg"] <- 07)
# df_covid <- within(df_covid, County.Code[Region >= "Norrbotten"] <- 25)
# df_covid <- within(df_covid, County.Code[Region >= "Skåne"] <- 12)
# df_covid <- within(df_covid, County.Code[Region >= "Stockholm"] <- 01)
# df_covid <- within(df_covid, County.Code[Region >= "Sörmland"] <- 04)
# df_covid <- within(df_covid, County.Code[Region >= "Uppsala"] <- 03)
# df_covid <- within(df_covid, County.Code[Region >= "Värmland"] <- 17)
# df_covid <- within(df_covid, County.Code[Region >= "Västerbotten"] <- 24)
# df_covid <- within(df_covid, County.Code[Region >= "Västernorrland"] <- 22)
# df_covid <- within(df_covid, County.Code[Region >= "Västmanland"] <- 19)
# df_covid <- within(df_covid, County.Code[Region >= "Västra Götaland"] <- 14)
# df_covid <- within(df_covid, County.Code[Region >= "Örebro"] <- 10)
# df_covid <- within(df_covid, County.Code[Region >= "Blekinge"] <- 10)
# df_covid <- within(df_covid, County.Code[Region >= "Blekinge"] <- 10)
# df_covid <- within(df_covid, County.Code[Region >= "Blekinge"] <- 10)
#View(df_covid)

#write.csv(df_sub,"E:/Thesis/Data.csv", row.names = FALSE)

df_sub <- subset(df_covid, 
                 select = c(
                   Week.Number, 
                   Region, 
                   No.of.cases,
                   ICU.Cases, 
                   No.of.deaths,
                   P1,
                   P2,
                   P3,
                   P4,
                   P5,
                   P6,
                   P7,
                   P8,
                   P9,
                   P10,
                   P11,
                   P12,
                   P13,
                   P14,
                   P15
                 )
)

write.csv(df_sub,"C:\\Users\\runajk\\Documents\\Study\\Thesis\\sub.csv", row.names = FALSE)

df_sub <- read.csv("C:\\Users\\runajk\\Documents\\Study\\Thesis\\sub.csv")
df_weather <- read.csv("C:\\Users\\runajk\\Documents\\Study\\Thesis\\weather.csv")
final_df <- merge(x = df_sub, y = df_weather, by = c("Week.Number","Region"), all = TRUE)
final_df <- final_df[order(final_df$Region, final_df$Week.Number),]
final_df[is.na(final_df)] <- 0
# df_county <- select(df_county, -c(21))
summary(final_df)
write.csv(final_df,"C:\\Users\\runajk\\Documents\\Study\\Thesis\\data_source_temp.csv", row.names = FALSE)
# final_df <- read.csv("E:/Thesis/data_source.csv")

df1 <- final_df
df2 <- final_df

df1 <- select(df1, c(1,2,3,4,5))
df2 <- select(df2, c(1,2,3,4,5))

df1["Week.Number"] <- (df1$Week.Number + 1)


View(df1)
View(df2)

new_df <- merge(x = df1, y = df2, by = c("Week.Number","Region"), all = TRUE)
new_df <- new_df[order(new_df$Region),]
new_df <- select(new_df, c(1,2,3,4,5))

new_df <- new_df[!(new_df$Week.Number==37),]
new_df[is.na(new_df)] <- 0

final_df <- merge(x=final_df, y=new_df, by = c("Week.Number","Region"), all = TRUE)
final_df <- final_df[order(final_df$Region, final_df$Week.Number),]
final_df <- select (final_df, c(1,2,3,23,4,24,5,25,21,22,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
write.csv(final_df,"E:/Thesis/final.data.set.csv", row.names = FALSE)

#df_county is final data set

#linear regression model for death cases in stockholm
lm.deaths <- glm (No.of.deaths ~ Region+P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15+
                               TempCat, 
                               data = final_df, 
                               family = "poisson"
                )
summary(lm.deaths)

# lm.deaths.summary$coefficients
# 
# lm.icu <- glm(formula = ICU.Cases ~ Policy.7 + Policy.8 + Policy.9 + 
#              Policy.10 + Policy.11 + Policy.12 + Policy.13 + Policy.15 + Temperature, 
#              family = "poisson",
#            data = df_county)
# 
# summary(lm.icu)
# summary(df_county)










