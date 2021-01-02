rm(list=ls())
mat.df <- read.csv("C:/Users/runajk/Documents/Study/Thesis/matrix.csv", header = FALSE)
#View(mat.df)
matrix <- data.matrix(mat.df, rownames.force = FALSE)

data.frame <- read.csv("E:/Thesis/final.data.set.csv")
#View(data.frame)

Region <- unique(data.frame$Region)
qvalues.df <- NULL

for (val in 1:36) {
  df_week <- data.frame[data.frame$Week.Number == val,]
  death.matrix <- df_week$No.of.deaths
  icu.matrix <- df_week$ICU.Cases
  
  QD <- (matrix %*% death.matrix) / rowSums(matrix)
  QI <- (matrix %*% icu.matrix) / rowSums(matrix)

  qvalues.df = rbind(qvalues.df, data.frame(Region, val, QD, QI))
}
names(qvalues.df)[names(qvalues.df) == "val"] <- "Week.Number"

#View(qvalues.df)

final_df <- merge(x=qvalues.df, y=data.frame, by = c("Week.Number","Region"), all = TRUE)
final_df <- final_df[order(final_df$Region, final_df$Week.Number),]
View(final_df)
write.csv(final_df,"E:/Thesis/final.data.set.with.q.csv", row.names = FALSE)



