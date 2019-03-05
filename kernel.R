setwd("C:\\Users\\CCYoTzom\\Desktop\\whats-cooking")

library(corrplot)
library(tidyverse)
library(jsonlite)

data <- fromJSON("train.json")
test_data <- fromJSON("test.json")
#sample_submission_data <- read.csv("sample_submission.csv", header = T, sep = ",", stringsAsFactors = F)
submission_answer <- data.frame(test_data[,1])

dim(data)
sum(is.na(data))

#列出每個變數的型態
for (i in 1:length(data[1,])) {
  cat(class(data[,i]), ", ")
  if(i %% 10 ==0){
    cat("\n")
  }
}

#列出NA>0的變數
for (i in 1:ncol(data)) {
  if(sum(is.na(data[,i])) > 0){
    cat("變數", i , ":", names(data)[i], ", NA數 :", sum(is.na(data[,i])), ",\n")
  }
}

for (i in 1:nrow(data)) {
  data[i,3] <- c(as.vector(data[i,3][[1]]))
}
# call value example data[1,3][[1]][1]

unique_ingredients <- c()
for (j in 1:nrow(data)) {
  for (i in 1:length(data[i,3][[1]])) {
    if(!(data[j,3][[1]][i] %in% unique_ingredients)){
      unique_ingredients <- c(unique_ingredients, data[j,3][[1]][i])
    }else{
      next
    }
  }
}

cat("All recipes have ", length(unique_ingredients), " ingredients.")
cat("All recipes have ", length(unique(data$cuisine)), " cuisines.")

#創建風味與食材對照表
cusines_ingredients_comparison_table <- data.frame(unique(data$cuisine))

for (i in 1:nrow(cusines_ingredients_comparison_table)) {
  for (j in 1:nrow(data)) {
    if(cusines_ingredients_comparison_table[i,1] == data[j,2]){
      for(k in 1:length(data[j,3][[1]])){
      tmp[i] <- list(tmp[i], data[j,3][[1]])
      }
    }
  }
}
cusines_ingredients_comparison_table[i,2]
#All Italian 0.19267

#隨機 0.05299 No.1386
#round(runif(1,1,length(unique(data$cuisine))))

for (i in 1:nrow(submission_answer)) {
  submission_answer[i,2] <- c(unique(data$cuisine))[round(runif(1,1,length(unique(data$cuisine))))]
}
names(submission_answer) <- c("id", "cuisine")
write.csv(submission_answer, "submission_answer_v1_random.csv", row.names = F)

