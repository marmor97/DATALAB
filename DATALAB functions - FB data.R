#DATALAB functions - FB data

#function for generating balanced dataset
my_sample <- function(df, col ,category1, category2, category3){
  df1 <- df[df[[col]] == category1,][sample(500,replace=T),]
  df2 <- df[df[[col]] == category2,][sample(500,replace=T),]
  df3 <- df[df[[col]] == category3,][sample(500,replace=T),]
  d <- rbind(df1,df2,df3)
  d <- d[sample(nrow(d)),]
  return(as.data.frame(d))
}
#random classifier
random_class <- function(list, col){ #col
  for (i in 1:length(list)){ 
    print(i)
    d <- list[[i]]
    d <- d[, c(col)]
    d_r <- d[sample(1500,replace=T)]
    preds <- data_frame(actual = as.factor(d), predictions = as.factor(d_r)) #preds <- data_frame(actual = as.factor(d[,1]), predictions = as.factor(d_r[,1]))
    confused <- caret::confusionMatrix(preds$predictions, preds$actual)
    accuracy_score <- as.numeric(confused$overall[1])
    preds$actual<- as.numeric(preds$actual)
    preds$predictions <- as.numeric(preds$predictions)
    multi_roc <- multiclass.roc(preds$actual, preds$predictions)
    #saving reslts
    scores <- data.frame(
      accuracy = accuracy_score,
      roc_auc = multi_roc$auc)
    if(i == 1){
      results <- scores
    }else{
      results <- rbind(results, scores)
    }
  }
  return(results)
}
