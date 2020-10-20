#p values for all samples 
set.seed(2020)
for (i in 1:length(all_samples)) {
  print(i)
  d <- all_samples[[i]]
  d$dominance <- as.factor(d$dominance)
  d$dominant_topic <- as.factor(d$dominant_topic)
  # Split dataframe into training & testing sets
  in_train <- caret::createDataPartition(d$dominance, p = .70, list = FALSE)
  train <- d[in_train,] # Training dataset for all model development
  test <- d[- in_train,]
  #dominance ~ topic + days
  best_model <- caret::train(
    dominance ~ dominant_topic + new_days,
    data = train[, c("dominance", "dominant_topic", "new_days")],
    method = "multinom",
    trace = FALSE,
    MaxNWts= 2000
  )
  
  #making a summary to get z, p and coefficient values
  output <- summary(best_model)
  z <- output$coefficients/output$standard.errors
  p <- (1 - pnorm(abs(z), 0, 1))*2 # we are using two-tailed z test
  #editing colnames and saving as dataframes
  dom_2 <- as.data.frame(cbind(output$coefficients[1,],output$coefnames,output$standard.errors[1,],z[1,],p[1,]))
  colnames(dom_2) <- c("coefficient","coef_name","std.errors","z_stat","p_value")
  dom_3 <- as.data.frame(cbind(output$coefficients[2,], output$coefnames,output$standard.errors[2,],z[2,],p[2,]))
  colnames(dom_3) <- c("coefficient","coef_name","std.errors","z_stat","p_value")
  
  if(i == 1){
    coef_2 <- dom_2
    coef_3 <- dom_3
  }else{
    coef_2 <- rbind(coef_2, dom_2)
    coef_3 <- rbind(coef_3, dom_3)
  }
}

write.csv(coef_2, "coef2.csv") #saving the values 
write.csv(coef_3, "coef3.csv") #saving the values 

#filtering significant coefficients
#for dominance 2
coef_2_num <- read_csv("coef2.csv")
coef_2_num <- coef_2_num %>% 
  mutate(
    p_value = as.numeric(as.character(p_value)),
    coefficient = as.numeric(as.character(coefficient))
  )

#for group dominance 3 (females)
coef_3_num <- read_csv("coef3.csv")
coef_3_num <- coef_3_num %>% 
  mutate(
    p_value = as.numeric(as.character(p_value)),
    coefficient = as.numeric(as.character(coefficient))
  )

#combining results from both coefficients
coef_2_num$group <- c("male")
coef_3_num$group <- c("female")
coef_2_num$sample <- rep(1:1000, each = 51)
coef_3_num$sample <- rep(1:1000, each = 51)

both_coef <- rbind(coef_2_num, coef_3_num)
both_coef$significant <- ifelse(both_coef$p_value < 0.05, 1, 0) #makes a column where "1" means that it is significant

#changing classes
both_coef <- both_coef %>% 
  mutate(
    coefficient = as.numeric(as.character(coefficient)),
    coef_name =  as.character(coef_name),
    z_stat = as.numeric(as.character(z_stat)),
    p_value = as.numeric(as.character(p_value)))
#as we don't have any values from topic 22 or 46 so they are  removed 
both_coef <- both_coef %>% 
  filter(coef_name != "dominant_topic22") %>% 
  filter(coef_name != "dominant_topic46") %>% 
  filter(p_value < 0.05)

#plotting each coefficient
ggplot(both_coef, aes(x= z_stat, color = group, fill= group))+
  geom_density(alpha = 0.2) +
  facet_wrap(.~coef_name, scales = "free")

ggplot(both_coef, aes(x= coefficient, color = group, fill= group))+
  geom_density(alpha = 0.2) +
  facet_wrap(.~coef_name, scales = "free")

#making a table to see which predictors are most frequent 
p_table_dom2 <- as.data.frame(table(dom_2_sigificant$coef_name))
#plotting horizontal barplot to see which predictors are most frequently significant
ggplot(p_table_dom2) + 
  geom_histogram(aes(reorder(Var1, -Freq), Freq), fill = rgb(0.2,0.4,0.6,0.6), stat="identity") +
  coord_flip() + 
  ggtitle("Male dominated group coefficients") + 
  labs(x="Coefficients", y="Frequency of significance")

#making a table to see which predictors are most frequent 
p_table_dom3 <- as.data.frame(table(dom_3_sigificant$coef_name))
#plotting horizontal barplot to see which predictors are most frequently significant
ggplot(p_table_dom3) +
  geom_histogram(aes(reorder(Var1, -Freq), Freq), fill =rgb(0.2,0.4,0.6,0.6), stat="identity") +  
  coord_flip() + 
  ggtitle("Female dominated group coefficients") + 
  labs(x="Coefficients", y="Frequency of significance")