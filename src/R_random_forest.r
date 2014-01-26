r_random_forest_classify <- function(train_data, train_class, test_data){
  
  library(randomForest)
  model <- randomForest(train_class ~ ., data = train_data)
  predict(model, test_data)   
}
