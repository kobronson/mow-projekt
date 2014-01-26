
r_svm_classify <- function(train_data, train_class, test_data){
  
  library(e1071)
  model <- svm(train_class ~ ., data = train_data)
  predict(model, test_data)   
}

