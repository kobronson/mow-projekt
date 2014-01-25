make_knn_classifier <- function(k){
  knn_classify <- function(train_data, train_class, test_data){
  
    make_knn_calculate_distance <- function(test_row){
      function(train_row){
        # Odleglosc euklidesowa.
        sqrt(sum((test_row - train_row) ^ 2))
      }
    }
    
    knn_choose_class <- function(neighbours){
      class <- as.numeric(names(which.max(table(neighbours))))
      class
    }
    
    knn_classify_one_row <- function(t_row){
      distances <- apply(train_data, 1, make_knn_calculate_distance(t_row))
      neighbours <- train_class[head(order(distances), k)]
      knn_choose_class(neighbours)
    }
    class <- apply(test_data, 1, knn_classify_one_row)
    class
  }
  knn_classify
}