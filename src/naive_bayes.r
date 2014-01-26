naive_bayes_classify <- function(train_data, train_class, test_data){
  
  # Wybiera unikatowe słowa wystepujace w wiadomosci.
  get_words <- function(message){
    words <- strsplit(message, "[^A-Za-z0-9]")[[1]]
    unique(tolower(setdiff(words,c(""))))
  }
  
  spam_word_counts <- c()
  ham_word_counts <- c()
  
  for (i in 1:length(train_data))
  {
    message <- train_data[i]
    class <- train_class[i]
    words <- get_words(message)
    if (class == 1){
      for (word in words){
        if (word %in% names(spam_word_counts)){
          spam_word_counts[word] <- spam_word_counts[word] + 1
        } else {
          new_word <- c(1)
          names(new_word) <- c(word)
          spam_word_counts <- c(spam_word_counts, new_word)
        }
      }
    }
    else
    {
      for(word in words){
        if (word %in% names(ham_word_counts)){
          ham_word_counts[word] <- ham_word_counts[word] + 1
        }
        else {
          new_word <- c(1)
          names(new_word) <- c(word)
          ham_word_counts <- c(ham_word_counts, new_word)
        }      
      }
    }
  }
  
  spam_count <- table(train_class)["1"]
  ham_count <- table(train_class)["0"]
  
  calculate_word_spamicity <- function(word){
    #TODO if not exist
    if (word %in% names(spam_word_counts)){
      pws <- spam_word_counts[word]/spam_count  
    }
    else{
      pws <- 0
    }
    if (word %in% names(ham_word_counts)){
      pwh <- ham_word_counts[word]/ham_count  
    }
    else{
      pwh <- 0
    }
    
    if (pws == 0 && pwh == 0)
    {
      spamicity <- 0.5
    }
    else{
      spamicity <- pws / (pws + pwh)
    }
    min(max(spamicity, 0.01), 0.99)
  }
  calculate_message_spamicity <- function(message){
    words <- get_words(message)
    p <- sapply(words, calculate_word_spamicity, USE.NAMES = FALSE)
    p <- unlist(p)
    prod_p <- prod(p)
    decision <- round(prod_p/(prod_p + prod(1-p)))
  }
  sapply(test_data, calculate_message_spamicity, USE.NAMES=FALSE)
}

