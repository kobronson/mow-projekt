#http://pyevolve.sourceforge.net/wordpress/?p=1589
#http://www.inside-r.org/packages/cran/tm/docs/as.TermDocumentMatrix
#https://stackoverflow.com/questions/14820590/trying-to-get-tf-idf-weighting-working-in-r


tf_idf_classify <- function(train_data, train_class, test_data){
  # Miejsce na klasyfikator tf-idf. Na razie zwraca zawsze 0.
  rep(0,nrow(test_data))
}