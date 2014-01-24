spamnumericdata <- read.csv("SpamBase/spambase.csv", header= TRUE, sep = ",", row.names = NULL)
spamliteraldata <- read.csv("SpamCollection/SMSSpamCollection.csv", header= TRUE, sep = "\t", row.names = NULL, quote = NULL)

install.packages("Rcpp")
pDL <- pkgDepends("RTextTools", local = FALSE)
installFoundDepends(pDL$Found, destdir = tmp)

Installing package(s) into ‘/home/doma/R/i686-pc-linux-gnu-library/2.15’
(as ‘lib’ is unspecified)
ERROR: dependencies ‘SparseM’, ‘randomForest’, ‘tree’, ‘tm’, ‘e1071’, ‘ipred’, ‘caTools’, ‘maxent’, ‘glmnet’, ‘tau’ are not available for package ‘RTextTools’
* removing ‘/home/doma/R/