library(pROC)
library(dplyr)

filenameROC = "Data/term3_IBk_3_multiclass.txt"
fileROC = readLines(filenameROC)
dfCV = read.csv2(text = fileROC,
                 nrows = length(fileROC) - 51 - 19,
                header = TRUE, 
                sep = ",",
                skip = 19, stringsAsFactors = FALSE)

dfCVROC = dfCV %>%
  dplyr::filter(inst. != 773) %>%
  arrange(inst.) %>%
  dplyr::mutate(cvfold = rep.int(1:10, 772)) %>%
  group_by(cvfold) %>%
  do(multiclass_roc = multiclass.roc(as.factor(.$actual), as.numeric(.$prediction)))

# get the AUCs by CV fold
sapply(dfCVROC$multiclass_roc, function(x) x$auc)


