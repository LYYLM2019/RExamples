
{ # Expression Begin
  while (TRUE) {
    ANSWER <- suppressWarnings(as.numeric(readline("Choose among these
                                                 1.\n
                                                 2.\n
                                                 3.\n
                                                 4.\n
                                                 5.\n")))
    if(!is.na(ANSWER)) {
      if(ANSWER==1 || ANSWER==2 || ANSWER==3|| ANSWER==4 || ANSWER==5)
        break
    }
    cat("You didn't press 1,2,3,4 or 5, try again")
  }

  if (ANSWER==1) {
    cat("davs")#Clearer skærmen
    dataLoad()
  }
  if (ANSWER == 2) {
    cat("\014")
    dataFilter()
  }
  if (ANSWER == 3) {
    cat("\014")
    dataStatistics()
  }
  if (ANSWER == 4) {
    cat("\014")
    dataPlot ()
  }
  if (ANSWER == 5) {
    cat("Goodbye")
    q(save = "default", status = 0, runLast = TRUE)
  }
}  # Expression End