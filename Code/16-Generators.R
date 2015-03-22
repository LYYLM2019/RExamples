# purpose: example if the use of generators in R
# author: tirthankar chakravarty
# created: 22nd march 2015
# revised:
# comments:

library(iterators)

# function to check sequence of natural numbers for
#   divisibility by a given list of factors
fnDivision = function(maxNum, vFactors) {
  # create an iterator up to 15! (=1.307674e+12)
  # 15! is the largest possible answer
  i = icount(factorial(maxNum))
  while(TRUE) {
    currentlyTesting = nextElem(i)
    # check if the number is divisible by all the numbers
    if(all(! currentlyTesting %% vFactors)) {
      return(currentlyTesting)
    }
  }
}

# test the function
vFactors = c(8, 9, 10, 11, 12, 13, 14, 15)
sprintf('The smallest natural number divisible by the first 15 natural numbers is %i.',
        fnDivision(15, vFactors))