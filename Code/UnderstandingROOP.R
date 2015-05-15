#====================================================================
# purpose: show some of the features of the S3 object system in R
# author: tirthankar chakravarty
# created: 1st may 2015
# comments: 
#====================================================================
library(pryr)

# a list
str1 = structure(list(slot1 = "a", slot2 = "b"))
class(str1) = "myclass"
pryr::address(str1)
class(str1) = "some_exotic_class"
pryr::address(str1)

add = function(...) UseMethod("add")
add.list = function(somelist) {
  Reduce(paste0, somelist)
}
str1 = structure(list(slot1 = "a", slot2 = "b"))
add(str1)
str2 = structure(list(slot1 = "a", slot2 = "b"), class = "mystr")
add(str2)   # this will fail even though the contents are identical str1


str3 = structure(list(1, 2, 3), class = "my_number_class")
incrementOne = function(...) UseMethod("incrementOne")
incrementOne.my_number_class = function(somelist) lapply(somelist, function(x) x+1)
incrementOne(str3)
str3
