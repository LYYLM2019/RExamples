library(readr)
dfX = readr::read_csv("Name,Age,Weight,City
Sam,13,30,
                 John,35,58,CA
                 Doe,20,50,IL
                 Ann,18,45,", na = "")

is.na(dfX$City)