download.file("http://www.amstat.org/publications/jse/datasets/93cars.dat.txt", 
                             destfile = "Data/93cars.txt")
firstPart = pipe(description = "awk 'NR%2==0' < Data/93cars.txt")
secondPart = pipe(description = "awk 'NR%2 != 0' < Data/93cars.txt")
dfCars = cbind(read.table(firstPart), read.table(secondPart))


firstData = originalData[]
secondData

dfX = read.fwf("http://www.amstat.org/publications/jse/datasets/93cars.dat.txt", 
               widths = list(
                 c(14, 15, 8, 5, 5, 5, 3, 3, 2, 2, 2, 4, 4, 4), 
                 c(5, 2, 5, 2, 4, 4, 3, 3, 5, 3, 5, 1)
               ), 
               header = FALSE)