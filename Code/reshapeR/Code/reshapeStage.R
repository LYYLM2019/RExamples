library(ggplot2)
library(reshape2)

# read in the data
dfStage = read.csv("reshapeR/Data/stage.csv", header = FALSE, stringsAsFactor = FALSE)
  
# remove the rows which are min, max, mean & redundant columns
condMMM = stringr::str_trim(dfStage[, 1]) %in% c("Min", "Max", "Mean", "Day")
dfStage = dfStage[!condMMM, 1:13]
dateVars = c("Day", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
colnames(dfStage) = dateVars

# get indices & names of year site combinations
condlSiteYear = grepl("^Daily means", stringr::str_trim(dfStage[, 1]))
condiSiteYear = grep("^Daily means", stringr::str_trim(dfStage[, 1]))
dfSiteYear = dfStage[condlSiteYear,  1, drop = FALSE]

# remove site-year rows from data
dfStage = dfStage[!condlSiteYear, ]

# get the list of sites and years
dfSiteYear$Year = regmatches(dfSiteYear[, 1], regexpr("(?<=Year\\s)([0-9]+)", dfSiteYear[, 1], perl = TRUE))
dfSiteYear$Site = regmatches(dfSiteYear[, 1], 
           regexpr("(?<=(Stage\\s\\(mm\\)\\sat\\s))([A-Za-z\\s0-9\\.]+)", dfSiteYear[, 1], perl = TRUE))

# add the site and years
dfSiteYearLong = dfSiteYear[rep(1:dim(dfSiteYear)[1], each = 31), c("Site", "Year")]
dfStageFinal = cbind(dfStage, dfSiteYearLong)

# reshape
dfStageFinalLong = reshape2::melt(dfStageFinal, id.vars = c("Day", "Site", "Year"), 
                                  measure.vars = dateVars[-1],
                        variable.name = "Month")
dfStageFinalWide = reshape2::dcast(dfStageFinalLong, Day + Month + Year ~ Site, 
                                   value.var = "value")

# cleanup
dfStageFinalWide[, -c(1:3)] = lapply(dfStageFinalWide[, -c(1:3)], as.numeric)

# create a date variable
dfStageFinalWide$Date = with(dfStageFinalWide, 
                             as.Date(paste(Day, Month, Year, sep = "-"), 
                                     format = "%d-%b-%Y"))
# remove the infeasible dates
dfStageFinalWide = dfStageFinalWide[!is.na(dfStageFinalWide$Date), ]
dfStageFinalWide = dfStageFinalWide[order(dfStageFinalWide$Date), ]

# plot the values over time
dfStageFinalLong = 
  reshape2::melt(dfStageFinalWide, id.vars = "Date", measure.vars = unique(dfSiteYear$Site),
       variable.name = "Site")
ggplot(dfStageFinalLong, aes(x = Date, y = value, color = Site))+
  geom_line() + theme_bw() + facet_wrap(~ Site, scale = "free_y") 
