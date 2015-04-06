#====================================================================
# purpose: examples of the use of RMySQL
# author: tirthankar chakravarty
# created: 6th april 2015
# revised:
# comments:
# 1. Some pages on using dplyr with a MySQL backend:
#   - http://zevross.com/blog/2014/03/26/four-reasons-why-you-should-check-out-the-r-package-dplyr-3/
#   - http://cran.rstudio.com/web/packages/dplyr/vignettes/databases.html
# 2. Create a credentials file to save the username and password:
#   - http://blog.ironholds.org/using-stored-credentials-with-rmysql/
#====================================================================

rm(list = ls())
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

#================================================
# establish a connection to the local MySQL server
#================================================
connLocalDB =  dbConnect(MySQL(), user = "root", password = "")                # connect to the local server
dbListConnections(MySQL())                                                     # list the open connections
dbSendQuery(connLocalDB, "create database dbExample")                          # create a new database
dbSendQuery(connLocalDB, "use dbExample")                                      # use the new database
dbListTables(connLocalDB)                                                      # list the tables in the database
dbSendQuery(connLocalDB, "drop database dbExample")                            # drop the database that has just been created

#================================================
# establish a connection to remote MySQL server
#================================================
connRemoteDB =  dbConnect(MySQL(),                                             # credentials are saved in ~/.my.cnf
                          host = "192.168.18.53",
                          dbname = "webscopedata")                             # connect to db on remote server on 192.168.18.53
dbListTables(connRemoteDB)                                                     # get the list of tables in the database

#================================================
# get the fields & column info in a table
#================================================
dbListFields(connRemoteDB, "bidders")                                          # get the list of fields in the table
dbColumnInfo(connRemoteDB, "bidders")                                          # get the column info for the table
dbGetQuery(connRemoteDB, statement = "select * from bidders limit 5")          # run a select query against the table
dbGetQuery(connRemoteDB, statement = "select count(*) from bidders")

#================================================
# write a new table from a data.frame
#================================================
dfX = data.frame(letters1 = sample(letters, 5), letters2 = sample(LETTERS, 5)) # create a dummy data.frame to test writes to the MySQL table
dbWriteTable(connRemoteDB, name = "deleteTable", value = dfX)                  # write the table, without first having created the table
dbListTables(connRemoteDB)                                                     # list the tables; a new one has been added
dbColumnInfo(connRemoteDB, "deleteTable")                                      # get the information on the newly created table
if(dbExistsTable(connRemoteDB, "deleteTable"))
  dbRemoveTable(connRemoteDB, "deleteTable")                                   # if the table exists; then drop it

#================================================
# create the table beforehand and then populate it,
#   so that we can fix the types
#================================================
dbSendQuery(connRemoteDB, "
            create table X (
            row_names int,
            letters1 varchar(10),
            letters2 varchar(10))")                                            # create the table schema beforehand
dbWriteTable(connRemoteDB, name = "X", value = dfX, append = TRUE)             # write the data to the table
dbListTables(connRemoteDB)                                                     # check that the table has been created
dbColumnInfo(connRemoteDB, "X")                                                # get column info for the table
dbGetQuery(connRemoteDB, "select * from X")                                    # get results of a query
dbReadTable(connRemoteDB, "X")                                                 # equivalent way of doing the above
if(dbExistsTable(connRemoteDB, "X")) dbRemoveTable(connRemoteDB, "X")          # remove the table
dbGetQuery(connRemoteDB, statement = "show table status like 'bidders'")       # different way to get (approximate) row count

#==========================================================
# check that dplyr works well with a database backend
#==========================================================
# aggregate the number of impressions by daynum
connRemoteDB2 = src_mysql("webscopedata",
                          host = "192.168.18.53",
                          user = NULL, password = NULL)                        # user & pass NULL to use the .my.cnf file
tblWebBid = tbl(connRemoteDB2, "bidders")                                      # create a link to MySQL table

tblBidDay = tblWebBid %>%                                                      #====================================
  group_by(daynum) %>%                                                         # group_by day number
  summarize(daily_imp = sum(impressions),
            daily_clicks = sum(clicks)) %>%                                    # aggregate impressions & clicks by day
  collect()                                                                    # collect the results in a local table

# plot the number of impressions by day
tblBidDay %>%
  gather(click_imp, num, -daynum) %>%
  ggplot(aes(x = daynum, y = num, group = click_imp, color = click_imp)) +
  geom_line() + theme_bw() + facet_wrap( ~ click_imp, scales = "free")