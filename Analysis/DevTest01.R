################################################################################
####### Get data from devtest ArtDealer and plot a little bit the choices ######
################################################################################
library(RMySQL)
dbConnection <- dbConnect(MySQL(), host="framboos.ugent.be", user="s238544", password="TKgjuuxk2tXM", dbname="braemlab")
data <- dbReadTable(dbConnection,"`TEST_data_log`")



