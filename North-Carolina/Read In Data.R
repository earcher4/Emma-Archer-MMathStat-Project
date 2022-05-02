library(geojsonio)
spdf <- geojson_read("Voting_Precincts (1).geojson",  what = "sp")


library("readxl")
library("dplyr")
D = read_excel("NC_data.xlsx")
D = D[order(D$OBJECTID),]

eta = D$DISTRICT
