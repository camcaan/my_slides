
library(tidyverse)
library(vroom)
library(sf) #spacial data
library(tigris) #for geographical data
library(leaflet) 
library(htmlwidgets)

# Import data 
download.file(url = "https://github.com/nychealth/coronavirus-data/archive/master.zip",
              destfile = "coronavirus-data-master.zip")

unzip(zipfile = "coronavirus-data-master.zip")

# Read in data 
percentPos <- vroom("coronavirus-data-master/trends/percentpositive-by-modzcta.csv")

caserates <- vroom("coronavirus-data-master/trends/caserate-by-modzcta.csv")

testrate <- vroom("coronavirus-data-master/trends/testrate-by-modzcta.csv")
modzcta <- st_read("coronavirus-data-master/Geography-resources/MODZCTA_2010.shp")

# zcta_conv <- vroom("coronavirus-data-master/Geography-resources/ZCTA-to-MODZCTA_2010.csv", delim = T)
