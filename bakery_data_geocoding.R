# Geocode bakery data ----

## Load clean bakery data ----

here() 

path <- here()

bakery_table <- read.csv(paste0(path,"/output/clean_bakery_table.csv"))

bakery_table <- bakery_table  %>% select( -c("X") )  # remove "X" column from reading in csv


## Geocode using ArcGIS method ----

bakery_table <- bakery_table %>% tidygeocoder::geocode(address = Address,
                                                       method = "arcgis",
                                                       long = "Longitude",  # new column will be created
                                                       lat = "Latitude")


## Save a copy of the geocoded data ----

write.csv( bakery_table, paste0(path,"/output/geocoded_bakery_table.csv") )