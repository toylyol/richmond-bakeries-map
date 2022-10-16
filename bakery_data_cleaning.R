# Clean bakery data scraped from web ----

## Load scraped data ----

here()

path <- here()

bakery_table <- read.csv(paste0(path,"/output/raw_bakery_data.csv"))


## Create some custom functions ----

extractDescription <- function(x){
  
  x <- stringr::str_extract(x, "(?<=:\\s).*")   # extract string after colon and whitespace
  x <- sub(".[^.]*$", ".", x)                   # extract string before last period and substitute period
  return(x)
  
}

extractAddress <- function(x){
  
  x <- str_extract( x,"(?<=\\s)\\d(...).*," )
  x <- paste0(x," Richmond, VA")
  return(x)
  
}


## Delineate some special categories ----

other_city <-c("7 Muffins a Day", "Anna B's Gluten Free", "Beaunuts", "Buttermilk Bake Shop",
               "River City Chocolate", "Sugar Shack", "Sweets by Keet", "The Treat Shop",
               "Williams Bakery", "Wonder City Bakery")

multi_location <- c("Early Bird Biscuit Company", "The Treat Shop", "Williams Bakery", "Coco + Hazel",
                    "Country Style Donuts", "Crumbl Cookies", "Sugar Shack")

online <- c("Arley Cakes", "Axelsdotter", "JC Desserts", "The Weekly Bake")

farmer_market <- c("Norwood Cottage Bakery", "Poor Georgie’s Bake Shop")  # note curly apostrophe

appt_only <- c("Keya & Co. Baking", "Morsels")


subset_list <- c("Minglewood Bake Shop", "Mixing Bowl Bakery", "Montana Gold Bread Co.",
                 "Panaderia El Globo", "Sub Rosa Bakery")                                 # numerals in the bakery name

abbrev_street <- c("Beaunuts", "Brecotea Baking Studio", "Cameo Cakery and Cafe", "Can Can Brasserie", 
                   "Carytown Cupcakes", "Claudia’s Bake Shop", "Europa Crust", "Fat Rabbit", "Flour Garden Bakery", 
                   "Frostings", "Idle Hands Bread Company", "Keya & Co. Baking", "Lebanese Bakery", "Sunflower Gardenz",
                   "Up All Night Bakery") # note the curly apostrophe in Claudia's


## Isolate bakeries' addresses ----

bakery_table <- bakery_table %>%
  mutate( address = info )          # create new column and copy all info


bakery_table$address <- sapply(bakery_table$address, extractAddress)


bakery_table$address <- ifelse(bakery_table$bakery_name %in% other_city,          # fix address for cities other than Richmond
                               sub("Richmond, VA", "VA", bakery_table$address),
                               bakery_table$address
)


bakery_table$address <- str_replace_all(bakery_table$address, "NA Richmond, VA", "")     # remove NA

bakery_table$info <- extractDescription(bakery_table$info)  # first-round of cleaning descriptions


### Handle bakeries with multiple locations ----

multi_bakery <- bakery_table %>%                      # create df with bakeries with multiple locations
  filter( bakery_name %in% multi_location ) %>%
  mutate ( count = 1 + (str_count(address, ";")) )    # add new column w/ count of locations


multi_bakery$info <- str_replace( multi_bakery$info, "\\..*", ".")    # remove text after first occurrence of a period


multi_bakery <- data.frame(lapply(multi_bakery, rep, multi_bakery$count))   # multiply rows based on location count


#### Rename to disambiguate each location ----

multi_bakery$bakery_name[[1]] <- paste0(multi_bakery$bakery_name[[1]]," #1") 
multi_bakery$bakery_name[[2]] <- paste0(multi_bakery$bakery_name[[2]]," #2")

multi_bakery$bakery_name[[3]] <- paste0(multi_bakery$bakery_name[[3]]," #1")
multi_bakery$bakery_name[[4]] <- paste0(multi_bakery$bakery_name[[4]]," #2")

multi_bakery$bakery_name[[5]] <- paste0(multi_bakery$bakery_name[[5]]," #1")
multi_bakery$bakery_name[[6]] <- paste0(multi_bakery$bakery_name[[6]]," #2")
multi_bakery$bakery_name[[7]] <- paste0(multi_bakery$bakery_name[[7]]," #3")

multi_bakery$bakery_name[[8]] <- paste0(multi_bakery$bakery_name[[8]]," #1")
multi_bakery$bakery_name[[9]] <- paste0(multi_bakery$bakery_name[[9]]," #2")

multi_bakery$bakery_name[[10]] <- paste0(multi_bakery$bakery_name[[10]]," #1")
multi_bakery$bakery_name[[11]] <- paste0(multi_bakery$bakery_name[[11]]," #2")

multi_bakery$bakery_name[[12]] <- paste0(multi_bakery$bakery_name[[12]]," #1")
multi_bakery$bakery_name[[13]] <- paste0(multi_bakery$bakery_name[[13]]," #2")

multi_bakery$bakery_name[[14]] <- paste0(multi_bakery$bakery_name[[14]]," #1")
multi_bakery$bakery_name[[15]] <- paste0(multi_bakery$bakery_name[[15]]," #2")
multi_bakery$bakery_name[[16]] <- paste0(multi_bakery$bakery_name[[16]]," #3")


#### Add each location's address ---- 

multi_bakery$address[[1]] <- "2733 McRae Road, Bon Air, VA"
multi_bakery$address[[2]] <- "411 N. Ridge Road, Richmond, VA"

multi_bakery$address[[3]] <- "4300 Williamsburg Road, Richmond, VA"
multi_bakery$address[[4]] <- "8900 W. Broad St., Richmond, VA"

multi_bakery$address[[5]] <- "7316 Bell Creek Road, Mechanicsville, VA"
multi_bakery$address[[6]] <- "2003 Huguenot Road, Suite 104, Richmond, VA"
multi_bakery$address[[7]] <- "9623 W. Broad St., Richmond, VA"

multi_bakery$address[[8]] <- "119 N. Robinson St., Richmond, VA"
multi_bakery$address[[9]] <- "1221 Bellevue Ave., Richmond, VA"

multi_bakery$address[[10]] <- "1001 N. Lombardy St., Richmond, VA"
multi_bakery$address[[11]] <- "1931 Huguenot Road, North Chesterfield, VA"

multi_bakery$address[[12]] <- "6114 Jahnke Road, Richmond, VA"
multi_bakery$address[[13]] <- "14736 Village Square Place, Midlothian, VA"

multi_bakery$address[[14]] <- "8084 Mechanicsville Turnpike, Mechanicsville, VA"
multi_bakery$address[[15]] <- "9502 Chamberlayne Road, Mechanicsville, VA"
multi_bakery$address[[16]] <- "309 E. Nine Mile Road, Richmond, VA"


#### Save a copy of multi_bakery ----

multi_bakery %>%
  select( -c(X, count) ) %>%
  write.csv( paste0(path, "/output/multi_bakery_data.csv") )


### Isolate bakeries with numerals in description ----

num_descrip <- bakery_table %>%
  filter( bakery_name %in% subset_list )


num_descrip$address[[1]] <- "3337 W. Cary St., Richmond, VA"
num_descrip$address[[2]] <- "8540 Patterson Ave., Richmond, VA"
num_descrip$address[[3]] <- "3543 W. Cary St., Richmond, VA"
num_descrip$address[[4]] <- "5701 Hull St., Richmond, VA"
num_descrip$address[[5]] <- "620 N. 25th St., Richmond, VA"


### Clean up num_descrip description ----

num_descrip$info <- str_replace( num_descrip$info, "\\..*", ".")    # remove text after first occurrence of a period


#### Save a copy of num_descrip ----

num_descrip %>%
  select( -c(X) ) %>%
  write.csv( paste0(path, "/output/num_descrip_data.csv") )


## Clean up descriptions for bakeries with abbreviated street ----

abbrev_bakery <- bakery_table %>%
  filter( bakery_name %in% abbrev_street )

abbrev_bakery$info <- str_replace(abbrev_bakery$info, "\\s[0-9].+", "")    # remove digits/text after whitespace+digits


abbrev_bakery$info[[7]] <- gsub(" opening in early April", "", abbrev_bakery$info[[7]]) # Claudia's 
abbrev_bakery$address[[7]] <- "3027 W. Cary St., Richmond, VA"

abbrev_bakery$info[[9]] <- gsub(" Opening in April in Union Hill.", "", abbrev_bakery$info[[9]])  # Fat Rabbit

abbrev_bakery$info[[14]] <- gsub("1810 Powhatan St.", "", abbrev_bakery$info[[14]]) # Sunflower Gardenz
abbrev_bakery$address[[14]] <- "1810 Powhatan St., Richmond, VA"

abbrev_bakery$address[[15]] <- "5411 Lakeside Ave., Richmond, VA" # Up All Night


### Save a copy of abbrev_bakery ----

abbrev_bakery %>% 
  select( -c(X) ) %>%
  write.csv( paste0(path, "/output/abbrev_bakery_data.csv") )


## Clean up bakery_table descriptions ----

bakery_table$info <- str_replace(bakery_table$info, "\\s[0-9].+", "")    # remove any character after whitespace+digits


## Merge separate dfs with bakery_table ----

bakery_table <- bind_rows(bakery_table, multi_bakery) %>%
  filter( !bakery_name %in% subset_list ) %>%                 # remove uncleaned bakery entries
  filter( !bakery_name %in% abbrev_street) %>%
  bind_rows( abbrev_bakery ) %>%
  bind_rows( num_descrip ) %>%
  select( -c(X, count) ) %>%                                  # remove count column and X column (from reading in csv)
  filter( !bakery_name %in% multi_location ) %>%              # remove rows with multiple addresses in one field
  arrange( bakery_name )                                      # arrange alphabetically by name


## Add columns for special categories ----

# Add filters for special categories: gluten-free, dairy-free, vegan, deliver, farmer's market, 
# online, multiple locations, and appointment only


bakery_table <- bakery_table %>% mutate( 
  `Gluten-Free` = case_when( 
    str_detect(info, "gluten-free") ~ "Yes",
    str_detect(info, "Gluten-free") ~ "Yes",
    TRUE ~ "No"
  )
)


bakery_table <- bakery_table %>% mutate( 
  Vegan = case_when( 
    str_detect(info, "vegan") ~ "Yes",
    str_detect(info, "Vegan") ~ "Yes",
    TRUE ~ "No"
  )
)


bakery_table <- bakery_table %>% mutate( 
  `Appointment Only` = case_when( 
    str_detect(info, "appointment") ~ "Yes",
    str_detect(info, "custom order") ~ "Yes",
    TRUE ~ "No"
  )
)


bakery_table <- bakery_table %>% mutate( 
  Delivery = case_when( 
    str_detect(info, "deliver") ~ "Yes",
    str_detect(info, "deliveries") ~ "Yes",
    str_detect(info, "delivery") ~ "Yes",
    TRUE ~ "No"
  )
)

bakery_table$`Delivery` <- ifelse(bakery_table$`Bakery Name` == "Anna B’s Gluten Free", "Yes", bakery_table$`Delivery`) # note curly apostrophe


bakery_table <- bakery_table %>% mutate( 
  `Dairy-Free` = case_when( 
    str_detect(info, "dairy") ~ "Yes",
    TRUE ~ "No"
  )
)


bakery_table <- bakery_table %>% mutate( 
  `Multiple Locations` = case_when( 
    str_detect(bakery_name, "#1") ~ "Yes",
    str_detect(bakery_name, "#2") ~ "Yes",
    str_detect(bakery_name, "#3") ~ "Yes",
    TRUE ~ "No"
  )
)


bakery_table <- bakery_table %>% mutate( Online = NA )
bakery_table$Online <-  ifelse( bakery_table$bakery_name %in% online, "Yes", "No" )


bakery_table <- bakery_table %>% mutate( `Farmer's Market` = NA )
bakery_table$`Farmer's Market` <-  ifelse( bakery_table$bakery_name %in% farmer_market, "Yes", "No" )


## Capitalize and rename columns ----

bakery_table <- bakery_table %>% rename(
  `Bakery Name` = bakery_name,
  Description = info,
  Address = address
)

## Save a copy of clean bakery_table ----

write.csv( bakery_table, paste0(path, "/output/clean_bakery_table.csv") )

