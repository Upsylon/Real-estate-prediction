
```{r, echo = FALSE}
my_theme <- function(base_size = 10, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 10),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 12),
      plot.title = element_text(hjust = 0.5),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", 
                                      color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 10, color = "black"),
      legend.position = "bottom",
      legend.justification = "top", 
      legend.box = "horizontal",
      legend.box.background = element_rect(colour = "grey50"),
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}
```

```{r}
get_immodata <- function(city_vector) {
  
  ### Getting all the cities in the form of a "list of lists"
  cities <- list()
  
  for (i in 1:length(city_vector)) {
    cities[[i]] <- list()
    names(cities)[i] = paste(city_vector[i])
  }
  
  ### Getting the URL of each cities in new variables linked to each of them:
  for (i in 1:length(cities)){
    attr(cities[[i]], which="url") <- paste(
      "https://www.immoscout24.ch/en/real-estate/rent/city-", 
      names(cities[i]), sep="") 
  }
  
  ### Getting the number of pages for each cities
  pages <- list()
  for( i in 1:length(cities)){
    pages[[i]] <- read_html(x = paste0(unlist(attributes(cities[[i]]), 
                                              use.names = FALSE))) %>%
      html_nodes(css = ".cXTlXt") %>%
      html_text() %>%
      as.numeric() %>%
      max(na.rm = TRUE) %>%
      subtract(e2 = 1) %>%
      seq(from = 1) 
  }
  
  ### Scrapping everything
  for (i in 1:length(cities)){
    for (page in pages[[i]]){  
      url_path_page_immoscout <- cities[[i]] %>% 
        attributes %>% 
        unlist(use.names = FALSE) %>% 
        paste0 %>%
        paste("?pn=", page, sep="") 
      
      cities[[i]][[page]] <- list()
      cities[[i]][[page]] <- read_html(url_path_page_immoscout) %>% 
        html_nodes(".csgoYM") %>%
        html_text()
    }
  }
  
  # unlisting
  unlisted_cities <- unlist(cities) %>% data.frame
  
  all_cities  <- data.frame()
  for (i in 1:length(cities)) {
    item_full_info <- unlisted_cities[grep(names(cities[i]), 
                                           rownames(unlisted_cities)),] 
    #extract number of rooms
    
    
    assign(paste("df_", names(cities[i]), sep=""),  
           data.frame(
             rooms = str_extract(item_full_info, ".*m\u00B2") %>%
               # first taking before m2 for the cases where the word "room" or "rooms" 
               # is mentionned in the description
               str_extract(., ".*rooms*") %>% 
               gsub(pattern = " rooms", replacement = "", fixed = TRUE) %>%
               gsub(pattern = " room", replacement = "", fixed = TRUE) %>% 
               as.numeric
             ,
             
             # Extract Size
             m2 = str_extract(item_full_info, ".*m\u00B2\u00AB") %>%
               str_extract(., ", .*") %>%
               gsub(pattern=" m\u00B2\u00AB", replacement = "", fixed = TRUE) %>%
               gsub(pattern= ", ", replacement = "", fixed = TRUE) 
             %>% as.integer
             ,
             
             # Extract localiation
             address =  str_extract(item_full_info, ".*Close") %>%
               str_extract(., ".*,") %>%
               str_extract(., "\u00bb.*") %>%
               gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
               gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
               gsub(pattern = "\u00bb", replacement = "", fixed = TRUE) %>%
               gsub(pattern = "\u00FC", replacement = "u", fixed = TRUE) %>%
               gsub(pattern = "\u00E4", replacement = "a", fixed = TRUE) %>%
               gsub(pattern = "\u00F6", replacement = "o", fixed = TRUE) %>%
               gsub(pattern = "\u00E8", replacement = "e", fixed = TRUE) %>%
               gsub(pattern = "\u00E9", replacement = "e", fixed = TRUE) %>%
               gsub(pattern = "\u00EA", replacement = "e", fixed = TRUE) %>%
               gsub(pattern = "\u00F4", replacement = "o", fixed = TRUE) %>%
               gsub(pattern = "\u00EF", replacement = "i", fixed = TRUE)
             ,
             
             #extract price
             price = str_extract(item_full_info, "eCHF .*") %>% str_extract(., ".*.\u2014 *") %>% 
               gsub(pattern = "eCHF ", replacement = "", fixed = TRUE) %>%
               gsub(pattern = ".\u2014", replacement = "", fixed = TRUE) %>%
               gsub(pattern = ",", replacement = "", fixed = TRUE) %>% as.integer
             ,
             
             # Assign the city
             city = names(cities[i])
           )
    )
    
    city <-  data.frame(
      rooms = str_extract(item_full_info, ".*\u00AB") %>%
        # first taking before m2 for the cases where the word "room" or "rooms" 
        # is mentionned in the description
        str_extract(., ".*room") %>% 
        gsub(pattern = " room", replacement = "", fixed = TRUE) %>% 
        as.numeric
      ,
      
      # Extract Size
      m2 = str_extract(item_full_info, ".*m\u00B2\u00AB") %>%
        # str_extract(., ", .*") %>%
        gsub(pattern=" m\u00B2\u00AB", replacement = "", fixed = TRUE) %>% 
        word(.,-1) %>% 
        as.integer
      ,
      
      #extract price
      price = str_extract(item_full_info, "eCHF .*") %>% 
        str_extract(., ".*.\u2014 *") %>% 
        gsub(pattern = "eCHF ", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ".\u2014", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ",", replacement = "", fixed = TRUE) %>% 
        as.integer
      ,
      
      # Extract localiation
      address =  str_extract(item_full_info, ".*Close") %>%
        str_extract(., ".*,") %>%
        str_extract(., "\u00bb.*") %>%
        gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "\u00bb", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "\u00FC", replacement = "u", fixed = TRUE) %>%
        gsub(pattern = "\u00E4", replacement = "a", fixed = TRUE) %>%
        gsub(pattern = "\u00F6", replacement = "o", fixed = TRUE) %>%
        gsub(pattern = "\u00E8", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00E9", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00EA", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00F4", replacement = "o", fixed = TRUE) %>%
        gsub(pattern = "\u00EF", replacement = "i", fixed = TRUE) %>%
        gsub(pattern = "\u00EB", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00EE", replacement = "e", fixed = TRUE) %>%
        gsub(pattern = "\u00E7", replacement = "c", fixed = TRUE) %>%
        gsub(pattern = "\u00E2", replacement = "a", fixed = TRUE) 
      ,
      
      postcode = str_extract(item_full_info, ".*Close") %>%
        str_extract(., ".*,") %>%
        str_extract(., "\u00bb.*") %>%
        gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ",", replacement = "", fixed = TRUE) %>% 
        gsub(pattern = "\u00bb", replacement = "", fixed = TRUE)  %>%
        word(., -2),
      
      city = str_extract(item_full_info, ".*Close") %>%
        str_extract(., ".*,") %>%
        str_extract(., "\u00bb.*") %>%
        gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
        gsub(pattern = ",", replacement = "", fixed = TRUE) %>% 
        gsub(pattern = "\u00bb", replacement = "", fixed = TRUE) %>%
        gsub(pattern = "\u00FC", replacement = "u", fixed = TRUE) %>%
        gsub(pattern = "\u00E8", replacement = "e", fixed = TRUE) %>%
        word(., -1)
    )
    all_cities <- rbind(all_cities, city)
  }
  
  # selecting only prices > 500 to get rid off most of wrong recorder data
  # and weakly rents
  all_cities <- all_cities[all_cities$price > 300,]
  
  # Deleting rows where we have NA (about 16% of the data)
  all_cities <- all_cities[complete.cases(all_cities),] 
  
  return(all_cities)
}

# The function enables to scrap all the data (except longitude/latitude) at once

all_cities <- get_immodata(c("zurich", "geneve", "basel", "lausanne"))
```


# Webscrapping

Example data structure de Iegor a rearranger

```{r}
cities <- list(
  zurich = list(),
  geneve = list(),
  basel = list(),
  lausanne = list(),
  bern = list(),
  winterthur = list(),
  lucerne = list(),
  lugano = list()
)
```

LOOPING OVER EACH CITY

Getting the URL of each cities in new variables linked to each of them:
  
  
  ```{r}
for (i in 1:length(cities)){
  attr(cities[[i]], which="url") <- paste(
    "https://www.immoscout24.ch/en/real-estate/rent/city-", 
    names(cities[i]), sep="") 
}
```

```{r}
pages <- list()
for( i in 1:length(cities)){
  pages[[i]] <- read_html(x = paste0(unlist(attributes(cities[[i]]), 
                                            use.names = FALSE))) %>%
    html_nodes(css = ".cXTlXt") %>%
    html_text() %>%
    as.numeric() %>%
    max(na.rm = TRUE) %>%
    subtract(e2 = 1) %>%
    seq(from = 1) 
}
```


Scrapping everything at once:
  ```{r}
for (i in 1:length(cities)){
  for (page in pages[[i]]){  
    url_path_page_immoscout <- cities[[i]] %>% 
      attributes %>% 
      unlist(use.names = FALSE) %>% 
      paste0 %>%
      paste("?pn=", page, sep="") 
    
    cities[[i]][[page]] <- list()
    cities[[i]][[page]] <- read_html(url_path_page_immoscout) %>% 
      html_nodes(".csgoYM") %>%
      html_text()
  }
}
```

Creating one big dataframe with all values of all cities to be used later (not cleaned yet) 
```{r}
unlisted_cities <- unlist(cities) %>% data.frame
```



Iegor's code
```{r}
item_full_info <- read_html(
"https://www.immoscout24.ch/en/real-estate/rent/city-lausanne?pn=1") %>%
html_nodes(".csgoYM") %>%
html_text()
```


Creating a dataframe for each city

Getting only the big dataframe (named all_cities) directly without creating "subdataframes"

```{r}
all_cities <- data.frame()
for (i in 1:length(cities)) {
item_full_info <- unlisted_cities[grep(names(cities[i]), 
rownames(unlisted_cities)),] 
#extract number of rooms


assign(paste("df_", names(cities[i]), sep=""),  
data.frame(
rooms = str_extract(item_full_info, ".*m\u00B2") %>%
# first taking before m2 for the cases where the word "room" or "rooms" 
# is mentionned in the description
str_extract(., ".*rooms*") %>% 
gsub(pattern = " rooms", replacement = "", fixed = TRUE) %>%
gsub(pattern = " room", replacement = "", fixed = TRUE) %>% 
as.numeric
,

# Extract Size
m2 = str_extract(item_full_info, ".*m\u00B2\u00AB") %>%
str_extract(., ", .*") %>%
gsub(pattern=" m\u00B2\u00AB", replacement = "", fixed = TRUE) %>%
gsub(pattern= ", ", replacement = "", fixed = TRUE) 
%>% as.integer
,

# Extract localiation
address =  str_extract(item_full_info, ".*Close") %>%
str_extract(., ".*,") %>%
str_extract(., "\u00bb.*") %>%
gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
gsub(pattern = "\u00bb", replacement = "", fixed = TRUE) %>%
gsub(pattern = "\u00FC", replacement = "u", fixed = TRUE) %>%
gsub(pattern = "\u00E4", replacement = "a", fixed = TRUE) %>%
gsub(pattern = "\u00F6", replacement = "o", fixed = TRUE) %>%
gsub(pattern = "\u00E8", replacement = "e", fixed = TRUE) %>%
gsub(pattern = "\u00E9", replacement = "e", fixed = TRUE) %>%
gsub(pattern = "\u00EA", replacement = "e", fixed = TRUE) %>%
gsub(pattern = "\u00F4", replacement = "o", fixed = TRUE) %>%
gsub(pattern = "\u00EF", replacement = "i", fixed = TRUE)
,

#extract price
price = str_extract(item_full_info, "eCHF .*") %>% str_extract(., ".*.\u2014 *") %>% 
gsub(pattern = "eCHF ", replacement = "", fixed = TRUE) %>%
gsub(pattern = ".\u2014", replacement = "", fixed = TRUE) %>%
gsub(pattern = ",", replacement = "", fixed = TRUE) %>% as.integer
,

# Assign the city
city = names(cities[i])
)
)

city <-  data.frame(
rooms = str_extract(item_full_info, ".*\u00AB") %>%
# first taking before m2 for the cases where the word "room" or "rooms" 
# is mentionned in the description
str_extract(., ".*room") %>% 
gsub(pattern = " room", replacement = "", fixed = TRUE) %>% 
as.numeric
,

# Extract Size
m2 = str_extract(item_full_info, ".*m\u00B2\u00AB") %>%
# str_extract(., ", .*") %>%
gsub(pattern=" m\u00B2\u00AB", replacement = "", fixed = TRUE) %>% 
word(.,-1) %>% 
as.integer
,

#extract price
price = str_extract(item_full_info, "eCHF .*") %>% 
str_extract(., ".*.\u2014 *") %>% 
gsub(pattern = "eCHF ", replacement = "", fixed = TRUE) %>%
gsub(pattern = ".\u2014", replacement = "", fixed = TRUE) %>%
gsub(pattern = ",", replacement = "", fixed = TRUE) %>% 
as.integer
,

# Extract localiation
address =  str_extract(item_full_info, ".*Close") %>%
str_extract(., ".*,") %>%
str_extract(., "\u00bb.*") %>%
gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
gsub(pattern = ",", replacement = "", fixed = TRUE) %>%
gsub(pattern = "\u00bb", replacement = "", fixed = TRUE) %>%
gsub(pattern = "\u00FC", replacement = "u", fixed = TRUE) %>%
gsub(pattern = "\u00E4", replacement = "a", fixed = TRUE) %>%
gsub(pattern = "\u00F6", replacement = "o", fixed = TRUE) %>%
gsub(pattern = "\u00E8", replacement = "e", fixed = TRUE) %>%
gsub(pattern = "\u00E9", replacement = "e", fixed = TRUE) %>%
gsub(pattern = "\u00EA", replacement = "e", fixed = TRUE) %>%
gsub(pattern = "\u00F4", replacement = "o", fixed = TRUE) %>%
gsub(pattern = "\u00EF", replacement = "i", fixed = TRUE) %>%
gsub(pattern = "\u00EB", replacement = "e", fixed = TRUE) %>%
gsub(pattern = "\u00EE", replacement = "e", fixed = TRUE) %>%
gsub(pattern = "\u00E7", replacement = "c", fixed = TRUE) %>%
gsub(pattern = "\u00E2", replacement = "a", fixed = TRUE) 
,

postcode = str_extract(item_full_info, ".*Close") %>%
str_extract(., ".*,") %>%
str_extract(., "\u00bb.*") %>%
gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
gsub(pattern = ",", replacement = "", fixed = TRUE) %>% 
gsub(pattern = "\u00bb", replacement = "", fixed = TRUE)  %>%
word(., -2),

city = str_extract(item_full_info, ".*Close") %>%
str_extract(., ".*,") %>%
str_extract(., "\u00bb.*") %>%
gsub(pattern = "Close", replacement = "", fixed = TRUE) %>%
gsub(pattern = ",", replacement = "", fixed = TRUE) %>% 
gsub(pattern = "\u00bb", replacement = "", fixed = TRUE) %>%
gsub(pattern = "\u00FC", replacement = "u", fixed = TRUE) %>%
gsub(pattern = "\u00E8", replacement = "e", fixed = TRUE) %>%
word(., -1)
)
all_cities <- rbind(all_cities, city)
}

# selecting only prices > 500 to get rid off most of wrong recorder data
# and weakly rents
all_cities <- all_cities[all_cities$price > 300,]
all_cities <- all_cities[all_cities$price < 10000,]

# Deleting rows where we have NA (about 16% of the data)
all_cities <- all_cities[complete.cases(all_cities),] 

all_cities <- cbind(all_cities, latitude = NA, longitude = NA)
```

#### Genereting map 

```{r}
# library(googleway)
# API key obtained with the test period (google)
# set_key("AIzaSyBQd93y1YsbrWKxMgnaRXoX0Wg6F-fiCqc")

# Create a df with raw information about the address coordinates
# raw_coordinates <- google_geocode(address = "Via Monte Carmen 8 6900 Lugano")


# Create a df with raw information about the address coordinates
# raw_coordinates <- google_geocode(address = "avenue de l'église anglaise 10 1006 lausanne")

# Create a df with only a column lng and a column ltd
# coordinates_df <- raw_coordinates$results$geometry$location

# Using leaflet package to map the coordinates
leaflet(all_cities, options = leafletOptions(minZoom = 7.4)) %>%
setMaxBounds(5.5, 48.2, 11, 45.3) %>%
addTiles() %>%
addCircles(
lng = all_cities$longitude,
lat = all_cities$latitude,
color = ifelse(all_cities$price < all_cities$predicted_price, "green", "red"),
popup = paste(
"<b>Price :</b>",
all_cities$price,
"   CHF",
"<br/>",
"<b>Predicted price :</b>",
all_cities$predicted_price,
" CHF",
"<br/>",
"<b>Adress :</b>",
all_cities$address,
"<br/>",
"<b>Number of rooms :</b>",
all_cities$rooms,
"<br/>",
"<b>Size :</b>",
all_cities$m2,
" m2"

)
)
```

```{r}

# GEOCODING FOR ALL THE ADDRESSES

# for (i in 2911:nrow(all_cities)) {
#   
#   location <- google_geocode(
#     as.vector(all_cities$address[i])
#   )$results$geometry$location
#   
#   if(!is.null(location)) {
#     all_cities[i, "latitude"] <- location$lat[1]
#     all_cities[i, "longitude"] <- location$lng[1]
#     
#   }
# }

# write.csv(all_cities, file = "all_cities.csv")
```


predict_price <- function(housings, rooms, m2, city, model = "rf", seed = 1) {
  
set.seed(seed)

if (missing(housings) && (missing(rooms) || missing(m2) || missing(city))){
message("Please enter an appropriate dataframe or complete informations")
} else if (missing(housings) && !missing(rooms) && !missing(m2) && !missing(city)) {
message("The output is the estimated price for an unique housing")
} else if (
!missing(housings) && (!missing(rooms) || !missing(m2) || !missing(city))
) { 
message("The output will be calculated based only on the dataframe")
}

if (!missing(housings)) {

model_used <- train(form = price ~ rooms + m2 + city,
data = housings,
method = model)

predictions <- predict(model_used)

df_predict <- housings %>% mutate(predicted_price = predictions)

rval <- list(
df_predict = df_predict,
points = data.frame(
predictions = predictions, 
real_price = housings$price %>% as.double)
)

class(rval) <- "pred"

return(rval)
}

if (missing(housings) && !missing(rooms) && !missing(m2) && !missing(city)) {
housings <- get_immodata(city)

model_used <- train(form = price ~ rooms + m2,
data = housings,
method = model)
predictions <- predict(model_used, newdata = city)
return(paste("The predicted price for this housing is", 
round(predictions, 0), 
"CHF."
)
)
}
}

