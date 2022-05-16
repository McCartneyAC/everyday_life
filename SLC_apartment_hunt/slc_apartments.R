library(tidyverse)
library(rvest)
library(DataExplorer)
library(data.table)


# craigslist querty
location <- "saltlakecity"
#area <- "sfc" #focus on postings in sfc specifically
hasPic <- 1 #don't want sketchy postings without pictures
min_bed <- 1 #guarantees the posts have some data to scrape
min_bath <- 1 #same as above
minSqft <- 0 #same as above
availabilityMode <- 0 
laundry <- 1 #I really want in-unit laundry, so I included this

# Constructing The Query by Concatenating The Features Above
# baseurl <- paste0("https://", location, ".craigslist.org/search/", area, "/apa")
baseurl<-paste0(
  "https://saltlakecity.craigslist.org/d/apts-housing-for-rent/search/apa"
  
)
queries <- c("?")
queries <- c(queries, paste0("hasPic=", hasPic))
queries <- c(queries, paste0("min_bedrooms=", min_bed))
queries <- c(queries, paste0("min_bathrooms=", min_bath))
queries <- c(queries, paste0("minSqft=", minSqft))
queries <- c(queries, paste0("availabilityMode=", availabilityMode))
queries <- c(queries, paste0("laundry=", laundry))
query_url <- paste0(baseurl,queries[1], paste(queries[2:length(queries)], collapse = "&"))

# actual scraping
# QUERY CRAIGSLIST
raw_query <- xml2::read_html(query_url)
raw_ads <- html_nodes(raw_query, "li.result-row")

# EXTRACT RELEVANT ATTRIBUTES
ids <- raw_ads %>%
  html_attr('data-pid')

titles <- raw_ads %>%
  html_node("a.result-title") %>%
  html_text()

links <- raw_ads %>% 
  html_node(".result-title") %>% 
  html_attr('href')

prices <- raw_ads %>% 
  html_node("span.result-price") %>%
  html_text() %>%
  str_extract("[0-9]+") %>%
  as.numeric()

dates <- raw_ads%>%
  html_node('time') %>%
  html_attr('datetime')

locales <- raw_ads %>%
  html_node(".result-hood") %>%
  html_text()

bedrooms <- raw_ads %>% 
  html_node("span.housing") %>% 
  html_text() %>% 
  str_extract("[0-9]+") %>% 
  as.numeric()

sqft <- raw_ads %>% 
  html_node("span.housing") %>% 
  html_text() %>% 
  gsub(".*-\\s([^.]+)[f][t].*","\\1",.) %>% 
  as.numeric()

latlongs <- map_dfr(links, function(x){
  xml2::read_html(x) %>% 
    html_node("#map") %>%
    html_attrs() %>%
    t() %>%
    as_tibble() %>%
    select_at(vars(starts_with("data-"))) %>%
    mutate_all(as.numeric)
}
)

# COMBINE INTO DATA FRAME
craigslist <- data.frame(ids, locales, prices, bedrooms, sqft, dates, titles, latlongs, links) %>% as_tibble()

# first page only:

craigslist
# save the basic
craigslist <- as.data.table(craigslist)
getwd()
setwd("C:\\Users\\Andrew\\Desktop\\Statistics and Data Analysis")
fwrite(craigslist, file = "craigslist.csv")

# loop through pages:
loopn <- seq(120, 600, 120)

for(i in loopn){
  Sys.sleep(5) #delays each query by 5 seconds
  queriesloop <- queries
  
  # ADD OFFSET TO URL IN INTERVALS OF 120
  queriesloop <- c(queries, paste0("s=", i))
  query_url <- paste0(baseurl,queriesloop[1], paste(queriesloop[2:length(queriesloop)], collapse = "&"))
  
  # The following loop body is going to be repetitive, but important!
  
  # QUERY CRAIGSLIST
  raw_query <- xml2::read_html(query_url)
  
  raw_ads <- html_nodes(raw_query, "li.result-row")
  
  # EXTRACT ATTRIBUTES
  ids <- raw_ads %>% html_attr('data-pid')
  titles <- raw_ads %>% html_node("a.result-title") %>% html_text()
  links <- raw_ads %>% html_node(".result-title") %>% html_attr('href')
  prices <- raw_ads %>% html_node("span.result-price") %>% html_text() %>% str_extract("[0-9]+") %>% as.numeric()
  dates <- raw_ads %>% html_node('time') %>% html_attr('datetime')
  locales <- raw_ads %>% html_node(".result-hood") %>% html_text()
  bedrooms <- raw_ads %>% html_node("span.housing") %>% html_text() %>% str_extract("[0-9]+") %>% as.numeric()
  sqft <- raw_ads %>% html_node("span.housing") %>% html_text() %>% gsub(".*-\\s([^.]+)[f][t].*","\\1",.) %>% as.numeric()
  latlongs <- map_dfr(links, function(x){
    xml2::read_html(x) %>% 
      html_node("#map") %>%
      html_attrs() %>%
      t() %>%
      as_tibble() %>%
      select_at(vars(starts_with("data-"))) %>%
      mutate_all(as.numeric)
  }
  )
  
  craigslistloop <- data.frame(ids, locales, prices, bedrooms, sqft, dates, titles, latlongs, links) %>% as_tibble()
  
  # RBIND POSTS IN EACH LOOP TO THE MASTER CRAIGSLIST DATA FRAME
  craigslist <- rbind(craigslist, craigslistloop)
  
}

# multiple pages:

craigslist <- craigslist %>% 
  as_tibble()

craigslist
work <- c(40.673639, -111.958067)
craigslist <- craigslist %>% 
  mutate(north = data.longitude + 111.958067, 
         north_work = north * 53)
craigslist %>% 
  psych::describe(fast = TRUE)

mean(craigslist$sqft, na.rm = T)
craigslist %>% 
  edlf8360::center(sqft, mean(sqft, na.rm = T)) %>% 
  lm(data = ., 
     prices ~ sqft+bedrooms + north_work
     ) %>% 
  summary()


craigslist %>% 
  ggplot(aes(x = north_work)) +
  geom_histogram()




craigslist %>% 
  filter(bedrooms < 3) %>% 
  filter(north_work < 12) %>% 
  filter(north_work > 0) %>% 
  edlf8360::center(sqft, mean(sqft, na.rm = T)) %>% 
  ggplot(aes(x = sqft, y = prices, color = factor(bedrooms), alpha = north_work)) + 
  geom_point(stroke = 0) +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Prices of SLC Apartments",
       x = "Square footage relative to Average (865 sq feet)", 
       y = "Monthly Rent", 
       subtitle = "Darker Points Indicate Farther from Work") +
  scale_y_continuous(labels = scales::dollar)+
  university::scale_color_wm() + 
  theme_light() + 
  guides(smooth = FALSE)






craigslist <- craigslist  %>% 
  filter(bedrooms < 3) %>% 
  filter(north_work < 25) %>% 
  filter(north_work > -25) %>% 
  filter(prices != 0)
  

# models? 
(-111.85 + 111.958067) * 53

1451 + (1.3*-76) -142(0) + (5.72*5.9)


craigslist %>% 
  filter(bedrooms ==2) %>% 
  filter(prices < 1000) %>% 
  filter(sqft > 865) %>% 
  select(ids)
craigslist %>% 
  mccrr::dossier(ids, 7145799439)
craigslist %>% 
  mccrr::dossier(ids, 7143257180)


#  mor's corners
bottom_left <- c(40.644808, -111.955843)
top_left <- c(40.762970, -111.947881)
top_right<-c(40.751061, -111.824990)
bottom_right<-c(40.664743, -111.823215)

top <- 40.762970
bottom <- 40.644808
left <-   -111.955843
right <-  -111.823215
mors_corners <- craigslist %>% 
  filter(data.latitude < top) %>% 
  filter(data.latitude > bottom) %>% 
  filter(data.longitude < right) %>% 
  filter(data.longitude > left)


mean(mors_corners$sqft, na.rm = T)

mors_corners %>% 
  filter(bedrooms < 3) %>% 
  filter(north_work < 12) %>% 
  filter(north_work > 0) %>% 
  edlf8360::center(sqft, mean(sqft, na.rm = T)) %>% 
  ggplot(aes(x = sqft, y = prices, color = factor(bedrooms), alpha = north_work)) + 
  geom_point(stroke = 0) +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Prices of SLC Apartments",
       x = "Square footage relative to Average (860 sq feet)", 
       y = "Monthly Rent", 
       subtitle = "Within Morioh's Corners",
       caption = "Darker Points Indicate Farther from Work") +
  scale_y_continuous(labels = scales::dollar)+
  university::scale_color_wm() + 
  theme_light() + 
  geom_hline(yintercept = 1300, color = "red")
  guides(smooth = FALSE)




selects<-mors_corners %>% 
  filter(prices < 1300) %>% 
  filter(bedrooms == 2) %>% 
  select(links) 

fwrite(selects, file = "mors_selects.csv")

