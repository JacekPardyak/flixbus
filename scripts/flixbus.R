library(rvest)
library(dplyr)
library(stringr)

# Load the webpage
url <- "https://www.flixbus.com/bus"
page <- read_html(url)
selector = ".alphabet-list-item a"
cities <- page %>%
  html_elements(css = selector) %>%
  html_text()
urls <- page %>%
  html_elements(css = selector) %>%
  html_attr("href")
cities <- data.frame(city = cities, url = paste0("https://www.flixbus.com", urls)) %>%
  mutate(routes = NA)
cities


routes <- tibble()
for(row in c(1:100)){
  city = cities %>% slice(row) %>% select(city) %>% pull()
  print(city)
  url = cities %>% slice(row) %>% select(url) %>% pull()
  print(url)
  page <- read_html(url)
  selector = ".flix-connection__item"
  trip <- page %>%
    html_elements(css = selector) %>%
    html_text() %>%
    str_replace_all("\n", " ") %>% 
    str_trim() %>%
    unique() %>%
    sort() %>%
    setdiff(city) %>%
    list()
  tmp <- tibble(city, url, trip)
  routes <- routes %>% bind_rows(tmp)
}

routes
