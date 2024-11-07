library(tidyverse)
cities_data <- read_csv("flixbus/cities_data.csv")
df <- read_csv("flixbus/trips_data.csv") 
trips_data <- df %>%
  filter(trip != "[]")

tbl <- tibble()
for (index in c(1:nrow(trips_data))) {
  city <- trips_data %>% 
    slice(index) %>%
    select(city) %>% 
    pull() 
  trips <- trips_data %>%
    slice(index) %>%
    select(trip) %>%
    pull() %>%
    str_extract_all(., "'(.*?)'") %>%
    unlist() %>%
    gsub("'", "", .) %>%
    tibble(trips = .) %>%
    mutate(city = city)
  tbl <- tbl %>% bind_rows(trips)
}
tbl

trips_data$trip


library(igraph)
library(visNetwork)
library(htmlwidgets)

# Create a simple graph
g <- graph_from_edgelist(tbl %>% as.matrix(), directed = FALSE)
# Remove one edge from each reciprocal pair
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

components <- components(g)

# Add subgraph labels as a vertex attribute
V(g)$subgraph <- components$membership

tmp = data.frame(name = V(g)$name, subgraph = V(g)$subgraph)
tmp2 = data.frame(name = E(g)$from)
# Calculate degree centrality
degree(g)

edge_list <- ends(g, E(g))

# Convert to a data frame for easier viewing
edge_df <- data.frame(From = edge_list[, 1], To = edge_list[, 2])

# Calculate betweenness centrality
betweenness(g)

# Detect communities
dd <- cluster_louvain(g)
dd <- tibble(cluster = dd$membership,
             name = dd$names) %>%
  mutate(cluster = as.character(cluster))


# Create an igraph graph
network <- toVisNetworkData(g)
nodes = network$nodes
edges = network$edges

visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 123)