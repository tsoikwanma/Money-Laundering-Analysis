library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggrepel)

setwd("")

files <- read.csv("SAML-D.csv")
files <- files[, -c(1:2, 5:7, 10, 12)]
write.csv(files, "filtered.csv")
df <- subset(files, Is_laundering == 1)
write.csv(df, "filtered_df.csv")

df <- read.csv("filtered_df.csv", header = TRUE, stringsAsFactors = FALSE)

account_nodes <- data.frame(Account_ID = unique(c(df$Sender_account, df$Receiver_account)))

account_edges <- df %>%
  select(Sender_account, Receiver_account) %>%
  rename(Source = Sender_account, Target = Receiver_account)

country_nodes <- data.frame(Country = unique(c(df$Sender_bank_location, df$Receiver_bank_location)))

country_edges <- df %>%
  select(Sender_bank_location, Receiver_bank_location) %>%
  rename(Source = Sender_bank_location, Target = Receiver_bank_location) %>%
  group_by(Source, Target) %>%
  summarise(Weight = n(), .groups = 'drop')

write.csv(account_nodes, "account_nodes.csv", row.names = FALSE)
write.csv(account_edges, "account_edges.csv", row.names = FALSE)
write.csv(country_nodes, "country_nodes.csv", row.names = FALSE)
write.csv(country_edges, "country_edges.csv", row.names = FALSE)

account_edges <- read.csv("account_edges.csv") 
account_nodes <- read.csv("account_nodes.csv")

g <- graph_from_data_frame(account_edges, vertices = account_nodes, directed = TRUE)
plot(g)

set.seed(123)
country_edges <- read.csv("country_edges.csv") 
country_nodes <- read.csv("country_nodes.csv") 

country_edges <- country_edges[country_edges$Source != country_edges$Target, ]
country_nodes$Longitude <- c(-0.1278, 137.7267, 20.0324, 12.4964, 8.2282, 10.4515, 35.2137, -102.5528, 8.6753, -95.7129, 30.3753, -3.7038, -7.0926, 14.5501, 2.3522, 55.2708, 77.1025, 4.8970)
country_nodes$Latitude <- c(51.5074, 35.6528, 41.1533, 41.9028, 46.8182, 51.1657, 39.9334, 23.6345, 9.0820, 37.0902, 33.6844, 40.4168, 31.6340, 47.0702, 48.8566, 25.2048, 28.6139, 52.3676)

head(country_edges)
head(country_nodes)

# edge coreness and betweenness centrality for each country
graph <- graph_from_data_frame(country_edges, vertices = country_nodes, directed = TRUE)
graph <- simplify(graph)
edge_betweenness <- edge.betweenness(graph, directed=TRUE)
vertex_core <- graph.coreness(graph)
brokers <- betweenness(graph, directed = TRUE, normalized = TRUE)
sort(brokers, decreasing = TRUE)[1:5]
V(graph)$color <- "gray"
V(graph)$color[order(brokers, decreasing = TRUE)[1:5]] <- "red" 
plot(graph, edge.width = edge_betweenness * 0.5, vertex.size = vertex_core * 0.5, edge.arrow.size = 0)

# country network with community clusters
country_communities <- fastgreedy.community(as.undirected(graph))
number_communities <- length(country_communities)
plot(country_communities, graph, edge.arrow.size=0)
layout <- layout_with_fr(graph)
layout <- layout.fruchterman.reingold(graph)
plot(graph, layout = layout, vertex.size = 20,
     vertex.color = heat.colors(number_communities)[membership(country_communities)],
     edge.width = 2, edge.arrow.size=0)

V(graph)$longitude <- country_nodes$Longitude[match(V(graph)$name, country_nodes$Country)]
V(graph)$latitude <- country_nodes$Latitude[match(V(graph)$name, country_nodes$Country)]

coordinates <- matrix(c(V(graph)$longitude, V(graph)$latitude), nrow = length(V(graph)$latitude), ncol = 2)

# better visualization on world map to see the relationship between countries
world_map <- map_data("world")
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "white") +
  geom_curve(data = country_edges, aes(
    x = V(graph)$longitude[match(Source, V(graph)$name)],
    y = V(graph)$latitude[match(Source, V(graph)$name)],
    xend = V(graph)$longitude[match(Target, V(graph)$name)],
    yend = V(graph)$latitude[match(Target, V(graph)$name)]), 
  color = "red", alpha = 0.5, curvature = 0.2, size = 0.5) +
  geom_point(data = country_nodes, aes(x = Longitude, y = Latitude), color = "blue", size = 3) +
  geom_text_repel(data = country_nodes, aes(x = Longitude, y = Latitude, label = Country), size = 4) +
  coord_fixed(ratio = 1.1, xlim = c(-100, 130), ylim = c(0, 60)) + theme_void()
