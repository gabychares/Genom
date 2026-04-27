data <-read.csv("rawdata/matriz_adyacencia_amigxs_2026.csv")
data
row.names(data) <-data[,1]
data <- data[,-1]
data <- as.matrix(data)
class(data)
g <- graph_from_adjacency_matrix(data, mode = "directed")
plot(g,
     vertex.color="hotpink",
     main= "red de amigos", 
     edge.arrow.size = 1)
degree(g, mode = "out") #¿cuántos consideré amigos?
degree(g,mode = "in")-degree(g, mode = "out")
which(data["GABY",]==1) -> amigos
mean (degree (g,amigos, mode="out"))
popularidad <- function(nombre){
  which(data[nombre,]==1) -> amigos
  length(amigos) ->n
  mean (degree (g,amigos, mode="out")) -> promedio
  print(paste("soy", nombre, "y tengo", n, "amigos", "el promedio de mis amigos es", promedio))
}
    popularidad("GABY") 
    

#Medir la distancia entre la red de amigos
    # ¿Cuál es la distancia más larga en el salón? --> Diametro
    diameter(g)
    #¿Cuál es el promedio de la distancia?
mean_distance(g)    

transitivity(g,"local")

#Red interactiva:

library(networkD3)  

wc <- cluster_walktrap(g)
members <- membership(wc)

# Convert to object suitable for networkD3
g_d3 <- igraph_to_networkD3(g, group = members)

# Create force directed network plot
forceNetwork(Links = g_d3$links, Nodes = g_d3$nodes,
             Source = 'source', Target = 'target',
             NodeID = 'name', Group = 'group')  

library(visNetwork)
visIgraph(g) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE)

#Medir robustez:
mean_distance(g, directed = T)
diameter(g,directed = T)
delete_vertices(g, "GABY") -> g_sin_gaby
plot(g_sin_gaby)
mean_distance(g_sin_gaby)
sort(degree(g,mode = "out"))
sort(degree(g,mode = "in"))

#Quitando a los hubs 
hubs<- delete_vertices(g,c("CESAR","MARIANA","MAYRA"))
mean_distance(hubs)

#Quitar los que tengan menos degree
mdg <- delete_vertices(g,c("DIEGO","FERNANDA","SAMUEL","ALONSO","JOSUE"))
mean_distance(mdg)


