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




    