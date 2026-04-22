install.packages ("igraph")
library (igraph)
# primero se tiene que cear un grafo vacio:

g <- make_empty_graph(n = 8, directed = FALSE)

# Asignar atributos visuales a los nodos
V(g)$color <- "hotpink2"
plot(g)
# Visualizar
plot(g, 
     main = "Red vacia con 8 nodos",
     vertex.size = 20)
     
#Agregar conexiones
     g <-add_edges(g, c(1,2, 1,3, 2,4, 3,4, 4,5, 7,8))
     
     plot(g, 
          main = "Red con conexiones agregadas",
          vertex.size = 30,
          edge.arrow.size = 0.6)
     
# Agregar un nodo nuevo con color rojo
     g <- add_vertices(g, 1, color = "red")
     
# Conectar el nuevo nodo (nodo 6)
     g <- add_edges(g, c(3,6, 6,5))
     
     plot(g, 
          main = "Nodo rojo y nuevas conexiones",
          vertex.size = 30,
          edge.arrow.size = 0.6)
     
   # Asignar nombres a los vertices
     V(g)$name <- c ("k1", "k2","k3", "k4", "k5", "k6", "k7", "k8", "k9")
     
     plot(g, 
          main = "Nodos con nombres A-F",
          vertex.size = 30,
          edge.arrow.size = 0.6)
     # Eliminar la segunda conexión (A→C) y agregar C→A
     g <- delete_edges(g, 2)
     g <- add_edges(g, c(4, 1))  # C → A
     
     plot(g, 
          main = "Cambio en dirección de conexión",
          vertex.size = 30,
          edge.arrow.size = 0.6)
     
     # grado (degree) total de cada nodo
     degree(g)
     #Visualizar el grado (degree)
     plot(g, 
          layout = layout_nicely(g),
          vertex.size = degree(g, mode = "in") * 10 + 10,
          vertex.label.dist = 0.5,
          edge.arrow.size = 0.5,
          main = "Tamaño proporcional al grado (degree) de entrada")     
     
     # Obtener la matriz de adyacencia, esta cuando no es dirigida esta es simétrica
     adj_mat <- as.matrix(get.adjacency(g))
     
     # Mostrar como tabla
     knitr::kable(adj_mat, caption = "Matriz de adyacencia de la red")
     
     #HEATMAP a partir de una matriz de adyacencia
     heatmap(adj_mat, 
             Rowv = NA, Colv = NA, 
             col = c("white", "steelblue"),
             main = "Mapa de calor — Matriz de adyacencia",
             scale = "none")

     
     #Red de estrella: 
     plot(make_star(10), 
          vertex.color = c("tomato", rep("gold", 9)),
          vertex.size = 20,
          main = "Red estrella")
     
     
     
     
     ## Ejercicio 1
#Ejercicio resuelto 1: Crea una red no dirigida de 7 nodos con colores aleatorios de una paleta, conéctalos formando una cadena (path), y visualiza el resultado.*
       
     #Crear un objeto grafo vacío 
     ej1 <- make_empty_graph(n=7, directed = FALSE)
     
     # Asignar atributos visuales a los nodos
     V(ej1)$color <- "hotpink2"
     plot(ej1)
     
     
     # Visualizar
     plot(ej1, 
          main = "Red vacia con 8 nodos",
          vertex.size = 20)
     
     #Agregar conexiones
     ej1<-add_edges(ej1, c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7))
     
     plot(ej1, 
          main = "Red con conexiones agregadas",
          vertex.size = 30,
          edge.arrow.size = 0.6)
     
     
     
     # Ejercicio 2:  Construye manualmente una red donde los nodos representen 5 ciudades mexicanas y las conexiones indiquen si comparten frontera estatal. Muestra la matriz de adyacencia.
     
     ej2 <- make_empty_graph(n=5, directed = F)
     plot(ej2)     
     # Asignar nombres a los vertices
     V(ej2)$name <- c ("Queretaro", "Hidalgo","Guanajuato", "SLP", "CDMX")
     plot(ej2)
     
     #Agregar conexiones
     ej2 <-add_edges(ej2, c("Queretaro","Hidalgo", "Hidalgo","CDMX", "Hidalgo","Queretaro", "CDMX","Queretaro", "SLP", "Queretaro", "SLP", "Hidalgo", "Guanajuato", "Queretaro"))
     
     plot(ej2, 
          main = "Red con conexiones agregadas",
          vertex.size = 30,
          edge.arrow.size = 0.6)
     
     
     #Ejercicio 3: Ver con quien estas sentados:
     ej3 <- make_empty_graph(n=4, directed = F)
     V(ej3)$name <- c("Mayra", "Celina", "Mariana", "Gaby")

     plot(ej3)   
     ej3 <- add.edges(ej3, c("Mayra", "Celina", "Mariana", "Gaby"))
     plot(ej3, 
          main = "Red dependiendo de con quien te sientas",
          vertex.size = 30,
          edge.arrow.size = 0.6)
     
     EJ4 <- make_empty_graph(n=13, directed = F)
     V(EJ4)$name <- c("Alonso", "Mayra", "Celina", "Mariana", "Gaby", "Samuel", "Cesar", "Andy", "Pao", "Josue", "Leidy", "Diego", "Fer")
EJ4 <- add.edges(EJ4, c("Alonso","Celina", "Alonso","Mayra", "Mayra", "Josue", "Mayra","Mariana", "Mariana","Gaby", "Gaby", "Mayra", "Gaby", "Celina", "Mariana","Andy", "Andy", "Pao", "Pao","Cesar", "Cesar","Samuel", "Pao","Diego", "Diego","Fer", "Diego","Leidy", "Leidy","Josue"))   
plot(EJ4)



#EJERCICIOS CON PISTA:
#Ejercicio 1: Crea una red vacía de 7 nodos no dirigidos. Agrega conexiones para formar un triángulo entre los nodos 1, 2 y 3, y otro triángulo entre 5, 6 y 7. Conecta ambos triángulos con una conexión entre 3 y 5. Visualiza el resultado y calcula el grado (degree) de cada nodo.
EJ1 <- make_empty_graph(n=7, directed = F)
plot(EJ1)
EJ1 <- add.edges (EJ1, c(1,2, 1,3, 2,3, 3,4, 4,5, 5,6, 5,7, 6,7))
plot(EJ1)
degree(EJ1)

#
##########
#Ejercicio 2: Genera un make_tree() con 3 hijos por nodo y profundidad suficiente para tener al menos 40 nodos. ¿Cuántos nodos tiene exactamente? ¿Cuántas hojas (nodos con grado (degree) 1)?
##########
ath <-plot(make_tree(40, children = 3), 
     vertex.color = "hotpink4",
     vertex.size = 15,
     layout = layout_as_tree,
     main = "Árbol con tres hijitos")
# Tiene 40 nodos
degree(ath)


#Ejercicio 3: ¿Qué pasa si agregas un self-loop (conexión de un nodo a sí mismo) y una conexión duplicada? Crea una red, agrégalos, visualiza, y luego “limpia” la red con simplify(). Compara el antes y después.

EJ3 <- make_empty_graph (n=5, directed = F)
EJ3 <- add.edges(EJ3, c(1,1, 1,1, 1,1, 1,2, 2,1, 3,3, 3,4, 4,5 ))
plot(EJ3)

EJ3 <- simplify(EJ3, remove.multiple = T, remove.loops = T)
plot(EJ3)

#EJERCICIOS PROPUESTOS
#Crea una red estrella de 15 nodos. Calcula su grado (degree) máximo y mínimo. ¿Cuál es la densidad de conexiones (edge_density())?
Ej1 <- make_star (n=15)
plot(Ej1,
     vertex.color ="hotpink")                  
degree(Ej1)
edge_density(Ej1)

#Crea una función en R llamada resumen_red() que reciba un grafo g y devuelva un data frame con las columnas: nodo, grado_total, grado_entrada, grado_salida. Pruébala con una red dirigida de al menos 10 nodos.
resumen_red() <- function(g) 
  

  
Columnas <- c("Actores, Distancia")
Actores <- c("Alfredo Dosal", "Bryan Craston", "Gael García Bernal", "Diego Luna", "Matt Damon")
KBnumber <- c(2,2,3,3,2)
tabla <-cbind(Columnas,Actores,KBnumber)
 
