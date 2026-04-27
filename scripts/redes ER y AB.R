# modelo de red Erdős–Rényi
set.seed(123)

# Red Erdős–Rényi: 230 nodos, probabilidad de conexión 0.08
g_er <- sample_gnp(230, p = 0.08)
V(g_er)$name <- paste0("ER_", 1:vcount(g_er))

# Red ER
plot(g_er, 
     vertex.size = degree(g_er) * 2 + 3,
     vertex.label = NA,
     vertex.color = "skyblue",
     edge.color = adjustcolor("gray50", alpha = 0.4),
     main = "Red Aleatoria (Erdős–Rényi)")
##EJERCICIOS
#Ejercicio resuelto 1: Genera una red ER con 500 nodos y Calcula el grado medio teórico y compáralo con el empírico. Visualiza la distribución de grado superpuesta con la distribución de Poisson teórica.
ejer1 <- sample_gnp(500, p= 0.01)
V(ejer1)$name <- paste0("ER_", 1:vcount(ejer1))
plot(ejer1,
     vertex.size = 5,
     vertex.color= "hotpink4",
     edge.color="yellow4",
     edge.size=15)

#EDn este objeto están guardados los degrees de mi red de 500 nodos
deg<- degree(ejer1)
hist <- hist(deg)
#Grado empírico
empirico <- mean(deg)
#Grado teoríco
teorico <- 0.01 * (500-1)
empirico
teorico

# Histograma normalizado (densidad)

Hej1 <- hist(deg,
     breaks = seq(-0.5, max(deg) + 0.5, by = 1),
     col = adjustcolor("hotpink2", alpha = 0.6), border = "white",
     probability = TRUE,
     main = "Red ER (n=500, p=0.01): Empírico vs. Poisson",
     xlab = "Grado (k)", ylab = "Densidad de probabilidad",
     xlim = c(0, max(deg) + 2))

# Curva de Poisson teórica
k_vals <- 0:max(deg)
lines(k_vals, dpois(k_vals, lambda = teorico), 
      col = "yellow4", lwd = 2.5, type = "b", pch = 16, cex = 0.8)

legend("topright", 
       legend = c("Empírico", paste0("Poisson(λ=", round(teorico, 1), ")")),
       fill = c(adjustcolor("purple4", 0.6), NA),
       border = c("white", NA),
       col = c(NA, "navy"), lwd = c(NA, 2.5), pch = c(NA, 16),
       bty = "n", merge = TRUE)
Hej1
