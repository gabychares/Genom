#ejercicio

iS <- function(abundancias, total){
 (abundancias / total)^2 ->suma
 sum(suma)->simpson
 return(simpson)
}
abundancias <- c(4,7,10,1)
total <- 22

iS (abundancias, total)

#Con un solo argumento 
#R
smp <- function (individuos){
  sum(individuos) -> N
sum((individuos/N) ^ 2)
}
##esto es usando la función de iS
igs <- function(simpson){
  1-1/simpson -> gini
  return(gini)
}

##función de Roberto, más fácil
inS <- function (abundancias){
  
  1/smp(abundancias)
  }
inS(c(990,10))
