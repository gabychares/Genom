plot(karate)
kdeg <- degree(karate)
sort(degree(karate), decreasing = T)
hist(kdeg,
     col = adjustcolor("hotpink2", alpha = 0.6), border = "white") #--> se asimila más a una distribución de ley de potencia 
plot(yeast)
sort(degree(yeast), decreasing = T)[1:2500] -> hubsy
ydeg <- degree(yeast)
hist(ydeg)
yeassh <- delete_vertices(yeast, hubsy)
plot(yeassh)
yeassh
