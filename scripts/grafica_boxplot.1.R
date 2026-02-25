x <- rnorm(200)

pdf("results/boxplot.pdf")
boxplot(x, col="hotpink4")
dev.off
#este script nada más es un ejemplo y una manera rápida de un boxplot