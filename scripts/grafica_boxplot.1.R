x <- rnorm(200)

pdf("results/boxplot.pdf")
boxplot(x, col="hotpink4")
dev.off