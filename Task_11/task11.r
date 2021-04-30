setwd('~/Desktop/Evolution/Tasks/Task_11')
x <- rnorm(100, mean=5, sd=2)
var(x)
mean(x)
y <- ((x*5)+2)+runif(100,0,0.1)
pdf('graph1.pdf', height=4, width=4)
plot(x,y)
abline(lm(y~x), col ='green')
dev.off()
coef(lm(y~x))
The y intercept is 2.036516 and the x (slope) is 5.002802. Adding many small numbers makes it difficult for the numbers to all fall on a perfect model. This is why the intercept and slope is not 2 and 5 respectively. 

z <- c()
x <- rnorm(100, mean=5, sd=2)
for (i in 1:100) {
	z[i] <- runif(1)
	y <- (x*z[1]) +2 +(rnorm(100, 0:0.1))
	l <- coef(lm(z[1:100]~y))
}
pdf('graph2.pdf', height=4, width=4)
plot(z[1:100], y)
abline(lm(y~z[1:100]))
dev.off()

pdf('graph3.pdf', height =4, width=4)
plot(c(z, -0.020))


