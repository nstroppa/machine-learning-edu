pdf('gradient.pdf',height=6)
f <- function(x) x - 0.2*(2*x)
applyn <- function(n,f,x){if (n==0) {x} else {applyn(n-1,f,f(x))}}
plot(function(x)x^2,xlim=c(-1,1),xlab=expression(theta),ylab=expression(theta^2),cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
points(c(-1,applyn(1,f,-1),applyn(2,f,-1),applyn(3,f,-1),applyn(4,f,-1),applyn(5,f,-1)),c(0,0,0,0,0,0),pch=19)
dev.off()
