usairpollution=matrix(c(46,11,24,47,11,31,110,23,65,26,9,17,17,35,
56,10,28,14,14,13,30,10,10,16,29,18,9,31,14,69,10,61,94,26,28,12,
29,56,29,8,36,47.6,56.8,61.5,55.0,47.1,55.2,50.6,54.0,49.7,51.5,
66.2,51.9,49.0,49.9,49.1,68.9,52.3,68.4,54.5,61.0,55.6,61.6,75.5,
45.7,43.5,59.4,68.3,59.3,51.5,54.6,70.3,50.4,50.0,57.8,51.0,56.7,
51.1,55.9,57.3,56.6,54.0,44,46,368,652,391,35,3344,462,1007,266,
641,454,104,1064,412,721,361,136,381,91,291,337,207,569,699,275,
204,96,181,1692,213,347,343,197,137,453,379,775,434,125,80,116,
244,497,905,463,71,3369,453,751,540,844,515,201,1513,158,1233
,746,529,507,132,593,624,335,717,744,448,361,308,347,1950,582,
520,179,299,176,716,531,622,757,277,80,8.8,8.9,9.1,9.6,12.4,6.5,
10.4,7.1,10.9,8.6,10.9,9.0,11.2,10.1,9.0,10.8,9.7,8.8,10.0,8.2,
8.3,9.2,9.0,11.8,10.6,7.9,8.4,10.6,10.9,9.6,6.0,9.4,10.6,7.6,8.7,
8.7,9.4,9.5,9.3,12.7,9.0,33.36,7.77,48.34,41.31,36.11,40.75,34.44,
39.04,34.99,37.01,35.94,12.95,30.85,30.96,43.37,48.19,38.74,54.47,
37.00,48.52,43.11,49.10,59.80,29.07,25.94,46.00,56.77,44.68,30.18,
39.93,7.05,36.22,42.75,42.59,15.17,20.66,38.79,35.89,38.89,30.58,
40.25,135,58,115,111,166,148,122,132,155,134,78,86,103,129,127,103,
121,116,99,100,123,105,128,123,137,119,113,116,98,115,36,147,125,115,
89,67,164,105,111,82,114),41)
cities = c("Albany", "Albuquerque","Atlanta","Baltimore","Buffalo",
"Charleston","Chicago", "Cincinnati","Cleveland","Columbus","Dallas",
"Denver","DesMoines","Detroit","Hartford","Houston","Indianapolis",
"Jacksonville","Kansas City","Little Rock","Louisville","Memphis",
"Miami", "Milwaukee","Minneapolis","Nashville","New Orleans","Norfolk",
"Omaha","Philadelphia", "Phoenix","Pittsburgh","Providence","Richmond",
"Salt Lake City","San Francisco","Seattle","St. Louis","Washington","Wichita","Wilmington")
variables = c("SO2","temp","manu","popul","wind", "precip","predays")
colnames(usairpollution) = variables
rownames(usairpollution)= cities
usairpollution
require(grid) 
require(lattice) 
require(scatterplot3d) 
require(HSAUR2) 
require(MVA)
x<-usairpollution[, c("SO2","temp","manu", "popul","wind","precip","predays")]
manu<-x[,c("manu")]
popul<-x[,c("popul")]
wind<-x[,c("wind")]
SO2<-x[,c("SO2")]
temp<-x[,c("temp")]

mlab <- "Manufacturing enterprises with 20 or more workers"
plab <- "Population size (1970 census) in thousands"
plot(popul ~ manu, data = usairpollution, xlab = mlab, ylab = plab)

plot(popul ~ manu, data = usairpollution, xlab = mlab, ylab = plab)
text(popul ~ manu, data=usairpollution, xlab = mlab, ylab = plab , labels=cities)

plot(popul ~ manu, data = usairpollution, xlab = mlab, ylab = plab)
text(popul ~ manu, data=usairpollution, xlab = mlab, ylab = plab , labels=cities)
rug(usairpollution[,c("manu")], side = 1)
rug(usairpollution[,c("popul")], side = 2)

outcity<- match(lab<- c("Chicago",  "Detroit", "Cleveland", "Philadelphia"),   rownames(usairpollution))
popul<-x[,c("popul")]
manu<-x[,c("manu")]
plot(popul[-outcity],manu[-outcity],xlim=c(-800,1800),ylim=c(-800,1800))
hist(manu)
boxplot(manu)

outcity<- match(lab<- c("Chicago",  "Detroit", "Cleveland", "Philadelphia"),   rownames(usairpollution))
x <- usairpollution[, c("manu", "popul")]
bvbox(x, mtitle = "", xlab = mlab, ylab = plab)
text(x[,c("manu")][outcity], x[,c("popul")][outcity], labels = lab,cex = 0.7)

outcity<-match(c("Chicago","Detroit","Cleveland","Philadelphia"),
rownames(usairpollution))
cor(manu, popul)
cor(manu[-outcity], popul[-outcity])

hull <- chull(manu, popul)

plot(manu, popul,pch=1,xlab=mlab,ylab=plab,xaxt="n",yaxt="n")

polygon(manu[hull], popul[hull], density = 15, angle = 30)

plot(manu, popul, xlab = mlab, ylab = plab, cex.lab = 0.9)
chiplot(manu, popul)

pairs(usairpollution, pch = ".", cex = 1.5)
pairs(usairpollution, panel = function (x, y, ...) { points(x, y, ...); abline(lm(y ~
x), col = "red") }, pch = ".", cex = 1.5)

library(squash)
x <- rnorm(20,mean=20,sd=5) 
y <- rnorm(20)
hist2(x, y)


rec <- function(x) (abs(x) < 1) * 0.5
tri <- function(x) (abs(x) < 1) * (1 - abs(x))
gauss <- function(x) 1/sqrt(2*pi) * exp(-(x^2)/2)
x <- seq(from = -3, to = 3, by = 0.001)
plot(x, rec(x), type = "l", ylim = c(0,1), lty = 1, ylab = expression(K(x)))
lines(x, tri(x), lty = 2)
lines(x, gauss(x), lty = 3)
legend("topleft", legend = c("Rectangular", "Triangular", "Gaussian"), lty = 1:3, title = "kernel functions", bty = "n")

x <- c(0, 1, 1.1,1.2,1.3,1.4,1.5, 1.9, 2.8, 2.9, 3.5)
n <- length(x)
xgrid <- seq(from = min(x) - 1, to = max(x) + 1, by = 0.01)
h <- .2
bumps <- sapply(x, function(a) gauss((xgrid - a)/h)/(n * h))
plot(xgrid, rowSums(bumps), ylab = expression(hat(f)(x)), type = "l", xlab = "x", lwd = 2)
rug(x, lwd = 2)
out <- apply(bumps, 2, function(b) lines(xgrid, b))



epa <- function(x, y) ((x^2 + y^2) < 1) * 2/pi * (1 - x^2 - y^2)
x <- seq(from = -1.1, to = 1.1, by = 0.05)
 epavals <- sapply(x, function(a) epa(a, x))
persp(x = x, y = x, z = epavals, xlab = "x", ylab = "y", zlab = expression(K(x, y)), theta = -35, axes = TRUE, box = TRUE)



logst<-c(4.37, 4.56, 4.26, 4.56, 4.30, 4.46, 3.84, 4.57, 4.26, 4.37, 3.49, 4.43, 4.48, 4.01, 4.29, 4.42, 4.23, 4.42, 4.23, 3.49, 4.29, 4.29, 4.42, 4.49, 4.38, 4.42, 4.29, 4.38, 4.22, 3.48, 4.38, 4.56, 4.45, 3.49, 4.23, 4.62, 4.53, 4.45, 4.53, 4.43, 4.38, 4.45, 4.50, 4.45, 4.55, 4.45, 4.42)
logli<-c(5.23, 5.74, 4.93, 5.74, 5.19, 5.46, 4.65, 5.27, 5.57, 5.12, 5.73, 5.45, 5.42, 4.05, 4.26, 4.58, 3.94, 4.18, 4.18, 5.89, 4.38, 4.22, 4.42, 4.85, 5.02, 4.66, 4.66, 4.90, 4.39, 6.05, 4.42, 5.10, 5.22, 6.29, 4.34, 5.62, 5.10, 5.22, 5.18, 5.57, 4.62, 5.06, 5.34, 5.34, 5.54, 4.98, 4.50)
CYGOB1<-data.frame(logst,logli)
library("KernSmooth")
CYGOB1d <- bkde2D(CYGOB1, bandwidth = sapply(CYGOB1, dpik))
plot(CYGOB1, xlab = "log surface temperature", ylab = "log light intensity")
contour(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat, add = TRUE)


persp(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat,
 xlab = "log surface temperature",
 ylab = "log light intensity",
 zlab = "density")

CYGOB2<-data.frame(manu, popul)
CYGOB1d2<-bkde2D(CYGOB2,bandwidth=sapply(CYGOB2,dpik))
plot(CYGOB2)
contour(x=CYGOB1d2$x1,y=CYGOB1d2$x2,z=CYGOB1d2$fhat,add=TRUE)

chest = c(34,37,38,36,38,43,40,38,40,41,36,36,34,33,36,37,34,36,38,35)
waist = c(30,32,30,33,29,32,33,30,30,32,24,25,24,22,26,26,25,26,28,23)
hips = c(32,37,36,39,33,38,42,40,37,39,35,37,37,34,38,37,38,37,40,35)

CYGOB3<-data.frame(chest,hips)
CYGOB1d3<-bkde2D(CYGOB3,bandwidth=sapply(CYGOB3,dpik))
plot(CYGOB3)
contour(x=CYGOB1d3$x1,y=CYGOB1d3$x2,z=CYGOB1d3$fhat,add=TRUE)
hist(chest)


library("scatterplot3d")
scatterplot3d(manu, wind, popul, type = "h",angle = 35)

require(MVA)

plot(xyplot(SO2 ~ temp| cut(wind,2)))

pollution <- equal.count(SO2,4)
plot(cloud(popul ~ temp * wind | cut(SO2, 6), panel.aspect = 2))

plot(xyplot(lat ~ long| cut(depth, 3), data = quakes,
 layout = c(3, 1), xlab = "Longitude",
 ylab = "Latitude"))

stalac(USairpollution)
