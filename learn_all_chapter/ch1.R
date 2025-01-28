gender<-c("Male","Female")
sex <- rep(gender, each=5)
age <- c(21, 43, 22, 86, 60, 16, NA, 43, 22, 80)
IQ <- c(120, NA, 135, 150, 92, 130, 150, NA, 84, 70)
depression <- c("Yes", "No", "No", "No", "Yes", "Yes", "Yes", "Yes", "No", "No")
health <- c("Very good", "Very good", "Average", "Very poor", "Good", "Good",
"Very good", "Average","Average","Good")
weight <- c(150, 160, 135, 140, 110, 110, 120, 120, 105, 100)
hypo <- data.frame(sex,age,IQ,depression,health,weight)

hypo [1:2, c("health", "weight")]

chest = c(34,37,38,36,38,43,40,38,40,41)
waist = c(30,32,30,33,29,32,33,30,30,32)
hips = c(32,37,36,39,33,38,42,40,37,39)
gender = rep("male",10)
f1= data.frame(chest,waist,hips,gender)
chest = c(36,36,34,33,36,37,34,36,38,35)
waist = c(24,25,24,22,26,26,25,26,28,23)
hips = c(35,37,37,34,38,37,38,37,40,35)
gender = rep("female",10)
f2= data.frame(chest,waist,hips,gender)
measure=cbind(f1,f2)


chest = c(34,37,38,36,38,43,40,38,40,41,36,36,34,33,36,37,34,36,38,35)
waist = c(30,32,30,33,29,32,33,30,30,32,24,25,24,22,26,26,25,26,28,23)
hips = c(32,37,36,39,33,38,42,40,37,39,35,37,37,34,38,37,38,37,40,35)
gender = c("Male","Female")
sex = rep(gender, each=10)
measure=data.frame(chest,waist,hips,sex)

V=cov(measure[, c("chest", "waist", "hips")])
D=diag(c(diag(V^-.5)));diag(c(1,2,3),3)
cov(measure[11:20, c("chest", "waist", "hips")])
cor(measure[, c("chest", "waist", "hips")])

library(MASS)
mu=c(1,2)
Sigma=matrix(c(10,3,3,2),2,2)
mvrnorm(n=5,mu,Sigma)
var(mvrnorm(n=5,mu,Sigma))

require(graphics)
require(mnormt)
x=seq(-2,2,len=100)
y=seq(-2,2,len=100)
mu=c(0,0)
Sigma=matrix(c(1,.5,.5,1),2,2)
g=function(x,y){ dmnorm(cbind(x,y),mu,Sigma)}
z=outer(x,y,g)
persp(x,y,z)
contour(x,y,z, drawlabels=FALSE, xlab="x", ylab="y")

#Q-Q plots
x=sort(rnorm(100,0,1))
qqnorm(x)
qqline(x)

#P-P plots
y=pnorm(x)
z=((1:100)-0.5)/100
plot(z~y,xlab='Theoretical Percentiles',
ylab='empirical Percentiles')
abline(0,1)

x <- measure [, c("chest", "waist", "hips")]
cm <- colMeans(x)
S <- cov(x)
d<-apply(x,MARGIN=1,function(x) t(x-cm)%*%solve(S)%*%(x-cm))
plot(qchisq((1:nrow(x) - 1/2) / nrow(x), df = 3), sort(d),
xlab = expression(paste(chi[3]^2, " Quantile")),
ylab = "Ordered distances")
abline(a = 0, b = 1)



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
x <- usairpollution

layout(matrix(1:8, nc = 2))
qqnorm(x[,"SO2"], main = "SO2"); qqline(x[,"SO2"])
qqnorm(x[,"temp"], main = "temp"); qqline(x[,"temp"])
qqnorm(x[,"manu"], main = "manu"); qqline(x[,"manu"])
qqnorm(x[,"popul"],main ="popul");qqline(x[,"popul"])
qqnorm(x[,"wind"],main="wind"); qqline(x[,"wind"])
qqnorm(x[,"precip"],main="precip");qqline(x[,"precip"])
qqnorm(x[,"predays"],main = "predays ");qqline(x[,"predays"])


cm <- colMeans(x)
S <- cov(x)
d <- apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 6), sd <- sort(d),
xlab = expression(paste(chi[6]^2, " Quantile")),
ylab = "Ordered distances", xlim = range(qc) * c(1, 1.1))
oups <- which(rank(abs(qc - sd),ties="random")>nrow(x) - 3)
text(qc[oups], sd[oups] - 1.5, names(oups))
abline(a = 0, b = 1)

#####SIMULATIONS###################################

dchisq(x=.90, df=2, ncp = 0)
pchisq(q=.2, df=2, ncp = 0, lower.tail = TRUE)
qchisq(p=.99, df=3, ncp = 0, lower.tail = TRUE, log.p = FALSE)
rchisq(n=100, df=2, ncp = 0)
curve(dchisq(x,df=2,ncp = 10), 0, 30, n = 50)
curve(dchisq(x, 5,10), 0, 30,add=TRUE)

dt(x=0, df=1);dnorm(x=0)
pt(q, df, ncp=0, lower.tail = TRUE)
qt(p, df, ncp=0,lower.tail = TRUE)
rt(n, df)
curve(dnorm(x,mean=0,sd=1), -3, 3,n=50000,col="red")
curve(dnorm(x,mean=.10,sd=2),-3,3,n=50000,col="green",add=TRUE)
curve(dt(x,df=1,ncp=0), -3, 3, n = 50,add=TRUE)


df(x, df1=2, df2=2)
pf(q, df1, df2, ncp=0, lower.tail = TRUE)
qf(p, df1, df2,lower.tail = TRUE)
rf(n=100, df1=1, df2=2,ncp=0, ncp=100)
curve(df(x,df1=4,df2=5,ncp=5),0,30,n=5000,col="red")
curve(df(x,df1=5,df2=2,ncp=5),0,30,n=5000,col="blue",add=TRUE)

?dnorm




#