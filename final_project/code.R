library(readxl)
rw1 <- read_excel("C:/Users/skr/Desktop/RW.xlsx")
rw1
library(readr)
matrix2<-cor(rw1[,-1])
matrix2
library(HSAUR2)
panel.hist  <-  function(x,  ...)  {
  usr  <-  par("usr");  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="grey", ...)
}
pairs(rw1[,-1],  diag.panel  =  panel.hist)
rw_pca  <-  princomp(rw1[,-1],  cor  =  TRUE)
summary(rw_pca,  loadings  =  TRUE)
par(mfrow=c(3,1)) 
out  <-  sapply(1,  function(i)  {
  plot(rw1$quality,rw_pca$scores[,i],
       xlab  =  paste("PC",  i,  sep  =  ""),
       ylab  =  "quality")
})
out  <-  sapply(2,  function(i)  {
  plot(rw1$quality,rw_pca$scores[,i],
       xlab  =  paste("PC",  i,  sep  =  ""),
       ylab  =  "quality")
})
out  <-  sapply(3,  function(i)  {
  plot(rw1$quality,rw_pca$scores[,i],
       xlab  =  paste("PC",  i,  sep  =  ""),
       ylab  =  "quality")
})
out  <-  sapply(4,  function(i)  {
  plot(rw1$quality,rw_pca$scores[,i],
       xlab  =  paste("PC",  i,  sep  =  ""),
       ylab  =  "quality")
})
out  <-  sapply(5,  function(i)  {
  plot(rw1$quality,rw_pca$scores[,i],
       xlab  =  paste("PC",  i,  sep  =  ""),
       ylab  =  "quality")
})
out  <-  sapply(6,  function(i)  {
  plot(rw1$quality,rw_pca$scores[,i],
       xlab  =  paste("PC",  i,  sep  =  ""),
       ylab  =  "quality")
})

rw_reg  <-  lm(rw1$quality~rw_pca$scores[,c(1,2,3,4,5,6)])
summary(rw_reg)
rw2<-rw1
x <- rw2
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 6), sd <- sort(d),
     xlab = expression(paste(chi[6]^2, " Quantile")),
     ylab = "Ordered distances", xlim = range(qc) * c(1, 1.1))
oups <- which(rank(abs(qc - sd), ties = "random") > nrow(x) - 3)
text(qc[oups], sd[oups] - 1.5, names(oups))
abline(a = 0.75, b = 1.5)
rw<-rw2[-oups,]
rw_pca2<- princomp(rw2, cor = TRUE)
summary(rw_pca2, loadings = TRUE)
rw_reg2<-  lm(rw1$quality~rw_pca2$scores)
summary(rw_reg2)
######################
sapply(1:6, function(f)
  factanal(rw[,-1], factors= f, method="mle" )$PVAL)
fa.rw1=factanal(rw[,-1], factors= 6, method="mle",rotation="none") 
fa.rw1
fa.rw2.varimax.varimax=factanal(rw[,-1], factors= 6, method="mle", rotation = "varimax")    
fa.rw2.varimax.varimax
#######################promax for tahlil ekteshafi
fa.rw3.promax <- factanal(rw[,-1], factors = 6, rotation = "promax") # with promax rotation
fa.rw3.promax
# Factanal plots#تحلیل اکتشافی
par(mfrow = c(1,3))
plot(fa.rw1$loadings[,1], 
     fa.rw1$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")
abline(h = 0, v = 0)
text(fa.rw1$loadings[,1]-0.08, 
     fa.rw1$loadings[,2]+0.08,
     colnames(rw),
     col="blue")
plot(fa.rw2.varimax.varimax$loadings[,1], 
     fa.rw2.varimax.varimax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")
abline(h = 0, v = 0)
text(fa.rw2.varimax.varimax$loadings[,1]-0.08, 
     fa.rw2.varimax.varimax$loadings[,2]+0.08,
     colnames(rw),
     col="blue")

plot(fa.rw3.promax$loadings[,1], 
     fa.rw3.promax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Promax rotation")
abline(h = 0, v = 0)
text(fa.rw3.promax$loadings[,1]-0.08, 
     fa.rw3.promax$loadings[,2]+0.08,
     colnames(rw),
     col="blue")


# Goodness of fit without non rotation
apply(fa.rw1$loadings^2,1,sum)     # communality
1 - apply(fa.rw1$loadings^2,1,sum) # uniqueness
Lambda <- fa.rw1$loadings
Psi <- diag(fa.rw1$uniquenesses)
S <- fa.rw1$correlation;S;cor(rw)
Shat <- Lambda %*% t(Lambda) + Psi
round(S - Shat, 6)

# Goodness of fit with varimax rotation
apply(fa.rw2.varimax.varimax$loadings^2,1,sum)      # communality
1 - apply(fa.rw2.varimax.varimax$loadings^2,1,sum)  # uniqueness
Lambda <- fa.rw2.varimax.varimax$loadings
Psi <- diag(fa.rw2.varimax.varimax$uniquenesses)
S <- fa.rw2.varimax.varimax$correlation;S;cor(rw)
Shat <- Lambda %*% t(Lambda) + Psi
round(S - Shat, 6)

# Goodness of fit with promax rotation

apply(fa.rw3.promax$loadings^2,1,sum)       # communality
1 - apply(fa.rw3.promax$loadings^2,1,sum)   # uniqueness
Lambda <- fa.rw3.promax$loadings
Psi <- diag(fa.rw3.promax$uniquenesses)
S <- fa.rw3.promax$correlation;S;cor(rw)
Shat <- Lambda %*% t(Lambda) + Psi
round(S - Shat, 6)

##############################################################
#install.packages("psych")
library(psych)
PCMfit <- principal(rw, nfactors=6, covar=FALSE, rotate="none");PCMfit
PCMfit1 <- principal(rw, nfactors=6, covar=FALSE, rotate="varimax");PCMfit1

#### Factor analysis with principal component method ###################tahlil ekteshafi
mat2.eigen <- eigen(matrix2)
mat2.eigen
D1=diag(c(mat2.eigen$values[1:4]))  
G1<- as.matrix(mat2.eigen$vectors[,1:4]) 
Lambda.hat=G1 %*% sqrt(D1) 
Lambda.hat
h2.hat <- rowSums(Lambda.hat^2)
h2.hat
psi.hat=1-h2.hat
psi.hat
#########################################prcomp
rw_prca <-prcomp(rw[,-1],scale=TRUE)
rw_prca
summary(rw_prca)
loadings <- rw_prca$rotation
#######################################################
library(MVA)
pairs(rw_pca$scores[,1:6], ylim = c(-6, 4), xlim = c(-6, 4),
      panel = function(x,y,...) {
        text(x, y, abbreviate(row.names(rw1)),
             cex = 0.6)
        bvbox(cbind(x,y),add=TRUE)
      })
##############################     Scores  1 in return 2 
count<-row.names(rw1)
(scores <- factanal(rw1, factors= 6, mrthod= "mle",
                    scores= "regression")$scores)
x<-scores[,1]
y<-scores[,2]
plot(x, y, xlab = "factor1", ylab = "factor2", xlim=c(-3,3), ylim=c(-3,3),
     type = "n")
text(x, y, labels = count, cex = 0.7)

##############################     scores  1 in return 3
count<-row.names(rw1)
(scores <- factanal(rw1, factors= 4, mrthod= "mle",
                    scores= "regression")$scores)

x<-scores[,1]
y<-scores[,3]
plot(x, y, xlab = "factor1", ylab = "factor3", xlim=c(-3,3), ylim=c(-3,3),
     type = "n")
text(x, y, labels = count, cex = 0.7)
##############################     Scores  2 in return 3
count<-row.names(rw1)
(scores <- factanal(rw1, factors= 4, mrthod= "mle",
                    scores= "regression")$scores)
x<-scores[,2]
y<-scores[,3]
plot(x, y, xlab = "factor2", ylab = "factor3", xlim=c(-3,3), ylim=c(-3,3),
     type = "n")
text(x, y, labels = count, cex = 0.7)
#########################
#install.packages("lattice")
library(lattice)
ord <- order.dendrogram(as.dendrogram(hclust(dist(matrix2))))  
panel.corrgram <-    
  function(x, y, z, subscripts, at,  
           level = 0.9, label = FALSE, ...) 
  {
    require("ellipse", quietly = TRUE)
    x <- as.numeric(x)[subscripts]   
    y <- as.numeric(y)[subscripts]     
    z <- as.numeric(z)[subscripts]   
    zcol <- level.colors(z, at = at, col.regions = grey.colors, ...)   
    for (i in seq(along = z)) {
      ell <- ellipse(z[i], level = level, npoints = 50,   
                     scale = c(.2, .2), centre = c(x[i], y[i]))
      panel.polygon(ell, col = zcol[i], border = zcol[i], ...)
    }
    if (label)  
      panel.text(x = x, y = y, lab = 100 * round(z, 2), cex = 0.8,
                 col = ifelse(z < 0, "white", "black"))   
  }    
print(levelplot(matrix2[ord, ord], at = do.breaks(c(-1.01, 1.01), 20),
                xlab = NULL, ylab = NULL, colorkey = list(space = "top"), 
                scales = list(x = list(rot = 90)),
                panel = panel.corrgram, label = TRUE))
##########################################

biplot(rw_prca, col = c("gray","black"))

