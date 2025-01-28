
#Principal component analysis mtcars
?mtcars
count =row.names(mtcars)

dataa = mtcars[,c(1:7,10,11)]
x <- cor(dataa)
mm = as.matrix(x)
mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)
summary(mtcars.pca)


#install.packages("devtools")
library(devtools)
remotes::install_github('vqv/ggbiplot')
library(ggbiplot)


ggbiplot(mtcars.pca)

ggbiplot(mtcars.pca, labels=rownames(mtcars))


mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))

ggbiplot(mtcars.pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars.country)


ggbiplot(mtcars.pca,ellipse=TRUE,choices=c(3,4),   labels=rownames(mtcars), groups=mtcars.country)


ggbiplot(mtcars.pca,ellipse=TRUE,circle=TRUE, labels=rownames(mtcars), groups=mtcars.country)


ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(mtcars), groups=mtcars.country)



ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE,   labels=rownames(mtcars), groups=mtcars.country)


ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(mtcars), groups=mtcars.country) +
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "dark blue"))+
  ggtitle("PCA of mtcars dataset")+
  theme_minimal()+
  theme(legend.position = "bottom")


spacecar <- c(1000,60,50,500,0,0.5,2.5,0,1,0,0)

mtcarsplus <- rbind(mtcars, spacecar)
mtcars.countryplus <- c(mtcars.country, "Jupiter")



mtcarsplus.pca <- prcomp(mtcarsplus[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

ggbiplot(mtcarsplus.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = FALSE, var.axes=TRUE, labels=c(rownames(mtcars), "spacecar"), groups=mtcars.countryplus)+
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "violet", "dark blue"))+
  ggtitle("PCA of mtcars dataset, with extra sample added")+
  theme_minimal()+
  theme(legend.position = "bottom")


s.sc <- scale(t(spacecar[c(1:7,10,11)]), center= mtcars.pca$center)
s.pred <- s.sc %*% mtcars.pca$rotation


mtcars.plusproj.pca <- mtcars.pca
mtcars.plusproj.pca$x <- rbind(mtcars.plusproj.pca$x, s.pred)


ggbiplot(mtcars.plusproj.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = FALSE, var.axes=TRUE, labels=c(rownames(mtcars), "spacecar"), groups=mtcars.countryplus)+
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "violet", "dark blue"))+
  ggtitle("PCA of mtcars dataset, with extra sample projected")+
  theme_minimal()+
  theme(legend.position = "bottom")
###########################################################
### teacher code 
#########################################################
data_pca <- princomp(dataa, cor = TRUE)
data_pca
summary(data_pca)

panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="grey", ...)
}
mtcars_pca <- princomp(dataa, cor = TRUE)
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="grey", ...)
}
# mtcars$negtemp <- dataa$temp * (-1)
mtcars$temp <- NULL
pairs(dataa, diag.panel = panel.hist,
      pch = ".", cex = 1.5)
summary(mtcars_pca, loadings = TRUE)
library(MVA)
pairs(mtcars_pca$scores[,1:3], ylim = c(-6, 4), xlim = c(-6, 4),
      panel = function(x,y,...) {
        text(x, y, abbreviate(row.names(mtcars)),
             cex = 0.6)
        bvbox(cbind(x,y),add=TRUE)
      })
par(mfrow=c(3,2))
out <- sapply(1:3, function(i) {
  plot(mtcars$mpg,mtcars_pca$scores[,i],
       xlab = paste("PC",i, sep = ""),
       ylab = "cylender")
})
mtcars

x=mtcars_pca$score
mpg=mtcars[,1]
mtcars_reg <- lm(mpg~x[,c(2,3,4)])
summary(mtcars_reg)
#######################################
########## MLE
mtcars
dd = rownames(mtcars)
sapply(1:3, function(f)
  factanal(covmat=mm, factors= f, method="mle",n.obs=32)$PVAL)

factanal(covmat = mm, factors = 3,n.obs =32,rotation="none")

out=factanal(covmat = mm, factors = 3,n.obs =280)
summary(out)

out$loadings
out$uniquenesses
######Goodness of fit

apply(out$loadings^2,1,sum)# communality
1 - apply(out$loadings^2,1,sum) # uniqueness
Lambda <- out$loadings
Psi <- diag(out$uniquenesses)
Rhat <- Lambda %*% t(Lambda) + Psi
round(mm - Rhat, 6)

####Factor analysis with principal component method######
mm.eigen <- eigen(mm)
D1=diag(c(mm.eigen$values[1:4]))
G1<- as.matrix(mm.eigen$vectors[,1:4])
Lambda.hat=G1 %*% sqrt(D1)
h2.hat <- rowSums(Lambda.hat^2)
psi.hat=1-h2.hat

############################################################
## factor analysis 
############################################################
sapply(1:3, function(f)
  factanal(dd, factors= f, method="mle")$PVAL)

fa.dd=factanal(dataa, factors= 3, method="mle", rotation="varimax")
summary(fa.dd)
fa.dd=factanal(dataa, factors= 3, method="mle")
fa.dd
(scores <- factanal(dd, factors= 3, mrthod= "mle",
                    scores= "regression")$scores)
x<-scores[,1]
y<-scores[,2]
plot(x, y, xlab = "factor1", ylab = "factor2", xlim=c(-2,3), ylim=c(-2,3),
     type = "n")
text(x, y, labels = count, cex = 0.5)

factanal(dd, factors = 3, rotation = "none")
(scores <- factanal(dd, factors= 3,rotation = "promax",
                    mrthod= "mle",scores= "regression")$scores)
x2<-scores[,1]
y2<-scores[,2]
plot(x2, y2, xlab = "factor1", ylab = "factor2", xlim=c(-2,3), ylim=c(-2,3),
     type = "n")
text(x, y, labels = count, cex = 0.5)

######Goodness of fit
apply(fa.dd$loadings^2,1,sum)# communality
1 - apply(fa.dd$loadings^2,1,sum) # uniqueness
Lambda <- fa.dd$loadings
Psi <- diag(fa.dd$uniquenesses)
S <- fa.dd$correlation;S;cor(dd)
Shat <- Lambda %*% t(Lambda) + Psi
round(S - Shat, 6)

###Rotation
dd.fa.none <- factanal(mtcars, factors = 2, rotation = "none")
dd.fa.varimax <- factanal(mtcars, factors = 2, rotation = "varimax")
dd.fa.promax <- factanal(mtcars, factors = 2, rotation = "promax")

par(mfrow = c(1,3))
plot(dd.fa.none$loadings[,1], 
     dd.fa.none$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")
abline(h = 0, v = 0)
text(dd.fa.varimax$loadings[,1]-0.08, 
     dd.fa.varimax$loadings[,2]+0.08,
     colnames(dd),
     col="blue")

plot(dd.fa.varimax$loadings[,1], 
     dd.fa.varimax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")

text(dd.fa.varimax$loadings[,1]-0.08, 
     dd.fa.varimax$loadings[,2]+0.08,
     colnames(dd),
     col="blue")
abline(h = 0, v = 0)

plot(dd.fa.promax$loadings[,1], 
     dd.fa.promax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Promax rotation")
abline(h = 0, v = 0)
text(dd.fa.varimax$loadings[,1]-0.08, 
     dd.fa.varimax$loadings[,2]+0.08,
     colnames(dd),
     col="blue")


############################ ch5 #################
### dendogram 
ord <- order.dendrogram(as.dendrogram(hclust(dist(mtcars))))  
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

print(levelplot(mtcars[ord, ord], at = do.breaks(c(-1.01, 1.01), 20),
                xlab = NULL, ylab = NULL, colorkey = list(space = "top"), 
                scales = list(x = list(rot = 90)),
                panel = panel.corrgram, label = TRUE))

