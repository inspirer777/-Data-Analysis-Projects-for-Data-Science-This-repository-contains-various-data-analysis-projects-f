## Hypothetical Example
#p5
X = matrix(c(3, 4, 4, 6, 1, 5, 1, 1, 7, 3, 6, 2, 0, 2, 6, 1, 1, 1, 0, 3, 4, 7, 3, 6, 2, 2, 2, 5, 1, 0, 0, 4, 1, 1,
             1, 0, 6, 4, 3, 5, 7, 6, 5, 1, 4, 2, 1, 4, 3, 1), 10, byrow = T)
X
#p6
(D <- dist(X))

cmdscale(D, k = 9, eig = TRUE)
#p.7
max(abs(dist(X) - dist(cmdscale(D, k = 5))))

max(abs(prcomp(X)$x) - abs(cmdscale(D, k = 5)))

#p.8
X_m <- cmdscale(dist(X, method = "manhattan"), k = nrow(X) - 1, eig = TRUE)
(X_eigen <- X_m$eig)

cumsum(abs(X_eigen)) / sum(abs(X_eigen))

cumsum(X_eigen^2) / sum(X_eigen^2)
## Airline Distance
# p 8,9
airdist <- matrix(c(0, 587, 1212, 701, 1936, 604, 748, 2139, 218, 543,
                   587, 0, 920, 940, 1745, 1188, 713, 1858, 1737, 597,
                    1212, 920, 0, 879, 831, 1726, 1631, 949, 1021, 1494,
                   701, 940, 879, 0, 1374, 968, 1420, 1645, 1891, 1220,
                   1936, 1745, 831, 1374, 0, 2338, 2451, 347, 959, 2300,
                   604, 1188, 1726, 968, 2338, 0, 1092, 2594, 2734, 923,
                   748, 713, 1631, 1420, 2451, 1092, 0, 2571, 2408, 205,
                   2139, 1858, 949, 1645, 347, 2594, 2571, 0, 678, 2442,
                   218, 1737, 1021, 1891, 959, 2734, 2408, 678, 0, 2329,
                   543, 597, 1494, 1220, 2300, 923, 205, 2442, 2329, 0),10,byrow = T)
colnames(airdist) = c("Atla", "Chic", "Denv", "Hous", "LA", "Mia", "NY", "SF", "Seat", "Wash")
rownames(airdist) = c("Atlanta", "Chicago", "Denver", "Houston", "Los Angeles", "Miami", "New York",
                      "San Francisco", "Seattle", "Wash. D.C")

airline_mds <- cmdscale(airdist, k = 9, eig = TRUE)

airline_mds$points

(lam <- airline_mds$eig)

cumsum(abs(lam)) / sum(abs(lam))

#p.10
cumsum(lam^2) / sum(lam^2)

plot(airline_mds$points[,1],airline_mds$points[,2],
     type="n",xlab="Coordinate 1",ylab="Coordinate 2",
     xlim=c(-1500,1500), ylim=c(-1500,1500))

text(airline_mds$points[,1],airline_mds$points[,2],
     labels=colnames(airdist))

## 3. Non-metric multidimensional scaling
# p.12
Htl = c(0, 3, 4, 7, 3, 8, 3, 4, 8, 9, 4, 7)
Mss = c(3, 0, 6, 8, 5, 9, 2, 4, 9, 9, 5, 8)
Chr = c(4, 6, 0, 4, 6, 3, 5, 3, 8, 5, 5, 2)
Esn = c(7, 8, 4, 0, 8, 9, 7, 5, 9, 4, 4, 4)
Stl = c(3, 5, 6, 8, 0, 8, 6, 6, 6, 7, 7, 7)
Att = c(8, 9, 3, 8, 9, 0, 7, 5, 9, 8, 2, 8)
Frn = c(3, 2, 5, 7, 6, 7, 0, 4, 8, 8, 2, 3)
DGl = c(4, 4, 3, 5, 6, 5, 4, 0, 7, 4, 5, 2)
MT = c(8, 9, 8, 9, 6, 9, 8, 7, 0, 4, 9, 4)
Trm = c(9, 9, 5, 4, 7, 8, 8, 4, 4, 0, 5, 5)
Chm = c(4, 5, 5, 4, 7, 2, 2, 5, 9, 5, 0, 7)
Tit = c(7, 8, 2, 4, 7, 8, 3, 2, 4, 5, 7, 0)
WWIIleaders = cbind(Htl, Mss, Chr, Esn, Stl, Att, Frn, DGl, MT, Trm, Chm, Tit)
rownames(WWIIleaders) = c('Hitler', 'Mussolini', 'Churchill', 'Eisenhower', 'Stalin', 'Attlee', 'Franco',
                          'De Gaulle', 'Mao Tse-Tung', 'Truman', 'Chamberlin', 'Tito')
#######
## library (MASS for isoMDS)
library(MASS)
(WWII_mds <- isoMDS(WWIIleaders))

plot(WWII_mds$points[,1], WWII_mds$points[,2],
     type="n",xlab="Coordinate 1",ylab="Coordinate 2",
     xlim=c(-6,6), ylim=c(-6,6))
text(WWII_mds$points[,1], WWII_mds$points[,2],
       labels=rownames(WWIIleaders))

# 4. Correspondence analysis
## p.16
SM = matrix(c(50, 315, 24, 4012, 9, 40 ,6, 459, 41, 147, 14, 1594, 4, 11, 1, 124), 4, 4, byrow=T)
rownames(SM) = c('YN', 'YS', 'ON', 'OS')
colnames(SM) = c('pd', 'pa', 'ftd', 'fta')

D <- function(x) {
  a <- t(t(x) / colSums(x))
  ret <- sqrt(colSums((a[,rep(1:ncol(x), ncol(x))] -
                         a[, rep(1:ncol(x), rep(ncol(x), ncol(x)))])^2 *
                        sum(x) / rowSums(x)))
  matrix(ret, ncol = ncol(x))
}

(dcols <- D(SM))

(drows <- D(t(SM)))

r1 <- cmdscale(dcols, eig = TRUE)
c1 <- cmdscale(drows, eig = TRUE)
plot(r1$points, xlim = range(r1$points[,1], c1$points[,1]) * 1.5,
       ylim = range(r1$points[,1], c1$points[,1]) * 1.5, type = "n",
       xlab = "Coordinate 1", ylab = "Coordinate 2", lwd = 2)
text(r1$points, labels = colnames(SM), cex = 0.7)
text(c1$points, labels = rownames(SM), cex = 0.7)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
       
       