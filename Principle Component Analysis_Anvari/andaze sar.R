head_dat= matrix(c(191,195,181,183,176,208,189,197,188,192,179,183,174,190,188,163,195,186,181,175,192,174,176,197,190,179,201,185,188,171,192,190,189,197,187,186,174,185,195,187,161,183,173,182,165,185,178,176,200,187),25,2)
head_dat
colMeans(head_dat)
cov(head_dat)
head_pca <- princomp(x = head_dat)
head_pca
print(summary(head_pca), loadings = TRUE)
a1<-183.84-0.721*185.72/0.693
b1<-0.721/0.693
a2<-183.84-(-0.693*185.72/0.721)
b2<--0.693/0.721
plot(head_dat,  xlab  =  "First  son ' s  head  length  (mm)",
   ylab  =  "Second  son ' s  head  length")
abline(a1, b1)
abline(a2, b2, lty = 2)
xlim <- range(head_pca$scores[,1])
plot(head_pca$scores, xlim = xlim, ylim = xlim)
headsize=matrix(c(191,195,181,183,176,208,189,197,188,192,179,183,174,190,188,163,195,186,181,175,192,174,176,197,190,155,149,148,153,144,157,150,159,152,150,158,147,150,159,151,137,155,153,145,140,154,143,139,167,163,179,201,185,188,171,192,190,189,197,187,186,174,185,195,187,161,183,173,182,165,185,178,176,200,187,145,152,149,149,142,152,149,152,159,151,148,147,152,157,158,130,158,148,146,137,152,147,143,158,150),25,4)
colnames(headsize)=c("head1","breadth1","head2","breadth2")
headsize
headsize.std  <-  sweep(headsize,  2,
 apply(headsize,  2,  sd),  FUN  =  "/")
headsize.std
R  <-  cor(headsize.std)
  r11  <-  R[1:2,  1:2]
  r22  <-  R[-(1:2),  -(1:2)]
  r12  <-  R[1:2,  -(1:2)]
  r21  <-  R[-(1:2),  1:2]
  (E1  <-  solve(r11)  %*%  r12  %*%  solve(r22)  %*%r21)
(E2  <-  solve(r22)  %*%  r21  %*%  solve(r11)  %*%r12)
(e1  <-  eigen(E1))
(e2  <-  eigen(E2))
girth1  <-  headsize.std[,1:2]  %*%  e1$vectors[,1]
  girth2  <-  headsize.std[,3:4]  %*%  e2$vectors[,1]       
  shape1  <-  headsize.std[,1:2]  %*%  e1$vectors[,2]
  shape2  <-  headsize.std[,3:4]  %*%  e2$vectors[,2]
  (g  <-  cor(girth1,  girth2))
(s  <-  cor(shape1,  shape2))
 plot(girth1,  girth2)
  plot(shape1,  shape2)
