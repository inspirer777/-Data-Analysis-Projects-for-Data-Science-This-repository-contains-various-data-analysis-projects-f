LAdepr=matrix(c(1,0.212,0.124,-0.167,-0.101,-0.158,0.212,1,0.098,0.308,-0.207,-0.183,0.124,0.098,1,0.044,-0.106,-0.180,-0.164,0.308,0.044,1,-0.208,-0.192,-0.101,-0.207,-0.106,-0.208,1,0.492,-0.158,-0.183,-0.180,-0.192,0.492,1),6,6)
LAdepr
colnames(LAdepr)=c("CESD","Health","Gender","Age","Edu","Income")
LAdepr
 r11  <-  LAdepr[1:2,  1:2]
  r22  <-  LAdepr[-(1:2),  -(1:2)]
  r12  <-  LAdepr[1:2,  -(1:2)]
  r21  <-  LAdepr[-(1:2),  1:2]
 (E1  <-  solve(r11)  %*%  r12  %*%  solve(r22)  %*%r21)
(E2  <-  solve(r22)  %*%  r21  %*%  solve(r11)  %*%r12)
(e1  <-  eigen(E1))
(e2  <-  eigen(E2))