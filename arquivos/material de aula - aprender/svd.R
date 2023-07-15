
# Topicos em Aprendizado Estatístico
# Decomposicao em valores singulares

### Exemplo: Avaliacao de Filmes por Usuarios

library(Matrix)
library(psych)

P <- matrix(c(8,8,9,3,4,
              7,6,7,3,5,
              8,9,9,2,4,
              9,8,9,1,2,
              3,2,2,9,8,
              2,1,2,8,8,
              3,2,2,7,8),7,5,byrow = T)
P
rankMatrix(P)[1]

(svdP <- svd(P))
(U <- svdP$u)
(V <- svdP$v)
(D <- diag(svdP$d))

round(P - U%*%D%*%t(V),2)


(svdP2 <- svd(P,nu=2,nv=2))
(U2 <- round(svdP2$u,1))
(V2 <- round(svdP2$v,1))
(D2 <- round(diag(svdP2$d[1:2]),1))

round(P - U2%*%D2%*%t(V2),1)

tr(D2) / tr(D) * 100  # Percentual da energia (ou informacao)
# retida pela SVD truncada



### Biplot de Gabriel

par(mfrow=c(1,2))

UT <- c('J1','J2','J3','J4','I1','I2','I3')
VT <- c('F1','F2','F3','R1','R2')

plot(U2, col='white', pch=16, 
     ylim=c(-1,1), xlim=c(-1,1))
text(U2,labels = UT, col='blue')
text(V2[,1]+0.15,V2[,2], labels = VT, col='red')
abline(v=0, h=0)
biplot(U2,V2,col=c('blue','red'),
       xlabs = UT, ylabs = VT)