
#### ANALISE MULTIVARIADA 
#### ANALISE DE CORRESPONDENCIAS 
###  PROF. GEORGE VON BORRIES
###  EXEMPLO SLIDES - COR DOS OLHOS E CABELOS

#### NBasica ####

library(ca)
# (M <- matrix(c(1, 3, 2, 2, 2, 3), 2, 3, byrow = T))

(M <- matrix(c(326,  38, 241, 110,   3,
               688, 116, 584, 188,   4,
               343,  84, 909, 412,  26,
               98,  48, 403, 681,  85),
             4, 5, byrow = T))  # Exemplo Izenman

# M indicadora 
(Mi <- caconv(M, from = "freq", to = "ind")) 
(X <- t(Mi[,1:nrow(M)]))
(Y <- t(Mi[,(nrow(M)+1):ncol(Mi)]))

#### NBasicaR ####

(XY <- X%*%t(Y))
(XX <- X%*%t(X))
(YY <- Y%*%t(Y))
(n <- sum(M))
(Dr <- XX / n)
(Dc <- YY / n)

(XbindY <- rbind(X,Y))
(B <- XbindY %*% t(XbindY))

#### PMC ####

(P <- X%*%t(Y) / n)

rowSums(P)
diag(Dr)

colSums(P)
diag(Dc)

(Pr <- solve(Dr) %*% P)
solve(XX)%*%XY

(Pc <- solve(Dc) %*% t(P))
YX <- Y%*%t(X)
solve(YY)%*%YX

c1 <- matrix(rep(1),ncol(M))
r1 <- matrix(rep(1,nrow(M)))
(r = P%*%c1) # = rowSums(P) - Massa linhas
as.matrix(diag(Dr))
as.matrix(rowSums(M)/n)
(c <- t(P)%*%r1)  # = colSums(P) - Massa Colunas
as.matrix(diag(Dc))
as.matrix(colSums(M)/n)


### Dist QQ ####

Pr
a <- Pr
t(a[1,] - a[2,]) %*% solve(Dc) %*% (a[1,] - a[2,])


Pc
b <- Pc
t(b[1,] - b[2,]) %*% solve(Dr) %*% (b[1,] - b[2,])

# Calculo com ai

qs <- 0
for(i in 1:nrow(a)){
  qs <- (qs + n * sum(P[i,]) * 
           t(a[i,] - c) %*% solve(Dc) %*% (a[i,] - c))
}

qs

# Calculo com bj

qs <- 0
for(j in 1:nrow(b)){
  qs <- (qs + n * sum(P[,j]) * 
           t(b[j,] - r) %*% solve(Dr) %*% (b[j,] - r))
}

qs

# Checagem
M.rowsum <- apply(M, 1, sum)
M.colsum <- apply(M, 2, sum)
M.sum    <- sum(M)
M.esp    <- M.rowsum%o%M.colsum/M.sum
(chi2 <- sum((M-M.esp)^2/M.esp))


(gl <- (nrow(M)-1)*(ncol(M)-1))
(pqs <- 1- pchisq(qs,gl))


#### Inercia  ####

(xx <- Dr - r%*%t(r))  # covariancias
(yy <- Dc - c%*%t(c))

solve(xx)  # Erro
solve(yy)  # Erro

(Pres <- P - r%*%t(c)) # residuos

(R0 <- (sqrt(solve(Dc))%*%t(Pres)%*%solve(Dr)
        %*%Pres%*%sqrt(solve(Dc))))

(inercia <- sum(diag(R0))) 
(inercia * n)
qs

eR0 <- eigen(R0)
cumsum(eR0$values/inercia)

#### Coordenadas Principais ####

(L <- sqrt(solve(Dr))%*%Pres%*%sqrt(solve(Dc)))
sum(L^2)
inercia

library(Matrix)
rankMatrix(Pres)[1]
rankMatrix(L)[1]

# Resultados:
t(L)%*%L
R0
R1 <- L%*%t(L)

(svdL <- svd(L))
eigen(R1)
eigen(R0)

(sqrt(Dr)%*%L%*%sqrt(Dc))
Pres

# Eixos principais de perfis de linhas
(A <- sqrt(Dr)%*%svdL$u) 
# Eixos principais de perfis de colunas
(B <- sqrt(Dc)%*%svdL$v)
(round(t(A)%*%solve(Dr)%*%A,2))
(round(t(B)%*%solve(Dc)%*%B,2))

# Coordenadas Principais dos perfis de linhas
(Gpt <- solve(Dr)%*%A%*%diag(svdL$d))
# Coordenadas Principais dos perfis de colunas
(Hpt <- solve(Dc)%*%B%*%diag(svdL$d))


#### Graficos ####  

plot(Gpt[,1],Gpt[,2], xlim=c(-0.7,1.2),  
     ylim=c(-0.4,0.3),
     xlab = "Coord. Principal 1",
     ylab = "Coord. Principal 2",
     main = "Mapa de Correspondencias")
text(Gpt[,1],Gpt[,2],
     labels=c("OA","OB","OC","OD"),
     pos = 2, col="blue")
points(Gpt[,1],Gpt[,2],pch=16,col="blue")
text(Hpt[,1],Hpt[,2],
     labels=c("CA","CB","CC","CD","CE"),
     pos = 2, col="#006666")
points(Hpt[,1],Hpt[,2],pch=16,col="#006666")


require(ca)
MC <- M
dimnames(MC) <- list(O = c("OA", "OB","OC","OD"),
                     C = c("CA","CB","CC","CD","CE"))

fit.ca <- ca(MC)
fit.ca
par(mfrow=c(1,1))
plot(fit.ca, main="Mapa de Correspondências",
     arrows = c(F,T)) # ambas em coordenadas principais

par(mfrow=c(2,2))
plot(fit.ca)
plot(fit.ca, main="Symmetric",
     arrows = c(F,T)) # ambas em coordenadas principais
plot(fit.ca, map="colgreen", main="Colgreen") 
# colunas em coordenadas principais, linhas em coord padrao
plot(fit.ca, map="rowgreen", main="Rowgreen")
# linhas em coordenadas principais, colunas em coord padrao


require(gplots)

par(mfrow=c(1,1))
balloonplot(t(as.table(MC)),
            main = "Rel. Cor dos Olhos e Cabelos",
            xlab = "", ylab = "",
            label = F, show.margins = F)



require(FactoMineR)
fac.ca <- CA(MC, graph=F)
fviz_ca_biplot(fac.ca,arrow=c(F,T),axes=c(1,2))
fviz_screeplot(fac.ca) +
  geom_hline(yintercept = 10.00, linetype = 2, 
             color = "blue")