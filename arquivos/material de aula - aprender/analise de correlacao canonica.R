
# Analise Multivariada 1
# Analise de Correlacao Canonica
#
# Professor: George von Borries.


#### EXEMPLO 10.1 ####
# Johnson e Wichern - pag. 543 a 545

# (1)
(rhoyy <- matrix(c(1, .4, .4, 1),2,2))
(rhoyx <- matrix(c(.5, .3, .6, .4), 2, 2))
(rhoxy <- t(rhoyx))
(rhoxx <- matrix(c(1,.2,0.2,1),2,2))

# (2)
rhoyysvd <- svd(rhoyy) 
(rhoyy.sqia <- rhoyysvd$u %*% solve(diag(sqrt(rhoyysvd$d))) %*% t(rhoyysvd$v))

# (3)
solve(rhoxx) 

# (4)
(mrhoy <- rhoyy.sqia %*% rhoyx %*% solve(rhoxx) %*% rhoxy %*% rhoyy.sqia)

# (5)
(mrhoysvd <- svd(mrhoy))
(rho12 <- mrhoysvd$d[1])

# (6)
(rho22 <- mrhoysvd$d[2])

# (7)
(a1 <- rhoyy.sqia %*% mrhoysvd$u[,1])

# (8)
rhoxxsvd <- svd(rhoxx) 
rhoxx.sqib <- rhoxxsvd$u %*% solve(diag(sqrt(rhoxxsvd$d))) %*% t(rhoxxsvd$v)
mrhox <- rhoxx.sqib %*% rhoxy %*% solve(rhoyy) %*% rhoyx %*% rhoxx.sqib
mrhoxsvd <- svd(mrhox)
rho21 <- mrhoysvd$d[1]
rho11 <- mrhoysvd$d[2]
(b1 <- rhoxx.sqib %*% mrhoxsvd$u[,1])

# (9) 
(rho1 <- sqrt(rho12))
# (10)
(rho2 <- sqrt(rho22))


(VarU1 <- t(a1) %*% rhoyy %*% a1)
(VarV1 <- t(b1) %*% rhoxx %*% b1)
# (11)
(CorU1V1 <- t(a1) %*% rhoyx %*% b1)


#### EXEMPLO 10.2 ####
# Johnson e Wichern - pag. 546 a 547

(Az <- a1)   # (1)
(Bz <- b1)   # (1)

(rhoUZy <- t(Az) %*% rhoyy)  # (2)
(rhoVZx <- t(Bz) %*% rhoxx)  # (3)

(rhoUZx <- t(Az) %*% rhoyx)  # (4)
(rhoVZy <- t(Bz) %*% rhoxy)  # (5)


#### EXEMPLO 10.3 ####
# Johnson e Wichern - pag. 549

# (1)
(covyy <- matrix(c(100, 0, 0, 1),2,2))
(covyx <- matrix(c( 0, .95, 0, 0), 2, 2))
(covxy <- t(rhoyx))
(covxx <- matrix(c(1, 0, 0, 100),2,2))

v1 <- cbind(covyy,covyx)
v2 <- cbind(covxy,covxx)
covyx <- rbind(v1,v2)

rho <- solve(diag(sqrt(diag(covyx)))) %*% covyx %*% solve(diag(sqrt(diag(covyx))))

(rhoyy <- rho[1:2,1:2])
(rhoyx <- rho[1:2,3:4])
(rhoxy <- rho[3:4,1:2])
(rhoxx <- rho[3:4,3:4])

rhoyysvd <- svd(rhoyy) 
(rhoyy.sqia <- rhoyysvd$u %*% solve(diag(sqrt(rhoyysvd$d))) %*% t(rhoyysvd$v))

solve(rhoxx) 

(mrhoy <- rhoyy.sqia %*% rhoyx %*% solve(rhoxx) %*% rhoxy %*% rhoyy.sqia)

(mrhoysvd <- svd(mrhoy))
(rho12 <- mrhoysvd$d[1])

(rho22 <- mrhoysvd$d[2])

(a1 <- rhoyy.sqia %*% mrhoysvd$u[,1])

rhoxxsvd <- svd(rhoxx) 
rhoxx.sqib <- rhoxxsvd$u %*% solve(diag(sqrt(rhoxxsvd$d))) %*% t(rhoxxsvd$v)
mrhox <- rhoxx.sqib %*% rhoxy %*% solve(rhoyy) %*% rhoyx %*% rhoxx.sqib
mrhoxsvd <- svd(mrhox)
rho21 <- mrhoysvd$d[1]
rho11 <- mrhoysvd$d[2]
(b1 <- rhoxx.sqib %*% mrhoxsvd$u[,1])

(rho1 <- sqrt(rho12))
(rho2 <- sqrt(rho22))

(VarU1 <- t(a1) %*% rhoyy %*% a1)
(VarV1 <- t(b1) %*% rhoxx %*% b1)

# (2)
(CorU1V1 <- t(a1) %*% rhoyx %*% b1)


#### EXEMPLO 10.4 ####
# Johnson e Wichern - pag. 552 e 553

# (1)
library(MCMCpack)
(coryx <- xpnd(c(1,0.505,0.569,0.602,
                 1,.422,0.467,
                 1,0.926,
                 1),4))

(rhoyy <- coryx[1:2,1:2])
(rhoyx <- coryx[1:2,3:4])
(rhoxy <- coryx[3:4,1:2])
(rhoxx <- coryx[3:4,3:4])

rhoyysvd <- svd(rhoyy) 
(rhoyy.sqia <- rhoyysvd$u %*% solve(diag(sqrt(rhoyysvd$d))) %*% t(rhoyysvd$v))

solve(rhoxx) 

(mrhoy <- rhoyy.sqia %*% rhoyx %*% solve(rhoxx) %*% rhoxy %*% rhoyy.sqia)

(mrhoysvd <- svd(mrhoy))
(rho12 <- mrhoysvd$d[1])

(rho22 <- mrhoysvd$d[2])


rhoxxsvd <- svd(rhoxx) 
rhoxx.sqib <- rhoxxsvd$u %*% solve(diag(sqrt(rhoxxsvd$d))) %*% t(rhoxxsvd$v)
mrhox <- rhoxx.sqib %*% rhoxy %*% solve(rhoyy) %*% rhoyx %*% rhoxx.sqib
mrhoxsvd <- svd(mrhox)
rho21 <- mrhoysvd$d[1]
rho11 <- mrhoysvd$d[2]


(a1 <- rhoyy.sqia %*% mrhoysvd$u[,1]) # (2)
(b1 <- rhoxx.sqib %*% mrhoxsvd$u[,1]) # (3)
(a2 <- rhoyy.sqia %*% mrhoysvd$u[,2]) # (4)
(b2 <- rhoxx.sqib %*% mrhoxsvd$u[,2]) # (5)

(rho1 <- sqrt(rho12)) # (6)
(rho2 <- sqrt(rho22)) # (7)

(CorU1V1 <- t(a1) %*% rhoyx %*% b1) # (6)
(CorU2V2 <- t(a2) %*% rhoyx %*% b2) # (7)


#### EXEMPLO 10.5 ####
# Johnson e Wichern - pag. 553 a 555

# (1)
library(MCMCpack)
(coryy <- xpnd(c(1,.49,.53,.49,.51,
                 1,.57,.46,.53,
                 1,.48,.57,
                 1,.57,
                 1),5))

(coryx <- matrix(
  c(.33, .32, .20, .19, .30, .37, .21,
    .30, .21, .16, .08, .27, .35, .20,
    .31, .23, .14, .07, .24, .37, .18,
    .24, .22, .12, .19, .21, .29, .16,
    .38, .32, .17, .23, .32, .36, .27),
  byrow = T, 5, 7))

(corxy <- t(coryx))
(corxx <- xpnd(c(1, .43, .27, .24, .34, .37, .40,
                 1, .33, .26, .54, .32, .58,
                 1, .25, .46, .29, .45,
                 1, .28, .30, .27,
                 1, .35, .59,
                 1, .31, 
                 1),7))


rhoyysvd <- svd(coryy) 
(rhoyy.sqia <- rhoyysvd$u %*% solve(diag(sqrt(rhoyysvd$d))) %*% t(rhoyysvd$v))

(mrhoy <- rhoyy.sqia %*% coryx %*% solve(corxx) %*% corxy %*% rhoyy.sqia)

(mrhoysvd <- svd(mrhoy))
(rho11 <- mrhoysvd$d[1])
(rho22 <- mrhoysvd$d[2])


rhoxxsvd <- svd(corxx) 
rhoxx.sqib <- rhoxxsvd$u %*% solve(diag(sqrt(rhoxxsvd$d))) %*% t(rhoxxsvd$v)
mrhox <- rhoxx.sqib %*% corxy %*% solve(coryy) %*% coryx %*% rhoxx.sqib
mrhoxsvd <- svd(mrhox)


(a1 <- rhoyy.sqia %*% mrhoysvd$u[,1]) # (2)
(b1 <- rhoxx.sqib %*% mrhoxsvd$u[,1]) # (3)
(a2 <- rhoyy.sqia %*% mrhoysvd$u[,2])  
(b2 <- rhoxx.sqib %*% mrhoxsvd$u[,2])  

(Az <- a1) 
(Bz <- b1)

(corUZy <- t(Az) %*% coryy) # (4)
(corVZy <- t(Bz) %*% corxy) # (5)

(corUZx <- t(Az) %*% coryx) # (6)
(corVZx <- t(Bz) %*% corxx) # (7)

(rho1 <- sqrt(rho12)) # (6)
(rho2 <- sqrt(rho22)) # (7)

(CorU1V1 <- t(a1) %*% rhoyx %*% b1) # (6)
(CorU2V2 <- t(a2) %*% rhoyx %*% b2) # (7)

