L <- c(.9,.7,.5)
Lt <- t(L)

L %*% Lt

psi <- matrix(data=c(.19,0,0,
              0,.51,0,
              0,0,.75),3,3)

L %*% Lt + psi

P1 <- t(matrix(data=c(.625,.594,.507,
                    -.291,-.491,.843,
                    .749,-.638,-.177),3,3))
D1 <- matrix(rep(0,9),3,3)
diag(D1) <- c(1.96,.68,.36)
P1T <- t(P1)

P1 %*% D1 %*% P1T


library(pacman)
p_load(pracma)



LL <- c(.625,.594,.507)

LL <- sqrt(1.96) * LL

LLT <- t(LL)

SIGMA <- LL %*% LLT

Psi <- matrix(rep(0,9),3,3)

diag(Psi) <- abs(diag(SIGMA)-1)

# Psi

SIGMA <- SIGMA + Psi

SIGMA


###
# ANALISE MULTIVARIADA 1
#### ANALISE FATORIAL - EXEMPLOS ####
# PROF. GEORGE VON BORRIES;

#### EXEMPLO 1 ####
# J&w - EXEMPLO 9.4, PAG. 493
# Solucao utilizando Componentes Principais

p_load(data.table)
p_load(psych)

setwd("c:/001-dados/cursos")
stock <- read.delim("T8-4.dat",header=F)
nomes <- c("JPMorgan", "Citibank", "WellsFargo", 
           "Shell", "Exxon")
setnames(stock,nomes)
head(stock)

cor(stock)

#### ..... F1 - PC ####

af1stock <- principal(dados, nfactors = 1, rotate = 'none',
                      covar = FALSE)
af1stock  

resaf1stock <- (round(cor(dados) 
                      - af1stock$loadings %*% t(af1stock$loadings)
                      - diag(af1stock$uniquenesses),3))
resaf1stock

#### ..... F1, F2 - PC ####

af2stock <- principal(dados, nfactors = 2, rotate = 'none',
                      covar = FALSE)
af2stock  

resaf2stock <- (round(cor(dados) 
                      - af2stock$loadings %*% t(af2stock$loadings)
                      - diag(af2stock$uniquenesses),3))
resaf2stock

load <- af2stock$loadings[,1:2]
plot(load,type="n")
text(load,labels=c("X1","X2","X3",
                   "X4","X5","X6","X7"),cex=1)

#### ..... F1, F2 - EMV ####
# cov ou cor
af2stockemv <- factanal(covmat = cor(dados),
                        factors = 2, rotation = "none")
af2stockemv$uniquenesses
af2stockemv$loadings
print(af2stockemv, digits=3, cutoff=.0003)

(resaf2stockemv <- (round(cor(dados)
                          - (af2stockemv$loadings[,1:2] 
                             %*% t(af2stockemv$loadings[,1:2])) 
                          - diag(af2stockemv$uniquenesses),3)))

load2 <- af2stockemv$loadings[,1:2]
plot(load2,type="n")
text(load2,labels=c("X1","X2","X3",
                    "X4","X5","X6","X7"),cex=1.2)

sum(abs(resaf1stock))
sum(abs(resaf2stock))
sum(abs(resaf2stockemv))

#### ..... F1, F2 - EMV - Varimax ####

af2stockemvvm <- factanal(covmat = cor(dados),
                          factors = 2, rotation = "varimax")
af2stockemvvm$uniquenesses
af2stockemvvm$loadings
print(af2stockemvvm, digits=3, cutoff=.0003)

(resaf2stockemvvm <- (round(cor(dados)
                            - (af2stockemvvm$loadings[,1:2] 
                               %*% t(af2stockemvvm$loadings[,1:2])) 
                            - diag(af2stockemvvm$uniquenesses),3)))

load3 <- af2stockemvvm$loadings[,1:2]
plot(load3,type="n")
text(load3,labels=c("X1","X2","X3",
                    "X4","X5","X6","X7"),cex=1.2)

sum(abs(resaf2stockemv))
sum(abs(resaf2stockemvvm))


#### EXEMPLO 2 ####
# J&w - EXEMPLO 9.3, PAG. 491
# Solucao utilizando Componentes Principais

# Taste, Money (good by for money), Flavor
# Snack (suitable for snack), Energy (provides lots of energy)

p_load(MCMCpack) # facil de gerar a matriz cor

(concor <- xpnd(c(1,0.02,0.96,0.42,0.01,
                  1,0.13,0.71,0.85,
                  1,0.50,0.11,
                  1,0.79,
                  1),5))

dimnames(concor) <- list(c("Taste","Money","Flavor","Snack","Energy"),
                         c("Taste","Money","Flavor","Snack","Energy"))
concor

sol <- principal(concor, nfactors = 2, rotate = 'none',
                 covar = FALSE)
sol

load <- sol$loadings[,1:2]
plot(load,type="n")
text(load,labels=c("Taste","Money","Flavor",
                   "Snack","Energy"),cex=1.2)



#### EXEMPLO 3 ####
# AMOSTRA HIPOTETICA DE 300 RESPOSTAS SOBRE 6 DISCIPLINAS.
# DISC. AVALIADAS NA ESCALA DE 1 (NAO GOSTA) A 5 (GOSTA)

facdis <- read.csv("c:/001-dados/cursos/facdis2.csv")
head(facdis)

# CUIDADO: DADOS DISCRETOS.

corMat  <- cor(facdis)
corMat

solution <- factanal(facdis,factors = 2, rotation = "varimax")
solution


load <- solution$loadings[,1:2]
plot(load,type="n")
text(load,labels=c("BIO","GEO","CHEM",
                   "ALG","CALC","STAT"),cex=1.2)


#### EXEMPLO 4 ####
# Exercicio 9.8 de J&W
# Caso Heywood 

covM <- xpnd(c(1,0.4,0.9,
               1,0.7,
               1),3)


(hs0 <- factanal(covM, factors=1, rotation="none"))

(hs1 <- principal(covM, nfactors = 1, rotate = 'none',
                  covar = T))
rhs1 <- (round(covM 
               - hs1$loadings %*% t(hs1$loadings)
               - diag(hs1$uniquenesses),3))
rhs1

(hs2 <- fa(covM, nfactors = 1, rotate = "none", fm = "ml"))
rhs2 <- (round(covM 
               - hs2$loadings %*% t(hs2$loadings)
               - diag(hs2$uniquenesses),3))
rhs2

