
### ANALISE MULTIVARIADA 1
### MEDIDAS DESCRITIVAS 
### REFERENCIA: EVERITT (2011) AN INTRODUCTION TO APPLIED
###             MULTIVARIATE ANALYSIS WITH R

### LEITURA DOS DADOS

medidas <- read.table("c:/001-dados/cursos/medidas.txt", 
                      sep="\t")

head(medidas)
tail(medidas)

medidas[1:2,]
medidas[,1]
t(medidas[,1])
dim(medidas)
medidas[1,1]


### RESUMINDO OS DADOS

mean(medidas[,1])
apply(medidas[,1:3],2,mean) 

aggregate(cbind(peito,cintura,quadril) ~ sexo, medidas, mean)

(R <- cor(medidas[,1:3]))
var(medidas[,1])
(S <- cov(medidas[,1:3]))
(D <- diag(apply(medidas[,1:3],2,var),3,3))

solve(sqrt(D))%*%S%*%solve(sqrt(D))
R
sqrt(D)%*%R%*%sqrt(D)
S

(gsv <- det(S)) # variancia amostral generalizada

library(psych)
(tsv <- tr(S))  # variancia total amostral

(X <- matrix(c(1,9,10,4,12,16,2,10,12,
               5,8,13,3,11,14),5,3,byrow=T))
(det(cov(X)))   # gsv
(tr(cov(X)))    # tsv

### Observacoes:

(R0 <- matrix(c(1,0,0,1),2,2))
(R1 <- matrix(c(1,.8,.8,1),2,2))
(R2 <- matrix(c(1,-.8,-.8,1),2,2))
(R3 <- matrix(c(1,1,1,1),2,2))

det(R0);det(R1);det(R2);det(R3)
tr(R0);tr(R1);tr(R2);tr(R3)

