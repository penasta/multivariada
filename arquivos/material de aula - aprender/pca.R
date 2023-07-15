
### Analise de Componentes Principais
### Prof. George von Borries

library(Matrix)
library(data.table)
library(AMR)      # para biplot
library(ggplot2)  # para biplot

### Exemplo 8.10 de J&W: Taxas de retorno semanais...

setwd("c:/001-dados/cursos")
stock <- read.delim("T8-4.dat",header=F)
head(stock)

nomes <- c("JPMorgan", "Citibank", "WellsFargo", 
           "Shell", "Exxon")
setnames(stock,nomes)
head(stock)

cpstock <- prcomp(~ JPMorgan + Citibank + WellsFargo
                  + Shell + Exxon, 
                  data=stock, scale = F)

plot(cpstock) # ou screeplot(cpstock,type=c("lines")) 
plot(cpstock$sdev^2/sum(cpstock$sdev^2), type="b",
     ylab = "% da Variancia")
plot(cumsum(cpstock$sdev^2/sum(cpstock$sdev^2)), type="b",
     ylab = "Var Explicada")
summary(cpstock)
cpstock

ggplot_pca(cpstock) +
  labs(title = "PCA Taxas de Retorno")


### Exemplo 8.18 de J&W: Recordes nacionais para provas 
### de corrida - feminino. 

recor <- read.delim("T1-9.dat",header=F)
head(recor)

nomes <- c("Pais","v100m","v200m","v400m","v800m",
           "v1500m","v3000m","Maratona")
setnames(recor,nomes)
head(recor)

var(recor[,2:8])
cor(recor[,2:8])

cprecor <- prcomp(~ v100m + v200m + v400m + v800m + v1500m
                  + v3000m + Maratona, 
                  data=recor, scale = T)

attributes(cprecor)
plot(cprecor) # ou screeplot(cprecor,type=c("lines"))
plot(cumsum(cprecor$sdev**2)/sum(cprecor$sdev**2), 
     type="b", ylim = c(0.5,1),
     xlab="CPs", ylab="% da Variancia")

summary(cprecor)
cprecor

ggplot_pca(cprecor, labels = recor$Pais) +
  labs(title = "PCA Recordes Femininos") 

cprecor$x[,1]
recor$Pais[order(cprecor$x[,1],decreasing = T)] # maior melhor


### Exemplo 12.6 em Rencher
### Quantas componentes reter no estudo?

S <- matrix(c(0.370, 0.602, 0.149, 0.044, 0.107, 0.209,
              0.602, 2.629, 0.801, 0.666, 0.103, 0.377,
              0.149, 0.801, 0.458, 0.011,-0.013, 0.120,
              0.044, 0.666, 0.011, 1.474, 0.252,-0.054,
              0.107, 0.103,-0.013, 0.252, 0.488,-0.036,
              0.209, 0.377, 0.120,-0.054,-0.036, 0.324),
            6,6,byrow = T)
S

sum(diag(S))

# Metodo 1: apenas escolher um certo % explicado da var. 
#           para 80%, reter 2 componentes

pca.football <- eigen(S)
pca.fp <- pca.football$values / sum(pca.football$values)
pca.fc <- cumsum(pca.football$values / sum(pca.football$values))
results <- data.frame(cbind(pca.football$values,pca.fp,pca.fc))
setnames(results,
         c("Eigenvalue", "Proportion", "Cumulative"))
round(results,3)


# Metodo 2: PCs com valor superior a media dos autovalores
#           reter 2 componentes

mean(pca.football$values)

# Metodo 3: Scree Plot
#           Duas primerias PCs se destacam das demais.

barplot(pca.football$values, main="Scree Plot",
        col="blue", ylim = c(0,4))

# Metodo 4: Teste de significancia
#           apenas os dois ultimos autovalores sao iguais
#           reter 4 componentes

ts <- function(n,p,values,alpha){
  lb <- mean(values)
  k <- length(values)
  u <- (n - (2*p+11)/6)*(k*log(lb)-sum(log(values)))
  st <- qchisq(1-alpha,0.5*(k-1)*(k+2))
  resp <- (c(u,st))
  return(round(resp,2))
}

ts(60,6,pca.football$values[1:6],0.05) # u > st
ts(60,6,pca.football$values[2:6],0.05) # u > st
ts(60,6,pca.football$values[3:6],0.05) # u > st
ts(60,6,pca.football$values[4:6],0.05) # u > st
ts(60,6,pca.football$values[5:6],0.05) # u < st