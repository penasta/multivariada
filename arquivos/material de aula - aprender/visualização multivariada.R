
### ANALISE MULTIVARIADA 1
### REPRESENTACAO GRAFICA 

### EXEMPLO 1: 
### REFERENCIA: EVERITT (2011) AN INTRODUCTION TO APPLIED
###             MULTIVARIATE ANALYSIS WITH R

### LEITURA DOS DADOS

medidas <- read.table("c:/001-dados/cursos/medidas.txt", 
                      sep="\t")

head(medidas)
tail(medidas)


### GRAFICOS UNIVARIADOS

boxplot(peito ~ sexo, data=medidas, col="blue",
        main="Medidas do Peito (em polegadas)")

library(ggplot2)
ggplot(medidas, aes(x=sexo, y=peito)) +
  geom_dotplot(binaxis = "y", stackdir="center", 
               binwidth = 0.5) 

ggplot(medidas, aes(x=sexo, y=peito, fill = factor(sexo))) +
  geom_dotplot(binaxis = "y", stackdir="center", 
               binwidth = 0.5) +
  theme(legend.position = c(0.2, 0.8))

ggplot(medidas, aes(x=peito, fill = factor(sexo))) +
  geom_dotplot(stackgroups = TRUE, method = "histodot", 
               binwidth = 0.5) +
  theme(legend.position = c(0.2, 0.8))


# install.packages("TeachingDemos")
require(TeachingDemos)

faces(medidas[,1:3])
faces2(medidas[,1:3])


### EXEMPLO 2: IRIS DATA
### GRAFICOS DE DADOS MULTIVARIADOS

## ANDREWS PLOT

# install.packages("andrews")
require(andrews)

data(iris)
head(iris)

par(mfrow=c(2, 2))

andrews(iris,type=1,step=500,clr=5,
        ymax=3,main='Andrews Plot 1')
andrews(iris,type=2,step=500,clr=5,
        ymax=3,main='Andrews Plot 2')
andrews(iris,type=3,step=500,clr=5,
        ymax=3,main='Andrews Plot 3')
andrews(iris,type=4,step=500,clr=5,
        ymax=3,main='Andrews Plot 4')

par(mfrow=c(1, 1))
# Nota:
# Para saber a ordem das cores basta pedir o comando
pie(rep(1,4),col = rainbow(4))

## MATRIZ DE CORRELACOES

head(iris)
library(data.table)
nomes <- c("Sepala.C", "Sepala.L", 
           "Petala.C", "Petala.L", "Especie")
setnames(iris,nomes)
head(iris)

library(lattice)
super.sym <- trellis.par.get("superpose.symbol")
splom(~iris[1:4], groups = Especie, data = iris,
      xlab=" ",
      panel = panel.superpose,
      key = list(title = "Tres Variedades de Iris",
                 columns = 3, 
                 points = list(pch = super.sym$pch[1:3],
                               col = super.sym$col[1:3]),
                 text = list(c("Setosa", 
                               "Versicolor", "Virginica"))))

## GRAFICO DE PERFIS OU PARALLEL PLOT

parallelplot(~iris[1:4] | Especie, iris) 
parallelplot(~iris[1:4], iris, groups = Especie,
             horizontal.axis = FALSE, 
             scales = list(x = list(rot = 90)))

## FACE PLOTS (OUTRO PACOTE R)

# install.packages("aplpack")
library(aplpack)

sampiris <- rbind(iris[1,],iris[5,],iris[51,],iris[55,],
                  iris[101,],iris[105,])
sampiris
faces(sampiris[,1:4],face.type=0)
faces(sampiris[,1:4],face.type=1)


## MAPAS DE CALOR OU HEATMAPS

iris2 <- as.matrix(iris[,1:4])
colnames(iris2) <- c("sl","sw","pl","pw")
head(iris2)

# install.packages("gplots")
library(gplots)

heatmap.2(iris2, col=redgreen(75), dendrogram = 'none',
          scale="row", Rowv = FALSE, Colv = FALSE, 
          key=TRUE, symkey=FALSE, 
          density.info="none", trace="none", cexRow=0.5)
