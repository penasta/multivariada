
#### ANALISE MULTIVARIADA 
#### ANALISE DE CORRESPONDENCIAS 
###  PROF. GEORGE VON BORRIES
###  CODIGO PARCIAL DE GREENACRE (2008)

#### AC: EXEMPLO VIAGENS ####

#### xxxx EV.Dados ####

viagens <- matrix(c(6,1,11,1,3,11,4,25,0,2,2,20),4,3,byrow = T)
colnames(viagens) <- c("Descanso","TbParcial","TbIntegral")
rownames(viagens) <- c("Noruega","Canada","Grecia","Franca/Alemanha")
viagens

sum(viagens)
rowSums(viagens)
colSums(viagens)

#### xxxx EV.Perfis e Espaço Perfis ####

pL <- viagens/rowSums(viagens)
round(pL,2)
round(colSums(viagens)/sum(viagens),2)

pC <- t(t(viagens)/colSums(viagens)) 
round(pC,2)
round(rowSums(viagens)/sum(viagens),2)

#### xxxx EV.Grafico (r = 3) ####

require(rgl)

# (z,x,y)
rgl.lines(c(0,1.2), c(0,0),   c(0,0))
rgl.lines(c(0,0)  , c(0,1.2), c(0,0))
rgl.lines(c(0,0)  , c(0,0),   c(0,1.2))

rgl.lines(c(0,0), c(0,1), c(1,0), size = 2)
rgl.lines(c(0,1), c(1,0), c(0,0), size = 2)
rgl.lines(c(0,1), c(0,0), c(1,0), size = 2)

rgl.points(pL[,3], pL[,1], pL[,2], size = 4)
rgl.texts(pL[,3], pL[,1], pL[,2], text = rownames(pL))
rgl.texts(matrix(c(0,1.25,0, 0,0,1.25,1.4,0,0), 
                 byrow = T, ncol = 3), 
          text = colnames(pL), col = "cyan")
rgl.points(0.49,0.15,0.36, size = 4, col="yellow")

# Para salva o grafico
# rgl.snapshot("c://001-dados//imagem2-4.png","png")


#### AC:EXEMPLO LEITURA ####

#### xxxx EL.Dados ####

leitor <- matrix(c(5,7,2,18,46,20,19,29,39,
                   12,40,49,3,7,16),5,3,byrow=T)
colnames(leitor) <- c("C1","C2","C3")
rownames(leitor) <- c("E1","E2","E3","E4","E5")
leitor

sum(leitor)
rowSums(leitor)
colSums(leitor)

#### xxxx EL.Massas e Centroides ####

(leitor.pro <- leitor/apply(leitor,1,sum))
(leitor.rowsum <- apply(leitor, 1, sum))
(leitor.colsum <- apply(leitor, 2, sum))
(leitor.sum    <- sum(leitor))

leitor.pL <- leitor/rowSums(leitor)
round(leitor.pL,3)
round(colSums(leitor)/sum(leitor),3)

# Não apresentado no texto
leitor.pC <- t(t(leitor)/colSums(leitor)) 
round(leitor.pC,3)
round(rowSums(leitor)/sum(leitor),3)

#### xxxx EL.Grafico (r = 3) ####

library(ade4)
plot.new()
par(mfrow = c(1, 1))
leitor.pro.df <- data.frame(leitor)
triangle.plot(leitor.pro.df, label = row.names(leitor.pro),
              clab = 1, scale=F, show=F, csub = 2)

#### xxxx EL.Freq Esperadas ####

(leitor.esp    <- leitor.rowsum%o%leitor.colsum/leitor.sum)
# %o% Outer Product of Arrays.

#### xxxx EL.Qui-Quadrado e Inercia ####

(chi2 <- sum((leitor-leitor.esp)^2/leitor.esp))
(gl <- (nrow(leitor)-1)*(ncol(leitor)-1))
(pchi2 <- 1- pchisq(chi2,gl))

(n <- sum(leitor.colsum))

(inercia <- chi2/sum(leitor.colsum))
