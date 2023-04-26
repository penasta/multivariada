library(pacman)
p_load(knitr,tidyverse,ggExtra,gridExtra,car,Matrix,psych,OpenImageR,gplots)

# --------------------------------------------------------------------------- #

# Questão 1

# Comprovando c/ exemplo numérico

set.seed(150167636)
matriz <- matrix(c(sample.int(n=9,replace=T)),
                 nrow = 3, byrow = TRUE)

cov <- cov(matriz)
eig <- eigen(cov)

svar <- sum(diag(cov))
seig <- sum(eig$values)

# É necessário "arredondar" os resultados para a igualdade valer, mas na realidade é uma limitação do R (truncamento)
round(svar) == round(seig)

# E o resultado está comprovado para este caso particular.

# Aumentando agora a matriz e verificando se a igualdade permanece
set.seed(636761051)
matriz2 <- matrix(c(sample.int(n=100,replace=T)),
                  nrow = 10, byrow = TRUE)

cov2 <- cov(matriz2)
eig2 <- eigen(cov2)

svar2 <- sum(diag(cov2))
seig2 <- sum(eig2$values)

# É necessário "arredondar" os resultados para a igualdade valer, mas na realidade é uma limitação do R (truncamento)
round(svar2) == round(seig2)

# O que fortalece a hipótese do resultado.

# --------------------------------------------------------------------------- #

# Questão 2 ----
# Exemplo com detalhes ----

img <- readImage("rdocs/maicon.jpeg")
dim(img)
imageShow(img,clear_viewer = T)
writeImage(img, file_name = 'rdocs/img.png')


img_gray <- rgb_2gray(img) 

dim(img_gray)
imageShow(img_gray,clear_viewer = T)
writeImage(img_gray, file_name = 'rdocs/img_gray.png')

# Aplicando SVD na Imagem

imgg.svd <- svd(img_gray)
imgg.svd$d
D <- diag(imgg.svd$d)
dim(D)

U <- imgg.svd$u
V <- imgg.svd$v


png(file="rdocs/plot1.png")
plot(1:length(imgg.svd$d), imgg.svd$d)
abline(v=c(5,10,20,40))
dev.off()

png(file="rdocs/plot2.png")
plot(1:length(imgg.svd$d), imgg.svd$d,
     xlim = c(1,60))
abline(v=c(5,10,20,40))
dev.off()

png(file="rdocs/plot3.png")
plot(1:length(imgg.svd$d), imgg.svd$d,
     xlim = c(10,80),ylim = c(0,30))
abline(v=c(20,40,60))
dev.off()

### Aproximacoes

U5 <- as.matrix(U[,1:5])
V5 <- as.matrix(V[,1:5])
D5 <- diag(imgg.svd$d[1:5])
img_gray5 <- U5 %*% D5 %*% t(V5)
imageShow(img_gray5,clear_viewer = T)
writeImage(img_gray5, file_name = 'rdocs/img_gray5.png')

tr(D5)/tr(D) * 100 


U10 <- as.matrix(U[,1:10])
V10 <- as.matrix(V[,1:10])
D10 <- diag(imgg.svd$d[1:10])
img_gray10 <- U10 %*% D10 %*% t(V10)
imageShow(img_gray10,clear_viewer = T)
writeImage(img_gray10, file_name = 'rdocs/img_gray10.png')

tr(D10)/tr(D) * 100 


U20 <- as.matrix(U[,1:20])
V20 <- as.matrix(V[,1:20])
D20 <- diag(imgg.svd$d[1:20])
img_gray20 <- U20 %*% D20 %*% t(V20)
imageShow(img_gray20,clear_viewer = T)
writeImage(img_gray20, file_name = 'rdocs/img_gray20.png')

tr(D20)/tr(D) * 100 


U30 <- as.matrix(U[,1:30])
V30 <- as.matrix(V[,1:30])
D30 <- diag(imgg.svd$d[1:30])
img_gray30 <- U30 %*% D30 %*% t(V30)
imageShow(img_gray30,clear_viewer = T)
writeImage(img_gray30, file_name = 'rdocs/img_gray30.png')

tr(D30)/tr(D) * 100 


U40 <- as.matrix(U[,1:40])
V40 <- as.matrix(V[,1:40])
D40 <- diag(imgg.svd$d[1:40])
img_gray40 <- U40 %*% D40 %*% t(V40)
imageShow(img_gray40,clear_viewer = T)
writeImage(img_gray40, file_name = 'rdocs/img_gray40.png')

tr(D40)/tr(D) * 100 


U60 <- as.matrix(U[,1:60])
V60 <- as.matrix(V[,1:60])
D60 <- diag(imgg.svd$d[1:60])
img_gray60 <- U60 %*% D60 %*% t(V60)
imageShow(img_gray60,clear_viewer = T)
writeImage(img_gray60, file_name = 'rdocs/img_gray60.png')

tr(D60)/tr(D) * 100 


U99 <- as.matrix(U[,1:99])
V99 <- as.matrix(V[,1:99])
D99 <- diag(imgg.svd$d[1:99])
img_gray99 <- U99 %*% D99 %*% t(V99)
imageShow(img_gray99,clear_viewer = T)
writeImage(img_gray99, file_name = 'rdocs/img_gray99.png')

tr(D99)/tr(D) * 100 

rankMatrix(img_gray)[1]
rankMatrix(img_gray99)[1]

# --------------------------------------------------------------------------- #

# Exemplo abstrato ----

img <- readImage("rdocs/volpi.jpg")
dim(img)
imageShow(img,clear_viewer = T)
#writeImage(img, file_name = 'rdocs/img.png')


img_gray <- rgb_2gray(img) 
dim(img_gray)
imageShow(img_gray,clear_viewer = T)
writeImage(img_gray, file_name = 'rdocs/volpi_gray.png')

# Aplicando SVD na Imagem

imgg.svd <- svd(img_gray)
imgg.svd$d
D <- diag(imgg.svd$d)
dim(D)

U <- imgg.svd$u
V <- imgg.svd$v


png(file="rdocs/plot4.png")
plot(1:length(imgg.svd$d), imgg.svd$d)
abline(v=c(5,10,20,40))
dev.off()

png(file="rdocs/plot5.png")
plot(1:length(imgg.svd$d), imgg.svd$d,xlim = c(1,60))
abline(v=c(5,10,20,40))
dev.off()

png(file="rdocs/plot6.png")
plot(1:length(imgg.svd$d), imgg.svd$d,xlim = c(10,80),ylim = c(0,30))
abline(v=c(20,40,60))
dev.off()

### Aproximacoes

U5 <- as.matrix(U[,1:5])
V5 <- as.matrix(V[,1:5])
D5 <- diag(imgg.svd$d[1:5])
img_gray5 <- U5 %*% D5 %*% t(V5)
imageShow(img_gray5,clear_viewer = T)
writeImage(img_gray5, file_name = 'rdocs/volpi_gray5.png')

tr(D5)/tr(D) * 100 


U10 <- as.matrix(U[,1:10])
V10 <- as.matrix(V[,1:10])
D10 <- diag(imgg.svd$d[1:10])
img_gray10 <- U10 %*% D10 %*% t(V10)
imageShow(img_gray10,clear_viewer = T)
writeImage(img_gray10, file_name = 'rdocs/volpi_gray10.png')

tr(D10)/tr(D) * 100 


U20 <- as.matrix(U[,1:20])
V20 <- as.matrix(V[,1:20])
D20 <- diag(imgg.svd$d[1:20])
img_gray20 <- U20 %*% D20 %*% t(V20)
imageShow(img_gray20,clear_viewer = T)
writeImage(img_gray20, file_name = 'rdocs/volpi_gray20.png')

tr(D20)/tr(D) * 100 


U30 <- as.matrix(U[,1:30])
V30 <- as.matrix(V[,1:30])
D30 <- diag(imgg.svd$d[1:30])
img_gray30 <- U30 %*% D30 %*% t(V30)
imageShow(img_gray30,clear_viewer = T)
writeImage(img_gray30, file_name = 'rdocs/volpi_gray30.png')

tr(D30)/tr(D) * 100 


U40 <- as.matrix(U[,1:40])
V40 <- as.matrix(V[,1:40])
D40 <- diag(imgg.svd$d[1:40])
img_gray40 <- U40 %*% D40 %*% t(V40)
imageShow(img_gray40,clear_viewer = T)
writeImage(img_gray40, file_name = 'rdocs/volpi_gray40.png')

tr(D40)/tr(D) * 100 


U60 <- as.matrix(U[,1:60])
V60 <- as.matrix(V[,1:60])
D60 <- diag(imgg.svd$d[1:60])
img_gray60 <- U60 %*% D60 %*% t(V60)
imageShow(img_gray60,clear_viewer = T)
writeImage(img_gray60, file_name = 'rdocs/volpi_gray60.png')

tr(D60)/tr(D) * 100 


U99 <- as.matrix(U[,1:99])
V99 <- as.matrix(V[,1:99])
D99 <- diag(imgg.svd$d[1:99])
img_gray99 <- U99 %*% D99 %*% t(V99)
imageShow(img_gray99,clear_viewer = T)
writeImage(img_gray99, file_name = 'rdocs/volpi_gray99.png')

tr(D99)/tr(D) * 100 

rankMatrix(img_gray)[1]
rankMatrix(img_gray99)[1]
