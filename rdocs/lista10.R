#pacman::p_load(effectsize,readr,DescTools,tidyverse)

# 11.1 J&W ----
X1 <- t(matrix(c(3,7,
               2,4,
               4,7),2,3))
#X1
X2 <- t(matrix(c(6,9,
                 5,7,
                 4,8),2,3))
#X2

# a) Calcular a função discriminante linear ----
#t(t(colMeans(X1)))
#t(t(colMeans(X2)))

pacman::p_load(effectsize)

#cov_pooled(X1,X2)

al <- t(t(t(colMeans(X1)))-t(t(colMeans(X2)))) %*% solve(cov_pooled(X1,X2))
# /\ * x = [-2  0]x = -2x_1

# b) ----
.5*(al%*%colMeans(X1)+al%*%colMeans(X2)) # m^ = -8
xl0 <- t(c(2,7))
al%*%t(xl0) # y^ = -4. Como -4 > -8; então x'0 deve ser alocado na pop pi_1

# 11.2 J&W ----

# a) ----
pacman::p_load(readr)
dados <- read_table("rdocs/dados/tabela11.1.txt", 
                         col_types = cols(X5 = col_skip()))

lcf <- function(x){
  X1 <- as.matrix(dados[,1:2])
  X2 <- as.matrix(dados[,3:4])
  colnames(X1) <- NULL
  colnames(X2) <- NULL
  al <- t(t(t(colMeans(X1)))-t(t(colMeans(X2)))) %*% solve(cov_pooled(X1,X2))
  limite <- .5*(al%*%colMeans(X1)+al%*%colMeans(X2))
  fronteira <- al%*%t(x)
  if(fronteira<=limite){
    return("2")
  }else{
    return("1")
  }
}
lcf(xl0)
lcf(xl0*100)

lcf(t(X1[2,]))

# b) ----

resultados1 <- character(12)
for (i in 1:12) {
  x <- t(X1[i, 1:2])
  resultado <- lcf(x)
  resultados1[i] <- resultado
} # 11 sucessos 1 fracasso

resultados2 <- character(12)
for (i in 1:12) {
  x <- t(X2[i, 1:2])
  resultado <- lcf(x)
  resultados2[i] <- resultado
} # 10 sucessos 2 fracassos

# A matriz de confusão será da forma:
t(matrix(c(11,1,
           2,10), nrow = 2, ncol = 2, dimnames = list(c("pop1", "pop2"), c("pop1", "pop2"))))

# Desta forma, podemos como em uma tabela de contingência ver diretamente quais valores foram corretamente classificados, e quais não foram. Isso é essencial para conjuntos grandes, onde começa a ficar difícil contar pontinhos no gráfico..

# c) ----
(APER <- 3/24)
# Portanto, a taxa de erro aparente é de 0,125. Ou seja, estamos errando 12,5% das classificações com este algoritmo.

# d) ----
# Pressupostos: Pi1 e Pi2 seguem distribuição normal multivariada N_2 com matriz de covariâncias iguais (Sigma1=Sigma2)

# J&W 11.4 ----

# a) ----
# b) ----

# J&W 11.10 ----
pacman::p_load(DescTools)
xb1 <- t(t(c(-1,-1)))
xb2 <- t(t(c(2,1)))
n1 <- 11
n2 <- 12
Sp <- t(matrix(c(7.3,-1.1,
                 -1.1,4.8),2,2))

# a) ----
# T^2 = 
t(xb1-xb2) %*% solve((1/n1+1/n2)*Sp) %*% (xb1-xb2) # = 14,52171

# H0): mu1 = mu2
p <- 2
gl1 <- p
gl2 <- n1+n2-p-1
a <- ((n1+n2-2)*p)/(n1+n2-p-1)
a*qf(.9,gl1,gl2) # = 5.437434
# Como T^2 >= 5.437434; rejeitamos a hipótese nula de igualdade de médias para um nivel de significância alpha=0,1.

# b) ----
al <- t(xb1-xb2) %*% solve(Sp) # Logo, \hat{y}_0 = -0.4906887x_1 - 0.5291162x_2 

m <- .5*(al%*%xb1+al%*%xb2)
x0l <- c(0,1)

al %*%x0l - m # > m . Logo, devemos alocar x_0 na população \pi_2

# J&W 11.24 ----
rm(list = ls())
dados <- read_table("rdocs/dados/T11-4-BankruptcyData.DAT.txt", 
                    col_names = FALSE, col_types = cols(X6 = col_skip()))
dados$X5 <- factor(dados$X5)

# a) ----

#plot(dados$X1~dados$X2)

dados <- dados |>
  mutate(grupo = ifelse(X5 == 0,"Faliu","Não faliu"))

ggplot(dados, aes(x = X1, y = X2, color = grupo)) +
  geom_point(size = 3) +
  labs(
    x = "(fluxo de caixa)/(dívida total)",
    y = "(lucro líquido)/(ativos totais)"
  ) +
  scale_color_manual(values = c("#A11D21", "#1D21A1")) +
  theme_minimal()

ggplot(dados, aes(x = X1, y = X3, color = grupo)) +
  geom_point(size = 3) +
  labs(
    x = "(fluxo de caixa)/(dívida total)",
    y = "(ativo circulante)/(passivo circulante)") +
  scale_color_manual(values = c("#A11D21", "#1D21A1")) +
  theme_minimal()

ggplot(dados, aes(x = X1, y = X4, color = grupo)) +
  geom_point(size = 3) +
  labs(
    x = "(fluxo de caixa)/(dívida total)",
    y = "(ativos atuais)/(vendas líquidas)") +
  scale_color_manual(values = c("#A11D21", "#1D21A1")) +
  theme_minimal()

# Em todos os gráficos, os pontos lembram a forma de elipsoides. Portanto, graficamente, não é possível rejeitar a normalidade bivariada dos dados.

# b) ----
falidos <- dados |>
  filter(X5 == 0) |>
  dplyr::select(X1,X2) |>
  summarise_all(mean)
falidos <- as.matrix(falidos)
colnames(falidos) <- NULL

ativos <- dados |>
  filter(X5==1) |>
  dplyr::select(X1,X2) |>
  summarise_all(mean)
ativos <- as.matrix(ativos)
colnames(ativos) <- NULL

xb1 <- falidos
xb2 <- ativos

falidos <- dados |>
  filter(X5 == 0) |>
  dplyr::select(X1,X2)

ativos <- dados |>
  filter(X5==1) |>
  dplyr::select(X1,X2)

S1 <- cov(falidos)
S2 <- cov(ativos)

# c) ----



# d) ----

# e) ----

# f) ----

# g) ----

# h) ----

# J&W 11.32 ----

# a) ----

# b) ----

# c) ----

# d) ----

# Ex. 91 ----