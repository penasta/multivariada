pacman::p_load(effectsize,DescTools,tidyverse,MASS,klaR,knitr,cowplot,nlme,
               Rchoice,AICcmodavg,mdscore,questionr,mda,mvnTest,gclus,mclust)

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
X1 <- as.matrix(dados[,1:2])
X2 <- as.matrix(dados[,3:4])

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

# x_1,x_2 ----
dados <- dados[,-6]
gqda <- qda(X5~X1+X2, data = dados,prior =c(.5,.5))

(gqdap1 <- predict(gqda))
(gqctable1 <- table(dados$X5, gqdap1$class))
(diag(prop.table(gqctable1,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(gqctable1)))) # prop total de classf. correta 

partimat(X5~X1+X2, data=dados, method="qda", 
         plot.matrix = F, imageplot = T,prec=100)


# d) ----

9/sum(gqctable1) # APER x1,x2

# e) ----

# x_1,x_2 ----

gqda <- qda(X5~X1+X2, data = dados,prior =c(.05,.95))

(gqdap1 <- predict(gqda))
(gqctable1 <- table(dados$X5, gqdap1$class))
(diag(prop.table(gqctable1,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(gqctable1)))) # prop total de classf. correta 

partimat(X5~X1+X2, data=dados, method="qda", 
         plot.matrix = F, imageplot = T,prec=100)

12/sum(gqctable1) # APER x1,x2

# Analisando os APER, concluímos que as prioris iguais (p1=.5;p2=.5) tem um erro de classificação inferior se comparado as prioris desiguais (p1=0.05;p2=0.95).

# f) ----
xb1
xb2
al <- t(t(xb1-xb2)) %*% solve(cov_pooled(ativos,falidos))
m <- t(t(xb1-xb2)) %*% solve(cov_pooled(ativos,falidos)) %*% t(xb1+xb2)

pop1 <- falidos |>
  rowwise() |>
  mutate(M = al %*% c(X1, X2)) |>
  mutate(pop = ifelse(M > m,"p1","p2")) |>
  pull()
pop1 <- factor(pop1)

pop2 <- ativos |>
  rowwise() |>
  mutate(M = al %*% c(X1, X2)) |>
  mutate(pop = ifelse(M > m,"p1","p2")) |>
  pull()
pop2 <- factor(pop2)
summary(pop1)
summary(pop2)

# APER:
8/46

# AVALIAÇÃO: Como as matrizes S_1 e S_2 aparentam ser diferentes, esta técnica não é a mais adequada. Entretanto, tomando como base apenas a performance do APER, até que a classificação por discriminantes lineares não ficou ruim, com resultados próximos ao obtido pelos discriminantes quadráticos.

# g) ----

#b)
# x1,x3
falidos2 <- dados |>
  filter(X5 == 0) |>
  dplyr::select(X1,X3) |>
  summarise_all(mean)
falidos2 <- as.matrix(falidos2)
colnames(falidos2) <- NULL

ativos2 <- dados |>
  filter(X5==1) |>
  dplyr::select(X1,X3) |>
  summarise_all(mean)
ativos2 <- as.matrix(ativos2)
colnames(ativos2) <- NULL

xb12 <- falidos2
xb22 <- ativos2

falidos2 <- dados |>
  filter(X5 == 0) |>
  dplyr::select(X1,X3)

ativos2 <- dados |>
  filter(X5==1) |>
  dplyr::select(X1,X3)

S12 <- cov(falidos2)
S22 <- cov(ativos2)

#x1,x4

falidos3 <- dados |>
  filter(X5 == 0) |>
  dplyr::select(X1,X4) |>
  summarise_all(mean)
falidos3 <- as.matrix(falidos3)
colnames(falidos3) <- NULL

ativos3 <- dados |>
  filter(X5==1) |>
  dplyr::select(X1,X4) |>
  summarise_all(mean)
ativos3 <- as.matrix(ativos3)
colnames(ativos3) <- NULL

xb13 <- falidos3
xb23 <- ativos3

falidos3 <- dados |>
  filter(X5 == 0) |>
  dplyr::select(X1,X4)

ativos3 <- dados |>
  filter(X5==1) |>
  dplyr::select(X1,X4)

S13 <- cov(falidos3)
S23 <- cov(ativos3)

#c)
# x_1,x_3 ----

gqda <- qda(X5~X1+X3, data = dados,prior =c(.5,.5))

(gqdap2 <- predict(gqda))
(gqctable2 <- table(dados$X5, gqdap2$class))
(diag(prop.table(gqctable2,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(gqctable2)))) # prop total de classf. correta 

partimat(X5~X1+X3, data=dados, method="qda", 
         plot.matrix = F, imageplot = T,prec=100)

# x_1,x_4 ----

gqda <- qda(X5~X1+X4, data = dados,prior =c(.5,.5))

(gqdap3 <- predict(gqda))
(gqctable3 <- table(dados$X5, gqdap3$class))
(diag(prop.table(gqctable3,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(gqctable3)))) # prop total de classf. correta 

partimat(X5~X1+X4, data=dados, method="qda", 
         plot.matrix = F, imageplot = T,prec=100)

#d)
5/sum(gqctable2) # APER x1,x3
8/sum(gqctable3) # APER x1,x4

#e)
# x_1,x_3 ----

gqda <- qda(X5~X1+X3, data = dados,prior =c(.05,.95))

(gqdap2 <- predict(gqda))
(gqctable2 <- table(dados$X5, gqdap2$class))
(diag(prop.table(gqctable2,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(gqctable2)))) # prop total de classf. correta 

# x_1,x_4 ----

gqda <- qda(X5~X1+X4, data = dados,prior =c(.05,.95))

(gqdap3 <- predict(gqda))
(gqctable3 <- table(dados$X5, gqdap3$class))
(diag(prop.table(gqctable3,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(gqctable3)))) # prop total de classf. correta 

17/sum(gqctable2) # APER x1,x3
18/sum(gqctable3) # APER x1,x4

# O par x1,x2 apresenta o menor APER

# h) ----

#b)
# x1,x2,x3,x4
falidos4 <- dados |>
  filter(X5 == 0) |>
  dplyr::select(X1,X2,X3,X4) |>
  summarise_all(mean)
falidos4 <- as.matrix(falidos4)
colnames(falidos4) <- NULL

ativos4 <- dados |>
  filter(X5==1) |>
  dplyr::select(X1,X2,X3,X4) |>
  summarise_all(mean)
ativos4 <- as.matrix(ativos4)
colnames(ativos4) <- NULL

xb14 <- falidos4
xb24 <- ativos4

falidos4 <- dados |>
  filter(X5 == 0) |>
  dplyr::select(X1,X2,X3,X4)

ativos4 <- dados |>
  filter(X5==1) |>
  dplyr::select(X1,X2,X3,X4)

S14 <- cov(falidos4)
S24 <- cov(ativos4)


#c)
# x_1,x_2,x_3,x_4 ----

gqda4 <- qda(X5~X1+X2+X3+X4, data = dados,prior =c(.5,.5))

(gqdap4 <- predict(gqda4))
(gqctable4 <- table(dados$X5, gqdap4$class))
(diag(prop.table(gqctable4,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(gqctable4)))) # prop total de classf. correta 

partimat(X5~X1+X2+X3+X4, data=dados, method="qda", 
         plot.matrix = F, imageplot = T,prec=100)

#d)
3/sum(gqctable4) # APER x1,x2,x3,x4 c/ prioris iguais


#e)
# x_1,x_2,x_3,x_4 ----

gqda5 <- qda(X5~X1+X2+X3+X4, data = dados,prior =c(.05,.95))

(gqdap5 <- predict(gqda5))
(gqctable5 <- table(dados$X5, gqdap5$class))
(diag(prop.table(gqctable5,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(gqctable5)))) # prop total de classf. correta 

9/sum(gqctable5) # APER x1,x2,x3,x4 c/ prioris desiguais

# No caso da inclusão de todas as 4 variáveis, o classificador com prioris iguais produziu as melhores classificações (menor APER). Também neste caso, o classificador com prioris .05;.95 produziu um APER bem maior que o de prioris iguais.

# J&W 11.32 ----
rm(list = ls())
gc()
dados <- read_table("rdocs/dados/T11-8-Hemofilia.DAT.txt",col_names = FALSE)
dados$X1 <- factor(dados$X1)
#n_1 = 30; n_2 = 45

# a) ----
ggplot(dados, aes(x = X2, y = X3, color = X1)) +
  geom_point(size = 3) +
  labs(
    x = "",
    y = ""
  ) +
  scale_color_manual(values = c("#A11D21", "#1D21A1")) +
  theme_minimal()
shapiro.test(dados$X2)
shapiro.test(dados$X3)
AD.test(dados[,2:3], qqplot = TRUE)

# Através da análise visual, não é possível rejeitar a normalidade bivariada, visto que os pontos aparentam formar uma elipsoide. Foi realizado ainda testes de Shapiro-Wilk nas duas marginais, que também não rejeitaram a normalidade; univariada, neste caso. Foi ainda utilizado o teste de Anderson-Darling para normalidade multivariada do pacote 'mvnTest', que também não rejeitou a normalidade multivariada. Portanto, não temos evidências para rejeitar a hipótese de normalidade multivariada dos dados.

# b) ----

medias1 <- dados |>
  filter(X1 == 1) |>
  dplyr::select(X2,X3) |>
  summarise_all(mean)
medias1 <- as.matrix(medias1)
colnames(medias1) <- NULL

medias2 <- dados |>
  filter(X1==2) |>
  dplyr::select(X2,X3) |>
  summarise_all(mean)
medias2 <- as.matrix(medias2)
colnames(medias2) <- NULL

xb1 <- medias1
xb2 <- medias2

grupo1 <- dados |>
  filter(X1 == 1) |>
  dplyr::select(X2,X3)

grupo2 <- dados |>
  filter(X1==2) |>
  dplyr::select(X2,X3)

al <- t(t(medias1-medias2)) %*% solve(cov_pooled(grupo1,grupo2))

m <- .5*(al%*%t(medias1)+al%*%t(medias2))

pop1 <- grupo1 |>
  rowwise() |>
  mutate(M = al %*% c(X2, X3)) |>
  mutate(pop = ifelse(M > m,"p1","p2")) |>
  pull()
pop1 <- factor(pop1)

pop2 <- grupo2 |>
  rowwise() |>
  mutate(M = al %*% c(X2, X3)) |>
  mutate(pop = ifelse(M > m,"p1","p2")) |>
  pull()
pop2 <- factor(pop2)

#table(pop1)
#table(pop2)

# Então, a matriz de confusão será:
mc <- t(matrix(c(27,3,
               8,37),2,2, dimnames = list(c("pop1", "pop2"), c("pop1", "pop2"))))

11/sum(mc) # APER

# c) ----
df <- read_table("rdocs/dados/tabela12.32c.txt", 
                    col_names = FALSE)
colnames(df) <- c("X2","X3")
df$X1 <- NA

#full_join(dados,df)

pop3 <- df |>
  rowwise() |>
  mutate(M = al %*% c(X2, X3)) |>
  mutate(pop = ifelse(M > m,"p1","p2")) |>
  pull()
pop3 <- factor(pop3)
table(pop3)

# Todas as 10 novas observações foram classificadas como percentence à população \pi_1

# d) ----

LDA <- lda(X1~., data = dados,prior=c(.75,.25))

(LDAp1 <- predict(LDA))
(LDAtable1 <- table(dados$X1, LDAp1$class))
(diag(prop.table(LDAtable1,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(LDAtable1)))) # prop total de classf. correta 

partimat(X1~X2+X3, data=dados, method="lda", 
         plot.matrix = F, imageplot = T,prec=100)

pred <- LDA |>
  predict(df)

# No caso desta priori, o APER = 0.24, significativamente maior do que no caso anterior com prioris iguais. Em relação às novas observações, o modelo classificou também todas no grupo 1, o que era de se esperar visto que este já havia sido o resultado com prioris iguais; e neste modelo a priori da pop 1 é maior ainda.

# Ex. 91 ----

# Discriminante linear
rm(list = ls())
data(bank)
bank$Status <- factor(bank$Status)

LDA <- lda(Status~., data = bank,prior=c(.5,.5))
(LDAp1 <- predict(LDA))
(LDAtable1 <- table(bank$Status, LDAp1$class))
(diag(prop.table(LDAtable1,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(LDAtable1)))) # prop total de classf. correta 

partimat(Status ~ ., data = bank, method = "lda")

# Discriminante quadrático
QDA <- qda(Status~., data = bank,prior=c(.5,.5))
(QDAp1 <- predict(QDA))
(QDAtable1 <- table(bank$Status, QDAp1$class))
(diag(prop.table(QDAtable1,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(QDAtable1)))) # prop total de classf. correta 

partimat(Status ~ ., data = bank, method = "qda")

# Análise de discriminantes por mistura de normais (mclust)

Class <- factor(bank$Status, levels = 0:1,
                labels = c("Genuína", "Falsificada"))

X <- data.matrix(bank[,-1])

mod <- Mclust(X)
summary(mod$BIC)

plot(mclustBIC(X))

summary(mod)
table(Class, mod$classification)
adjustedRandIndex(Class, mod$classification)

18/200  #APER

# A mistura de normais não operou tão bem quanto os discriminantes lineares e quadráticos. Enquanto nesses dois, 199 das 200 notas foram classificadas corretamente, o algorítmo de mistura de normais encontrou m=3 como o número ideal de clusters (sendo que neste caso sabemos que há apenas dois: genuínas e falsificadas). Com isso, classificou corretamente 188 das 200 notas. Interessante notar que não houve classificação de notas falsas como notas genuínas ou vice-versa; e sim algumas notas desses dois grupos foram classificadas em outro cluster, que seria talvez um cluster de "confusão", ou seja, notas em que não estava claro o suficiente se eram genuínas ou classificadas.
