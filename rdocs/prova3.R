# Pacotes e seed ----
if (!require("pacman")) install.packages("pacman")
p_load(knitr,effectsize,DescTools,tidyverse,MASS,klaR,knitr,cowplot,nlme,
       Rchoice,AICcmodavg,mdscore,questionr,mda,mvnTest,gclus,mclust,caTools,
       aplpack,gridExtra,factoextra,cluster,biotools)
M <- 150167636

# 79 ----

S <- matrix(0, nrow = 5, ncol = 5)
S[lower.tri(S)] <- c(.63,.51,.57,.12,.32,.18,.16,.21,.15,.68)
S[upper.tri(S)] <- rev(c(.63,.51,.57,.12,.32,.18,.16,.21,.15,.68))
diag(S) <- 1

D <- 1 - S # convertendo a matriz de correlação em uma matriz de dissimilaridades D:
par(mfrow = c(1, 2))

clust <- hclust(as.dist(D), method = "single")
plot(clust, main = "Single Linkage")

clust2 <- hclust(as.dist(D), method = "complete")
plot(clust2, main = "Complete Linkage")

#clust3 <- hclust(as.dist(D), method = "average")
#plot(clust3, main = "Average Linkage")

# 81 ----

x1 <- c(5,-1,1,-3)
x2 <- c(3,1,-2,-2)
item <- c("A","B","C","D")
df <- data.frame(item,x1,x2)

centro1 <- df |>
  filter(item == c("A","C")) |>
  dplyr::select(!item) |>
  summarise_all(list(mean))

centro2 <- df |>
  filter(item == c("B","D")) |>
  dplyr::select(!item) |>
  summarise_all(list(mean))

centro <- as.matrix(rbind(centro1,centro2))
kmeans_result <- kmeans(df[,2:3], centers = centro, iter.max = M)


fviz_cluster(kmeans_result, data=df[,2:3],
             palette = c("#00AFBB","#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal())

# 83 ----
ponto <- 1:22
x <- c(1,2,2,2,3,7,12,13,13,14,14,15,7,6,7,8,6,7,8,6,7,8)
y <- c(9,10,9,8,9,14,9,10,8,10,8,9,7,3,3,3,2,2,2,1,1,1)

df <- data.frame(ponto,x,y)

# a) ----
plot(x=x,y=y)

# b) ----
# Euclidiana
D_euclidiana <- dist(df[,-1], method = "euclidean")
#D_euclidiana

# Manhattan
D_manhattan <- dist(df[,-1], method = "manhattan")
#D_manhattan

# Mahalanobis.
D_Mahalanobis <- D2.dist(df[,-1], cov(df[,-1]))
#D_Mahalanobis

# c) ----
par(mfrow = c(1, 2))
clust4 <- hclust(D_euclidiana, method = "single")
plot(clust4, main = "Single Linkage")

clust5 <- hclust(D_euclidiana, method = "average")
plot(clust5, main = "Average Linkage")

par(mfrow = c(1, 2))
clust6 <- hclust(D_manhattan, method = "single")
plot(clust6, main = "Single Linkage")

clust7 <- hclust(D_manhattan, method = "average")
plot(clust7, main = "Average Linkage")

par(mfrow = c(1, 2))
clust8 <- hclust(D_Mahalanobis, method = "single")
plot(clust8, main = "Single Linkage")

clust9 <- hclust(D_Mahalanobis, method = "average")
plot(clust9, main = "Average Linkage")

#d) ----
df <- data.frame(ponto,x,y)
df <- scale(df[,-1])
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "wss")+
  geom_vline(xintercept = 3, linetype = 2)
set.seed(M)
km <- kmeans(df, 3, iter.max = M)
aggregate(df, by=list(cluster=km$cluster), mean)

df <- data.frame(ponto,x,y)
df <- cbind(df, cluster=km$cluster)
#km$centers

fviz_cluster(km, data=df,
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal())
set.seed(M)
km2 <- kmeans(df, 5, iter.max = M)
aggregate(df, by=list(cluster=km2$cluster), mean)

df <- data.frame(ponto,x,y)
df <- cbind(df, cluster=km2$cluster)
#km$centers

fviz_cluster(km2, data=df,
             palette = c("#2E9FDF","#00AFBB", "#E7B800", "#FC4E07","#a11d21"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal())

# 84 ----
data(bank)
bank$Status <- factor(bank$Status)

mu <- bank %>%
  group_by(Status) %>%
  summarise_all(list(mean)) %>%
  dplyr::select(!Status)

S0 <- bank %>%
  filter(Status == 0) %>%
  dplyr::select(!Status) %>%
  cov(.)

S1 <- bank %>%
  filter(Status == 1) %>%
  dplyr::select(!Status) %>%
  cov(.)
# a) ----
faces(mu)

# b) ----
X <- bank[,-1]
X <- scale(X)
set.seed(M)
km.res=kmeans(X, 2, iter.max = M)
#print(km.res)

#aggregate(X, by=list(cluster=km.res$cluster), mean)
X<-cbind(X, cluster=km.res$cluster)

fviz_cluster(km.res, data=X,
             palette = c("#00AFBB","#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal())

#km.res$withinss / km.res$totss 
#km.res$betweenss / km.res$totss  

#km.res$totss - km.res$tot.withinss
#km.res$betweenss

n <- nrow(X)
r2g2 <- km.res$betweenss / km.res$totss
g <- 2

# medidas de avaliação do modelo:
pseudoF2 <- (n-g)/(g-1) * r2g2/(1-r2g2) # maior Pseudo-F => melhor agrupamento
rand_kmeans <- adjustedRandIndex(km.res$cluster,bank$Status)

X <- bank[,-1]
X <- scale(X)

dista <- dist(X, method="euclidean")
#as.matrix(dista)[1:3,1:3]
dista.hc <- hclust(d=dista, method="ward.D")
fviz_dend(dista.hc, cex=0.5)


clarax.2 <- clara(X, 2, samples = 20, metric = "euclidean")

#clarax.2$clusinfo

# Cluster de cada valor:
#clarax.2$clustering

par(mfrow=c(1, 1))
plot(X, col = clarax.2$clustering)
points(clarax.2$medoids, col = 1:2, pch = 19, cex=2)

kable(table(clarax.2$clustering))


rand_clara <- adjustedRandIndex(clarax.2$clustering,bank$Status)


pamx.2 <- pam(X, 2)

# Informações:
#summary(pamx.2)
#pamx.2$clusinfo

# Agrupamento:
#pamx.2$clustering

par(mfrow=c(1, 1))
plot(X, col = pamx.2$clustering)
points(pamx.2$medoids, col = 1:2, pch = 19, cex=2)

# Validação:
rand_pam <- adjustedRandIndex(pamx.2$clustering,bank$Status)


agn1 <- agnes(X, metric = "manhattan", stand = TRUE)

#agn1
par(mfrow=c(1, 2))
plot(agn1)
# pares de dissimilaridades (distancias)
agn2 <- agnes(daisy(X), diss = TRUE, 
              method = "complete")

par(mfrow=c(1, 2))
plot(agn2)
# A AGNES não performou tão bem para este conjunto de dados.

# c) ----
X <- bank[,-1]
BIC <- mclustBIC(X)
plot(BIC)
summary(BIC)
n <- length(bank[,1])
bank.mclust <- densityMclust(bank[,-1], model="VVE", G = 3)

# simulando amostra da densidade
sim.results <- simVVE(bank.mclust$parameters, n, seed = M)
ysim <- sim.results[,c(2:4)]
gsim <- sim.results[,"group"]
ysim1 <- ysim[gsim==1, ]
ysim2 <- ysim[gsim==2, ]
ysim3 <- ysim[gsim==3, ]
kable(table(gsim))

rand_mclustVVE3 <- adjustedRandIndex(sim.results[,1],bank$Status)
n <- length(bank[,1])
bank.mclust <- densityMclust(bank[,-1], model="VVV", G = 2)

# simulando amostra da densidade
sim.results <- simVVV(bank.mclust$parameters, n, seed = M)
ysim <- sim.results[,c(2,3)]
gsim <- sim.results[,"group"]
ysim1 <- ysim[gsim==1, ]
ysim2 <- ysim[gsim==2, ]
kable(table(gsim))

rand_mclustVVV2 <- adjustedRandIndex(data.frame(sim.results)$group,bank$Status)

# d) ----
APER_mclustVVE3 <- 1-(21/200)
APER_mclustVVV2 <- 1-(16/200)
kable(data.frame(rand_kmeans,rand_clara,rand_pam,APER_mclustVVE3,APER_mclustVVV2,rand_mclustVVE3,rand_mclustVVV2))

# 89 ----
dados <- read_table("rdocs/dados/T11-4-BankruptcyData.DAT.txt", 
                    col_names = FALSE, col_types = cols(X6 = col_skip()))
dados$X5 <- factor(dados$X5)

# a) ----
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
dados <- dados[,-6]
gqda <- qda(X5~X1+X2, data = dados,prior =c(.5,.5))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$X5, gqdap1$class)

partimat(X5~X1+X2, data=dados, method="qda", 
         plot.matrix = F, imageplot = T,prec=100)

# Com validação cruzada
gqdaVC <- qda(X5~X1+X2, data = dados,prior =c(.5,.5),CV=T)

# d) ----
# Matrizes de confusão:
M <- table(dados$X5, gqdap1$class) 
MCV <- table(dados$X5, gqdaVC$class) 

# APER e \hat{E}APR:
APER <- (sum(M)-sum(diag(M)))/sum(M) # APER x_1,x_2
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) # \hat{E} APR x_1,x_2

# e) ----
#dados <- dados[,-6]
gqda <- qda(X5~X1+X2, data = dados,prior =c(.05,.95))

gqdap1 <- predict(gqda)
gqctable1 <- table(dados$X5, gqdap1$class)

partimat(X5~X1+X2, data=dados, method="qda", 
         plot.matrix = F, imageplot = T,prec=100)

# Com validação cruzada
gqdaVC <- qda(X5~X1+X2, data = dados,prior =c(.05,.95),CV=T)

# Matrizes de confusão:
M <- table(dados$X5, gqdap1$class) 
MCV <- table(dados$X5, gqdaVC$class) 

# APER e \hat{E}APR:
APER <- (sum(M)-sum(diag(M)))/sum(M) # APER x_1,x_2
E_APR <- (sum(MCV)-sum(diag(MCV)))/sum(MCV) # \hat{E} APR x_1,x_2

# f) ----
#xb1
#xb2
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
#summary(pop1)
#summary(pop2)

# APER:
APER <- 8/46

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
# x_1,x_3

gqda <- qda(X5~X1+X3, data = dados,prior =c(.5,.5))

gqdap2 <- predict(gqda)
gqctable2 <- table(dados$X5, gqdap2$class)
prop2 <- diag(prop.table(gqctable2,1)) # prop de classif. correta no grupo
propt2 <- sum(diag(prop.table(gqctable2))) # prop total de classf. correta 

partimat(X5~X1+X3, data=dados, method="qda", 
         plot.matrix = F, imageplot = T,prec=100)

# Com validação cruzada
gqdaVC2 <- qda(X5~X1+X3, data = dados,prior =c(.5,.5),CV=T)

# Matrizes de confusão:
M2 <- table(dados$X5, gqdap2$class) 
MCV2 <- table(dados$X5, gqdaVC2$class) 

# APER e \hat{E}APR:
APER2 <- (sum(M2)-sum(diag(M2)))/sum(M2) # APER x_1,x_2
E_APR2 <- (sum(MCV2)-sum(diag(MCV2)))/sum(MCV2) # \hat{E} APR x_1,x_2

# x_1,x_4

gqda <- qda(X5~X1+X4, data = dados,prior =c(.5,.5))

gqdap3 <- predict(gqda)
gqctable3 <- table(dados$X5, gqdap3$class)
prop3 <- (diag(prop.table(gqctable3,1))) # prop de classif. correta no grupo
propt3 <- (sum(diag(prop.table(gqctable3)))) # prop total de classf. correta 

partimat(X5~X1+X4, data=dados, method="qda", 
         plot.matrix = F, imageplot = T,prec=100)

# Com validação cruzada
gqdaVC3 <- qda(X5~X1+X4, data = dados,prior =c(.5,.5),CV=T)

# Matrizes de confusão:
M3 <- table(dados$X5, gqdap3$class) 
MCV3 <- table(dados$X5, gqdaVC3$class) 

# APER e \hat{E}APR:
APER3 <- (sum(M3)-sum(diag(M3)))/sum(M3) # APER x_1,x_2
E_APR3 <- (sum(MCV3)-sum(diag(MCV3)))/sum(MCV3) # \hat{E} APR x_1,x_2

#e)
# x_1,x_3

gqda <- qda(X5~X1+X3, data = dados,prior =c(.05,.95))

gqdap4 <- predict(gqda)
gqctable4 <- table(dados$X5, gqdap4$class)
prop4 <- (diag(prop.table(gqctable4,1))) # prop de classif. correta no grupo
propt4 <- (sum(diag(prop.table(gqctable4)))) # prop total de classf. correta 

# Com validação cruzada
gqdaVC4 <- qda(X5~X1+X3, data = dados,prior =c(.05,.95),CV=T)

# Matrizes de confusão:
M4 <- table(dados$X5, gqdap4$class) 
MCV4 <- table(dados$X5, gqdaVC4$class) 

# APER e \hat{E}APR:
APER4 <- (sum(M4)-sum(diag(M4)))/sum(M4) # APER x_1,x_2
E_APR4 <- (sum(MCV4)-sum(diag(MCV4)))/sum(MCV4) # \hat{E} APR x_1,x_2

# x_1,x_4

gqda <- qda(X5~X1+X4, data = dados,prior =c(.05,.95))

gqdap5 <- predict(gqda)
gqctable5 <- table(dados$X5, gqdap5$class)
prop5 <- (diag(prop.table(gqctable5,1))) # prop de classif. correta no grupo
propt5 <- (sum(diag(prop.table(gqctable5)))) # prop total de classf. correta 

# Com validação cruzada
gqdaVC5 <- qda(X5~X1+X4, data = dados,prior =c(.05,.95),CV=T)

# Matrizes de confusão:
M5 <- table(dados$X5, gqdap5$class) 
MCV5 <- table(dados$X5, gqdaVC5$class) 

# APER e \hat{E}APR:
APER5 <- (sum(M5)-sum(diag(M5)))/sum(M5) # APER x_1,x_2
E_APR5 <- (sum(MCV5)-sum(diag(MCV5)))/sum(MCV5) # \hat{E} APR x_1,x_2

# h) ----
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

# Modelo c priori igual

gqda6 <- qda(X5~X1+X2+X3+X4, data = dados,prior =c(.5,.5))

gqdap6 <- predict(gqda6)
gqctable6 <- table(dados$X5, gqdap6$class)
prop6 <- diag(prop.table(gqctable6,1)) # prop de classif. correta no grupo
propt6 <- (sum(diag(prop.table(gqctable6)))) # prop total de classf. correta 

partimat(X5~X1+X2+X3+X4, data=dados, method="qda", 
         plot.matrix = F, imageplot = T,prec=100)

# Com validação cruzada
gqdaVC6 <- qda(X5~X1+X2+X3+X4, data = dados,prior =c(.5,.5),CV=T)

# Matrizes de confusão:
M6 <- table(dados$X5, gqdap6$class) 
MCV6 <- table(dados$X5, gqdaVC6$class) 

# APER e \hat{E}APR:
APER6 <- (sum(M6)-sum(diag(M6)))/sum(M6) # APER x_1,x_2
E_APR6 <- (sum(MCV6)-sum(diag(MCV6)))/sum(MCV6) # \hat{E} APR x_1,x_2


# Alterando a priori
gqda7 <- qda(X5~X1+X2+X3+X4, data = dados,prior =c(.05,.95))

gqdap7 <- predict(gqda7)
gqctable7 <- table(dados$X5, gqdap7$class)
prop7 <- (diag(prop.table(gqctable7,1))) # prop de classif. correta no grupo
propt7 <- (sum(diag(prop.table(gqctable7)))) # prop total de classf. correta 

# Com validação cruzada
gqdaVC7 <- qda(X5~X1+X2+X3+X4, data = dados,prior =c(.05,.95),CV=T)

# Matrizes de confusão:
M7 <- table(dados$X5, gqdap7$class) 
MCV7 <- table(dados$X5, gqdaVC7$class) 

# APER e \hat{E}APR:
APER7 <- (sum(M7)-sum(diag(M7)))/sum(M7) # APER x_1,x_2
E_APR7 <- (sum(MCV7)-sum(diag(MCV7)))/sum(MCV7) # \hat{E} APR x_1,x_2

# 90 ----
dados <- read_table("rdocs/dados/T11-8-Hemofilia.DAT.txt",col_names = FALSE)
dados$X1 <- factor(dados$X1)
#n_1 = 30; n_2 = 45
df <- read_table("dados/tabela12.32c.txt", 
                 col_names = FALSE)
colnames(df) <- c("X2","X3")
df$X1 <- NA

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

#11/sum(mc) # APER

# Fazendo por funções prontas:

lda <- lda(X1~X2+X3, data = dados,prior =c(.5,.5))

gldap <- predict(lda)
glctable <- table(dados$X1, gldap$class)
prop <- (diag(prop.table(glctable,1))) # prop de classif. correta no grupo
propt <- (sum(diag(prop.table(glctable)))) # prop total de classf. correta 

# Validação hold-out
#table(dados$X1)
set.seed(M)
split <- sample.split(dados$X1, SplitRatio = 0.3) 
train <- subset(dados, split==T)
test <- subset(dados, split==F)

lda1 <- lda(X1~X2+X3, data = train,prior =c(.5,.5))

PT <- predict(lda1, newdata = test, type = "response")
glctable <- table(test$X1, PT$x >= 0.5)

# c) ----
pop3 <- df |>
  rowwise() |>
  mutate(M = al %*% c(X2, X3)) |>
  mutate(pop = ifelse(M > m,"p1","p2")) |>
  pull()
pop3 <- factor(pop3)
table(pop3)

# d) ----
LDA <- lda(X1~., data = dados,prior=c(.75,.25))

LDAp1 <- predict(LDA)
LDAtable1 <- table(dados$X1, LDAp1$class)
prop <- (diag(prop.table(LDAtable1,1))) # prop de classif. correta no grupo
propt <- (sum(diag(prop.table(LDAtable1)))) # prop total de classf. correta 

partimat(X1~X2+X3, data=dados, method="lda", 
         plot.matrix = F, imageplot = T,prec=100)

# Validação hold-out
#table(dados$X1)
set.seed(M)
split <- sample.split(dados$X1, SplitRatio = 0.3) 
train <- subset(dados, split==T)
test <- subset(dados, split==F)

lda1 <- lda(X1~X2+X3, data = train,prior =c(.75,.25))

PT <- predict(lda1, newdata = test, type = "response")
glctable <- table(test$X1, PT$x >= 0.5)


pred <- LDA |>
  predict(df)

# 91 ----
# Discriminante linear
data(bank)
bank$Status <- factor(bank$Status)

LDA <- lda(Status~., data = bank,prior=c(.5,.5))
LDAp1 <- predict(LDA)
LDAtable1 <- table(bank$Status, LDAp1$class)
prop1 <- (diag(prop.table(LDAtable1,1))) # prop de classif. correta no grupo
propt1 <- (sum(diag(prop.table(LDAtable1)))) # prop total de classf. correta 

# Matrizes de confusão:
M <- table(bank$Status, LDAp1$class) 

# APER:
APER <- (sum(M)-sum(diag(M)))/sum(M)

#partimat(Status ~ ., data = bank, method = "lda")

# Discriminante quadrático
QDA <- qda(Status~., data = bank,prior=c(.5,.5))
QDAp1 <- predict(QDA)
QDAtable1 <- table(bank$Status, QDAp1$class)
prop2 <- (diag(prop.table(QDAtable1,1))) # prop de classif. correta no grupo
propt2 <- (sum(diag(prop.table(QDAtable1)))) # prop total de classf. correta 

# Matrizes de confusão:
M1 <- table(bank$Status, QDAp1$class) 

# APER:
APER1 <- (sum(M1)-sum(diag(M1)))/sum(M1)

#partimat(Status ~ ., data = bank, method = "qda")

# Análise de discriminantes por mistura de normais (mclust)

Class <- factor(bank$Status, levels = 0:1,
                labels = c("Genuína", "Falsificada"))

X <- data.matrix(bank[,-1])

mod <- Mclust(X)
summary(mod$BIC)

plot(mclustBIC(X))

summary(mod)
table(Class, mod$classification)    # por algum motivo este não renderiza
RAND <- adjustedRandIndex(Class, mod$classification)

# Matrizes de confusão:
M2 <- table(bank$Status, mod$class) 

# APER:
APER2 <- (2+16)/(2+98+16+84)

