# ---------------------------------------------------------------------------- #
pacman::p_load(gclus,tidyverse,aplpack,gridExtra,mclust,factoextra,cluster,
               biotools)
M <- 150167636
# ---------------------------------------------------------------------------- #
# ex. 12.5 J&W ---- 
# a)

D <- matrix(0, nrow = 4, ncol = 4)
D[lower.tri(D)] <- c(1,11,2,5,3,4)
D[upper.tri(D)] <- rev(c(1,11,2,5,3,4))

#par(mfrow = c(1, 3))
clust <- hclust(as.dist(D), method = "single")
plot(clust)

# b)

clust2 <- hclust(as.dist(D), method = "complete")
plot(clust2)

# c)

clust3 <- hclust(as.dist(D), method = "average")
plot(clust3)

# ---------------------------------------------------------------------------- #

# ex. 12.6 J&W ---- 

D <- matrix(0, nrow = 5, ncol = 5)
D[lower.tri(D)] <- c(4,6,9,1,7,10,6,3,5,8)
D[upper.tri(D)] <- rev(c(4,6,9,1,7,10,6,3,5,8))

par(mfrow = c(1, 3))

clust <- hclust(as.dist(D), method = "single")
plot(clust)

clust2 <- hclust(as.dist(D), method = "complete")
plot(clust2)

clust3 <- hclust(as.dist(D), method = "average")
plot(clust3)

# ---------------------------------------------------------------------------- #

# ex. 12.7 J&W ---- 

S <- matrix(0, nrow = 5, ncol = 5)
S[lower.tri(S)] <- c(.63,.51,.57,.12,.32,.18,.16,.21,.15,.68)
S[upper.tri(S)] <- rev(c(.63,.51,.57,.12,.32,.18,.16,.21,.15,.68))
diag(S) <- 1

D <- 1 - S # convertendo a matriz de correlação em uma matriz de dissimilaridades D:
par(mfrow = c(1, 3))

clust <- hclust(as.dist(D), method = "single")
plot(clust, main = "Single Linkage")

clust2 <- hclust(as.dist(D), method = "complete")
plot(clust2, main = "Complete Linkage")

clust3 <- hclust(as.dist(D), method = "average")
plot(clust3, main = "Average Linkage")

# ---------------------------------------------------------------------------- #

# ex. 12.11 J&W ---- 

x1 <- c(5,1,-1,3)
x2 <- c(4,-2,1,1)
item <- c("A","B","C","D")
df <- data.frame(item,x1,x2)

linhas_iniciais <- c(1, 4)
kc <- df[linhas_iniciais, 2:3]

kmeans_result <- kmeans(df[,2:3], centers = kc, iter.max = M)

df$cluster <- kmeans_result$cluster

# Parece estar errado isso...

# ex. 12.12 J&W ---- 

# ex. 12.13 J&W ----

# ---------------------------------------------------------------------------- #

# ex 83 ----
ponto <- 1:22
x <- c(1,2,2,2,3,7,12,13,13,14,14,15,7,6,7,8,6,7,8,6,7,8)
y <- c(9,10,9,8,9,14,9,10,8,10,8,9,7,3,3,3,2,2,2,1,1,1)

df <- data.frame(ponto,x,y)

# a) Plote o gráfico de dispersão para o conjunto de pontos. Quantos e quais grupos você indicaria através do gráfico. ----
plot(x=x,y=y)
# parecem haver 3~5 grupos: sendo 3 grupos sólidos agrupados, e 2 outliers dispersos que provavelmente otimizariam formando um grupo para cada.

# b) Construa a matriz de distâncias D, calculando as distâncias Euclidiana, de Manhattan e Mahalanobis. Compare os resultados. Explique a razão de possíveis diferenças nos resultados para as três distâncias. ----

# Euclidiana
D_euclidiana <- dist(df[,-1], method = "euclidean")
#D_euclidiana

# Manhattan
D_manhattan <- dist(df[,-1], method = "manhattan")
#D_manhattan

# Mahalanobis.
D_Mahalanobis <- D2.dist(df[,-1], cov(df[,-1]))
#D_Mahalanobis

# Apesar dos valores serem bem diferentes, isso se dá mais pelo método de cálculo de distância de cada uma das técnicas.
# A distância euclidiana trabalha basicamente com a "distância bruta" entre um ponto e outro, literalmente medindo a distância linear.
# A distância manhattan trabalha com distância absoluta entre as coordenadas dos pontos.
# A distância de mahalanobis busca centralizar os dados, calculando as distâncias levando em consideração a correlação entre as dimensões.

# Portanto, apesar de improvável, é possível que mesmo com valores observados absolutamente distoantes, agrupar as variáveis
# segundo as três distâncias trabalhadas e em todos os casos, retornar os exatos mesmos clusters pros dados.

# c) Utilize os algoritmos de ligação simples e média, e obtenha os respectivos dendogramas. Em cada caso indique sua escolha para o número de grupos e liste os elementos de cada grupo. Os dendogramas obtidos são únicos? Justifique. ---- 

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

# Em todos os dendogramas, foi confirmada a suspeita levantada no item (a); em que haviam 3 grupos aglomerativos bem definidos, e mais 2 grupos formados cada um por apenas um outlier.
# cada dendograma teve seu formato específico, mas todos foram eficientes em agrupar os dados pelos seus similares.

# d) Descreva e aplique o algoritmo de k-means para agrupar estes dados. Apresente os resultados e discuta. ----

df <- data.frame(ponto,x,y)
df <- scale(df[,-1])

# Determinando o número ideal de clusters
fviz_nbclust(df, kmeans, method = "wss")

# Pelo método elbow, o número ideal são 3 clusters...
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


# Aqui, notamos que o k-means foi relativamente eficiente em classificar os dois outliers em um dos clusters, sem muita perda de generalização.
# Se quisermos forçar a mão e testar a aglomeração k-means com 5 grupos, este será o resultado:

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

# E o resultado não ficou nada legal...


# ---------------------------------------------------------------------------- #

# ex 84 ---- 

data(bank)

mu <- bank %>%
  group_by(Status) %>%
  summarise_all(list(mean))

S0 <- bank %>%
  filter(Status == 0) %>%
  select(!Status) %>%
  cov(.)

S1 <- bank %>%
  filter(Status == 1) %>%
  select(!Status) %>%
  cov(.)

# a)
faces(mu)
# Discutir..

# b) ----

# k-means: ----
X <- bank[,-1]
X <- scale(X)
set.seed(M)
km.res=kmeans(X, 2, iter.max = M)
#print(km.res)

aggregate(X, by=list(cluster=km.res$cluster), mean)
X<-cbind(X, cluster=km.res$cluster)

fviz_cluster(km.res, data=X,
             palette = c("#00AFBB","#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal())

km.res$withinss / km.res$totss 
km.res$betweenss / km.res$totss  

km.res$totss - km.res$tot.withinss
km.res$betweenss

n <- nrow(X)
(r2g2 <- km.res$betweenss / km.res$totss)  

g <- 2
(pseudoF2 <- (n-g)/(g-1) * r2g2/(1-r2g2)) # maior Pseudo-F => melhor agrupamento

rand_kmeans <- adjustedRandIndex(km.res$cluster,bank$Status)



# Aglomerativa: ----

X <- bank[,-1]
X <- scale(X)

dista <- dist(X, method="euclidean")
as.matrix(dista)[1:3,1:3]
dista.hc <- hclust(d=dista, method="ward.D")
fviz_dend(dista.hc, cex=0.5)
# notamos que pouquíssimos valores foram classificados errados no dendograma...

# ALGORITMOS NAO-HIERARQUICOS ----

# CLARA ----

clarax.2 <- clara(X, 2, samples = 20, metric = "euclidean")
clarax.2$clusinfo
clarax.2$clustering

par(mfrow=c(1, 1))
plot(X, col = clarax.2$clustering)
points(clarax.2$medoids, col = 1:2, pch = 19, cex=2)

table(clarax.2$clustering)
# Notamos que a Clara agrupou apenas 3 valores errados, apontando 3 notas genuínas como falsificadas. Não apontou nenhuma falsificada como genuína.

rand_clara <- adjustedRandIndex(clarax.2$clustering,bank$Status)



# PAM ----
pamx.2 <- pam(X, 2)
summary(pamx.2)
pamx.2$clusinfo
pamx.2$clustering

par(mfrow=c(1, 1))
plot(X, col = pamx.2$clustering)
points(pamx.2$medoids, col = 1:2, pch = 19, cex=2)
# Notamos que a PAM também agrupou apenas 3 valores errados, também apontando 3 notas genuínas como falsificadas. Também não apontou nenhuma falsificada como genuína.

rand_pam <- adjustedRandIndex(pamx.2$clustering,bank$Status)



# AGNES ----
agn1 <- agnes(X, metric = "manhattan", stand = TRUE)
agn1
plot(agn1)
# pares de dissimilaridades (distancias)
agn2 <- agnes(daisy(X), diss = TRUE, 
              method = "complete")
plot(agn2)
# A AGNES não performou tão bem para este conjunto de dados.

# c) ----

# MCLUST ----

X <- bank[,-1]
BIC <- mclustBIC(X)
plot(BIC)
summary(BIC)

# Para o método mclust, está indicando que o ideal seriam 3 ou 4 agrupamentos, com o modelo VEE
# como sabemos que são apenas 2 grupos, temos que este método provavelmente não irá funcionar bem.

n <- length(bank[,1])
bank.mclust <- densityMclust(bank[,-1], model="VEE", G = 2)

# simulando amostra da densidade
sim.results <- simVVV(bank.mclust$parameters, n, seed = M)
ysim <- sim.results[,c(2,3)]
gsim <- sim.results[,"group"]
ysim1 <- ysim[gsim==1, ]
ysim2 <- ysim[gsim==2, ]

# De fato, este foi o modelo que mais errou.

rand_mclust <- adjustedRandIndex((sim.results[,1]-1),bank$Status) # Parece errado isso...

# d) ----

data.frame(rand_kmeans,rand_clara,rand_pam,rand_mclust)
# Notamos que Clara e PAM foram as que perfomaram melhor. k-means acompanha de perto. M-clust não funcionou bem para este conjunto de dados.
