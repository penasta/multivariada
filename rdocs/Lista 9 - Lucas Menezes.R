# Lista 9 - Análise de Agrupamentos

# Leitura dos pacotes necesários:

pacman::p_load(tidyverse,gridExtra,dendextend,gclus,aplpack,mclust)

# Exercicio 12.3================================================================

# Feito a mão!
#_______________________________________________________________________________

# Exercicio 12.5================================================================

# Leitura das matrizes:_________________________________________________________

Distant = matrix(c(0,1,11,5,
                   1,0,2,3,
                   11,2,0,4,
                   5,3,4,0),
                 nrow = 4,ncol = 4,
                 byrow = T);Distant


# A)____________________________________________________________________________

# Gráfico de dendograma:

plot(hclust(dist(Distant,method="euclidean",diag=F), method = "single"),
     main = "Single Linkage Clustering")

plot(hclust(dist(Distant,method="manhattan",diag=F), method = "single"),
     main = "Single Linkage Clustering")

plot(hclust(dist(Distant,method="minkowski",diag=F), method = "single"),
     main = "Single Linkage Clustering")


# B)____________________________________________________________________________

# Gráfico de dendograma:


plot(hclust(dist(Distant,method="euclidean",diag=F), method = "complete"),
     main = "Complete Linkage Clustering")

plot(hclust(dist(Distant,method="manhattan",diag=F), method = "complete"),
     main = "Single Linkage Clustering")

plot(hclust(dist(Distant,method="minkowski",diag=F), method = "complete"),
     main = "Single Linkage Clustering")

# C)____________________________________________________________________________

# Gráfico de dendograma:

plot(hclust(dist(Distant,method="euclidean",diag=F), method = "average"),
     main = "Average Linkage Clustering")

plot(hclust(dist(Distant,method="manhattan",diag=F), method = "average"),
     main = "Single Linkage Clustering")

plot(hclust(dist(Distant,method="minkowski",diag=F), method = "average"),
     main = "Single Linkage Clustering")

#_______________________________________________________________________________
# Exercicio 12.6================================================================

# Leitura da Matriz:

Distant = matrix(c(0,4,6,1,6,
                   4,0,9,7,3,
                   6,9,0,10,5,
                   1,7,10,0,8,
                   6,3,5,8,0),
                 nrow = 5,ncol =5,
                 byrow = T);Distant


# Sigle:

plot(hclust(dist(Distant,method="euclidean",diag=F), method = "single"),
     main = "Single Linkage Clustering")

# Complete;


plot(hclust(dist(Distant,method="euclidean",diag=F), method = "complete"),
     main = "Complete Linkage Clustering")

# Average:


plot(hclust(dist(Distant,method="euclidean",diag=F), method = "average"),
     main = "Average Linkage Clustering")


# Resultado:

# Todos os 3 possuem métodos de arranjo hierárquico parecidos.
# talvez a primeira visualização, do metodo singular possui uma diferença mais
# acentuada em comparação com os outros dois.





#_______________________________________________________________________________
# Exercicio 12.7================================================================

# Leitura da Matriz:

Correl = matrix(c(1,.63,.51,.12,.16,
                   .63,1,.57,.32,.21,
                   .51,.57,1,.18,.15,
                   .12,.32,.18,1,.68,
                   .16,.21,.15,.68,1),
                 nrow = 5,ncol =5,
                 byrow = T);Correl



# Criação do gráfico:

# Método Singular:
plot(hclust(dist(Correl,method="euclidean",diag=F), method = "single"),
     main = "single Linkage Clustering"); Sing


# Metodo Completo:
plot(hclust(dist(Correl,method="euclidean",diag=F), method = "complete"),
     main = "Complete Linkage Clustering");

# Resultado:

# Ambos métodos chegaram em resultados muito parecidos, tanto nos clusters quanto
# em qual momento o cluster foi traçado.



# EXTRA - Método da média:
plot(hclust(dist(Correl,method="euclidean",diag=F), method = "average"),
     main = "Average Linkage Clustering")

#_______________________________________________________________________________
# Exercicio 12.11================================================================

Tab = matrix(c(5,4,
               1,-2,
               -1,1,
               3,1),
             nrow = 4,
             ncol = 2, 
             byrow = T);Tab

# Clusters iniciais como sendo (AB) e (CD), logo:

AB = colMeans(Tab[c(1,2),]);AB
DC = colMeans(Tab[c(3,4),]);DC

# Tabela com os clusters iniciais:
AB_DC = matrix(c(AB,DC), nrow = 2, ncol = 2, byrow = F);AB_DC

# Criação dos novos clusters pela função K-means:

# Ciriação dos clusters:
(kmeans(Tab,centers = AB_DC))$cluster

# Nova Matriz com os valores dos novos centros de clusterização para k = 2:
(kmeans(Tab,centers = AB_DC))$centers


# visualização:
# Converter a tabela em um data frame
df <- as.data.frame(Tab)
colnames(df) <- c("X1", "X2")
df$Label <- c("A", "B", "C", "D")  # Rótulos para cada ponto
df$Cluster = as.factor((kmeans(Tab,centers = AB_DC))$cluster)

# Criar o gráfico de dispersão com legenda
ggplot(df, aes(x = X1, y = X2, label = Label, color = Cluster)) +
  geom_point() +
  geom_text(vjust = -1.5) +
  labs(x = "X1", y = "X2", title = "Gráfico de Dispersão AD x BC")+
  ylim(c(-2,5))



#_______________________________________________________________________________
# Exercicio 12.12===============================================================

Tab <- matrix(c(5,3,
                  -1,1,
                  1,-2,
                  -3,-2),
                nrow = 4,
                ncol = 2,
                byrow = TRUE);Tab



# Clusters iniciais como sendo (AB) e (CD), logo:

AC = colMeans(Tab[c(1,3),]);AC
BD = colMeans(Tab[c(2,4),]);BD

# Tabela com os clusters iniciais:
AC_BD = matrix(c(AC,BD), nrow = 2, ncol = 2, byrow = T);AC_BD

# Criação dos novos clusters pela função K-means:

# Ciriação dos clusters:
(kmeans(Tab,centers = 2))$cluster

# Nova Matriz com os valores dos novos centros de clusterização para k = 2:
(kmeans(Tab,centers = 2))$centers


# visualização:
# Converter a tabela em um data frame
df <- as.data.frame(Tab)
colnames(df) <- c("X1", "X2")
df$Label <- c("A", "B", "C", "D")  # Rótulos para cada ponto
df$Cluster = as.factor((kmeans(Tab,centers = 2))$cluster)

# Criar o gráfico de dispersão com legenda
ggplot(df, aes(x = X1, y = X2, label = Label, color = Cluster)) +
  geom_point() +
  geom_text(vjust = -1.5) +
  labs(x = "X1", y = "X2", title = "Gráfico de Dispersão BCD x A")+
  ylim(c(-2,5))

# Com o cluster demonstrado no gráfico fica fácil de ver a dissoancia do grupo A
# com os demais. Dessa forma os clusters nos parecem bem alocados onde,
# (BCD) é o cluster 1 e A está isolado no cluster 2. Matematicamente isso se deve
# a distancia maior dos demais de A, então assim que a alocação do algoritmo acontece
# onde A fica sozinho, rapidamente ele computa como o novo cluster criado e por
# consequência é o cluster 2.


#_______________________________________________________________________________
# Exercicio 12.13===============================================================



Tab <- matrix(c(-3,-2,
                1,-2,
                -1,1,
                5,3),
              nrow = 4,
              ncol = 2,
              byrow = TRUE);Tab



# Clusters iniciais como sendo (AB) e (CD), logo:

AB = colMeans(Tab[c(3,4),]);AB
CD = colMeans(Tab[c(1,2),]);CD

# Tabela com os clusters iniciais:
AB_CD = matrix(c(AB,CD), nrow = 2, ncol = 2, byrow = T);AB_CD

# Criação dos novos clusters pela função K-means:

# Ciriação dos clusters:
(kmeans(Tab,centers = 2))$cluster

# Nova Matriz com os valores dos novos centros de clusterização para k = 2:
(kmeans(Tab,centers = 2))$centers


# visualização:
# Converter a tabela em um data frame
df <- as.data.frame(Tab)
colnames(df) <- c("X1", "X2")
df$Label <- c("D", "C", "B", "A")  # Rótulos para cada ponto
df$Cluster = as.factor((kmeans(Tab,centers = 2))$cluster)

# Criar o gráfico de dispersão com legenda
ggplot(df, aes(x = X1, y = X2, label = Label, color = Cluster)) +
  geom_point() +
  geom_text(vjust = -1.5) +
  labs(x = "X1", y = "X2", title = "Gráfico de Dispersão BCD x A")+
  ylim(c(-2,5))


# Mais uma vez o algoritmo foi realizado contudo os dados estavam em ordem contrária, 
# sendo assim, os calculos matematicos de distancias seriam necessariamente diferentes.
# O ponto do exercicio é discutir o quão aleatórios podem ser os dados e o algoritmo, 
# necessariamente, vai chegar nos mesmo resultados sempre, independente da ordem que
# os dados estejam. Portanto, o comportamento esperado foi o mesmo do exercicio anterior.


#_______________________________________________________________________________
# Exercicio 83==================================================================

# Ler os dados
dados <- matrix(c(1, 1, 9, 2, 2, 10,
                  3, 2, 9, 4, 2, 8,
                  5, 3, 9, 6, 7, 14,
                  7, 12, 9, 8, 13, 10,
                  9, 13, 8, 10, 14, 10,
                  11, 14, 8, 12, 15, 9,
                  13, 7, 7, 14, 6, 3,
                  15, 7, 3, 16, 8, 3,
                  17, 6, 2, 18, 7, 2,
                  19, 8, 2, 20, 6, 1,
                  21, 7, 1, 22, 8, 1),
                nrow = 11,
                ncol = 6,
                byrow = TRUE)

# Separar os dados em vetores individuais
col1 <- dados[, 1];col1
col2 <- dados[, 2];col2
col3 <- dados[, 3];col3
col4 <- dados[, 4];col4
col5 <- dados[, 5];col5
col6 <- dados[, 6];col6

#Sequencia de dados:

# primeira parte:

dados1 = as.data.frame(col1)
names(dados1) = c("Seq")
dados1$X = col2
dados1$Y = col3

# segunda parte:

dados2 = as.data.frame(col4)
names(dados2) = c("Seq")
dados2$X = col5
dados2$Y = col6


dados = rbind(dados1,dados2);head(dados)

# ordenamos o banco de dados: ( só por caprixo )

dados = arrange(dados,Seq);head(dados)# banco pronto


# A)____________________________________________________________________________

# Criar o gráfico de dispersão com legenda

ggplot(dados, aes(x = X, y = Y)) +
  geom_point() +
  labs(x = "X", y = "Y", title = "Gráfico de Dispersão")

# Claramente vemos 3 grandes grupos, contudo temos 2 pontos isolados onde veremos
# quais indicações de classificação são possibilidades.


dist(dados[,c(2,3)])


# B) e C)_______________________________________________________________________

# Calculo da distancia Euclidiana:

dist(dados[,c(2,3)],method="euclidean",diag=F)

# Criação dos clusters:
hclust_sing_euc =  hclust(dist(dados[,c(2,3)],method="euclidean",diag=F), method = "single");hclust_sing_euc
hclust_comp_euc =  hclust(dist(dados[,c(2,3)],method="euclidean",diag=F), method = "complete");hclust_comp_euc
hclust_avg_euc =  hclust(dist(dados[,c(2,3)],method="euclidean",diag=F), method = "average");hclust_avg_euc

# Todas retornam 5 clusters diferentes.

# Visualização da distancia pelo dendograma:

# Visão macro:

# Single
sing_dend_obj <- as.dendrogram(hclust_sing_euc)
sing_col_dend <- color_branches(sing_dend_obj, h = 5)
plot(sing_col_dend,
     main = "Single Linkage Clustering - Euclidean")

# Complete
comp_dend_obj <- as.dendrogram(hclust_comp_euc)
comp_col_dend <- color_branches(comp_dend_obj, h = 5)
plot(comp_col_dend,
     main = "Complete Linkage Clustering - Euclidean")

# Average
avg_dend_obj <- as.dendrogram(hclust_avg_euc)
avg_col_dend <- color_branches(avg_dend_obj, h = 5)
plot(avg_col_dend,
     main = "Single Linkage Clustering - Euclidean")

# Os 3 resultados são iguais.








# Calculo da distancia de Manhattan:____________________________________________

dist(dados[,c(2,3)],method="manhattan",diag=F)

# Criação dos clusters:

hclust_sing_man =  hclust(dist(dados[,c(2,3)],method="manhattan",diag=F), method = "single");hclust_sing_man
hclust_comp_man =  hclust(dist(dados[,c(2,3)],method="manhattan",diag=F), method = "complete");hclust_comp_man
hclust_avg_man =  hclust(dist(dados[,c(2,3)],method="manhattan",diag=F), method = "average");hclust_avg_man

# Todas retornam 5 clusters diferentes.

# Visualização da distancia pelo dendograma:

# Visão macro:


# Single
sing_dend_obj <- as.dendrogram(hclust_sing_man)
sing_col_dend <- color_branches(sing_dend_obj, h = 5)
plot(sing_col_dend,
     main = "Single Linkage Clustering - Manhattan")

# Complete
comp_dend_obj <- as.dendrogram(hclust_comp_man)
comp_col_dend <- color_branches(comp_dend_obj, h = 5)
plot(comp_col_dend,
     main = "Complete Linkage Clustering - Manhattan")

# Average
avg_dend_obj <- as.dendrogram(hclust_avg_man)
avg_col_dend <- color_branches(avg_dend_obj, h = 5)
plot(avg_col_dend,
     main = "Single Linkage Clustering - Manhattan")



# Os 3 resultados são iguais.











# Calculo da distancia de Mahalanobis:__________________________________________

cov_matrix <- cov(dados)

# Calcular a distância de Mahalanobis
mahalanobis <- mahalanobis(dados, center = colMeans(dados), cov = cov_matrix)

# Imprimir as distâncias de Mahalanobis
dist(mahalanobis)


# Visualização da distancia:
 # Defina o número desejado de clusters
num_clusters <- 3


# Single________________________________________________________________________

plot(hclust(dist(mahalanobis,method="euclidean",diag=F), method = "single"),
     main = "Single Linkage Clustering")

# Crie os grupos de clusters a partir do corte no dendrograma
grupos_clusters <- cutree(hclust(dist(mahalanobis,method="euclidean",diag=F), method = "single"),
                          k = num_clusters)

# Colorir o dendrograma para mostrar os clusters
rect.hclust(hclust(dist(mahalanobis,method="euclidean",diag=F), method = "single"),
            k = num_clusters,
            border = num_clusters:1)





# Complete______________________________________________________________________

# Crie os grupos de clusters a partir do corte no dendrograma

plot(hclust(dist(mahalanobis,method="euclidean",diag=F), method = "complete"),
     main = "Complete Linkage Clustering")

# Numeração dos clusters
grupos_clusters <- cutree(hclust(dist(mahalanobis,method="euclidean",diag=F), method = "complete"),
                          k = num_clusters)

# Colorir o dendrograma para mostrar os clusters
rect.hclust(hclust(dist(mahalanobis,method="euclidean",diag=F), method = "complete"),
            k = num_clusters,
            border = num_clusters:1)



# Average_______________________________________________________________________

plot(hclust(dist(mahalanobis,method="euclidean",diag=F), method = "average"),
     main = "Average Linkage Clustering")


# Numeração dos clusters
grupos_clusters <- cutree(hclust(dist(mahalanobis,method="euclidean",diag=F), method = "average"),
                          k = num_clusters)

# Colorir o dendrograma para mostrar os clusters
rect.hclust(hclust(dist(mahalanobis,method="euclidean",diag=F), method = "average"),
            k = num_clusters,
            border = num_clusters:1)



#_______________________________________________________________________________
# Exercicio 84==================================================================

# Leitura dos dados:

data(bank)

# A)____________________________________________________________________________

# Notas Genuinas(0):
Med0 = colMeans(bank[1:100,-1]);Med0
COV0 = cov(bank[1:100,-1]);COV0


# Notas Falsas(1):
Med1 = colMeans(bank[101:200,-1]);Med1
COV1 = cov(bank[101:200,-1]);COV1


# Criação do face Plot para cada população:

bank$Status <- factor(bank$Status)

Med <- bank %>%
  group_by(Status) %>%
  summarise_all(list(mean)) %>%
  dplyr::select(!Status)


# Grafico face Plot:
faces(Med,face.type=1)

# B)____________________________________________________________________________


# Criação dos clusters:
hclust_avg_euc =  hclust(dist(bank[,-1],method="euclidean",diag=F), method = "average");hclust_avg_euc

# Todas retornam 5 clusters diferentes.

# Visualização da distancia pelo dendograma:

# Visão macro:

# Average
med_dendo <- as.dendrogram(hclust_avg_euc)
Dendo_med <- color_branches(med_dendo, h = 2)
plot(Dendo_med,
     main = "Average Linkage Clustering - Euclidean")

rect.hclust(hclust_avg_euc,
            k = 2,
            border = 2:1)


# Numeração dos clusters
grupos_clusters <- cutree(hclust_avg_euc,
                          k = 2)
table(grupos_clusters)# errou por 1

bank$G1 = grupos_clusters


# C)____________________________________________________________________________

# Criação do cluster
Mclust(bank[,-1],G = 2)
(Mclust(bank[,-1],G = 2))$BIC
plot(Mclust(bank[,-1],G = 2), what = "BIC")
# 3 modelos bons para agrupamento


# Classificação:

table((Mclust(bank[,-1],G = 2))$classification)# errou por 1

bank$G2 = (Mclust(bank[,-1],G = 2))$classification


# D)____________________________________________________________________________

# Calcular o ARI:

ari1 <- adjustedRandIndex(bank$G1,bank$Status);ari1
ari2 <- adjustedRandIndex(bank$G2,bank$Status);ari2

# os dois métodos possuem resultados iguais devido a proporção de acertividade

