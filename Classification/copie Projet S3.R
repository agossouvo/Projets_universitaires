## PROJET DU COURS S3 ##

library(readxl)
library(FactoMineR)
library (factoextra)
library(questionr)
library(prettyR)
library(corrplot)
library(PerformanceAnalytics)
library(ggpubr)
library(cluster)
library(doBy)

## 1) Chargement de jeu de Données 

BDD <- read.csv("C:/Users/berni/Desktop/cours_Eiffel/Cours S3/travaux pratiques/BDD.csv", row.names=1, sep=";")

## 2) Analyse elementaire et commentaire

BDD <- BDD[,1:9]

attach(BDD)

cv=function(x, na.rm=F){
  sd(x, na.rm = na.rm)/mean(x,na.rm = na.rm)
}

describe(BDD[,c(1:9)], num.desc = c("mean","cv"))

## Correlation entre les variables

mcor <-cor(BDD)
corrplot(mcor, method = "number")
mcor

chart.Correlation(BDD, histogram=TRUE, pch=19)


## III / Analyse en Composantes Principales 

res_pca <- PCA(BDD, scale.unit = TRUE,ncp=5, graph = TRUE)

print (res_pca)
res_pca$ind$dist

###  les valeurs propres de l'ACP 
eig_val <- get_eigenvalue(res_pca)
eig_val
copie(eig_val)

### le graphique des eboulis de valeurs propres

fviz_eig(res_pca, geom = c("line"),linecolor = "blue",addlabels = TRUE, ylim = c(0, 50))


### Les variables 
var <- get_pca_var(res_pca)
var

#### Coordonnées 
head(var$coord)
#### Cos2: qualité de répresentation
head(var$cos2)
#### Contributions aux composantes principales
cos = head(var$contrib)
copie (cos)
fviz_contrib(res_pca, choice = "var", axes = 3)

### correlation entre variables et chaque dimension
corrplot(var$cos2,is.corr=FALSE)


### cercle de correlation 
#### Sur les axes 1 et 2
fviz_pca_var(res_pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)


### Sur les axes 1 et 3
fviz_pca_var(res_pca, axes = c(1,3), col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

### Qualité de représentation des individus

#### Sur les axes 1 et 2
fviz_pca_ind (res_pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)

#### Sur les axes 1 et 3
fviz_pca_ind (res_pca,axes = c(1,3), col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)

### Contribution 
fviz_cos2(res_pca, choice = "ind", axes = 1:2)

fviz_contrib(res_pca, choice = "ind", axes = 2:3)



### Biplot 

fviz_pca_biplot (res_pca,
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)




## respca <- PCA(BDDX[,2:11], scale.unit = TRUE, graph = TRUE)

### fviz_pca_ind (respca,axes = 2:3,
      ###        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
 ##             repel = TRUE # Évite le chevauchement de texte)####



### le plan pour les axes 1 et 3

# plot(rescap,choix = "ind",axes = c(1,3))

# plot(rescap,choix = "var",axes =c(1,3))


## CLASSIFICATION PAR LA METHODE K MEANS
# Calculer k-means avec k = 4
set.seed(123) # garde la classification fixe
res.km <- kmeans(scale(BDD),4, nstart = 25)
res.km
## Ajout de la variable res.km$cluster dans le jeu de données.
BDD_NEW=cbind(BDD,res.km$cluster) #ajout la variable res.km$cluster
View(BDD_NEW)

# Renommer la variable cla.km en classes
BDD_NEW = rename.variable(BDD_NEW, "res.km$cluster", "Kmean")

as.factor(BDD_NEW$kmean)
attach(BDD_NEW)

# Clustering K-means montrant le groupe de chaque individu
res.km$cluster # repartition des observations dans la parties
res.km$size # répartition des données dans chaque classe
res.km$centers #coordonnées des centres de classes( qui sont aussi les moyennes dans chaque classe pour une variables
res.km$withinss # somme des carrées des ecart intra classes pour chaque classe

# sum(res.km$withinss/51)
# res.km$tot.withinss
# res.km$betweenss
# res.km$totss
# copie(round(res.km$centers),3)

## Calcul de la moyenne et ecart-type dans chaque classe pour chaque variable
summaryBy(T+TM+Tm+PP+V+RA+SN+TS+FG ~ BDD_NEW$Kmean, data=BDD_NEW, FUN=mean) # moyennes
apply(BDD, 2, mean)
summaryBy(T+TM+Tm+PP+V+RA+SN+TS+FG ~ BDD_NEW$Kmean, data=BDD_NEW, FUN=sd)  # ecart-type
apply(BDD, 2, sd)

# graphique schématisant l'affectation dans chaque classe

fviz_cluster(res.km, data = BDD,
             palette = c("#2E9FDF", "#42ff33", "#E7B800","#ff6833"), 
             geom = "point",
             labels=TRUE,
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
# AUtre méthode pour la moyenne
meanClass <- aggregate(scale(BDD[ , 1: 9]),
                       by = list(res.km$cluster),
                       FUN = mean)
colnames(meanClass)[ 1] <- "CLUSTER"

meanClass




#clusplot(BDD,res.km$cluster, color=TRUE,shade = TRUE,labels=2,lines=0)

# plotcluster(BDD,res.km$cluster)






# Pour Comparer deux clusters 
# cluster.stats(BDD, res.km$cluster,res.km$cluster)

### CLASSIFICATION ASCENDANTE HIERACHIQUE
reponse <- scale(BDD)
#matrice des distances entre individus
d_reponse<- dist(reponse)

#### Dendogramme de CAH par la méthode Ward avec la fontion hclust

cah.ward <- hclust(d_reponse,method="ward")
plot(cah.ward)

#Schémas Dendrogramme 
cah.ward_Den <- as.dendrogram(cah.ward)
plot(cah.ward_Den)
cah.ward_Den <- color_branches(cah.ward_Den, k = 2) 
plot(cah.ward_Den)






# Découpage du dendogramme, A quel niveau le coupons nous ?
inertie <- sort(cah.ward$height, decreasing = TRUE)
inertie
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 3, 4,5), inertie[c(2, 3, 4,5)], col = c("green", "red", "blue", "yellow"), cex = 2, lwd = 4)
abline(v=c(2, 3, 4,5),col=c("green", "red", "blue", "yellow"))


#dendrogramme avec matérialisation des groupes de 3 à 6
plot(cah.ward, main = "Partition en 2, 3,4 ou 5 classes", xlab = "", ylab = "", sub = "", axes = FALSE)
rect.hclust(cah.ward,2, border ="green")
rect.hclust(cah.ward,3, border = "red")
rect.hclust(cah.ward,4, border = "blue")
rect.hclust(cah.ward,5, border = "yellow")

#La fonction best.cutree permet de déterminer le nombre de classes optimal(la perte d'inertie est minimale)

best.cutree <- function(hc, min=3, max=20, loss=FALSE, graph=FALSE, ...){
  if (class(hc)!="hclust") hc <- as.hclust(hc)
  max <- min(max, length(hc$height))
  inert.gain <- rev(hc$height)
  intra <- rev(cumsum(rev(inert.gain)))
  relative.loss = intra[min:(max)]/intra[(min - 1):(max - 1)]
  best = which.min(relative.loss)
  names(relative.loss) <- min:max
  if (graph) {
    temp <- relative.loss
    temp[best] <- NA
    best2 <- which.min(temp)
    pch <- rep(1, max-min+1)
    pch[best] <- 16
    pch[best2] <- 21
    plot(min:max, relative.loss, pch=pch, bg="grey75", ...)
  } else {
    if (loss)
      relative.loss
    else
      best + min - 1
  }
}
## On applique best.cutree à cah.ward en affichant la liste des inerties
best.cutree(cah.ward)

# graphique des pertes d'inerties 
best.cutree(cah.ward,graph =TRUE,xlab= "Nombres de classes", ylab = "Inertie relative")

#découpage en 3 groupes
Kclust <- cutree(cah.ward,k=2)
table(res.km$cluster)

# ajout de la variable Kclust au jeu de données
BDD_NEW = cbind(BDD_NEW,Kclust)
# Calcul des moyennes et ecart-types par cluster
summaryBy(T+TM+Tm+PP+V+RA+SN+TS+FG ~ BDD_NEW$Kclust, data=BDD_NEW, FUN=mean) # moyennes
apply(BDD, 2, mean)
summaryBy(T+TM+Tm+PP+V+RA+SN+TS+FG ~ BDD_NEW$Kclust, data=BDD_NEW, FUN=sd)  # ecart-type
apply(BDD, 2, sd)

#liste des groupes
print(sort(Kclust))




### Classification CAH avec le package HCPC

res.hcpc <- HCPC(res_pca, graph = FALSE)
table (res.hcpc$data.clust$clust)

HCPC <- res.hcpc$data.clust$clust
plot(res.hcpc, choice = "tree")

plot(res.hcpc, choice = "3D.map")

### Dendogramme des donnees 

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 0.8      # Augment l'espace pour le texte
)


### Visualiser les individus et colorier par groupe 
fviz_cluster(res.hcpc,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

# Caractérisation des groupes 

BDD_NEW = cbind(BDD_NEW,HCPC)
table(HCPC) 
summaryBy(T+TM+Tm+PP+V+RA+SN+TS+FG ~ BDD_NEW$HCPC, data=BDD_NEW, FUN=mean) # moyennes
apply(BDD, 2, mean)
summaryBy(T+TM+Tm+PP+V+RA+SN+TS+FG ~ BDD_NEW$HCPC, data=BDD_NEW, FUN=sd)  # ecart-type
apply(BDD, 2, sd)


res.hcpc$desc.var$quanti

res.hcpc$desc.axes$quanti
res.hcpc$desc.ind$para
sort(res.hcpc$data.clust$clust,decreasing = FALSE)


res.hcpc$data.clust











