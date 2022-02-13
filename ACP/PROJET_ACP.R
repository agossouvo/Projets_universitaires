
library(readxl)
library(FactoMineR)
library(factoextra)
library(prettyR)
library(questionr)

Donnees <- read_excel("Donnees.xlsx", col_types = c("text",
                      "numeric", "numeric", "numeric", "numeric",
                      "numeric", "numeric", "numeric"))

## 1) Description du jeu de données

dim(Donnees)
str(Donnees)
head(Donnees)
tail(Donnees)

# 2) statistique univariée
summary(Donnees[-1])

describe(Donnees[,c(2:8)], num.desc = c("min","mean","median","max","sd","Cv"))

cv=function(x, na.rm=F){
  sd(x, na.rm = na.rm)/mean(x,na.rm = na.rm)
}


CV <- apply(Donnees[-1],2,cv)
copie(CV)

# 3)STATISTIQUES BIVARIEES

## Matrice de correlation entre les variables

mcor <-round( cor(Donnees[,2:8]), 2)
mcor

"On voit bien qu'il existe une forte correlation positive entre la variable IDH et Internet_users quiest de 0.92.
De la meme facon entre la variable life_expectancy et internet_users soit 0.81.
POur finir entre la variable IDH et life_expectancy qui est de 0.90. Par contre,
on peut voir qu'il y a une faible corrÃ©lation negative entre la varibale HDI et la variable GINI qui est de 0.43.
"
"On peut se demander si cette relation est significative ou non. Pour cela, on peut proceder par un test de significativitÃ©
car les coefficients de pearson et Spearman ne permet de determiner la significativitÃ© d'une relation  "

# Test de significativité
with(Donnees[,2:8],cor.test(HDI, internet_users))
with(Donnees[,2:8],cor.test(life_expectancy, internet_users))
with(Donnees[,2:8],cor.test(HDI, life_expectancy))



# 4) ACP

rescap = PCA(Donnees[,2:8])
summary(rescap)

## le plan pour les axes 1 et 2
plot(rescap,choix = "ind",axes =1:2)
fviz_pca_ind (rescap) # autre mÃ©thode
plot(rescap,choix = "var",axes =1:2)

## le plan pour les axes 1 et 3

plot(rescap,choix = "ind",axes = c(1,3))

plot(rescap,choix = "var",axes =c(1,3))

## le graphique des valeurs propres 
x = fviz_eig(rescap, addlabels = TRUE, ylim = c(0, 50))
x



rescap$ind$cos2 > 0.85

