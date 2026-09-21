##############################################
#                                            #
# TP2 : Analyses Multivariees avec R : L'ACP #
#                                            #
##############################################



#### Chargement des packages ade4 (pour faire l'analyse) et factoextra (pour visualiser les resultats)
library(ade4)
library(factoextra)

### Chargement du jeu de donnees "meau"
data(meau)
TAB1=meau$env
TAB1 # obtenir le tableau de donnees
summary(TAB1)
# Temperature, Courant, pH, Conductivite, Oxygene, Demande biologique en oxygene sur 5 jours (DBO5)
# Matieres oxydees, Ammonium, Nitrate, Phosphate
# Les sites sont six stations (S1 a S6)
# prelevees en fonction des saisons (sp = spring, su = summer, etc ...)
meau$design


#### Essais d'analyses univariee ou bivariee : 
summary(TAB1) # parametres de position et de dispersion
pairs(TAB1) # correlations 2 a 2

# Pour y voir plus clair dans les correlations 2 a 2 : faire appel au package GGally
library(GGally)
ggpairs(TAB1) # correlations 2 a 2
ggpairs(TAB1, diag = list(continuous = "blankDiag")) # sans la diagonale

cov(TAB1) # la matrice des covariances
cor(TAB1) # la matrice des correlations


# Les donnees ne sont pas toutes de meme unite
# On va alors les normaliser, en centrant/reduisant avec la fonction scale

TAB1norm=scale(TAB1)

#### Visualisation graphique des donnees TAB1 et TAB1norm
par(mfrow=c(1,2)) # partitionne la fenetre graphique en 1 ligne, 2 colonnes
boxplot(TAB1, las=2, main="TAB1") # l'argument las positionne l'etiquette perpendiculairement a l'axe
boxplot(TAB1norm, las=2, main="TAB1norm")

# Matrice des covariances et des correlations sur les donnees centrees-reduites : 
cov(TAB1norm)
cor(TAB1norm)
# A comparer avec cor(TAB1) :
cor(TAB1)


#### Determiner "a la main" les composantes principales et le nombre de composantes a garder.
VP = eigen(cor(TAB1)) # Valeurs et vecteurs propres de la matrice de correlation de TAB1
VP
par(mfrow=c(1,1))
barplot(VP$values) # Graphique des eboulis, ou screeplot = graphique des valeurs propres
# Droite a 1 (= moyenne des valeurs propres) sur le graphique
abline(h=mean(VP$values)) # Critere de Kaiser

CP = VP$values / sum(VP$values)
CP # Proportions
CP*100 # En % de variance expliquee
cumsum(CP*100) # somme cumulee


#### La fonction dudi.pca (du package ade4, qui a deja ete charge pour travailler sur le jeu de donnees meau)
par(mfrow=c(1,1))
ACP = dudi.pca(TAB1) #la fonction dudi.pca se charge de prendre la matrice de correlations
# ATTENTION ! La fonction affiche le graphique des eboulis et attend que vous donniez le nombre de composantes a extraire.
# Ici, choisissez 3 ; vous pouvez aussi la relancer avec plus ou moins de composantes.
3 #on indique le nombre de composantes a garder

# On peut aussi ecrire directement dans la fonction le nombre de composantes a garder :
# ACP = dudi.pca(df = TAB1, scannf = FALSE, nf = 3)

ACP

# Dans l'objet ACP sont stockes differents elements :
ACP$eig # les valeurs propres
# ACP$li les nouvelles coordonnees des individus
# ACP$co les nouvelles coordonnees des variables (loading matrix)
# D'autres parametres sont presents, ils ne sont que peu ou pas utiles

# la fonction summary sur l'objet ACP permet d'obtenir les informations sur les valeurs propres
summary(ACP)

#Le graphique des valeurs propres (des eboulis) peut etre obtenu avec la fonction fviz_eig
fviz_eig(ACP)


#### Cercles des correlations
fviz_pca_var(ACP, repel=TRUE) # par defaut, le graphe est realise sur les axes 1 et 2
fviz_pca_var(ACP, axes=c(1,3), repel=TRUE) # Composantes 1 et 3
fviz_pca_var(ACP, axes=c(2,3), repel=TRUE) # Composantes 2 et 3


## Resultats sur les variables actives
var = get_pca_var(ACP)
var
var$coord # coordonnees des variables sur chaque axe


#### Association entre individus
# on represente les individus dans les composantes avec la fonction fviz_pca_ind :
fviz_pca_ind(ACP, repel=TRUE) # Composantes 1 et 2 par defaut
fviz_pca_ind(ACP, axes=c(1,3), repel=TRUE) # Composantes 1 et 3
fviz_pca_ind(ACP, axes=c(2,3), repel=TRUE) # Composantes 2 et 3


# Si on peut classer les individus selon un facteur, l'interpretation est facilitee.
# ici, on peut mettre les saisons comme facteur d'interpretation.
# Elles sont dans le design du jeu de donnees "meau"
Saisons = meau$design$season
Saisons

## Superposition sur le graphique des individus avec l'argument col.ind =...
fviz_pca_ind(ACP,
             col.ind = Saisons,
             axes=c(1,2),
             repel=TRUE,
             palette=c(1:4),
             addEllipses = TRUE,
             ellipse.type = "convex",
             legend.title = "Saisons")


# palette represente les couleurs, codees ici par des nombres, demandant 4 couleurs puisqu'il y a 4 saisons
# on peut choisir n'importe quelle palette de R, ou preciser nos propres couleurs.
# AddEllipses ajoute les ellipses, et ellipse.type precise leur type.


# Comme precedemment on peut egalement acceder aux coordonnees des individus
# C'est cependant moins utile que pour les variables
# mais cela permet parfois de reperer des donnees particulieres.
ind <- get_pca_ind(ACP) 
ind
head(ind$coord) # coordonnees des individus


### Rotation Varimax
ACP.rot <- ACP
ACP.rot[["co"]] <- varimax(as.matrix(ACP.rot$co))$loadings
fviz_pca_var(ACP, axes=c(2,3), repel=TRUE) # sans rotation
fviz_pca_var(ACP.rot, axes=c(2,3), repel=TRUE) # avec rotation


#######################
# A VOUS DE JOUER !!! #
#######################

# Exercice 1
# On a mesure la longueur et la largeur des sepales et des petales de 3 especes d'Iris (I. setosa, I. versicolor et I. virginica).
# Donnees contenues dans le fichier "Iris.csv" (ou directement dans R sous le nom "iris")
# On cherche a savoir si on peut distinguer les 3 especes, sur leurs caracteristiques morphologiques.

data(iris)

# Exercice 2
# Analysez les donnees du fichier Voiture2 du TP precedent a l'aide d'une ACP.
# Presentez la repartition des individus regroupes par Gamme.


