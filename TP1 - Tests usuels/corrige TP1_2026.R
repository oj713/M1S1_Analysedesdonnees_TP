

#####################################################
#####################################################
##                                                 ##
##                   MODULE 4U001                  ##
##                                                 ##
##                TP1 - Tests simples              ##
##                                                 ##
#####################################################
#####################################################



###########################################################
##  1ere partie : le test de Student pour 1 echantillon  ##
###########################################################


# On prendra systematiquement alpha = 5% 
# Pour chaque exercice: vous enoncerez ce que sont mesure et facteur(s) s'il y a lieu,
# les conditions d'application, 
# et poserez les hypothèses testées.

##########
########## Test de Student pour 1 echantillon
##########

# Cela revient a calculer l'intervalle de confiance de l'espérance µ de la population dont est issu l'echantillon.

## EXERCICE DIRIGE

# Importons le jeu de donnees Datafish, qui contient les donnees de taille de poissons
# soumis a deux regimes alimentaires (1 et 2) differents.
Datafish <-read.csv("Datafish.csv", header =T, sep=";", dec=",")

# Representons graphiquement les donnees
boxplot(Mesure~Groupe, data = Datafish, col=c("purple","pink"))

# On veut tester si la moyenne theorique µ de la pop dont est issu le premier groupe est de µ0=53.
#Définir les variables!! 
#Mesure = taille des poissons (quanti continue)
# Facteur = regime alimentaire, a deux modalites, consituant deux echantillons independants adu1 et adu2
# Choix du test : t.test a 1 ech ;

# Pour avoir l'effectif : fonction length
n=length(Datafish$Mesure[Datafish$Groupe=="adu1"]) ; n
# Notez que comme l'effectif est plus grand que 30, on n'a pas besoin de tester la normalite
# On peut aussi utiliser la fonction tapply() pour avoir l'effectif de tous les échantillons
tapply(Datafish$Mesure, Datafish$Groupe, length)

# Hypotheses : H0 : µ1 = µ0 et H1 : µ1 =! µ0 = on teste une différence donc test bilatéral
t.test(Datafish$Mesure[Datafish$Groupe=="adu1"],mu=53)
# rejet de H0 au risque alpha = 5%

# Il donne la p-value du test de comparaison dont l'hypothèse H0 est µ=53, mais aussi l'intervalle de confiance a 95%

# Même si l'effectif est de 31, on peut verifier si l'échantillon est issu d'une distribution normale
# avec un test de Shapiro-Wilk (il en existe d'autres)
shapiro.test(Datafish$Mesure[Datafish$Groupe=="adu1"])
# On peut utiliser la fonction tapply pour le vérifier dans tous les échantilons
tapply(Datafish$Mesure, Datafish$Groupe, shapiro.test)
# non-rejet de H0, les donnees sont issues d'une distribution normale.

## Facultatif
# Visualisation graphique de la distribution et d'une distribution normale

adu1=Datafish$Mesure[Datafish$Groupe=="adu1"]
seq.adu1=seq(min(adu1),max(adu1),0.001)
hist(adu1,freq=F) #l'axe des ordonnées représente des densités
lines(seq.adu1,dnorm(seq.adu1,mean(adu1),sd(adu1)))
# => On superpose à la distribution empirique des données une distribution normale théorique 
# ayant la même moyenne et le même écart-type que l'échantillon
# dnorm calcule, pour chaque valeur de seq.adu1, la hauteur de la courbe normale de moyenne théo celle de adu1 et d'ecart-type celui de adu1

# QQ-plot
qqnorm(adu1)
qqline(adu1)
shapiro.test(adu1)

# FACULTATIF 2 - les memes graphiques avec ggplot2 => les intervalles de classes ne sont pas les memes
# On isole les donnees adu1 dans Datafish
data_adu1 <- subset(Datafish, Groupe == "adu1")
## Histogramme avec courbe de densitÃ© normale
ggplot(data_adu1, aes(x = Mesure)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1,
                 fill = "grey", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = mean(data_adu1$Mesure),
                            sd = sd(data_adu1$Mesure)),
                color = "red", linewidth = 1) +
  labs(x = "Taille des poissons (cm)", y = "Densite") +
  theme_minimal()
# QQ-plot
ggplot(data_adu1, aes(sample = Mesure)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(x = "Quantiles theoriques", y = "Quantiles observes") +
  theme_minimal()

##########
########## Exercices sur le test de Student pour 1 echantillon
##########

### Exercice 1
### Verifiez si la moyenne theorique est 46 dans le groupe adu2 des donnees Datafish

# Mesure : taille des poissons (quantitative continue) et facteur qualitatif nominal: regime alimentaire,t
# dont on ne considère ici qu'une seule modalité (adu2 <=> 2ème régime alimentaire)

# t.test a 1 ech : n suffisament grand, pas besoin de tester la normalite -mais on peut le faire quand même!!

shapiro.test(Datafish$Mesure[Datafish$Groupe=="adu2"])

#on represente graphiquement la distribution:
###################################################
# FACULTATIF 1
# Visualisation graphique de la distribution et d'une distribution normale
adu2=Datafish$Mesure[Datafish$Groupe=="adu2"]
seq.adu2=seq(min(adu2),max(adu2),0.001)
hist(adu2,freq=F)
lines(seq.adu2,dnorm(seq.adu2,mean(adu2),sd(adu2)))
# QQ-plot
qqnorm(adu2)
qqline(adu2)

# FACULTATIF 2 - les memes graphiques avec ggplot2
# On isole les donnees adu2 dans Datafish
data_adu2 <- subset(Datafish, Groupe == "adu2")
## Histogramme avec courbe de densitÃ© normale
ggplot(data_adu2, aes(x = Mesure)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1,
                 fill = "grey", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = mean(data_adu2$Mesure),
                            sd = sd(data_adu2$Mesure)),
                color = "red", linewidth = 1) +
  labs(x = "Taille des poissons (cm)", y = "Densite") +
  theme_minimal()
# QQ-plot
ggplot(data_adu2, aes(sample = Mesure)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(x = "Quantiles thÃ©oriques", y = "Quantiles observÃ©s") +
  theme_minimal()
###################################################

# H0 : µ2 = µ0 =46 : pas de difference significative entre la moyenne µ dont est issue notre ech et la moyenne théorique µ0 = 46
# H1 : µ2 =! µ0
t.test(Datafish$Mesure[Datafish$Groupe=="adu2"],mu=46)
# rejet de H0 au risque alpha = 5%

### Exercice 2
### On a mesure la durete d'une serie de mineraux identiques preleves dans une meme mine. Les donnees sont dans le fichier Mineraux.
### Sont-ils aussi durs que le corindon (durete 8) ?
### Sans commande supplementaire, donner la moyenne et l'intervalle de confiance a 95%.

DataMin <-read.csv("Mineraux.csv", header=T, sep=";", dec=",")
str(DataMin)
#str affiche la structure d'un objet => ici 37 obs d'une seule variable, valeurs numériques, et c'est un data.frame
summary(DataMin)
# on peut aussi utiliser la fonction dim
dim(DataMin)

# Mesure : durete (quanti continue)
## Distribution de la durete dans l'echantillon=> représentation graphique sous forme histogramme
hist(DataMin$Durete)

# choix du test (t.test a 1 ech), conditions d'application (pas besoin de tester normalite car n=37)
# mais si on vuet le faire c'est possible:
shapiro.test(DataMin$Durete)

#et hypotheses : H0 : µ = µ0 = 8 et H1 =! µ0
t.test(DataMin$Durete, mu=8)
# p > risque alpha, les donnees ne permettent pas de rejeter H0
# Les mineraux sont aussi durs que le corindon
# mean of x = 8.0405
# 95 percent confidence interval : 7.946742 8.134339


#############################################
##  2eme partie : autres tests de Student  ##
#############################################

##########
########## Test de Student pour 2 échantillons indépendants
##########


### Pour faire un test de Student sur des effectifs inferieurs à 30, il faut que les donnees soient gaussiennes
### On vérifie la normalite par le test de Shapiro-Wilk (il en existe d'autres)

##### EXERCICE DIRIGE

# Question : existe-t-il une difference de taille des poissons entre les deux groupes ?
# Autre formulation: Existe-t-il un effet du regime alimentaire sur la taille des poissons ?
# Rappel des variables : mesure dependante = taille (VAC) 
# facteur: regime alimentaire (qualitatif nominal) qui existe sous 2 modalites (2 regimes alimentaires differents) 
# permettant d'identifier 2 echantillons independants

# représentation graphique:

boxplot(Datafish$Mesure ~ Datafish$Groupe,col=c("purple","pink"),
        xlab = "Régime alimentaire",
        ylab = "Taille des poissons (cm)")

# FACULTATIF : le meme graphique avec ggplot2
library(ggplot2)
ggplot(Datafish, aes(x = Groupe, y = Mesure, fill = Groupe)) +
  geom_boxplot() +
  labs(
    x = "Regime alimentaire",
    y = "Taille des poissons (cm)"
  )
# si on veut choisir ses couleurs:
ggplot(Datafish, aes(x = Groupe, y = Mesure, fill = Groupe)) +
  geom_boxplot() +
  labs(
    x = "Regime alimentaire",
    y = "Taille des poissons (cm)",
    fill = "Regime alimentaire" 
  ) +
 scale_fill_manual(values = c("purple", "pink"))

# Choix du test : test t pour deux echantillons independants de grande taille
# Pas de condition d'application car n > 30
# Mais comme les effectifs sont seulement légèrement > 30, il est prudent de vérifier les CA
# 
#    * les echantillons sont issus de distribution normale ?
#    * egalite des variances ?
## Vérification de la normalité de la distribution de X dans chaque échantillon avec un test Shapiro/Wilk
### H0 : pas de difference entre la distribution dont est issu l'échantillon et la distribution normale
### H1 : il existe une difference significative entre la distribution observee et une distribution normale

shapiro.test(Datafish$Mesure[Datafish$Groupe=="adu1"])
# non rejet de H0
shapiro.test(Datafish$Mesure[Datafish$Groupe=="adu2"])
# non rejet de H0

# Avec la fonction tapply()
tapply(Datafish$Mesure, Datafish$Groupe, shapiro.test)

# Pour tester l'egalite de deux variances sous R :

#  H0: variances homogène
#  H1: variances hétérogènes 
bartlett.test(Datafish$Mesure ~ Datafish$Groupe) 
var.test(Datafish$Mesure~Datafish$Groupe)
# Non rejet de H0, pas de différence significative entre les variances
#(de toute facon, ici n>30 dans chaque groupe donc il ne s'agissait pas d'une condition d'application au test).

# Test t de Student:
#Hypothèses du test de comparaison des moyennes
#H0: µ1 = µ2 
#H1: µ1 =! µ2
t.test(Datafish$Mesure~Datafish$Groupe,var.equal=T) # test de Student avec vérification du critère d'homoscédasticité
# par défaut: les variances ne sont pas egales
# la différence est significative => le regime alimentaire impacte la taille des poissons

#### attention!!!!! si n <30 et hétéroscédasticité, alors on ajoute
## l'argument var.equal = F : on conduit alors un test t de Welch avec ajustement des ddl.


# Si pour un petit echantillon les donnees ne sont ni gaussiennes, ni transformables,
# alors on peut conduire un test par re-echantillonnage -disponible dans les packages lmperm, resample, boot ...

# On peut aussi faire un test non parametrique, le test de Mann-Whitney, c'est plus frequent (mais moins puissant).
wilcox.test(Datafish$Mesure~Datafish$Groupe)
# Le warning indique juste que la p-value est calculee sans ajustement avec les valeurs ex-aequo ;
# ce n'est pas un message d'erreur !


##########
########## Test de Student pour 2 echantillons apparies
##########

#### EXERCICE DIRIGE

# On recupere le fichier Sommeil.csv, qui contient les mesures de la duree du sommeil d'etudiants avant et apres un examen
Sommeil <-read.csv("Sommeil.csv", sep=";", dec=",")
# On cherche a savoir si les etudiants dorment moins avant (qu'après) un examen

# Mesure = duree sommeil (quantitative continue) ; Facteur quali nominal a 2 modalites (avant ou ap l'examen), constituant 2 ech apparies
str(Sommeil)
# ou bien
dim(Sommeil) #les echantillons sont d'effectif < 30

boxplot(Sommeil[,2:3], las=1, col=c("darkgreen","orange")) # toutes les lignes, les colonnes 2 à 3, 
#las: orientation étiquette

# choix du test : t.test pour 2 ech apparies
# On verifie la normalite des differences (effectif n < 30)
 
#on crée une nouvelle variable correspondant à la différence entre les mesures de chaque paire
delta=Sommeil$Sommeil.Avant-Sommeil$Sommeil.Apres
delta
#vérification de la normalité de delta
shapiro.test(delta)
# non-rejet de H0

# Puis on fait le test t 
#H0: µ1 = µ2 
#H1: µ1 < µ2 => test unilatéral gauche => ajouter alternative="less" dans le t.test
t.test(Sommeil$Sommeil.Avant,Sommeil$Sommeil.Apres,
       paired=T,
       alternative = "less")
# On ne verifie pas l'egalite des variances car c'est un test a 1 echantillon "deguise"
## alternative = less pour faire le test en unilatéral (ou "greater" en fonction de H1); 
#en bilatéral, on peut préciser "two-sided"
### Bien regarder la formulation de l'intervalle de confiance en unilatéral

t.test(delta,mu=0, alternative = "less")
# C'est exactement la meme chose 

# La encore, en cas de petits echantillons non gaussiens, on peut aussi faire un test non parametrique.
# C'est le test des rangs signes de Wilcoxon, frequemment employe dans ce cas mais moins puissant qu'un test par re-echantillonnage.
wilcox.test(Sommeil$Sommeil.Avant,Sommeil$Sommeil.Apres,
            paired=T,
            alternative = "less")
# Concernant le warning : voir plus haut !
# difference significative

###### EXERCICES

### Exercice 3
### La ductilite (faculte de se deformer) a ete mesuree en determinant
### la pression necessaire pour briser/deformer plusieurs echantillons d'une roche, avant et apres chauffage.
### Le chauffage modifie t-il la ductilite ? Les donnees sont dans le fichier Ductilite.

ductilite<-read.csv("Ductilite.csv", sep=";",dec=",")
str(ductilite)
# Mesure : ductilite (ou pression, quanti continue)
# et facteur qualitatif nominal a deux modalites avant/apres chauffage (2 ech apparies)

# Exploration des donnees
boxplot(ductilite, col=c("orange", "violet"), ylab="Ductilite", las=1)

###################################################
# FACULTATIF - graphique avec ggplot2
# Passage du jeu de donnees au format long
library(tidyr)   # pour pivot_longer
ductilite_long <- ductilite |>
  pivot_longer(cols = c(preChauffage, postChauffage),
               names_to = "Condition",
               values_to = "Ductilite")
# Forcer l'ordre des modalites
ductilite_long$Condition <- factor(ductilite_long$Condition,
                                   levels = c("preChauffage", "postChauffage"))
#Graphique
ggplot(ductilite_long, aes(x = Condition, y = Ductilite, fill = Condition)) +
  geom_boxplot() +
  scale_fill_manual(values = c("orange", "violet")) +
  labs(x = "", y = "Ductilite") +
  theme_minimal()
###################################################

# Choix d'un test de Student de comparaison de 2 ech apparies
# H0 : µ_avant = µ_apres : pas de diff significative de la durete avant et apres chauffage : le chauffage ne modifie pas la durete
# H1 : µ_avant =! µ_apres : diff significative  (test bilateral)

# Conditions d'application : 
# comme n<30 on doit vérifier la normalité de la différence
# Test de shapiro 

ductilite$difference=ductilite$postChauffage-ductilite$preChauffage
#permet de rajouter une colonne

shapiro.test(ductilite$difference)
# rejet de H0 au risque alpha = 5% : les donnees ne sont pas issues d'une distribution normale

# Solution : transformer les donnes avec la racine carree pour chercher a les normaliser

racine=sqrt(ductilite$difference)
shapiro.test(racine)   #la transformation des variables les normalise

#H0: mu_D=0
#H1: mu_D=!0
t.test(racine, mu=0)   # on lance le test sur les donnees transformees!!
# rejet de H0 au risque alpha = 5% : le chauffage modifie la ductilite

# ou choix d'un test non parametrique sur la variable non transformée (mais moins puissant)
# H0 : pas de diff significative de la durete avant et apres chauffage : le chauffage ne modifie pas la durete
wilcox.test(ductilite$postChauffage, ductilite$preChauffage,paired=T)
# rejet de H0 au risque alpha = 5% : le chauffage modifie la ductilite => même résultat qu'avec le test paramétrique sur données normalisées


##########
########## Chi2 de conformite (comparaison a des donnees theoriques)
##########


#### EXERCICES DIRIGES

# Considerons le pelage de cobayes (gris ou blanc, lisse ou rude) ; on obtient les effectifs suivants apres un croisement,
# correspondant aux 4 categories "gris et lisse", "blanc et lisse", "gris et rude" et "blanc et rude"
# ici on a 1 variable qualitative nominale à 4 modalités: couleur/texture du pelage des cobaye / pas de facteur

# on génère le tableau de données
cobaye = c(50,20,22,5)


#Representation des donnees
barplot(cobaye,names=c("G & L","B & L","G & R","B & R"), ylab="Effectifs")
# names est ici un argument, pour donner un nom aux differentes modalites.

# si on veut voir les proportions
barplot(cobaye/sum(cobaye),names=c("G & L","B & L","G & R","B & R"), ylab="proportions observees")

# On veut savoir si la distribution observée s'ecarte ou non des proportions theoriques prevues par la genetique (lois de Mendel)
# qui sont les suivantes : 9/16 de "gris et lisse", 3/16 de "blanc et lisse", 3/16 de "gris et rude" et 1/16 de "blanc et rude"
prop = c(9/16,3/16,3/16,1/16)

# Conditions d'application ? Regle de Cochran

# H0 : distribution observee des pelages est conforme a la distribution theorique de Mendel  
# H1 : non conforme

#test du chi2 de conformité
chisq.test(x=cobaye,p=prop)
#p-value > alpha => H0 non rejetée

#Verification de la regle de Cochran:
# on va extraire les effectifs theoriques (ceux attendus sous H0) dans le test du chi-deux
# Pour cela on utilise le signe d'extraction $ suivi de expected ("attendus" en anglais).

chisq.test(x=cobaye,p=prop)$expected
# Règle de Cochran validée


##########
########## Chi2 de comparaison (sur table de contingence, d'independance / d'homogeneite)
##########

# La categorie sociale et le sentiment d'appartenir a une classe sociale sont-ils independants ?
# On rentre les donnees dans une table, a l'aide d'une nouvelle fonction : matrix
categorie=matrix(c(125, 190,588, 842, 1105, 888, 194, 300, 433, 694, 1227, 1024, 9, 6, 9, 10, 38, 45),
                 nrow = 6,
                 ncol = 3) #6 lignes, 3 colonnes
categorie

# Mettons des titres aux lignes, puis aux colonnes
rownames(categorie)=c("Pas du tout","Tres peu","Un peu","Partiellement","En grande partie","Completement")
colnames(categorie)=c("Basse","Moyenne","Superieure")
categorie # C'est plus clair

# On définit une mesure qualitative représentée par une variable quali ordinale: le sentiment d'appartenir a une classe sociale 
# qui existe en 6 modalités ordonnees permettant de définir 6 classes independantes
# et une mesure qualitative représentée par une variable quali ordinale: la catégorie sociale qui existe en 3
# 3 modalites permettant de définir 3 echantillons indep

# Representation graphique
barplot(categorie, beside=TRUE, col=rainbow(6))
barplot(categorie, col=rainbow(6), legend.text=FALSE, beside=TRUE, ylab="Effectifs")#barplot des effectifs observes
legend(15,1000,legend=rownames(categorie),cex=0.8,col=rainbow(6), pch=15)


# Choix du test : chi-deux d'independance (d'homogénéité / sur table de contingence)
# pour comparer les distributions entre elles = ou voir si l'échantillon influence les classes 
# ou si il y a indépendance entre classes et échantillons

# Condition d'application : Cochran

# hypotheses : H0 : independance entre categories sociale et sentiment d'appartenance à une classe = sentiment 
# d'appartenance est homogene quelle que soit la classe
#H1: ce n'est pas independant

# Il n'y a plus qu'a calculer le chi2, directement sur categorie
chisq.test(categorie,correct=F)
# correct=F empeche R d'effectuer par defaut la correction de continuite de Yates, injustifiee quand le critere de Cochran est valide, et inutile autrement
# La correction de continuite de Yates a pour objectif d'empecher une surestimation de la significativite d'un test quand on a de petits effectifs,
# mais inversement, elle risque d'empecher de detecter un effet quand il existe
# Utilisee (rarement) quand l'effectif total est petit

# Il y a un message d'alerte, qui indique qu'au moins un des effectifs theoriques est < 5. Voyons les effectifs theoriques :
chisq.test(categorie,correct=F)$expected
# Dans ce cas la c'est bon : 1 classe sur 18 a un effectif < 5, soit plus de 80% des ni théo > 5;
# la regle de Cochran est respectee

# Decision / conclusion : rejet de H0 au risque alpha = 5% ;
# il n'y a pas independance entre la classe sociale et le sentiment d'appartenir a une classe sociale
#le sentiment d'appartenir à une classe sociale dépend de la classe sociale à laquelle on appartient

#########

# Quand le critere de Cochran n'est pas respecte, on peut a la place utiliser le test exact de Fisher.

# Dans les donnees suivantes, il s'agit de performance a un test de memoire (+ ou -) par des abeilles traitees avec differents produits
memoire=matrix(c(0,10,3,7,8,1),2,3) # 2 lignes, 3 colonnes
rownames(memoire)=c("-","+")
colnames(memoire)=c("Controles","Produit 1","Produit 2")
memoire
# Echantillondefini par la performance ici variable qualitative nominale a deux modalites (+/-) 
# et classes définies par le traitement variable quali nominal a 3 modalites (controle, produit 1, produit 2)

# On souhaite savoir si traitement et performance des abeilles sont lies ; chi-deux d'independance
# Donnez H0 et H1
#H0: pas d'effet du traitement sur la mémoire => mémoire et traitement sont indépendants=> performance homogene quel que soit le traitement
#H1: mémoire et traitement sont liés
chisq.test(memoire,correct=F)
chisq.test(memoire,correct=F)$expected # Ce n'est pas bon, 50% des classes ont des effectifs < 5 ; condition d'application non respectee
fisher.test(memoire)
#rejet de H0 : traitement et performance sont lies


##########
########## Exercices sur les tests
##########

### Exercice 4
### Lors d'une etude sur la pollution bacterienne, la presence ou l'absence de salmonelles a ete recensee a partir d'echantillons d'eau
### preleves dans l'estuaire de 3 bassins europeens. Les analyses fournissent les resultats suivants:
###                         Rhin	Loire 	Seine
### Presence de salmonelles	8	    10	    16
### Absence de salmonelles	2	    11	    23

### La presence / absence de salmonelles est-elle homogene dans les 3 estuaires ?

# Echantillon défini par la variable quali nominale a deux modalites (presence/absence) et 
# les classes définies par la variable qualitative nominale a 3 modalites (estuaires)

# Conditions d'application (Cochran)

# Hypotheses : H0 : homogeneite => la distribution des salmonelles est homogène entre les sites
#              H1: la distribution des salmonelles n'est pas homogène entre les sites

# Construisons d'abord le tableau de contingence
salmonelles=matrix(c(8,10,16,2,11,23),2,3,byrow=TRUE)
# par défaut le remplissage d'une matrice se fait par colonne; si on veut un remplissage
#par ligne utiliser byrow=True
rownames(salmonelles) <- c("Présence de salmonelles","Absence de salmonelles")
colnames(salmonelles) <- c("Rhin","Loire","Seine")
salmonelles

# Representation graphique des donnees
barplot(salmonelles, beside=TRUE, col=c("red","lightblue"))
legend(1, 20,legend=rownames(salmonelles),cex=1,col=c("red","lightblue"), pch=15)

chisq.test(salmonelles, correct=FALSE)
chisq.test(salmonelles, correct=FALSE)$expected
# les donnees ne permettent pas de rejeter H0, la presence/asbence de salmonelles est homogene dans les 3 bassins




### Exercice 5
### Le couvert vegetal du Parc naturel de Yellowstone est compose a 25,8 % de peuplements feuillus, 38 % de peuplements mixtes,
### 25,8 % de resineux et 10,4 % de marecages. Au sein du parc, l'elan d'Amerique a ete localise par telemetrie a 511 reprises au cours d'une annee.
### Sur les 511 localisations, 118 se situaient dans les feuillus, 201 dans les peuplements mixtes, 110 dans les resineux et 82 dans les marecages. 
### L'elan d'Amerique frequente-il indifferemment les differents milieux du parc ?

# Mesure quali nominale a 4 modalites : milieux du parc ; pas de facteur
# Choix du test : chi-deux de conformite
# condition d'application : Cochran
# Hypotheses : H0 : l'elan frequente indifferemment les milieux du parc ; conformite de sa distribution obs a celle de la distribution des milieux
# H1 : l'elan ne frequente pas indifferemment les differents milieux du parc

elan=c(118,201,110,82)
parc=c("feuillus","mixtes","resineux","marecages")
obs=matrix(elan, nrow=1)
colnames(obs)=c("feuillus","mixtes","resineux","marecages")
obs

barplot(obs/sum(obs))

prop=c(0.258,0.38,0.258,0.104)
chisq.test(elan,p=prop)
chisq.test(elan,p=prop)$expected
# rejet de H0 au risque alpha = 5% ; l'elan ne frequente pas indifferemment les differents milieux du parc


##############################################################################################################
##############################################################################################################
####################################### FIN TP1 -TESTs



### Exercice 9
### On a mesure la temperature moyenne en Juin a Guayaquil (Equateur) entre 1951 et 1970.
### Les asterisques designent les annees ou le phenomene El Nino a ete observe dans l'ocean Pacifique.
### Annee	Temperature (°C)
### 1951*	   26.1
### 1952	   24.5
### 1953*	   24.8
### 1954	   24.5
### 1955	   24.1
### 1956	   24.3
### 1957*	   26.4
### 1958	   24.9
### 1959	   23.7
### 1960	   23.5
### 1961	   24.0
### 1962	   24.1
### 1963	   23.7
### 1964	   24.3
### 1965*	   26.6
### 1966	   24.6
### 1967	   24.8
### 1968	   24.4
### 1969*	   26.8
### 1970	   25.2
### Est-ce que la temperature en Juin a Guayaquil est significativement differente au cours des annees El Nino ?
###

# Mesure : temperature en Juin (quanti continue) et facteur qualitatif nominal a deux modalites : El Nino ou pas El Nino (2 ech independants)
# Choix du test : test t pour 2 ech independants de petites tailles
# Conditions : normalite et egalite des variances
# Hypotheses: H0 : µelnino = µn_elnino et H1 : µelnino =! µn_elnino

temp=c(26.1,24.5,24.8,24.5,24.1,24.3,26.4,24.9,23.7,23.5,24.0,24.1,23.7,24.3,26.6,24.6,24.8,24.4,26.8,25.2)
annee=c("ElNinio","Normal","ElNinio","Normal","Normal","Normal","ElNinio","Normal","Normal","Normal","Normal","Normal","Normal","Normal","ElNinio","Normal","Normal","Normal","ElNinio","Normal")
shapiro.test(temp[annee=="Normal"])
shapiro.test(temp[annee=="ElNinio"])
bartlett.test(temp~annee)
t.test(temp~annee,var.equal=T)#rejet de H0

# ou creer un fichier csv ou txt et l'importer dans R
ElNino <-read.csv("ElNino.csv", sep=";", dec=",")
boxplot(ElNino$Temperature~ElNino$Annee)
shapiro.test(ElNino$Temperature[ElNino$Annee=="normal"])
shapiro.test(ElNino$Temperature[ElNino$Annee=="ElNino"])
# ou avec la fonction tapply
tapply(ElNino$Temperature, ElNino$Annee, shapiro.test)
bartlett.test(ElNino$Temperature~ElNino$Annee)
t.test(Temperature~Annee, data=ElNino, var.equal=TRUE)

### Methode alternative (moins preferable car elle ne fait pas apparaitre les variables)
#elnino=c(26.1, 24.8, 26.4, 26.6, 26.8)
#n_elnino=c(24.5, 24.5, 24.1, 24.3, 24.9, 23.7, 23.5, 24.0, 24.1, 23.7, 24.3, 24.6, 24.8, 24.4, 25.2)
#shapiro.test(elnino)
#shapiro.test(n_elnino)
#var.test(elnino, n_elnino)
#t.test(elnino, n_elnino, var.equal=TRUE)



#####Exercices supplementaires####
### Exercice supp 1 - Illustration du paradoxe de Simpson

### Les patients souffrant de la maladie tres grave MG_7 sont operes soit par un robot, soit par un chirurgien.
### Les donnees concernant les patients gueris et non gueris suite a l'operation sont les suivantes :
### Robot : non gueri 469 ; gueri 70
### Chirurgien : non gueri 176 ; gueri 20

### 1) Quel est le taux de guerison lorsque le robot et le chirurgien interviennent ?
70/(469+70)#Robot
20/(176+20)#Chir
op=matrix(c(469,176,70,20),2,2)
rownames(op)=c("Robot","Chir")
colnames(op)=c("non gueri","gueri")
addmargins(op)

### 2) Y a t-il a premiere vue un lien entre la guerison et le fait d'etre opere par un robot ou un chirurgien ?
#H0: homogeneite des distributions des guerisons entre robot et chirurgien
#H1: il y a un lien entre guerison et pratique chirurgicale (entre robot et chirurgien)
# Le robot semble avoir un meilleur taux de guerison, meme si ce n'est pas significatif

chisq.test(op,correct=F)
#H0 non rejetée

### On souhaite affiner notre analyse en prenant en compte les differents cas d'intervention :
# les cas ou l'operation est tres compliquee et les cas d'operation un peu plus simples

### Cas simples
### Robot : non gueri 414 ; gueri 70
### Chir : non gueri 37 ; gueri 9
### Cas compliques
### Robot : non gueri 55 ; gueri 0
### Chir : non gueri 139 ; gueri 11
#3) Quel est le taux de guerison entre le robot et le chirurgien, en fonction des cas d'intervention ?
#Quand le cas est simple
simple=matrix(c(414,37,70,9),2,2)
rownames(simple)=c("Robot","Chir")
colnames(simple)=c("non gueri","gueri")
addmargins(simple)
70/(414+70)#robot
9/(37+9)#chir
#Quand le cas est complique
complique=matrix(c(55,139,0,11),2,2)
rownames(complique)=c("Robot","Chir")
colnames(complique)=c("non gueri","gueri")
addmargins(complique)
0/(0+55)#robot
11/(139+11)#chir
#En realite, que les cas soient simples ou compliques, le taux de guerison par le medecin est meilleur que le robot !

### 4) La repartition des patients gueris et non gueris est-elle la meme entre le robot et le chirurgien, en fonction des cas d'intervention ?
#Cas simple
chisq.test(simple,correct=F)
#Cas complique
chisq.test(complique,correct=F)
chisq.test(complique,correct=F)$expected
#La regle de Cochran n'est pas respectee
fisher.test(complique)

#La difference n'est significative que dans les cas compliques.

### 5) Conclure. Comment s'appelle le phenomene mis en evidence ?
#Le facteur "type d'intervention" correspond a un facteur de confusion, qui, lorsqu'il est pris en compte, inverse la perception du resultat.
#Il s'agit d'une illustration du paradoxe de Simpson
#Cela vient egalement du fait que la repartition des patients entre robot et chirurgien n'est pas homogene :
#Le medecin se voit confier plus de cas compliques, tandis que les cas simples sont davantage confies au robot.


##Exercice supp 2
### Le fichier Rythme contient des mesures de rythme cardiaque sur un lot de rats, avant et apres la prise d'un traitement cense traiter le rythme cardiaque.
### Le traitement a t-il un effet sur le rythme cardiaque ?
rythme<-read.csv("Rythme.csv", sep=";", dec=",")
str(rythme)
# Mesure : rythme cardiaque (quanti continue) et facteur quali a deux modalites (2 ech apparies)
# Choix du test : t.test pour 2 ech apparies)
# Ccondition d'application : normalite de la difference
#Exploration des donnees
boxplot(rythme, vertical=T,col=c("orange", "violet"), ylab="Rythme cardiaque")

# Test de shapiro sur la difference
shapiro.test(rythme$avant-rythme$apres)

# Test t pour echantillons apparies
# H0: µ_avant = µ_apres : le rythme cardiaque avant la prise du traitement n'est pas significativement different du rythme cardiaque apres la prise du traitement
# la difference observee n'est due qu'au hasard de l'echantillonnage
# H1: µ_apres =! µ_avant : le traitement a un effet sur le rytme cardiaque

t.test(rythme$apres, rythme$avant, paired=T)
# les donnees ne permettent pas de rejeter H0


### Exercice supp 3
### On considere deux echantillons: le premier correspond au rythme cardiaque de 20 cyclistes lors d'un test d'effort.
### Le deuxieme correspond au rythme cardiaque des memes cyclistes lors du meme test d'effort realise apres la prise de vitamines.
### On se demande si le rythme cardiaque sans vitamines est different de celui avec vitamines ; les donnees sont dans le fichier Cyclistes.
cyclistes <- read.csv("Cyclistes.csv", sep=";",dec=",")
str(cyclistes)
# Mesure : rythme cardiaque (quanti continue) et facteur qualitatif nominal a deux modalites (avant/apres, 2 ech apparies)
# Graphe
boxplot(cyclistes$sans_vitamines,cyclistes$avec_vitamines,main="cyclistes",ylab="Rythme cardiaque",names=c("sans vitamines","avec vitamines"))
# Choix du test (t.test pour apparies) et conditions d'application (normalite de la diff)
cyclistes$difference=cyclistes$sans_vitamines-cyclistes$avec_vitamines;head(cyclistes)
shapiro.test(cyclistes$difference)#normalite verifiee

# test de Student pour 2 ech apparies
# Hypotheses : µ_avant = µ_apres et µ_avant =! µ_apres
t.test(cyclistes$sans_vitamines,cyclistes$avec_vitamines, paired=TRUE)
# les donnees ne permettent pas de rejeter H0

# si on estime que l'effectif est trop faible pour conduire un test parametrique
# Test de Wilcoxon
wilcox.test(cyclistes$sans_vitamines, cyclistes$avec_vitamines,paired=T)

### Exercice supp 4
### On s'interesse aux nuages observes dans des villes du nord et du Sud de la France.
### On voudrait savoir si la repartition des nuages est la meme entre le Sud et le Nord de la France, dans le fichier Nuages. (Indication: utiliser la fonction table() sur le fichier)

nuages<-read.csv("Nuages.csv", sep=";",dec=",")
str(nuages)

# Mesure quali nominale a 3 modalites (Cumulus, Stratocumulus et absent) et facteur quali nominal a deux modalites (nord/sud)
# Choix d'un test du chi-deux d'homogeneite
# Condition d'application : Cochran
# Hypotheses : H0 : repartition homogene des nuages entre le Sud et le Nord
# Exploration des donnees
obs=table(nuages$Station, nuages$Nuages)
barplot(obs, beside=TRUE, legend.text = TRUE)
# test du chi-deux d'homogeneite
chisq.test(obs, correct=FALSE)
chisq.test(obs, correct=FALSE)$expected
# critere de Cochran non respecte
fisher.test(obs)

### Exercice supp 5
# Fichier = cidre.csv
# Le cidre brut presente-t-il une saveur sucree significativement plus faible que le cidre doux ?
# Et une saveur astringente significativement plus elevee ?
# Mesure de la saveur (quantitative continue) en fonction du facteur type de cidre (facteur qualitatif binomial)
# constituant deux grands echantillons independants
cidre=read.csv("cidre.csv", sep=";", dec=",")
summary(cidre)

par(mfrow=c(2,2)) # permet de separer la fenetre graphique en 2 lignes et 2 colonnes
boxplot(cidre$Sucree~cidre$Type,main="saveur sucree",col=rainbow(2))
boxplot(cidre$Acide~cidre$Type,main="saveur acide",col=rainbow(2))
boxplot(cidre$Amere~cidre$Type,main="saveur amere",col=rainbow(2))
boxplot(cidre$Astringente~cidre$Type,main="saveur astringente",col=rainbow(2))

# il semble y avoir une difference entre les cidres doux et bruts, notamment pour les notes sucrees et astringentes

# Comparons d'abord la note sucree entre les deux types de cidre
# n > 30, le TCL s'applique
t.test(Sucree~Type, data=cidre, var.equal=TRUE, alternative = "less")
#alternative "less" permet de procéder à un test unilatéral gauche
# p < 0,05, Le cidre brut presente une saveur sucree significativement plus faible que le cidre doux

# Saveur astringente
t.test(cidre$Astringente~cidre$Type, var.equal=TRUE, alternative = "greater")
# Le cidre brut presente une saveur astringente significativement plus elevee





############################################################################################################
############################################################################################################
######                 EXECRICES SUPPLEMENTAIRES CORRIGES NON TRAITES                              #########

### Exercice I-3       => comparaison 1 moyenne à une norme

### Un nouveau quartier a ete construit avec de nouveaux materiaux reflechissants. Une serie de mesures (fichier Albedo) a ete faite sur l'albedo des nouveaux batiments.
### Les materiaux habituels ont un albedo de 0.556. Les nouveaux materiaux sont-ils differents ?
### A partir du resultat obtenu, comment pouvez-vous savoir le nombre de mesures ?
data <-read.csv("Albedo.csv", sep=";", dec=",")
str(data)
which(is.na(data))
#La fonction  which  permet de ressortir les indices des éléments d'un objet (vecteur, dataframe, etc.) 
#satisfaisant une condition donnée. Ici, par exemple, elle nous permet de ressortir l'ensemble des indices 
#des éléments qui sont des NA (= not available => donnée manquante), dans notre dataframe.

# Mesure quantitative continue : albedo
# Representation graphique des donnees
hist(data$Albedo)

boxplot(data$Albedo)

#on regarde où se situe le point de comparaison 0,556 sur le boxplot
points(1,0.556, pch=16, col="blue", cex=1.5)
points(1,0.5976, pch=18, col="green", cex=1.5)
n=length(data$Albedo) ; n

mean(data$Albedo)

# choix du test (t.test a 1 ech) et conditions d'application (pas besoin de tester normalite car n = 144)
# H0 : µ = µ0 et H1 µ =! µ0
t.test(data$Albedo, mu=0.556)
# rejet de H0 au risque alpha = 5%

#on peut connaître le nb de valeurs de la variable mesurée à partir du ddl
# df = n-1

### Exercice I-4

### On a mesure la temperature corporelle de groupes de crabes dans de l'eau a 15°C et a 25°C (fichier crabe).
### Pour le moment, on suppose la normalite respectee dans chacun des groupes.
### Pour chaque groupe, verifier si la temperature moyenne des crabes est comparable a celle de l'eau.
datacrab <-read.csv("Crabes.csv", sep=";", dec=",")
str(datacrab)

boxplot(Temp.Crabes~Temp.Eau, data = datacrab, col=c("pink","skyblue"))

# Mesure quantitative continue : temperature corporelle des crabes
# facteur: température d'élevage considérée comme qualitative ordinale 
# choix du test : t.test a 1 ech

# conditions d'application : n=16 donc il faut tester la normalite 
shapiro.test(datacrab$Temp.Crabes[datacrab$Temp.Eau=="15 degres"])

# H0 : µ = µ0 et H1 µ =! µ0
t.test(datacrab$Temp.Crabes[datacrab$Temp.Eau=="15 degres"], mu = 15)
# rejet de H0 au risque alpha = 5%

# conditions d'application : n=16 donc il faut tester la normalite 
shapiro.test(datacrab$Temp.Crabes[datacrab$Temp.Eau=="25 degres"])

t.test(datacrab$Temp.Crabes[datacrab$Temp.Eau=="25 degres"], mu = 25)
# rejet de H0 au risque alpha = 5%
