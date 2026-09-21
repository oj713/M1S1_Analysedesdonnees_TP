

#####################################################
#####################################################
##                                                 ##
##                   MODULE 4U001                  ##
##                                                 ##
##         TP 3 : regression simple et ANOVA       ##
##                                                 ##
#####################################################
#####################################################


##################################
##  1ere partie : regression    ##
##################################

# On souhaite etudier la relation entre des parametres morphologiques de fleurs ;
# La mesure est la hauteur en cm (mesure quantitative continue),
# et le facteur est la taille de l'opercule en cm (facteur quantitatif continu).
# On va donc pouvoir effectuer une regression simple.
# Les donnees sont dans COR.csv
# Commencons par representer graphiquement les donnees, avec la fonction plot().
plot(x = COR$opercule, y = COR$hauteur, xlab="opercule", ylab="hauteur")

# La relation semble bien lineaire, mais les premiers points semblent mal decrits par le modele.
# Un des exercices proposera une meilleure approche.

# On lance l'analyse par regression lineaire simple avec la fonction lm() :
reg=lm(hauteur~opercule,data=COR)
summary(reg)
# On peut aussi obtenir l'analyse par table ANOVA (mais le modele reste une regression)
anova(reg)

# Les coefficients sont significatifs ;
# l'intercept donne la hauteur theorique pour un opercule de 0.
# R2 = 0,669 : 66.9% des variations de la hauteur
# sont rendues compte par la regression avec l'opercule
# (R2 = coefficient de determination)
# Le modele est globalement significatif (test F, p < 2.2e-16)
# L'equation est hauteur = 1.54*opercule + 4.12 + epsilon
# ou hauteur_predit = 1.54*opercule + 4.12

# Ajoutons la droite au graphique :
abline(reg, col="red")

###################################################
# FACULTATIF : ggplot2
ggplot(COR, aes(x = opercule, y = hauteur)) +
  geom_point(shape = 1, size = 3, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5) +
  labs(
    x = "Opercule (cm)",
    y = "Hauteur (cm)"
  ) +
  theme_classic() +
  theme(
    panel.grid = element_blank()
  )

# autre option : utiliser geom_abline(intercept = coef(reg)[1], slope = coef(reg)[2], color = "red", linewidth = 0.5)
# a la place de geom_smooth
###################################################

# On peut recuperer les coefficients, les valeurs predites et les residus :
reg$coefficients
reg$fitted.values
reg$residuals

# Pour finir l'analyse de la regression, on affiche les graphiques diagnostiques
par(mfrow=c(2,2))
# La fonction par() permet de gerer des parametres graphiques ;
# en l'occurence ici, d'avoir 4 petits graphiques dans une fenetre 2*2
plot(reg)

# Comme prevu, les premiers points sont mal decrits par le modele.
# (voir exercice 3 pour une meilleure approche).

###################################################
# FACULTATIF : les graphiques diagnostiques façon ggplot
# avec le package ggfortify
library(ggfortify)
# Graphiques diagnostiques façon ggplot2
autoplot(reg)
autoplot(reg, colour = "blue", shape = 1, size = 2, label.size = 3) +
  theme_classic()
###################################################


####
#### Regression à deux facteurs
####

# On cherche a savoir si les rejets en CO2 (mesure quantitative continue)
# dependent de la puissance (facteur quantitatif continu) 
# et des emissions d'oxyde d'azote (NOx) de la voiture (facteur quantitatif continu).
# Notez qu'on ne peut pas regarder les emissions de CO2 en fonction de la consommation et de la puissance,
# car ces facteurs ne sont pas independants.

# Importez les donnees Voiture2.csv

# La regression a deux facteurs se conduit egalement avec la fonction lm(),
# et les deux facteurs sont separes par * (cela prend en compte leur interaction).
reg1 = lm(co2 ~ Puissance * nox, data=Voiture2)
anova(reg1)

# L'interaction n'est pas significative,
# voyons le modele sans cette interaction
reg2 = lm(co2 ~ Puissance + nox, data=Voiture2)
anova(reg2)
summary(reg2)

# Comparons les deux modeles :
anova(reg1,reg2)
# Pas de difference significative, on garde donc le modele le plus simple

# Les coefficients sont significatifs :
# les emissions de CO2 dependent de la puissance et des emissions de NOx ;
# elles diminuent avec les emissions de NOx (pente < 0), mais augmentent avec la puissance (pente >0)
# l'intercept indique la valeur theorique d'emission de CO2 d'une voiture
# pour une puissance nulle et des emissions de NOx nulles
# Adjusted R2 = 0.5644 : 56,4% des variations des emissions de CO2 sont rendues compte
# par la regression en fonction de la puissance et des emissions de NOx.
# Le modele est gobalement significatif (test F, p < 2.2e-16).
# L'equation s'ecrit : CO2 = 77.48 + 0.56*Puissance -107.35*NOx + epsilon
# (ou CO2_predit = 77.48 + 0.56*Puissance -107.35*NOx)

# Verifions les conditions d'application de la regression (choix du modele sans l'interaction)
plot(reg2)



###############################
##  2eme partie : exercices  ##
###############################

###
# EXERCICE 1
###
# On cherche a savoir si la vitesse influence le temps de freinage d'une serie de voitures.
# Les donnees sont dans le fichier Voitures.csv
# Vous donnerez l'equation du modele.

###
# EXERCICE 2
###
# Essayons de faire un polynome plutot qu'une droite sur les donnees COR.
# On pourra s'orienter vers un polynome du second degre de type y = a1*X + a2*X^2 + b.
# Pour cela, faire un changement de variables en posant X2=carre de l'opercule, et l'ajouter a la regression.
# Ne pas prendre leur interaction, ca n'a pas de sens puisqu'en fait il ne s'agit que d'une seule variable !!!
# Tester quel est le meilleur modele.

# Pour ajouter la courbe au graphique, il faut trier les donnees et utiliser la fonction lines
# lines(sort(x),sort(y)), y etant les donnees ajustees (fitted values)



#############################
##  3eme partie : L'ANOVA  ##
#############################

# On souhaite comparer le poids moyen de 3 groupes de plantes soumises a des traitements differents (fichier Plantes.csv).
# Le traitement a-t-il un effet sur le poids des plantes ?

# Donnez la mesure et le facteur.
# Importez les donnees et representez les graphiquement.

# Pour comparer les moyennes de ces 3 groupes, on va conduire une ANOVA a 1 facteur.
# Posez H0 et H1.

# L'ANOVA peut se realiser avec la fonction aov.
# On stocke l'analyse dans un objet,
# pour faire appel ensuite a la fonction summary sur cet objet.

test=aov(poids~group, data=Plantes)
# Notez l'usage de l'argument data= qui permet de se dispenser d'ecrire Plantes$

summary(test)
# Vous retrouvez tous les elements de la table d'ANOVA vus en cours.
# Que concluez-vous ?

## Cherchons l'origine de la difference observee par un test post-hoc (test a posteriori) :
#par exemple, le test HSD de Tukey
TukeyHSD(test)
plot(TukeyHSD(test)) # intervalles de confiance de la difference des moyennes de 2 modalites
# les moyennes sont significativement differentes lorsque l'intervalle de confiance n'inclut pas la valeur 0
# Interpretez les resultats obtenus.

# Il existe d'autres test post-hoc, disponibles dans le package DescTools.
# A installer au prealable sur vos ordinateurs
# ATTENTION :  c'est deja fait sur les ordinateurs de l'UTES, ne pas le re-installer
# Onglet "Packages" en bas a droite
# Install puis taper le nom du package dans "Packages"
# ou executer : install.packages("DescTools")
# Une fois le package installé,
# il faut le charger pour pouvoir l'utiliser, avec la fonction `library`.
library(DescTools)

# Test de Scheffe
# Un peu moins sensible que le test de Tukey (utilisez plot pour le constater),
# mais plus robuste et preferable quand les effectifs sont tres differents
ScheffeTest(test)

# Test de Dunnett
# Compare uniquement au groupe controle. A utiliser si c'est ce que vous voulez faire.
DunnettTest(poids ~ as.factor(group), data=Plantes) 
# Notez que le test de Dunnett ne peut pas prendre test comme argument ;
# et notez aussi qu'il est necessaire de faire as.factor sur le groupe

## Verification des conditions d'application
# Normalite des residus

# Normalite des residus par visualisation graphique
# QQ-plot (graphe quantiles-quantiles)
plot(test, 2)

# Remarque 1 : cette condition d'application est souvent testee directement sur les donnees
# Normalite de la variable "poids" dans chaque groupe :
shapiro.test(Plantes$poids[Plantes$group=="ctrl"])
shapiro.test(Plantes$poids[Plantes$group=="trt1"])
shapiro.test(Plantes$poids[Plantes$group=="trt2"])
# Ou avec la fonction tapply
tapply(Plantes$poids, Plantes$group, shapiro.test)


# Variances constantes
# Visualisation graphique : Residus vs valeurs predites
plot(test, 1) # il n'y a pas de relation evidente entre les residus et les valeurs predites

###################################################
# FACULTATIF : les graphiques diagnostiques avec autoplot de ggfortify
autoplot(test, which = c(2,1)) +
  theme_classic() +
  theme(panel.grid = element_blank())
###################################################


# Remarque 2 : cette condition d'application est aussi souvent testees directement sur les donnees
# test de l'egalite des variances par un test de Bartlett:
bartlett.test(poids~group,data=Plantes)

# Remarque 3 : Si les residus ne sont pas gaussiens : anova par permutation par la fonction aovp du package lmperm
# install.packages(lmPerm)
library(lmPerm) #charger le package qui a ete installe

testp = aovp(poids~group,data=Plantes)
summary(testp)
# si on conduit le test plusieurs fois, la p-value calculee change legerement a chaque fois

# Remarque 4 : si l'homogeneite des variances n'est pas verifiee,
# on peut conduire une ANOVA de Welch avec la fonction oneway.test
oneway.test(poids ~ group, data = Plantes) # l'argument ici par defaut est var.equal=FALSE 
# mettre var.equal=TRUE pour retrouver le resultat obtenu plus haut avec aov().


###### ANOVA a deux facteurs
# Source : site Internet STHDA
# data inclues dans R : ToothGrowth.
# Ce jeu de donnees decrit la croissance des dents de cochons d'Inde,
# en fonction de differentes doses de vitamine C et de 2 formats d'administration de cette vitamine : jus d'orange ou acide ascorbique
Dents <- ToothGrowth
summary(Dents)

# Quelle et la mesure et quels sont les facteurs ?

# Convertir la variable dose en facteur et renommner les niveaux du facteur en "D0.5", "D1", "D2"
Dents$dose <- factor(Dents$dose, levels = c(0.5, 1, 2), labels = c("D0.5", "D1", "D2"))

# Verifions si les effectifs sont equilibres
table(Dents$supp,Dents$dose)

# Visualisation des donnees
boxplot(len ~ supp*dose, data=Dents,
        frame=FALSE,
        col=c("blue","green"),
        ylab="Tooth Length")

###################################################
# FACULTATIF : avec ggplot2
ggplot(Dents, aes(x = interaction(supp, dose), y = len, fill = supp)) +
  geom_boxplot() +
  labs(
    x = "Supplement et Dose",
    y = "Tooth Length"
  ) +
  scale_fill_manual(values = c("blue", "green")) +
  theme_classic()

# separer OJ et VC dans le graphe avec facet_wrap
ggplot(Dents, aes(x = factor(dose), y = len, fill = supp)) +
  geom_boxplot() +
  labs(
    x = "Dose",
    y = "Tooth Length"
  ) +
  scale_fill_manual(values = c("blue", "green")) +
  facet_wrap(~ supp) +   
  theme_classic()

###################################################
# FACULTATIF : interaction plot
interaction.plot(x.factor = Dents$dose, trace.factor = Dents$supp, 
                 response = Dents$len, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "", trace.label ="",
                 ylab="Tooth Length",
                 pch=c(1,19), col = c("blue", "green"))
###################################################

# ANOVA a 2 facteurs sous R : fonction aov, en separant les 2 facteurs par *
res.aov = aov(len ~ supp * dose, data = Dents)
res.aov = aov(len ~ supp + dose + supp:dose, data = Dents)
# 2 facons d'ecrire la meme chose

summary(res.aov)

# Remarque : si les deux facteurs ont des effets independamment l'un de l'autre
# alors ils n'ont pas d'interaction ;
# remplacer * par + dans aov, et l'interaction des facteurs ne sera pas prise en compte


# Verification des conditions d'application : graphiques diagnostiques
plot(res.aov, 1)
plot(res.aov, 2)

###################################################
# FACULTATIF : les graphiques diagnostiques avec autoplot de ggfortify
autoplot(res.aov, which = c(2,1)) +
  theme_classic() +
  theme(panel.grid = element_blank())
###################################################

# On peut aussi tester les conditions d'application directement sur les données
# Creer un facteur correspondant a l'interaction,
# utile dans ce cas-la pour avoir acces a tous les groupes
SuppDose=interaction(Dents$supp,Dents$dose)
# On peut verifier la normalite sur les donnees
tapply(Dents$len,SuppDose,shapiro.test)
# Verification des conditions d'application : homogeneite des variances
bartlett.test(Dents$len ~ SuppDose)


###
# EXERCICE 3
###
# On a place des patelles (mollusque marin) dans differentes dilutions d'eau de mer (donc differentes conditions de salinite)
# et on a mesure leur consommation en oxygene.
# Les donnees sont dans le fichier Patelles.csv ; y a t-il un effet de la salinite sur la consommation en oxygene ?



