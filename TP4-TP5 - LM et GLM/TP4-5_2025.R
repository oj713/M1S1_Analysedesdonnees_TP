

#####################################################
#####################################################
##                                                 ##
##                   MODULE 4U001                  ##
##                                                 ##
##    Modeles lineaires general et generalise      ##
##                                                 ##
#####################################################
#####################################################


#############################################
##  1ere partie : modele lineaire general  ##
#############################################

#########
#  ANCOVA
#########

# C'est une regression multiple avec un ou plusieurs facteurs qualitatifs.
# On peut voir l'ANCOVA comme un mix entre regression multiple et ANOVA.

#### Considerons les donnees Tourbieres.csv (attention le separateur decimal est le point).
# Elles rapportent la surface de tourbieres (en km2)
# en fonction du lieu (versant ou vallee) et de la mineralisation (en µS/cm).

Tourbieres=read.csv("Tourbieres.csv", sep=";", dec=".")

# Donnez la mesure et le(les) facteur(s), et justifiez le choix d'une ANCOVA.

# Graphique
plot(Surface ~ Mineralisation, data = Tourbieres,
     pch=as.numeric(as.factor(Tourbieres$Lieu)),
     col=as.factor(Tourbieres$Lieu),
     cex.axis=0.7, cex.lab=0.9)
# Pour ajouter la legende sur le graphe
legend(40,4.5, legend=c("Vallee", "Versant"),
       pch=c(1,2), col=c(1,2), cex = 0.8, text.font=3)

# On va verifier que les facteurs sont independants (condition de non-colinearite) :
# la mineralisation est-elle independante du lieu ? 
t.test(Tourbieres$Mineralisation ~ Tourbieres$Lieu, var.equal=T) # Notons que l'on a de grands effectifs
# Que concluez-vous ?

# Lançons l'analyse pour l'ANCOVA : on utilise la fonction dediee aux modeles lineaires generaux,
# la fonction lm.
reg=lm(Surface ~ Lieu * Mineralisation, data=Tourbieres)
anova(reg)
# Les deux facteurs et leur interaction ont un effet significatif sur la surface des tourbieres.

summary(reg)
# Tous les coefficients sont significatifs
### Cas de la reference
# La modalite Vallee sert de reference ici.
# L'intercept pour Vallee (0,373) indique la surface theorique de tourbiere pour une mineralisation nulle,
# lorsque la tourbiere se situe dans une vallee.
# La troisieme ligne (Mineralisation) correspond a l'effet de la mineralisation sur la surface de la tourbiere
# lorsqu'elle est dans la vallee. Ce coefficient est significatif et positif : la surface augmente avec la mineralisation.

### Effet du lieu sur la surface
# On teste ensuite l'effet sur l'intercept lorsqu'on se situe sur un versant, par rapport a la vallee : ligne LieuVersant (-0.364);
# ce coefficient est < 0 : la surface d'une tourbiere est plus petite si elle se situe sur un versant.
# ce coefficient va s'ajouter a l'intercept deja trouve pour Vallee : 0.373-0.364

### Interaction des deux facteurs
# La quatrieme ligne rend compte de l'interaction entre lieu et mineralisation :
# la pente de la relation entre surface et mineralisation diminue
# lorsque la tourbiere est sur un versant plutot qu'en vallee (coefficient = -0.019) ;
# le coefficient reste cependant > 0 (0.034 - 0.019 = 0.015),
# la surface d'une tourbiere augmente egalement avec la mineralisation lorsqu'elle est sur un versant,
# mais elle augmente moins que lorsqu'elle se situe dans la vallee.

### R2 ajuste et significativite globale du modele
# Comme il y a plusieurs facteurs, le R2 ajuste est employe, soit une variance en commun a 99%.
# 99% des variations de la surface d'une tourbiere
# sont rendus compte par le lieu où elle se trouve et la mineralisation du sol.
# La regression est globalement significative (test F, p-value = 5.1e-12).

### Mise en equation
# En introduisant une variable muette Ve (qui vaut 1 si on est sur un versant, et 0 sinon), l'equation est :
# Surface_predite = 0.373-0.364*Ve + Mineralisation * 0.034-0.019*Ve
# ou encore :
# Surface = 0.373-0.364*Ve + Mineralisation * (0.034-0.019*Ve) + epsilon

# Alternativement, on peut ecrire que la surface predite de tourbieres est :
# Surface_predite = 0.009 + Mineralisation * 0.015 sur les versants, et
# Surface_predite = 0.373 + Mineralisation * 0.034 dans les vallees

### Conclusion
# On peut conclure que la surface d'une tourbiere est liee a la mineralisation du sol, elle augmente avec celle-ci.
# Il y a un effet du lieu sur cette surface : la surface est plus petite sur un versant que dans la vallee.
# Par ailleurs, la relation entre surface et mineralisation est differente selon le lieu (interaction des deux facteurs) :
# la surface d'une tourbiere augmente moins avec la mineralisation
# si elle se situe sur le versant que si elle se situe dans la vallee.

### Graphiques diagnostiques :
par(mfrow=c(2,2))
plot(reg)
# Les conditions d'application sont respectees.

### Droites de regression
# Relancons le graphique :
par(mfrow=c(1,1))
plot(Surface ~ Mineralisation, data = Tourbieres,
     pch=as.numeric(as.factor(Tourbieres$Lieu)),
     col=as.factor(Tourbieres$Lieu),
     cex.axis=0.7, cex.lab=0.9)
legend(40,4.5, legend=c("Vallee", "Versant"),
       pch=c(1,2), col=c(1,2), cex = 0.8, text.font=3)

# On peut afficher les coefficents pour se reperer avant de tracer les droites
coef(reg)

# Pour tracer les droites, on utilise la fonction abline, avec les coefficients de l'analyse reg :
abline(coef(reg)[1],coef(reg)[3],col="black") # droite pour les vallees
abline(coef(reg)[1]+coef(reg)[2],coef(reg)[3]+coef(reg)[4],col="red") # droite pour les versants
# Il est important de bien comprendre de quelle façon les coefficients s'additionnent.

###################################################
# FACULTATIF : ggplot 2
library(ggplot2)
ggplot(Tourbieres, aes(x = Mineralisation, y = Surface, 
                       shape = Lieu, color = Lieu)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  labs(x = "Minéralisation (µS/cm)", y = "Surface (km2)") +
  scale_shape_manual(values = c(1, 2)) +   
  scale_color_manual(values = c("black", "red")) +  
  theme_classic() +
  theme(
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 9)
  )
library(ggfortify)
autoplot(reg)
###################################################


### Remarque
# Comme tout etait significatif y compris l'interaction, il n'y avait pas lieu de retirer l'interaction
# On peut s'en assurer :
reg2=lm(Surface ~ Lieu+Mineralisation,data=Tourbieres)
summary(reg2)
# Tout est significatif ! mais verifions les conditions d'application du modele :
par(mfrow=c(2,2))
plot(reg2)
# Il y a un probleme de linearite, l'interaction semble necessaire pour l'eviter
# Comparons les deux modeles :
anova(reg,reg2) # Difference tres significative, on garde reg qui a un meilleur R2 : 0.990 contre 0.968


#### Utilisons maintenant les donnees Porcelet.
# On etudie l'effet d'un traitement hormonal sur le poids de la glande thyroide chez des porcelets.
# Comme ce poids depend aussi du poids de l'animal, on l'ajoute dans notre regression.

Porcelet=read.csv("Porcelet.csv", sep=";", dec=".")
# Attention, le marqueur decimal est ici le point
# On etudie l'effet d'un traitement hormonal sur le poids de la glande thyroide chez des porcelets.
# Comme ce poids depend aussi du poids de l'animal, on l'ajoute dans notre regression.

# Donnez la mesure et le(les) facteur(s), et justifiez le choix d'une ANCOVA.

Porcelet$Traitement = as.factor(Porcelet$Traitement) # facteur "Traitement" si necessaire

# Graphique
par(mfrow=c(1,1))
plot(Thyroide ~ Poids, data = Porcelet,
     pch=as.numeric(as.factor(Porcelet$Traitement)),
     col=as.factor(Porcelet$Traitement))

legend(31,5.5, legend=c("Controle","Hormone"),
       pch=c(1,2), col=c(1,2), cex = 0.9, text.font=3)

# On va verifier que les facteurs sont independants (condition de non-colinearite) :
# le poids de l'animal est-il independant du traitement ?
tapply(Porcelet$Poids, Porcelet$Traitement, length) #2 echantillons independants de petites tailles
norm = tapply(Porcelet$Poids, Porcelet$Traitement, shapiro.test) ; sapply(norm,"[[","p.value")
bartlett.test(Porcelet$Poids ~ Porcelet$Traitement)
t.test(Porcelet$Poids ~ Porcelet$Traitement, var.equal=TRUE)
# Que concluez-vous ?

# Lançons l'analyse pour l'ANCOVA, avec la fonction lm :
reg1 = lm(Thyroide ~ Poids * Traitement, data = Porcelet)
# Equivalent a :
reg1 = lm(Thyroide ~ Poids + Traitement + Poids:Traitement, data = Porcelet)
anova(reg1)
# L'interaction entre le poids de l'animal et le traitement 
# n'a pas d'effet significatif sur le poids de la glande thyroide 

# Essayons alors un modele sans l'interaction
reg2 = lm(Thyroide ~ Poids + Traitement, data = Porcelet)
anova(reg2) # effet du poids de l'animal et du traitement sur le poids de la thyroide
summary(reg2)

# Remarque : la fonction step
# la fonction step permet d'optimiser le modele.
# step() va avancer pas a pas pour supprimer des coefficients
# et reoptimiser les autres sans que la regression ne perde de sa qualite
reg_step=step(reg1)
summary(reg_step) # le modele pertinent est celui sans l'interaction

### Cas de la reference
# Le traitement "Controle" sert de reference ici.
# Cela correspond a ce qu'on veut, sinon utiliser la fonction relevel.
# Porcelet$Traitement = relevel(Porcelet$Traitement, ref = "Controle")
# L'intercept pour les controle n'est pas significativement different de 0.
# La deuxieme ligne (Poids) est la pente de la droite Thyroide ~ Poids
# ici quel que soit le traitement
# (puisque l'interaction n'est pas prise en compte), soit 0.313.
# Cette pente est > 0, le poids de la thyroide augmente significativement avec le poids de l'animal.

### Effet du traitement sur le poids de la thyroide
# On teste ensuite l'effet sur l'intercept du traitement Hormone, par rapport a Controle :
# ligne TraitementHormone (2.140). Cela correspond au poids theorique de la thyroide d'un animal de poids nul.
# Ce coefficient devrait s'ajouter a l'intercept deja trouve pour les Controle
# qui lui n'est pas significativement different de 0.
# le poids de la thyroide est donc plus grand sous traitement que sans traitement.

### R2 ajuste et significativite globale du modele
# Comme il y a plusieurs facteurs, le R2 ajuste est employe, soit une variance en commun a 75,1%.
# La regression est globalement significative (test F, p-value = 5.1e-12).

### Mise en equation
# L'equation est alors pour les controle :
# Thyroide = 0,313 * Poids + epsilon

# L'equation est alors pour le groupe traite aux hormones :
# Thyroide = 0.313 * Poids + 2.140 + epsilon

### Droites de regression
# droite de regression pour Controle
abline(coef(reg2)[1:2], col="black")

# droite de regression pour Hormone
# pour l'intercept, il faut ajouter coef(reg2[1]) + coef(reg2)[3]
# il n'y a pas d'interaction, donc la pente est la meme que pour Controle
abline(a = coef(reg2)[1]+coef(reg2)[3], b = coef(reg2)[2], col="red")

### Conclusion
# On voit bien sur le graphique que les pentes sont les memes,
# et que l'ordonnee a l'origine varie avec le traitement hormone.
# On peut conclure que la thyroide est liee au poids des porcelets: le poids de la thyroide augmente significativement avec le poids de l'animal.
# Il y a un effet significatif du traitement : la thyroide est plus lourde avec traitement, 
# sans interaction avec le poids de l'animal

### Graphiques diagnostiques
par(mfrow=c(2,2))
plot(reg2)
# Les conditions d'application du modele sont respectees.

###################################################
# FACULTATIF : ggplot2
ggplot(Porcelet, aes(x = Poids, y = Thyroide,
                     shape = Traitement, color = Traitement)) +
  geom_point(size = 2) +
  geom_abline(intercept = coef(reg2)[1], slope = coef(reg2)[2], color = "black", linewidth = 0.5)+
  geom_abline(intercept = coef(reg2)[1]+coef(reg2)[3], slope = coef(reg2)[2], color = "red", linewidth = 0.5)+
  labs(x = "Poids (g)", y = "Thyroide (g)") +
  scale_shape_manual(values = c(1, 2)) +   
  scale_color_manual(values = c("black", "red")) +  
  theme_classic()
autoplot(reg2)
###################################################

###
# EXERCICE 1
###
# On mesure le poids de crabes en fonction de la concentration d'un polluant pour 3 sites differents.
# On se questionne sur l'effet de la concentration en polluant sur le poids, et si cette relation varie en fonction du site.
# Donnees : PolluCrabe.csv


###
# EXERCICE 2
###
# Differentes doses d'auxines ont ete administrees a des plantes normales ou mutantes, et on a mesure leur taille.
# Les donnees sont dans le fichier Auxine.csv.
# Analyser les donnees a l'aide de la fonction lm().
# Reflechir a l'interpretation des coefficients et ecrire les equations.
# Indication : ici il n'y a pas de pente, uniquement des intercepts
# (graphiquement, on emploie des boxplot plutot que des nuages de points).



################################################
##  2eme partie : modele lineaire generalise  ##
################################################

###### Regression logistique
# Nous prendrons d'abord l'exemple de la regression logistique,
# qu'on emploie pour des mesures binaires/donnees binomiales.
# L'interface et l'interpretation des resultats ressemblent a ce que nous avons deja vu,
# mais les mathematiques sous-jacentes sont differentes.

# Des larves d'insectes sont exposees a differentes doses de pesticides.
# On s'interesse a leur survie en fonction de leur sexe et du log de la dose du produit.
# Il y a 20 males et 20 femelles pour chaque dose. Les donnees sont dans le fichier Larves.
Larves=read.csv("Larves.csv", sep=";", dec=",")

# Donnez la mesure et le(les) facteur(s), et justifiez le choix d'une régression logistique.

# Representons graphiquement les donnees
LogDose = rep(0:5, 2)
Survie = tapply(Larves$Survie, interaction(Larves$LogDose, Larves$Sexe), mean)

plot(LogDose, Survie, pch=c(rep(1,6), rep(2,6)), col=c(rep(1,6), rep(2,6)))
legend(.5,.3, legend=c("Femelles","Mâles"), pch=c(1,2), col=c(1,2), cex = 0.9, text.font=3)

# Regression logistique
glm_1 = glm(Survie ~ LogDose * Sexe, family=binomial, data=Larves)
# family permet de dire quel type de glm on veut, ici une regression logistique
anova(glm_1, test = "Chisq")
# Les facteurs sont significatifs, mais pas l'interaction
summary(glm_1) #AIC = 219.48

# Testons un modele sans interaction
glm_2 = glm(Survie ~ LogDose + Sexe, family = binomial, data = Larves)
anova(glm_2, test = "Chisq")
summary(glm_2) # AIC = 219.24

# Comparons les deux modeles
anova(glm_1, glm_2, test="Chisq") # Notez l'usage d'une loi de khi2
# la difference n'est pas significative, les AIC sont equivalents
# Gardons le modele le plus simple : glm_2

# Notez qu'il n'y a pas de graphique diagnostique
# pour une regression logistique

# Quelle est alors l'equation du modele ?

# Dans une regression logistique,
# le signe du coefficient indique si le facteur
# augmente ou diminue la probabilite de l'evenement.
# Les femelles sont prises comme references.

# L'intercept est positif, ce qui signifie que la probabilite d'etre vivante pour des larves femelles
# et une dose de 0 est > 50% (puisque 3.47 > 0).

# La pente est negative, la probabilite d'etre vivante pour une larve femelle diminue
# avec le log de la dose  (puisque -1.06 < 0)

# Pour les males, la pente ne change pas (pas d'interaction),
# mais l'ordonnee a l'origine (SexeM) change significativement :
# le coefficient = -1.10 indique qu'ils survivent moins que les femelles ;
# mais comme 3.47 - 1.10 = 2.37 > 0, leur taux de survie pour une dose nulle est plus grand que 50%.

# L'equation s'ecrit ainsi :
# pour les femelles : ln[p(etre vivant)/p(etre mort)] = -1.06*LogDose + 3.47 + epsilon
# pour les males : ln[p(etre vivant)/p(etre mort)] = -1.06*LogDose + (3.47-1.10) + epsilon
# Les signes s'inversent si on inverse le rapport par ln[p(etre mort)/p(etre vivant)]

# On peut aussi ecrire : p(etre vivant) = exp(-1.06*LogDose + 3.47) / (1 + exp(-1.06*LogDose + 3.47) ) + epsilon
# pour les femelles
# et pour les males on remplacerait juste 3.47 par 3.47 - 1.10 = 2.37
# ce qui donne dans les deux cas une courbe sigmoide.

# Autre facon d'ecrire l'equation :
# p(etre vivant) = exp(-1.06*ldose + 3.47 - 1.10sexM) / (1 + exp(-1.06*ldose + 3.47 - 1.10sexM) ) + epsilon
# avec sexM = 1 si male, 0 si femelle


# Completons notre graphique
# Il faut calculer au prealable les resultats :
d = 0:5
f = exp(-1.06*d + 3.47) / (1 + exp(-1.06*d + 3.47) )
m = exp(-1.06*d + 2.37) / (1 + exp(-1.06*d + 2.37) )

lines(d,f,col=1) # attention, lines ne marche que si la valeur x est triee (sinon tout est melange)
lines(d,m,col=2)

# On peut egalement se servir de la fonction ggeffects pour tracer le graphique
library(ggeffects)
plot(ggpredict(glm_2,c("LogDose","Sexe")))

###################################################
# FACULTATIF : ggplot 2
newdat <- expand.grid(
  LogDose = seq(min(Larves$LogDose), max(Larves$LogDose), length.out = 6),
  Sexe = unique(Larves$Sexe)
)
# Predictions avec glm_2
newdat$prop <- predict(glm_2, newdata = newdat, type = "response")
# Graphique
ggplot(newdat, aes(x = LogDose, y = prop, color = Sexe, shape = Sexe)) +
  geom_point(size = 3, fill = "white", stroke = 1) +
  geom_line(data = newdat, aes(x = LogDose, y = prop, color = Sexe), linewidth = 0.5) +
  scale_color_manual(values = c("red", "blue")) +
  scale_shape_manual(values = c(2, 1)) +
  labs(x = "LogDose", y = "Proportion de survie") +
  theme_classic()
###################################################

### Remarque : autres formats des donnees
# La regression logistique peut également s'appliquer sur des tables avec des denombrements (type tables de contingence,
# vues pour le khi2) ou des proportions.
# Voyez par exemple les fichiers LarvesProportions et LarvesTable, qui
# contiennent les memes donnees presentees differement.

glm_prop=glm(Taux.survie ~ LogDose + Sexe, family = binomial, data = LarvesProportions, weights=Nombre)
# Notez qu'il est necessaire de preciser l'effectif dans les donnees, avec l'argument weights dans la formule
summary(glm_2)
summary(glm_prop)
# Cela donne exactement le meme resultat, sauf pour les deviances et l'AIC (mais ceci est du au fait que
# la grandeur numerique en mesure n'est pas la meme : proportion ou mesure binaire).

# On peut de meme travailler sur les tables
glm_table=glm(cbind(Vivants,Morts) ~ LogDose + Sexe, family = binomial, data = LarvesTable)
summary(glm_table)
# Identique a glm_prop, car R calcule les proportions implicitement

LarvesProportions$Sexe=as.factor(LarvesProportions$Sexe)
glm_prop=glm(Taux.survie ~ LogDose + Sexe, family = binomial, data = LarvesProportions, weights=Nombre)
plot(ggpredict(glm_prop,c("LogDose","Sexe"),interval = "confidence"))


###### Regression log-lineaire
# Pour realiser un modele log-lineaire (loi de Poisson, pour les comptages), il suffit simplement de faire
# glm(Mesures ~ Facteurs,family = poisson, data = xxx)
# Contrairement a la regression logistique, il est possible de faire les graphiques diagnostiques.
# Un des exercices ci-dessous s'analyse avec un modele log-lineaire.


###
# EXERCICE 3 (fichier Marnes.csv)
###

# (exercice adapte de la 3eme partie du sujet d'examen 2018-2019, session 1)

# On cherche a determiner la probabilite que des echantillons de marnes (roches sedimentaires)
# se brisent lors d'un choc normalise (test d'impact) en fonction de 3 parametres :
# la masse de l'echantillon,
# la vitesse du choc (faible, moyenne et forte)
# et la presence ou non de fissures a la surface des echantillons
# Apres le test d'impact, on observe sur 768 echantillons s'ils sont brises (1) ou non (0).
# Vous donnerez l'equation du modele, apres avoir essaye de le simplifier.


###
# EXERCICE 4 (fichier Climat.csv)
###

# (exercice adapte du sujet d'examen 2020-2021, session 1)

# On produit plusieurs simulations numeriques pour caracteriser la relation entre 
# l’extension de la banquise Arctique (en millions de km2 ; variable notee banquise dans la base de donnees),
# et la temperature moyenne de l’air a la surface de la Terre (en °C ; variable temp).\
# Ces simulations sont produites avec trois modeles numeriques differents CESM2, IPSL et MPI (variable model).
# Pour toutes les simulations, on note l’extension moyenne et la temperature moyenne obtenue pour differentes periodes de 20 ans.

# 1) Indiquez la nature de chaque variable, et leur role.
# 2) Quelle analyse conduire pour caracteriser la relation entre l'extension de la banquise
#    et la temperature dans ces simulations numeriques ?
# 3) Realisez cette analyse sous R et interpretez les resultats obtenus (vous donnerez notamment l'equation obtenue).


###
# EXERCICE 5 (fichier Biomasse.csv)
###
# On a compte le nombre d'especes d'un sol, en fonction de la biomasse et du pH du sol.
# Ces deux facteurs influencent-ils le nombre d'especes ?


###
# EXERCICE BONUS (fichier Reins.csv)
###
# On compare l'effet de deux traitements A et B sur la guerison de calculs renaux de differentes tailles.
# Utilisez d'abord traitement comme facteur, puis traitement, taille du calcul et leur interaction.
# Qu'en pensez-vous ?
# Le phenomene observe s'appelle le paradoxe de Simpson.
# Indication : utiliser as.factor() sur la mesure. Notez qu'il n'y a que des intercepts.
