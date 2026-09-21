
#####################################################
#####################################################
##                                                 ##
##                   MODULE 4U001                  ##
##                                                 ##
##                  Initiation a R                 ##
##                                                 ##
#####################################################
#####################################################


##############################
##  Presentation            ##
##############################

# Le logiciel R est un logiciel gratuit qui permet d'effectuer de nombreux calculs statistiques.
# Il est telechargeable sur la page http://cran.r-project.org/
# Veillez a choisir l'installateur correspondant a votre systeme d'exploitation.

# Comme R est logiciel aride, on ajoute autour de lui un environnement pour le rendre un peu plus convivial.
# C'est le logiciel RStudio, que nous emploierons pour ce TP. Lui aussi est gratuit.
# Comme RStudio est un environnement de developpement integre pour R (IDE), il faut avoir installer R avant d'installer RStudio.
# On le trouve sur : http://www.rstudio.com/products/rstudio/download/

# L'interface de RStudio se decoupe en 4 fenetres.
# Le fichier que vous etes en train de lire est un script. C'est un editeur de texte, qui va contenir l'ensemble des commandes que R peut executer.
# R ignore tout ce qui est situe derriere un # dans une ligne donnee, comme par exemple ce texte.
# Cela s'appelle un commentaire. Les instructions pour le TP seront donc situees dans ce script, sous forme de commentaires.
# Vous pourrez aussi annoter les scripts des TP avec vos propres commentaires, en les faisant preceder d'un #.
# Les scripts s'ouvrent en haut a gauche de R studio.
# Ils peuvent s'enregistrer (ils le sont avec l'extension .R) :
# >File >Save ou Save as, ou directement en cliquant sur l'icone disquette).
# Pour ouvrir un script existant : >File >Open File 
# Pour creer un nouveau script, cliquet sur >File >New File >R Script (ou directement l'icone + en haut a gauche).

# En bas a gauche de R studio, se trouve la console,
# qui est une fenetre de commandes ou s'afficheront les commandes executees et leurs resultats (si un resultat doit s'afficher),
# Le plus simple est d'executer, a partir du script, les commandes,
# qui s'afficheront ensuite automatiquement dans la console.
# Pour executer une commande a partir du script : selectionner la ou les lignes a executer dans la fenetre script
# puis appuyer sur les touches ctrl + R (version PC) ou ⌘ + enter (version Mac) .
# Vous pouvez aussi selectionner les commandes dans le script et cliquer sur "Run" en haut a droite de la fenêtre de script.

# Dans la fenetre en haut a droite, vous trouverez l'onglet Environment,
# qui liste l'ensemble des elements que vous avez crees ou importes, et sur lesquels vous pouvez travailler.
# A chaque ouverture de jeu de donnees, le nom et la description de ces donnees seront affiches ici.
# L'onglet History recapitule tout ce qui a ete fait sur le logiciel

# En bas a droite, l'onglet Files est un navigateur de fichiers
# pour selectionner les scripts R a editer ou les fichiers de donnees a importer)
# l'onglet Plots regroupe les graphiques realises; vous pourrez sauvegarder vos graphes a partir de cet onglet
# l'onglet Packages regroupe les differentes librairies de R installees
# (a l'installation de R, la librairie de base est automatiquement installee,
# et d'autres pourront etre installees en fonction des besoins, lors de vos analyses statistiques ou graphiques)
# L'onglet Help sert a chercher de l'aide sur des commandes et trouver la documentation de R


############################################
##  1ere partie : Commandes simples       ##
############################################

# Executez les commandes suivantes a partir de la fenetre de script.
# (en les selectionnant puis ctrl+R ou cmd+enter, ou en cliquant sur "Run").
1+3
4/789
pi
10^3

# Voyons un cas particulier
1/0
# On nous signale que c'est l'infini "Inf"

# Entrons lui une lettre :
a
# On obtient une erreur.
#Quand vous obtenez une erreur, essayer de comprendre ce qui n'a pas marche.
# En revanche, a l'aide de lettres on peut tout de meme utiliser des commandes 
# que R va reconnaitre, on appelle ces commandes des FONCTIONS.

# Racine carree (square root en anglais)
sqrt(81)
# Exponentielle
exp(1)
# Logarithme naturel
log(10)
# Logarithme decimal
log10(10)
# Il est tout a fait possible de combiner les fonctions
log(exp(3))

# Les fonctions sont toujours suivies de parentheses.
# Les elements contenus dans ces parentheses (= ce sur quoi la fonction travaille)
# s'appellent des ARGUMENTS.
# S'il y a plusieurs arguments, ceux-ci sont separes par des virgules.
# R Studio propose automatiquement une parenthese fermante quand vous en ouvrez une, et R affiche un + 
# dans la console quand il en manque.

# Pour comprendre ce que fait une fonction et quels sont ses arguments, 
# on peut utiliser l'aide en mettant un ? devant la fonction,
# ou regarder dans l'onglet help en bas a droite
# ou executer la fonction help().
?log
help(log)



#################################
##  2eme partie : les donnees  ##
#################################

# QUESTION : concretement, comment utiliser R ?
# REPONSE : on emploie des FONCTIONS sur des donnees que l'on va maintenant apprendre a saisir

a<-42 # on peut aussi faire 42->a ou a=42
# Il s'agit de l'assignation de la valeur 42 à l'OBJET a
a # R connait l'objet a maintenant, qui est un vecteur numerique

# pour creer un vecteur numerique de plusieurs valeurs, on utilise la fonction c()
a=c(1,2,412,4) # on peut aussi faire a<-c(1,2,412,4) c(1,2,412,4)->a
a # l'ancien a est remplace

# ATTENTION : R differencie les majuscules et les minuscules (source d'erreur frequente)
A<-18 ; A # remarquez le point-virgule qui separe deux commandes sur une meme ligne

# Essayez maintenant d'autres commandes sur vos donnees contenues dans le vecteur numerique a :
a*2
a+1
a[3] # les crochets permettent d'acceder a un element donne de a
a[42] #le NA signifie qu'il n'y a rien "Not Assigned"
sqrt(a)
a*a

# On voit que R vous permet de faire des calculs sur ces donnees.
# Voyez par exemple :
mean(a)
median(a)
quantile(a)
var(a)
sqrt(var(a))
sd(a) #sd pour standard deviation = ecart type en anglais
max(a)
min(a)
summary(a)

# Enfin, rien ne vous empeche de manipuler des lettres (ce sera utile pour les variables categorielles)
z=c("a","a")
z
# a comparer a :
x=c(a,a)
x

##################################################
####Importer des donnees a partir d'un fichier####

# On peut aussi lire des donnees dans un fichier de format .csv ou .txt (facile a creer avec excel). 
# Il existe de nombreuses facons d'importer des donnees dans R Studio, on vous en presente quelques unes.

####A partir de l'onglet Environment, puis Import Dataset
### From Text (base)
# Allez chercher le fichier Datafish.csv, une fenetre s'ouvre,
# dans laquelle vous allez pouvoir parametrer certaines caracteristiques de votre fichier de donnees.

# Un nom est genere automatiquement, mais il est possible de le changer (dans "Name")
# Heading indique que la premiere ligne de votre fichier correspond aux titres de vos variables.
# Separator permet d'indiquer sous quel format vos variables sont separees dans les differentes colonnes du fichier ;
# cela peut varier selon le format du fichier : en .txt, le separateur sera Tabulation,
# .csv, le separateur sera Semicolon, etc...
# Decimal permet de preciser sous quelle forme est le marqueur decimal dans le fichier de donnees (point ou virgule)
# Attention a bien preciser vore marqueur décimal, car R ne reconnaitra pas directement la virgule comme marqueur decimal.
# Ici, preciser Comma car le marqueur decimal utilise dans le fichier Datafish est la virgule.


### From Text (readr)
# Avec Browse en haut a droite, choisir le fichier Datafish.csv
# Name permet d'indiquer le nom que vous voulez donner a votre jeu de donnees
# (par defaut, R reprend le nom sous lequel le fichier eest enregistre ou remplace par dataset ;
# attention si ce nom a deja ete utilise !!)
# Delimiter pour indiquer le format du separateur des differentes variables dans les differentes colonnes du fichier
# (a changer selon le format du fichier, cela pourra etre Tab pour tabulation, etc...)
# Dans Locale, decimal mark permet de preciser la forme du marqueur decimal
# Cliquer sur Import en bas a droite

# Les donnees Datafish apparaissent dans l'onglet Environment.
# Dans cet onglet, si vous cliquez sur la fleche a cote de Datafish, vous obtenez la description du jeu de donnees (ses differentes variables)
# A cet endroit, en double-cliquant sur Datafish, vous affichez son contenu dans un onglet navigable
# (ce qui est plus pratique que de lire dans la console de R).


### Autre façon de proceder, a partir d'une commande executée dans le script
# d'abord se placer au bon endroit dans le navigateur Files en bas a droite ; 
# il s'agit d'aller chercher votre dossier ou se trouve votre jeu de donnees.
# Cliquer ensuite sur "More" dans cet onglet puis sur
# Set as working directory : cela permet de definir votre espace de travail, reconnu par R (notamment pour aller chercher des fichiers)
# Ensuite, faire la commande :
Datafish <-read.csv("Datafish.csv", sep=";", dec=",")
#L'argument sep pour definir le separateur, et dec pour le marqueur decimal.


#Ce jeu de donnees est un nouvel objet de R, appele **Dataframe**. 
#il s'agit d'un tableau, ou les individus statistiques figurent en lignes 
#et les differentes variables mesurees ou observees sur ces individus figurent en colonnes.

# Le fichier contient les tailles de deux groupes de poissons (adu1 et adu2).
# Une fois le fichier lu, on peut y acceder par ligne et colonne, en utilisant les crochets
# ou on precise [n° de ligne, n° de colonne]

Datafish[3,2]#3eme ligne et 2eme colonne
Datafish[3,1]=55 # ici pour corriger la valeur de l'individu 3, 1ere colonne
Datafish[,2] # tous les individus, seulement la seconde colonne
Datafish[3,] # seulement la troisieme ligne, toutes les colonnes
Datafish$Groupe # le $ permet d'utiliser la ligne de titre, extraire la variable ou le facteur "Groupe" dans l'objet "Datafish"
names(Datafish) # donne le nom des colonnes

mean(Datafish$Mes)

# Notez l'usage astucieux des crochets pour extraire une modalite donnee d'une variable qualitative.
# Cette variable s'appellera facteur (factor) dans R, et ses modalites niveaux (levels).

mean(Datafish$Mesure[Datafish$Groupe=="adu1"]) # moyenne de l'échantillon adu1

mean(Datafish$Mesure[Datafish$Groupe=="adu2"]) # moyenne de l'échantillon adu2

# La fonction subset() permet egalement de selectionner des donnees, qui remplissent une (ou plusieurs) condition(s)
# Cette fonction prend comme 1er argument le nom du fichier de donnees,
# comme 2eme argument, la(les) differente(s) condition(s) que vous souhaitez voir remplies
# comme 3eme argument, le nom des variables a selectionner
subset(Datafish, Groupe=="adu1", Mesure)
# seules les valeurs de la variable Mes du Grpe adu1 sont selectionnees

# Pour ajouter une condition, utiliser &
subset(Datafish, Groupe=="adu1" & Mes > 55, Mesure)
# seules les valeurs > 55 de la variable Mesure du Groupe adu1 sont selectionnees

# Enfin, pour calculer la moyenne sur chacun des 2 echantillons adu1 et adu2,
# on pourra utiliser la fonction 'tapply'. Cette fonction attend comme 
# 1er argument, la variable numerique sur laquelle on souhaite appliquer la commande
# 2eme argument, la variable qualitative qui definit les differents echantillons
# 3eme argument, la commande choisie (par exemple ici 'mean()' pour calculer la moyenne)
tapply(Datafish$Mesure,Datafish$Groupe, mean)


## Representation graphique des donnees
# La fonction boxplot() permet de realiser une boite a moustaches.
# On peut utiliser cette fonction avec le symbole **~** (tilde),
#en placant la variable numérique avant le tilde
#et la variable qualitative (ou facteur) definissant nos groupes apres :
#variable numérique ~ variable qualitative

boxplot(Datafish$Mesure ~ Datafish$Groupe)
# Autre facon d'ecrire la meme commande :
boxplot(Mesure ~ Groupe, data = Datafish)
# Changer les couleurs avec l'argument col :
boxplot(Mesure~Groupe, data = Datafish, col=c("green","blue"))
