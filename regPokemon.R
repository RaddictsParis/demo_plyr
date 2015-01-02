# Meetup Paris R Addicts
#
# DEMONSTRATION DU PACKAGE PLYR
# Timeri Veccella
###############################

# dlply :
library("plyr")
library("data.table")

# Charger les donnees. Le tableau "morphology" contient une ligne par pokemon
# avec pour chacun le nom, la taille, le poids et le type. Un pokemon a un type
# principal (variable "Type1") et peut avoir un type secondaire ("Type2").
load("morphology.rda")

morphology <- as.data.table(morphology)
Encoding(levels(morphology$Type1)) <- "UTF-8"

# pour chaque element faire une regression du poids en fonction de la taille
# avec dlply.
# Le résultat de chaque régression est stocké dans une liste.
listeRegType <- dlply(morphology, .variables = .(Type1), .fun = function(donnees)
{
  lm(log(Weight) ~ log(Height), data = donnees)
}, .progress = "text")
head(listeRegType)

# Ensuite generer avec ldply un tableau qui contient les coefficients
# de chacun des modeles. On pourra voir par
# exemple que le type pierre la constante est super eleve alors que pour le type
# spectre elle doit etre faible.

dfCoef <- ldply(listeRegType,.fun = function(maListe){coef(maListe)})
dfCoef

# Super graphique !

dfCoef$Couleur <- c("grey","orange", "purple",
                    "steelblue", "gold","darkorange",
                    "lightskyblue","olivedrab3","mistyrose4",
                    "limegreen","magenta4","maroon2","moccasin",
                    "navajowhite3","mediumpurple3","peachpuff4",
                    "mediumpurple")

png("interceptHeight.png",
    height= 600,width=1000,
    pointsize = 20)
plot(dfCoef$`(Intercept)`,dfCoef$`log(Height)`, pch = 25,
     xlim = c(1.5,5),
     ylim = c(-1,3.5),
     xlab = "Intercept",
     ylab = "Height",
     main = "Pokemon : type, taille et poids ",
     col = "black",
     bg = dfCoef$Couleur)
grid()

text(dfCoef$`(Intercept)`,dfCoef$`log(Height)`, 
     labels=dfCoef$Type1, 
     adj= 1.2,
     col=dfCoef$Couleur
     )

dev.off()