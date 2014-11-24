# dlply :
library("plyr")
library("data.table")
Encoding(levels(morphology$Type1)) <- "UTF-8"

# pour chaque element faire une regression du poids en fonction de la taille
# avec dlply 
listeRegType <- dlply(morphology, .variables = .(Type1), .fun = function(donnees)
{
  lm(log(Weight) ~ log(Height), data = donnees)
}, .progress = "text")

morphology <- as.data.table(morphology)
pkmVol <- morphology[Type1 == "Vol"]
plot(pkmVol$Height, pkmVol$Weight)

# et ensuite avec un ldply générer un tableau qui contient le coef
dfCoef <- ldply(listeRegType,.fun = function(maListe){coef(maListe)})

# de chacun des modèles et ça renvoie une liste de modeles on pourra voir par
# exemple que le type pierre le coef est super élevé alors que pr el type
# spectre il doit être faible
dfCoef <- as.data.table(dfCoef)
dfCoef[ Type1 %in% c("Vol", "Acier", "Combat", "Sol")]

dfCoef$Couleur <- c("grey","orange", "purple",
                    "steelblue", "gold","darkorange",
                    "lightskyblue","olivedrab3","mistyrose4",
                    "limegreen","magenta4","maroon2","moccasin",
                    "navajowhite3","mediumpurple3","peachpuff4",
                    "mediumpurple")
# graphiques :
png("D:\\MeetUp\\Raddicts\\presentation\\8 meetup\\interceptHeight.png",
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