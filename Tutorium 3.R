##Tutorium 3

install.packages("ggplot2")
library(ggplot2)

#remove the clutter
rm(list = ls())

#Aufgabe 1
#Daten einlesen und sortieren
stats.df <- read.csv("Data/Biostatistik2026.csv", header = TRUE, sep = ";")
#hier war es ein wenig ungünstig, dass ich euch letzte Woche gesagt habe csv steht für
#comma seperated values; das Dateiformat hier ist scheinbar nicht im UTC-8 Format


#Nominal: Beschreibende Daten, aber ohne Ordnung
#Ordinal: Beschreibende Daten, mit einer natürlichen Ordnung
#Verhältnis: Zahlen-Werte, die ein Verhältnis beschreiben und >= 0 sind
#Intervall: Zahlen-Werte, die vergleichbar sind und der Teilmenge |R angehören


#Nominal: Augenfarbe, Blutgruppe AB0, Blutgruppe Rhesus, Geschlecht
#Ordinal: Musik, Sport
#Verhältnis: Körpergrößen, Schuhgröße, Bierkonsum, D2, D4
#Intervall: Nada

#Aufgabe 2
#Wie viele NAs in welcher Spalte?

summary(stats.df)

#Dietmar's komplizierte Lösung, das in einen data frame zu verwandeln
#data frame erstellen mit Spaltennahmen in einer Spalte und einer leeren Spalte
miss.df <- data.frame(var = names(stats.df), miss.df = NA)

#mit einem for-loop lassen wir R jetzt für jede Spalte einmal eintragen,
#wie viele NAs in der dazugehörigen Spalte sind
for (i in 1:13) {
  miss.df$miss.df[i] <- 35 - table(is.na(stats.df[ , i]))[1]
}

miss.df
#müsst ihr aber noch nicht verstehen, is okay

#Aufgabe 3
#ganz viele Histogramme  und die deskriptive Statistik dazu

#wir beginnen wieder mit der Aufteilung unseres plotfensters
par(mfrow = c(2,2))

#dann unsere Plots
hist(stats.df$KG_Mutter_cm, main = "", ylab = "Eltern", xlab = "", col = "dodgerblue",
     breaks = seq(150, 200, 5))
hist(stats.df$KG_Vater_cm, main = "", ylab = "", xlab = "", col = "forestgreen",
     breaks = seq(150, 200, 5))
hist(stats.df[stats.df$Geschlecht == "w", "KG_cm"], main = "", ylab = "Kinder", xlab = "weiblich",
     col = "dodgerblue", breaks = seq(150, 200, 5))
hist(stats.df[stats.df$Geschlecht == "m", "KG_cm"], main = "", ylab = "", xlab = "männlich",
     col = "forestgreen", breaks = seq(150, 200, 5))

#und zu guter letzt die Statistik
c(mean(stats.df$KG_Mutter_cm), sd(stats.df$KG_Mutter_cm), sd(stats.df$KG_Mutter_cm)/mean(stats.df$KG_Mutter_cm))

c(mean(stats.df$KG_Vater_cm), sd(stats.df$KG_Vater_cm), sd(stats.df$KG_Vater_cm)/mean(stats.df$KG_Vater_cm))

aggregate(stats.df$KG_cm ~ stats.df$Geschlecht, data=stats.df, function(x) c(m = mean(x), s = sd(x), cv = sd(x)/mean(x)))


#Aufgabe 4
#Streudiagramme für Körpergrößenvergleich
par(mfrow = c(1,1))

#a)
#farbe müssen wir vorher festlegen, da baseplot blöd ist
colv <- c("purple", "dodgerblue", "forestgreen")

plot(data = stats.df, KG_Mutter_cm ~ KG_cm, col = colv[as.factor(Geschlecht)], pch = 19)
legend('topleft', pch = 19, cex=1.3,  col = colv, c('d', 'm', 'w'))

#b)
#hier können wir den selben df für die Farbe verwenden
plot(data = stats.df, KG_Vater_cm ~ KG_cm, col = colv[as.factor(Geschlecht)], pch = 19)
legend('topleft', pch = 19, cex=1.3,  col = colv, c('d', 'm', 'w'))

#c)
#und hier auch; außerdem berechnen wir noch den Mittelwert zwischen den KGs
#ich mache das im plot()-command, aber theoretisch könnt ihr das auch vorher in ner
#extra Spalte machen, wie in den Musterlösungen
plot(data = stats.df, (KG_Mutter_cm + KG_Vater_cm)/2 ~ KG_cm, col = colv[as.factor(Geschlecht)], pch = 19)
legend('topleft', pch = 19, cex=1.3,  col = colv, c('d', 'm', 'w'))

################################################################################
#ggplot-Intervention; da ist das mit den Farben leichter
p1 <- ggplot()+
  geom_point(data = stats.df, aes(x = KG_cm, y = KG_Mutter_cm, colour = Geschlecht))+
  scale_colour_manual(values = c(d = "purple", m = "forestgreen", w = "dodgerblue"))+
  theme_minimal()
p1
#Intervention Ende
################################################################################

#Aufgabe 5
#Kovarianzen und Korrelationen
cov(stats.df$KG_Mutter_cm, stats.df$KG_cm, use = "complete.obs")
cor(stats.df$KG_Mutter_cm, stats.df$KG_cm, use = "complete.obs")
cov(stats.df$KG_Vater_cm, stats.df$KG_cm, use = "complete.obs")
cor(stats.df$KG_Vater_cm, stats.df$KG_cm, use = "complete.obs")

#spätestens für den Test brauchen wir jetzt aber eine Spalte mit den Mean-Daten
stats.df$KG_mean <- (stats.df$KG_Mutter_cm + stats.df$KG_Vater_cm)/2
cov(stats.df$KG_mean, stats.df$KG_cm, use = "complete.obs")
cor(stats.df$KG_mean, stats.df$KG_cm, use = "complete.obs")

#das Argument use = "complete.obs" hilft hier, die Daten auch zu berechnen, wenn
#wir NAs haben. Ansonsten geht der Test nicht


#Aufgabe 6
#geordnete Faktoren für Sport und Musik und dann als Boxplot darstellen
#als geordnete Faktoren deklarieren; hier ist wichtig, das wir bei der Level-Definition
#darauf achten, die richtige reihenfolge in den Vektor einzutragen
stats.df$Sport <- factor(stats.df$Sport, levels = c("keine", "wenig", "mittel", "viel"), ordered = TRUE)
stats.df$Musik <- factor(stats.df$Musik, levels = c("keine", "wenig", "mittel", "viel"), ordered = TRUE)

#und dann die plots, whoop whoop
#erst wieder die Aufteilung festlegen
par(mfrow = c(1,2))

boxplot(stats.df$Sport, yaxt = "no")
axis(2, at = c(1:4), levels(stats.df$Sport))
axis(1, at = c(1), "Sport")

boxplot(stats.df$Musik, yaxt = "no")
axis(2, at = c(1:4), levels(stats.df$Musik))
axis(1, at = c(1), "Musik")


#Aufgabe 7
#Median-Sachen
quantile(stats.df$Sport, 0.5, type = 1, na.rm = T)
quantile(stats.df$Musik, 0.5, type = 1, na.rm = T)


#Aufgabe 8
#Zählen der Beobachtungen und schöne Darstellung in Boxplot und Mosaikplot
hobby.table <- table(Musik = stats.df$Musik, Sport = stats.df$Sport)

#a)
par(mfrow = c(1,1))
barplot(hobby.table, col = c(1:4), xlab = 'Sport', ylab = 'Häufigkeit', cex.lab = 1.3)
legend('topleft', fill = 1:4, levels(stats.df$Musik), cex = 1.2, title = 'Musik')

#b)
mosaicplot(hobby.table, main = "", col = c(1:4))

#und hier brauchen wir nichtmal ne legende, yay

