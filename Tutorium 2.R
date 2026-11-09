#remove clutter
rm(list = ls())

install.packages("ggplot2")
library(ggplot2)

#set working directory
setwd("C:/Users/ruehl/Nextcloud/Uni/11. Semester/Biostatistik Tut/Statistics")

#Datei "brain allometry data" einlesen
brains.df <- read.table("Data/BrainAllometry_Supplement_Data.csv", header = TRUE, skip = 1, sep = ",")


#Aufgabe 1: Streudiagramm/Scatterplot mit doppelter log-scale
plot(Mean_brain_mass_g ~ Mean_body_mass_g, data = brains.df, log = "xy",
     las = 1, ylab = "Gehirngewicht [g]", xlab = "Körpergewicht [g]", pch = 19)

#Aufgabe 2: Wale einfärben
points(Mean_brain_mass_g ~ Mean_body_mass_g, data = subset(brains.df, order == "Cetacea"),
       col = "cyan", pch = 19)

#Aufgabe 3: Menschen einfärben
points(Mean_brain_mass_g ~ Mean_body_mass_g, data = subset(brains.df, Binomial == "Homo_sapiens"),
       col = "goldenrod", pch = 19, cex = 1.5)

#Aufgabe 4: Legende erstellen
legend("topleft", legend = c("other mammals", "whales", "Homo sapiens"), pch = 19, ncol = 1,
       col = c("black", "cyan", "goldenrod"))




#Aufgabe 5: Mittelwert, Standartabwechung und Variationskoeffizient für vier Gattungen
myord <- subset(brains.df, order %in% c("Rodentia", "Carnivora", "Cetacea", "Primates"))

#alternativ via Matrixnotation --> funktioniert nicht gut, deswegen machen wir das hier per subset()
myord <- brains.df[c(brains.df$order == "Rodentia" , brains.df$order == "Carnivora" ,
                       brains.df$order == "Cetacea" , brains.df$order == "Primates"), ]

myord.sums <- aggregate(data = myord, cbind(Mean_brain_mass_g, Mean_body_mass_g) ~ order,
                        function(x) c(m = mean(x), s = sd(x), cv = sd(x)/mean(x)))

myord.sums


#Aufgabe 6: Darstellen als Histogramm
par(mfrow = c(2,2)) #legt fest, wie viele Plots nebeneinander gezeigt werden können, hier 2 Reihen und
                    #zwei Spalten
hist(subset(myord, order == "Rodentia")$Mean_brain_mass_g, main = "Rodentia", las = 1,
     xlab = "Gehirngewicht [g]", seq(0, 10000, 500))
hist(subset(myord, order == "Carnivora")$Mean_brain_mass_g, main = "Carnivora", las = 1,
     xlab = "Gehirngewicht [g]", seq(0, 10000, 500))
hist(subset(myord, order == "Primates")$Mean_brain_mass_g, main = "Primates", las = 1,
     xlab = "Gehirngewicht [g]", seq(0, 10000, 500))
hist(subset(myord, order == "Cetacea")$Mean_brain_mass_g, main = "Cetacea", las = 1,
     xlab = "Gehirngewicht [g]", seq(0, 10000, 500))



#Aufgabe 7: Histogramm der logarithmierten Gehirngewichte
myord$lbrain <- log10(myord$Mean_brain_mass_g)

hist(subset(myord, order == "Rodentia")$lbrain, main = "Rodentia", las = 1, xlab = "Gehirngewicht [g]",
     seq(-1, 4, 0.25))
hist(subset(myord, order == "Carnivora")$lbrain, main = "Carnivora", las = 1, xlab = "Gehirngewicht [g]",
     seq(-1, 4, 0.25))
hist(subset(myord, order == "Primates")$lbrain, main = "Primates", las = 1, xlab = "Gehirngewicht [g]",
     seq(-1, 4, 0.25))
hist(subset(myord, order == "Cetacea")$lbrain, main = "Cetacea", las = 1, xlab = "Gehirngewicht [g]",
     seq(-1, 4, 0.25))



#Aufgabe 8: Boxplots
par(mfrow = c(1,3))
boxplot(Mean_brain_mass_g ~ order, data = myord)
boxplot(lbrain ~ order, data = myord)
boxplot(log10(Mean_brain_mass_g) ~ order, data = myord)
#es muss nicht zwingend vorher berechnet werden, dass wir den log10 wollen
#stattdessen können wir es einfach in das Formel-Argument integrieren
#dabei müssen wir drauf achten, nur den teil zu logarithmieren, den wir wollen







