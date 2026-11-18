##Tutorium 4

install.packages("tinyplot")
install.packages("plotrix")
library(tinyplot)
library(plotrix)

#remove the clutter
rm(list = ls())

#Aufgabe 1
#Daten einlesen und scatterplot mit grouping
stats.df <- read.csv("Data/Biostatistik2026.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE)

par(mfrow = c(2,2))

plot(KG_cm ~ jitter(as.numeric(Geschlecht)), data = stats.df,
     pch = 19, col = "dodgerblue", xaxt = "n", xlab = "", xlim = c(0.5, 3.5))
#sieht nicht so schön aus, das liegt daran, dass wir es gleich bei Aufgabe 2 leicht haben wollen


#Aufgabe 2
#descriptive Statistik dem Plot hinzufügen, hehe
#also eine kleine Matrix machen, mit all den Funktionen, die uns interessieren
stats.desc.df <- aggregate(KG_cm ~ Geschlecht, data = stats.df, function(x)
  c(m = mean(x), sd = sd(x), se = std.error(x), med = median(x),
    q25 = quantile(x, probs = 0.25), q75 = quantile(x, probs = 0.75)))

#dann zwingen (coerce) wir das noch in einen data frame
stats.desc.df <- do.call(data.frame, stats.desc.df)

#und dann schmeißen wir das in den Plot
plotCI(as.numeric(stats.desc.df$Geschlecht), stats.desc.df$KG_cm.m, 
       stats.desc.df$KG_cm.sd, add = T, pch = 19, cex = 1.2, lwd = 2)
axis(1, at = c(1,2,3), label = c("d","m","w"))


#Aufgabe 3
#das selbe wie oben nur mit doppeltem Standartfehler statt Standardabweichung
plot(KG_cm ~ jitter(as.numeric(Geschlecht)), data = stats.df,
     pch = 19, col = "dodgerblue", xaxt = "n", xlab = "", xlim = c(0.5, 3.5))
plotCI(as.numeric(stats.desc.df$Geschlecht), stats.desc.df$KG_cm.m, 
       2 * stats.desc.df$KG_cm.se, add = T, pch = 19, cex = 1.2, lwd = 2)
axis(1, at = c(1,2,3), label = c("d","m","w"))


#Aufgabe4
#und nochmal das selbe, nur diesmal mit Median statt Mittelwert und q25 und q75 statt sd und se
plot(KG_cm ~ jitter(as.numeric(Geschlecht)), data = stats.df,
     pch = 19, col = "dodgerblue", xaxt = "n", xlab = "", xlim = c(0.5, 3.5))
plotCI(as.numeric(stats.desc.df$Geschlecht), stats.desc.df$KG_cm.med, 
       li = stats.desc.df$KG_cm.q25.25., ui = stats.desc.df$KG_cm.q75.75.,
       add = T, pch = 19, cex = 1.2, lwd = 2)
axis(1, at = c(1,2,3), label = c("d","m","w"))


#Aufgabe 5
#und jetzt als bocplot, yippee
boxplot(KG_cm ~ Geschlecht, data = stats.df, ylim = c(150, 200))
points(KG_cm ~ jitter(as.numeric(Geschlecht)), data = stats.df, pch = 19, col = "dodgerblue")


#Aufgabe 6
#Diskussion!!!


#Aufgabe 7
#Und zu guter letzt machen wir den ganzen Spaß, aber mit Bierkonsum statt mit Körpergröße
par(mfrow = c(2,2))

plot(Bierkonsum ~ jitter(as.numeric(Geschlecht)), data = stats.df,
     pch = 19, col = "dodgerblue", xaxt = "n", xlab = "", xlim = c(0.5, 3.5))


stats.beer.df <- aggregate(Bierkonsum ~ Geschlecht, data = stats.df, function(x)
  c(m = mean(x), sd = sd(x), se = std.error(x), med = median(x),
    q25 = quantile(x, probs = 0.25), q75 = quantile(x, probs = 0.75)))


stats.beer.df <- do.call(data.frame, stats.beer.df)


plotCI(as.numeric(stats.beer.df$Geschlecht), stats.beer.df$Bierkonsum.m, 
       stats.beer.df$Bierkonsum.sd, add = T, pch = 19, cex = 1.2, lwd = 2)
axis(1, at = c(1,2,3), label = c("d","m","w"))


plot(Bierkonsum ~ jitter(as.numeric(Geschlecht)), data = stats.df,
     pch = 19, col = "dodgerblue", xaxt = "n", xlab = "", xlim = c(0.5, 3.5))
plotCI(as.numeric(stats.beer.df$Geschlecht), stats.beer.df$Bierkonsum.m, 
       2 * stats.beer.df$Bierkonsum.se, add = T, pch = 19, cex = 1.2, lwd = 2)
axis(1, at = c(1,2,3), label = c("d","m","w"))


plot(Bierkonsum ~ jitter(as.numeric(Geschlecht)), data = stats.df,
     pch = 19, col = "dodgerblue", xaxt = "n", xlab = "", xlim = c(0.5, 3.5))
plotCI(as.numeric(stats.beer.df$Geschlecht), stats.beer.df$Bierkonsum.med, 
       li = stats.beer.df$Bierkonsum.q25.25., ui = stats.beer.df$Bierkonsum.q75.75.,
       add = T, pch = 19, cex = 1.2, lwd = 2)
axis(1, at = c(1,2,3), label = c("d","m","w"))


boxplot(Bierkonsum ~ Geschlecht, data = stats.df, ylim = c(0, 11000))
points(Bierkonsum ~ jitter(as.numeric(Geschlecht)), data = stats.df, pch = 19, col = "dodgerblue")


