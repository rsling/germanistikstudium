# Source "load.R" first. This will also initialize the random number generator.

require(coin)
require(rgdal)
require(beanplot)

# Set this TRUE for file output, FALSE for screen.
save.persistent <- F

if (save.persistent) sink("results.txt", append=F)
cat(" Wie viel Grammatik braucht das Germanistikstudium?\n",
    " Roland Schäfer und Ulrike Sayatz (2016)\n",
    " http://rolandschaefer.net\n\n")
if (save.persistent) sink()



###################
# Geomapping

herkunft <- order.by.freq(join.factors(gl$Bundesland1, gl$Bundesland2)[which(complete.cases(join.factors(gl$Bundesland1, gl$Bundesland2)))])
herkunft.stat <- summary(herkunft)

bl.map.orig <- c("Berlin", "Brandenburg", "Nrw", "Niedersachsen", "Schleswigholstein", "Sachsenanhalt", "Thueringen",
                 "Bayern", "Sachsen", "Badenwuerttemberg", "Bremen", "Hamburg", "Hessen", "Meckpomm", "Rheinlandpfalz", "Saarland")
bl.map.geo <- c(10, 11, 4, 2, 16, 14, 15, 8, 13, 7, 3, 1, 5, 12, 6, 9)
bl.map.pretty <- c("Berlin", "Brandenburg", "Nordrhein-Westfalen", "Niedersachsen", "Schleswig-Holstein", "Sachsen-Anhalt",
                   "Thüringen", "Bayern", "Sachsen", "Baden-Württemberg", "Bremen", "Hamburg", "Hessen",
                   "Mecklenburg-Vorpommern", "Rheinland-Pfalz", "Saarland")
bl.map <- data.frame(bl.map.orig)
bl.map <- cbind(bl.map, as.numeric(bl.map.geo), bl.map.pretty, as.numeric(herkunft.stat),
                log(as.numeric(herkunft.stat)), round(linear.map(log(as.numeric(herkunft.stat)), 1, 100), 0))

colnames(bl.map) <- c("Original", "Geo", "Pretty", "Freq", "Logfreq", "Color")

colfunc <- colorRampPalette(c("white", "darkgray"))
cols <- colfunc(110)
cols <- cols[seq(11,110)]

bl <- readOGR("geo","vg2500_bld")

if (save.persistent) svg("herkunft.svg")

par(mai=c(0.5,0.4,0.4,0.2))
plot(bl, col="white", main="Herkunft der TeilnehmerInnen nach Bundesländern")

for (land in seq(1, nrow(bl.map))) {
  geo <- as.numeric(bl.map[land, "Geo"])
  colo <- bl.map[land,"Color"]
  plot(bl[ geo, ], col = cols[colo], add = TRUE)
}

leg.cols <- c(130,100,50,10,5,1,0)
leg.labels <- as.character(leg.cols)
leg.vals <- log(leg.cols+1)
leg.col.idx <- round(linear.map(leg.vals, 1, 100), 0)

legend("bottomleft", legend=leg.labels, fill = cols[leg.col.idx], border = cols[leg.col.idx],
       bty = "n", title = "Anzahl")

# Draw numbers.
centroids <- getSpPPolygonsLabptSlots(bl)
text((centroids[bl.map$Geo,])[c(3:10,13:16),], labels = bl.map$Freq[c(3:10,13:16)], cex=1)

# Brandenburg
text(13.8, 52.0, labels = bl.map$Freq[2], cex = 1)

# Berlin
lines(c(13.39, 13.39), c(52.65, 52.78), lty=1, lwd=1)
text(13.39, 52.88, labels = bl.map$Freq[1], cex = 1)

# Hamburg
lines(c(10.04, 10.04), c(53.42575, 53.3), lty=1, lwd=1)
text(10.04, 53.2, labels = bl.map$Freq[12], cex = 1)

# Bremen
lines(c(8.6457, 8.39), c(53.1, 53.1), lty=1, lwd=1)
text(8.24, 53.1, labels = bl.map$Freq[11], cex = 1)

if (save.persistent) dev.off()
par(mai=c(1.02,0.82,0.82,0.42))



###################
# Distribution of results ("success")


if (save.persistent) svg("prozentverteilung.svg")
lwd <- 1

par(mfrow=c(3,4))
par(mai=c(0.5,0.4,0.4,0.2))

succ.cex = 0.8

plot(density(evals[,"Gesamt"]),
     main="Gesamtergebnis (ohne Aufgabe 8)", lwd=lwd, col="darkblue", 
     xaxt = "n", ylab = "", xlab = "", bty = "n",
     cex.main = succ.cex, cex.axis = succ.cex, cex.lab = succ.cex,
     ylim=c(0, max(density(evals[,"Gesamt"])$y)*1.15))
axis(1, seq(10,100,10), seq(10,100,10), cex.axis = succ.cex)
meen <- mean(evals[,"Gesamt"])
meen.den <- density(evals[,"Gesamt"], from=meen, to=meen, n=1)$y
lines(c(meen, meen), c(0, meen.den), col="darkgreen",
      lwd=lwd, lty=1)
legend("topleft", legend = paste("Mittel=", round(meen,2), sep=""), bty = "n", cex=succ.cex)

for (exercise in 1:11) {
  meen <- mean(evals[,exercise])
  meen.den <- density(evals[,exercise], from=meen, to=meen, n=1)$y
  
  plot(density(evals[,exercise]),
       main=paste("[", exercise, "] ", themen[exercise], sep=""), lwd=lwd,
       col="darkblue", xlim=c(0,100),
       xaxt = "n", ylab = "", xlab = "", bty = "n",
       cex.main = succ.cex, cex.axis = succ.cex, cex.lab = succ.cex,
       ylim=c(0, max(density(evals[,exercise])$y)*1.15))
  axis(1, seq(0,100,10), seq(0,100,10), cex.axis = succ.cex)

  lines(c(meen, meen), c(0, meen.den), col="darkgreen",
        lwd=lwd, lty=1)
  legend("topleft", legend = paste("Mittel=", round(meen,2), sep=""), bty = "n", cex=succ.cex)
}

par(mfrow=c(1,1))

if (save.persistent) dev.off()

par(mai=c(1.02,0.82,0.82,0.42))



###################
# Overall success in German grades
# (school system from 1 = best to 5 = fail)


if (save.persistent) svg("notenspiegel.svg")
notenspiegel <- round(summary(noten)/220*100,0)
notenplot <- barplot(notenspiegel, main="Notenverteilung in Prozent\nohne Aufgabe 8 (n=220)", ylim=c(0,16),
                     xlab = "Schulnote", ylab = "Anteil")
text(notenplot, notenspiegel+0.5, labels = paste(notenspiegel, "%", sep=""))
if (save.persistent) dev.off()


###################
# Success and subjective difficulty

for (i in 1:11) {
  kw.t <- kruskal_test(evals[,paste("Aufgabe", i, sep="")]~gl[,paste(i, ".Schwierigkeit", sep="")], distribution=approximate())
  kw.text <- paste("approx. KW-Test (MC, B=10.000): p=", round(pvalue(kw.t), 3), sep="")

  if (save.persistent) svg(paste("schwierigkeit", i, ".svg", sep=""))  
  beanplot(evals[,paste("Aufgabe", i, sep="")]~gl[,paste(i, ".Schwierigkeit", sep="")], bw="nrd0", 
           what = c(1,1,1,1), method = "jitter", 
           col = "lightyellow", axes = F, main=paste("Schwierigkeit [", i, "] ", themen[i], "\n", kw.text, sep=""), ylim=c(0,100),
           cex.main = 0.7)
  axis(1, 1:4, paste(levels(gl[,paste(i, ".Schwierigkeit", sep="")]), "\n(",
                     summary(gl[,paste(i, ".Schwierigkeit", sep="")])[1:4], ")", sep=""), tick = F,
       cex.axis = 0.7)
  axis(2, seq(0, 100, 10), labels = paste(seq(0, 100, 10), "%", seqp=""), cex.axis = 0.7)
  if (save.persistent) dev.off()
}



###################
# Success and subjective clarity

for (i in 1:11) {
  kw.t <- kruskal_test(evals[,paste("Aufgabe", i, sep="")]~gl[,paste(i, ".Verstaendlichkeit", sep="")], distribution=approximate())
  kw.text <- paste("approx. KW-Test (MC, B=10.000): p=", round(pvalue(kw.t), 3), sep="")

  if (save.persistent) svg(paste("verstaendlichkeit", i,".svg", sep=""))
  beanplot(evals[,paste("Aufgabe", i, sep="")]~gl[,paste(i, ".Verstaendlichkeit", sep="")], bw="nrd0", 
       what = c(1,1,1,1), method = "jitter", 
       col = "lightyellow", axes = F, main=paste("Verständlichkeit [", i, "] ", themen[i], "\n", kw.text, sep=""), ylim=c(0,100),
       cex.main = 0.7)
  axis(1, 1:4, paste(levels(gl[,paste(i, ".Verstaendlichkeit", sep="")]), "\n(",
       summary(gl[,paste(i, ".Verstaendlichkeit", sep="")])[1:4], ")", sep=""), tick = F,
       cex.axis = 0.7)
  axis(2, seq(0, 100, 10), labels = paste(seq(0, 100, 10), "%", seqp=""), cex.axis = 0.7)
  if (save.persistent) dev.off()
}




###################
# Text books

if (save.persistent) svg("einfuehrungen.svg")

books <- as.character(join.factors(gl$Einfuehrung1, gl$Einfuehrung2))
books.top <- rle(sort(books))
books.ord <- rev(order(books.top$lengths))

books.titles <- as.character(books.top$values)[books.ord]
books.freq <- floor(as.numeric(books.top$lengths)[books.ord]/2.20)
books.sel <- which(books.freq > 1)

the.plot <- barplot(books.freq[books.sel], names.arg = books.titles[books.sel], ylim=c(0,50),
                    main="Bekanntheit von Einführungen",
                    ylab="Anteil der TeilnehmerInnen, die damit gearbeitet haben",
                    cex.names = 0.9)
text(the.plot, books.freq[books.sel]+1, labels=paste(books.freq[books.sel], "%", sep=""))

if (save.persistent) dev.off()



###################
# Self-confidence and success

vorb.appr <- kruskal_test(evals[,"Gesamt"]~gl$Vorbildung, conf.int=T,
             distribution = approximate())
vorb.kw <- kruskal.test(evals[,"Gesamt"]~gl$Vorbildung)

vorbildungs <- split(evals[, "Gesamt"], gl$Vorbildung)

if (save.persistent) svg("selbsteinschaetzung.svg")

beanplot(evals[,"Gesamt"]~gl$Vorbildung, col="lightyellow", axes = F,
         main="Selbsteinschätzung und Erfolg",
         xlab="Selbsteinschätzung der Grammatikkenntnisse",
         ylab="Anteil korrekt")
axis(1, 1:5, labels=paste(c("Sehr gut\n(", "Gut\n(", "Mittel\n(", "Schlecht\n(", "Sehr schlecht\n("), summary(gl$Vorbildung)[1:5], ")", sep=""), tick=F)
axis(2, seq(0, 100, 10), labels = paste(seq(0, 100, 10), "%", seqp=""))

if (save.persistent) dev.off()

if (save.persistent) sink("results.txt", append=T)

cat("\n\n ============================================================\n\n")
cat(" Selbsteinschätzung\n\n")

cat(" Gruppenmediane: ", median(vorbildungs$Sehrgut), "<", median(vorbildungs$Gut),
    "<", median(vorbildungs$Mittel), "<", median(vorbildungs$Schlecht),
    "<", median(vorbildungs$Sehrschlecht), "\n")

print(vorb.kw)
print(vorb.appr)

if (save.persistent) sink()



###################
# Success by semester

years <- ordered(ifelse(gl$Fachsemester <= 2, "1", ifelse(gl$Fachsemester <= 4, "2", ifelse(gl$Fachsemester <= 6, "3", "4+"))))
years.kw <- kruskal.test(evals[,"Gesamt"]~years)
years.approx <- kruskal_test(evals[,"Gesamt"]~years, distribution=approximate())
yearse <- split(evals[,"Gesamt"], years)
yearse.median <- as.numeric(unlist(lapply(yearse, median)))

if (save.persistent) svg("semester.svg")

beanplot(evals[,"Gesamt"]~years, col="lightyellow", axes = F,
         main="Studienjahr und Erfolg",
         xlab="Studienjahr", ylab = "Anteil korrekt")
axis(1, 1:4, labels=paste(c("Erstes\n(", "Zweites\n(", "Drittes\n(", "Viertes oder höher\n("), summary(years)[1:4], ")", sep=""), tick=F)
axis(2, seq(0, 100, 10), labels = paste(seq(0, 100, 10), "%", seqp=""))

if (save.persistent) dev.off()

if (save.persistent) sink("results.txt", append=T)

cat("\n\n ============================================================\n\n")
cat(" Effekt des Studienjahrs\n\n")
cat(" Gruppenmediane: ", yearse.median, "\n")

print(years.kw)
print(years.approx)

if (save.persistent) sink()



###################
# Success by "Basismodul"

basis.nein <- evals[which(gl$Basismodul == "Nein"), "Gesamt"]
basis.ja <- evals[which(gl$Basismodul == "Ja"), "Gesamt"]
basis.ja.med <- median(basis.ja)
basis.ja.meen <- mean(basis.ja)
basis.nein.med <- median(basis.nein)
basis.nein.meen <- mean(basis.nein)

basis.kw <- wilcox.test(basis.ja, basis.nein)
basis.exakt <- wilcox_test(evals[,"Gesamt"]~gl$Basismodul, distribution = exact(), conf.int =T)

if (save.persistent) sink("results.txt", append=T)

cat("\n\n ============================================================\n\n")
cat(" Effekt des Basismoduls\n\n")
cat(" Median(ja) = ", basis.ja.med, "\n")
cat(" Mittel(ja) = ", basis.ja.meen, "\n")
cat(" Median(nein) = ", basis.nein.med, "\n")
cat(" Mittel(nein) = ", basis.nein.meen, "\n")

print(basis.kw)
print(basis.exakt)

if (save.persistent) sink()

if (save.persistent) svg("basis.svg")

beanplot(evals[,"Gesamt"]~gl$Basismodul, col="lightyellow", axes = F,
         main="Basismodul (Einführung) und Erfolg",
         xlab = "Einführung in die Sprachwissenschaft (Basismodul) absolviert",
         ylab = "Anteil korrekt")
axis(1, 1:2, labels=paste(levels(gl$Basismodul), "\n(", summary(gl$Basismodul)[1:2], ")", sep=""), tick=F)
axis(2, seq(0, 100, 10), labels = paste(seq(0, 100, 10), "%", seqp=""))

if (save.persistent) dev.off()




###################
# Details about single exercises...


cat("\n\n ============================================================\n\n")
cat(" Fehleranalyse Aufgabe 3\n")

for (i in 1:6) {
  cat("\n\n*** 3.", i, "  Korrekt: ", ml$Correct1[which(ml$Item==paste("3.", i, sep=""))], "\n", sep="")
  print(summary(gl[,paste("3.", i, sep="")]))
}

cat("\n\n ============================================================\n\n")
cat(" Fehleranalyse Aufgabe 5\n")

ids.5 <- c("5.1","5.2","5.3","5.zuviel.ArtPron","5.zuviel.Subst","5.zuviel.Adv","5.zuviel.Praep","5.zuviel.V")

for (id in ids.5) {
  cat("\n\n*** ", id, "  Korrekt: ", ml$Correct1[which(ml$Item==id)], "\n", sep="")
  print(summary(factor(gl[,id])))
}


cat("\n\n ============================================================\n\n")
cat(" Fehleranalyse Aufgabe 1\n")

ids.11 <- c("11.1.NP", "11.1.Kasus", "11.2.NP", "11.2.Kasus", "11.3.NP", "11.3.Kasus", "11.4.NP", "11.4.Kasus", "11.5.NP", "11.5.Kasus", "11.6.NP", "11.6.Kasus", "11.7.NP", "11.7.Kasus", "11.zuviel.1", "11.zuviel.2", "11.zuviel.3", "11.zuviel.4", "11.zuviel.5", "11.zuviel.6", "11.zuviel.7", "11.zuviel.8")

for (id in ids.11) {
  cat("\n\n*** ", id, "  Korrekt: ", ml$Correct1[which(ml$Item==id)], "\n", sep="")
  print(summary(factor(gl[,id])))
}


cat("\n\n ============================================================\n\n")
cat(" Fehleranalyse Aufgabe 7\n")

for (id in c("7.1", "7.2")) {
  cat("\n\n*** ", id, "  Korrekt: ", ml$Correct1[which(ml$Item==id)], "\n", sep="")
  print(summary(factor(gl[,id])))
}

cat("\n\n ============================================================\n\n")
cat(" Fehleranalyse Aufgabe 10\n")

ids.10 <- c("10.Kind", "10.Bind", "10.Offen", "10.Stell", "10.Froh")

for (id in ids.10) {
  cat("\n\n*** ", id, "  Korrekt: ", ml$Correct1[which(ml$Item==id)], "\n", sep="")
  print(summary(factor(gl[,id])))
}

cat(" Aufgabe 10: Gar nicht bearbeitet\n\n")
summary(gl[, "10.NB"])

gestell.ja <- length(which(gl[, "10.NB"] != T & gl[,"10.Gestellstamm"] == T))
gestell.nein <- length(which(gl[, "10.NB"] != T &gl[,"10.Gestellstamm"] == F))

cat(" Aufgabe 10: 'Gestell' als Stamm (nur TN, die die Aufgabe überhaupt bearbeitet haben)\n\n")
cat(" ja = ", gestell.ja, " (", round(gestell.ja/(gestell.ja+gestell.nein)*100, 0), "%)", "\n", sep="")
cat(" nein = ", gestell.nein, " (", round(gestell.nein/(gestell.ja+gestell.nein)*100, 0), "%)", "\n", sep="")

wk.ja <- length(which(gl[, "10.NB"] != T & gl[,"10.Wortklassen"] == T))
wk.nein <- length(which(gl[, "10.NB"] != T &gl[,"10.Wortklassen"] == F))

cat(" Aufgabe 10: Wortklassen (nur TN, die die Aufgabe überhaupt bearbeitet haben)\n\n")
cat(" ja = ", wk.ja, " (", round(wk.ja/(wk.ja+wk.nein)*100, 0), "%)", "\n", sep="")
cat(" nein = ", wk.nein, " (", round(wk.nein/(wk.ja+wk.nein)*100, 0), "%)", "\n", sep="")



###################
# Some basic figures...

if (save.persistent) sink("results.txt", append=T)

cat("\n\n ============================================================\n\n")
cat(" Basisdaten\n")

cat("\n\n --------------------\n")
cat(" Studienfächer\n\n")
print(summary(gl$Studiengang))
print(round(summary(gl$Studiengang)/sum(summary(gl$Studiengang))*100, 2))

cat("\n\n --------------------\n")
cat(" Geschlechter\n\n")
print(summary(gl$Geschlecht))
print(round(summary(gl$Geschlecht)/sum(summary(gl$Studiengang))*100, 2))


cat("\n\n --------------------\n")
cat(" Effekt für Geschlecht \n\n")

maennlich <- which(gl$Geschlecht == "Maennlich")
weiblich <- which(gl$Geschlecht == "Weiblich")
gender.mw <- which(gl$Geschlecht == "Weiblich" | gl$Geschlecht == "Maennlich")
gender.wt <- wilcox.test(evals[maennlich, "Gesamt"], evals[weiblich, "Gesamt"])
gender.exakt <- wilcox_test(evals[gender.mw,"Gesamt"]~gl[gender.mw,"Geschlecht"], distribution = exact(), conf.int =T)

cat("Weiblich (", length(weiblich), "): Median=", median(evals[weiblich, "Gesamt"]), "\n", sep="")
cat("Weiblich (", length(weiblich), "): Mittel=", mean(evals[weiblich, "Gesamt"]), "\n", sep="")
cat("Männlich (", length(maennlich), "):: Median=", median(evals[maennlich, "Gesamt"]), "\n", sep="")
cat("Männlich (", length(maennlich), "):: Mittel=", mean(evals[maennlich, "Gesamt"]), "\n", sep="")
print(gender.wt)
print(gender.exakt)



  cat("\n\n --------------------\n")
cat(" Aufgaben, die überhaupt nicht bearbeitet wurden \n\n")
nbs <- gl[1:220, paste(1:11, ".NB", sep="")]
nbs.count <- as.numeric(apply(nbs, 2, function(x) { length(which(x==T)) } ))
nbs.perc <- round(nbs.count/220*100, 1)
nbs.tab <- rbind(nbs.count,nbs.perc)
colnames(nbs.tab) <- themen
rownames(nbs.tab) <- c("Anzahl", "Prozent")
print(nbs.tab)




cat("\n\n --------------------\n")
cat(" L1-Mehrsprachiger Hintergrund \n\n")

multiling.ja <- which(!is.na(gl$L1.2))
multiling.nein <- which(is.na(gl$L1.2))
multiling.wt <- wilcox.test(evals[multiling.ja, "Gesamt"], evals[multiling.nein, "Gesamt"])
multiling.exakt <- wilcox_test(evals[,"Gesamt"]~factor(is.na(gl$L1.2)), distribution = exact(), conf.int =T)

cat("Mehrsprachig (", length(multiling.ja), "): Median=", median(evals[multiling.ja, "Gesamt"]), "\n", sep="")
cat("Mehrsprachig (", length(multiling.ja), "): Mittel=", mean(evals[multiling.ja, "Gesamt"]), "\n", sep="")
cat("Einsprachig (", length(multiling.nein), "):: Median=", median(evals[multiling.nein, "Gesamt"]), "\n", sep="")
cat("Einsprachig (", length(multiling.nein), "):: Mittel=", mean(evals[multiling.nein, "Gesamt"]), "\n", sep="")
print(multiling.wt)
print(multiling.exakt)

print(summary(gl$L1.1))
print(summary(gl$L1.2))


cat("\n\n --------------------\n")
cat(" Verteilung der 'Noten'\n\n")
print(summary(ordered(noten)))
cat("\n")
cat("Q1=", levels(noten)[sort(noten)[length(noten)/4]], "\n", sep="")
cat("Median=", levels(noten)[sort(noten)[length(noten)/2]], "\n", sep="")
cat("Q3=", levels(noten)[sort(noten)[length(noten)/4+length(noten)/2]], "\n", sep="")

cat("\n\n --------------------\n")
cat(" Vergleich der ErfasserInnen der Daten \n\n")

erfasserin.exakt <- wilcox_test(evals[,"Gesamt"]~gl$Auswerterin, distribution = exact(), conf.int =T)

cat("Ulrike Sayatz\n")
print(summary(evals[which(gl$Auswerterin=="Ulrike"),"Gesamt"]))
cat("Roland Schäfer\n")
print(summary(evals[which(gl$Auswerterin=="Roland"),"Gesamt"]))
print(erfasserin.exakt)



if (save.persistent) sink()

######################################################
# Byee!

if (save.persistent) sink("results.txt", append=T)
cat("\n\n")
if (save.persistent) sink()
