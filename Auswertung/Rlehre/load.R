rm(list = ls())

require(gnumeric)
require(plyr)

set.seed(4711)

# Adapt this to point to the actual directory containing the data files.
setwd("~/Data/linguistics/ulrike/Grammatiklehre/Auswertung/Rlehre")

join.factors <- function(...) {
  unlist(list(...))
}

order.by.freq <- function(f) {
  tb <- table(f)
  factor(f, levels = names(tb[order(tb, decreasing = TRUE)]))
}

hist.discrete <- function(x, ...) {
  Rle <- rle(sort(x))
  barplot(Rle$lengths, ...)
  axis(side = 1, at = Rle$values, labels = Rle$values, tick = F)
}

linear.map <- function(x, from, to)
  (x - min(x)) / max(x - min(x)) * (to - from) + from


# Load, transpose and paste.
ulrike <- as.data.frame(read.gnumeric.sheet(file = "../auswertung_fertig.ods", head = F, sheet.name = "ulrike"))
roland <- as.data.frame(read.gnumeric.sheet(file = "../auswertung_fertig.ods", head = F, sheet.name = "roland"))
gl <- as.data.frame(rbind(t(ulrike)[-1,], t(roland)[-1,] ))
colnames(gl) <- ulrike[,1]

# Fix numeric variables.
numerics.null <- c("5.zuviel.ArtPron", "5.zuviel.Subst", "5.zuviel.Adv", "5.zuviel.Praep", "5.zuviel.V")
for (num in numerics.null) {
  gl[,num] <- as.character(gl[,num])
  gl[,num] <- ifelse(gl[,num] == "", 0, gl[,num])
  gl[,num] <- as.integer(gl[,num])
}

numerics.na <- c("Fachsemester", "Alter", "L2.1.Jahre", "L2.2.Jahre", "L2.3.Jahre")
for (num in numerics.na) {
  gl[,num] <- as.character(gl[,num])
  gl[,num] <- ifelse(gl[,num] == "", NA, gl[,num])
  gl[,num] <- as.integer(gl[,num])
}

# Fix factors where "" means TRUE or FALSE
null2nein <- c("1.NB", "2.NB", "3.NB", "4.NB", "5.NB", "6.NB", "7.NB", "8.NB", "9.NB", "10.NB", "11.NB",
               "10.Wortklassen", "10.Gestellstamm")
for (fac in null2nein) {
  gl[,fac] <- as.factor(as.character(ifelse(gl[,fac]=="", FALSE, TRUE)))
}

# Fix evaluation questions.
understands <- c("1.Verstaendlichkeit", "2.Verstaendlichkeit", "3.Verstaendlichkeit", "4.Verstaendlichkeit",
                 "5.Verstaendlichkeit", "6.Verstaendlichkeit", "7.Verstaendlichkeit", "8.Verstaendlichkeit",
                 "9.Verstaendlichkeit", "10.Verstaendlichkeit", "11.Verstaendlichkeit")
for (fac in understands) {
  gl[,fac] <- ordered(gl[,fac], levels=c("Sehrgut", "Gut", "Schlecht", "Sehrschlecht"))
}

difficults <- c("1.Schwierigkeit", "2.Schwierigkeit", "3.Schwierigkeit", "4.Schwierigkeit", "5.Schwierigkeit",
                "6.Schwierigkeit", "7.Schwierigkeit", "8.Schwierigkeit", "9.Schwierigkeit", "10.Schwierigkeit",
                "11.Schwierigkeit")
for (fac in difficults) {
  gl[,fac] <- as.character(gl[,fac])
  gl[which(gl[,fac] == "SehrLeicht"), fac] <- "Sehrleicht"
  gl[which(gl[,fac] == "SehrSchwierig"), fac] <- "Sehrschwierig"
  gl[,fac] <- ordered(gl[,fac], levels=c("Sehrleicht", "Leicht", "Schwierig", "Sehrschwierig"))
}

# Ordered factors that need fixing with level order.
gl$Vorbildung <- ordered(gl$Vorbildung, levels=c("Sehrgut", "Gut", "Mittel", "Schlecht", "Sehrschlecht"))
gl$Grammatikkentnisse <- ordered(gl$Grammatikkentnisse, levels=c("7", "6", "5", "4", "3", "2", "1"))

# Make presentation of options more consistent.
touppers <- paste("3.", seq(1, 6), sep="")
for (tu in touppers) {
  gl[,tu] <- toupper(gl[,tu])
}

# Fix all ordinary factors, incl NA.
fax <- c("Kursnummer", "Auswerterin", "Studiengang", "Geschlecht", "Basismodul", "L1.1", "L1.2",
         "L2.1", "L2.2", "L2.3", "Bundesland1", "Bundesland2", "Einfuehrung1", "Einfuehrung2",
         paste("1.", seq(1,8), sep=""), paste("2.", seq(1,19), sep=""), paste("3.", seq(1,6), sep=""),
         paste("4.", seq(1,4), sep=""), paste("5.", seq(1,3), sep=""), paste("6.", seq(1,3), sep=""), 
         paste("7.", seq(1,2), sep=""), paste("8.", seq(1,4), sep=""), paste("9.", seq(1,5), sep=""),
         paste("11.", seq(1,7), ".NP", sep=""), paste("11.", seq(1,7), ".Kasus", sep=""),
         "10.Kind", "10.Bind", "10.Offen", "10.Stell", "10.Froh",
         "11.Konzeptfehler", paste("11.zuviel.", seq(1,8), sep=""))
for (fac in fax) {
  gl[,fac] <- factor(ifelse(gl[,fac] == "", NA, as.character(gl[,fac])))
}

##### Generate scoring #####

# Load model solution.
ml <- as.data.frame(read.gnumeric.sheet(file = "../musterloesung.ods", head = F, sheet.name = "musterloesung"))
colnames(ml) <- c("Item", "Group", "Correct1", "Op1", "Val1", "Correct2", "Op2", "Val2")
ml[,"Item"] <- as.character(ml[,"Item"])
ml[,"Group"] <- as.integer(ml[,"Group"])

ml[,"Op1"] <- as.character(ml[,"Op1"])
ml[,"Val1"] <- as.numeric(ml[,"Val1"])
ml[,"Correct1"] <- as.character(ml[,"Correct1"])

ml[,"Op2"] <- as.character(ml[,"Op2"])
ml[,"Val2"] <- as.numeric(ml[,"Val2"])
ml[,"Correct2"] <- as.character(ml[,"Correct2"])


# Pass a data row r from experiment and model solution s.
eval.gl <- function(roww, solution) {
  res <- rep(0, length(unique(sort(ml[,2]))))

  for (s in seq(1,nrow(solution))) {
    it <- as.character(solution[s,"Item"])

    if (solution[s, "Op1"] == "IF") {
      if (!(is.na(roww[it])) && solution[s, "Correct1"] == roww[it])
        res[solution[s, "Group"]] <- res[solution[s, "Group"]] + solution[s, "Val1"]
    }
    else if (solution[s, "Op1"] == "IFNNA") {
      if (!is.na(roww[it])) {
        res[solution[s, "Group"]] <- res[solution[s, "Group"]] + solution[s, "Val1"]
      }
    }
    else if (solution[s, "Op1"] == "MUL") {
      if (!is.na(roww[it])) {
        res[solution[s, "Group"]] <- res[solution[s, "Group"]] + as.numeric(roww[it])*solution[s, "Val1"]
      }
    }
    
    if (solution[s, "Op2"] == "IF") {
      if (!(is.na(roww[it])) && solution[s, "Correct2"] == roww[it])
        res[solution[s, "Group"]] <- res[solution[s, "Group"]] + solution[s, "Val2"]
    }
    else if (solution[s, "Op2"] == "IFNNA") {
      if (!is.na(roww[it])) {
        res[solution[s, "Group"]] <- res[solution[s, "Group"]] + solution[s, "Val2"]
      }
    }
    else if (solution[s, "Op2"] == "MUL") {
      if (!is.na(roww[it])) {
        res[solution[s, "Group"]] <- res[solution[s, "Group"]] + as.numeric(roww[it])*solution[s, "Val2"]
      }
    }
    
  }
  res
}


to.schulnoten <- function(x) {
  if      (x < 51) r <- 5.0
  else if (x < 56) r <- 4.0
  else if (x < 61) r <- 3.7
  else if (x < 66) r <- 3.3
  else if (x < 71) r <- 3.0
  else if (x < 76) r <- 2.7
  else if (x < 81) r <- 2.3
  else if (x < 86) r <- 2.0
  else if (x < 91) r <- 1.7
  else if (x < 96) r <- 1.3
  else             r <- 1.0
  r
}

# Get results for single questions.
evals <- as.matrix(t(apply(gl, 1, function(r) {eval.gl(r, ml)})))

# get overall score WITHOUT q. 8 (designated "test question").
evals <- cbind(evals, apply(evals[,-8], 1, function(x) { sum(x)/10 } ))

# Turn into percent values for better readability.
evals <- round(evals*100, 2)

# Set column names.
colnames(evals) <- c(paste(c("Aufgabe"), seq(1,11), sep = ""), "Gesamt")
noten <- factor(unlist(lapply(evals[,"Gesamt"], to.schulnoten)))


themen <- c("Diathese", "S/P/O/Adv (Konst. gegeben)", "NP-Struktur", "Objekte & Adverbiale", "Attribute",
            "Konjunktiv", "Nebensätze", "Relativpronomina", "Majuskeln & Spatien",
            "Wortfamilien", "Dativ & Akkusativ (Konst. frei)")
