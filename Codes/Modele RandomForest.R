
time = Sys.time ()

# parametrage du proxy
source ("K:\\7- ETUDES\\S - Perf-Meth-Risk\\PRIVE\\Methodologie\\Codes\\parametrage proxy.R")

# On charge les packages qui vont etre utilises
library (lubridate)
library (randomForest)

# On importe l'historique Spot
{ 
  hist.spot = read.csv2 ("Tables\\Historique Spot.csv")
  hist.spot$Date = dmy (as.character (hist.spot$Date))
  hist.spot = hist.spot[1:(which (hist.spot$Prix == 0)[1] - 1), ]
  hist.spot$Delta = hist.spot$Reel - hist.spot$Normale
}

# On importe la table pour les simulations
{
  table.simul = read.csv2 ("Table Simul.csv")
  table.simul$Date = dmy (as.character (table.simul$Date))
  table.simul$WE = dmy (as.character (table.simul$WE))
  table.simul$Semaine = dmy (as.character (table.simul$Semaine))
  table.simul = table.simul[which (table.simul$Date >= as.Date ("2021-01-01")), ]
}

# On ajoute une barre de progression
pb = winProgressBar (title = "Simulation Scenarios Spot Gaz RandomForest", min = 0, max = 10000, initial = 0, width = 3)

# On initialise la simulation a 0
table.simul$Simulation = 0

# On calcul 10 000 simulations avec le modele RandomForest
for (i in 1:10000)
{
  # On calcul une simulation
  table.simul$Simulation = predict (randomForest (Prix~., hist.spot[, -c (1, 8)], ntree = 300, mtry = 7), table.simul)
  # On exporte la simulation
  write.csv2 (table.simul, paste0 ("Export\\Simul ", i, ".csv"), row.names = F)
  setWinProgressBar (pb, value = i, label = paste0 ("Progression : ", round (i / 100, 2), " %"))
}
close (pb)

Sys.time () - time
