# Padel-Turnierplaner mit Partner- und Gegner-Optimierung

## set.seed(4), max-Iterations 20 000 -> 4 x gleiches Teams
##set.seed(7), max-Iterations 100 000 -> 3 x gleiches Teams, Score 32
##set.seed(8), max-Iterations 100 000 -> 2 x gleiches Teams, Score 32
##set.seed(11), max-Iterations 100 000 -> 1 x gleiches Teams, Score 30
##set.seed(20), max-Iterations 100 000 -> 0 x gleiches Teams, Score 32
set.seed(20) # Für reproduzierbare Ergebnisse

num_rounds <- 8
total_players <- 16

# 1. Feste Pausengruppen definieren
pause_groups <- list(1:4, 5:8, 9:12, 13:16)
active_players <- list()
for (r in 1:num_rounds) {
  group_idx <- ((r - 1) %% 4) + 1
  active_players[[r]] <- setdiff(1:total_players, pause_groups[[group_idx]])
}

# NEU: Kombinierte Bewertungsfunktion für Partner und Gegner
evaluate_schedule <- function(schedule) {
  partner_score <- 0
  opponent_score <- 0
  
  # Matrizen zum Mitzählen während der Prüfung
  partner_matrix <- matrix(0, nrow = total_players, ncol = total_players)
  opponent_matrix <- matrix(0, nrow = total_players, ncol = total_players)
  
  for (r in 1:num_rounds) {
    s <- schedule[[r]]
    
    # Indizes für die 3 Courts (je 4 Spieler)
    courts <- list(s[1:4], s[5:8], s[9:12])
    
    for (court in courts) {
      # Partner-Paare extrahieren (1+2 und 3+4)
      p1 <- court[1:2]
      p2 <- court[3:4]
      
      # Partner eintragen
      partner_matrix[p1[1], p1[2]] <- partner_matrix[p1[1], p1[2]] + 1
      partner_matrix[p2[1], p2[2]] <- partner_matrix[p2[1], p2[2]] + 1
      
      # Gegner eintragen (jeder aus Team 1 gegen jeden aus Team 2)
      for (g1 in p1) {
        for (g2 in p2) {
          opponent_matrix[g1, g2] <- opponent_matrix[g1, g2] + 1
          opponent_matrix[g2, g1] <- opponent_matrix[g2, g1] + 1
        }
      }
    }
  }
  
  # Strafpunkte berechnen: Alles was öfter als 1-mal vorkommt kostet Punkte
  # Partner-Duplikate gewichten wir extrem hoch (Fokus bleibt auf 0 Partner-Duplikate)
  partner_score <- sum(partner_matrix[partner_matrix > 1] - 1) * 1000
  
  # Gegner-Wiederholungen (wenn man 2x oder öfter gegen denselben spielt)
  opponent_score <- sum(opponent_matrix[opponent_matrix > 1] - 1)
  
  return(partner_score + opponent_score)
}

# 2. Optimierungs-Loop (Sucht den Plan mit dem geringsten Penalty-Score)
best_schedule <- list()
best_score <- Inf
max_iterations <- 100000  # Erhöht auf 20k für spürbar bessere Gegnerverteilung

for (iter in 1:max_iterations) {
  current_schedule <- list()
  for (r in 1:num_rounds) {
    current_schedule[[r]] <- sample(active_players[[r]])
  }
  
  current_score <- evaluate_schedule(current_schedule)
  
  if (current_score < best_score) {
    best_score <- current_score
    best_schedule <- current_schedule
    if (best_score == 0) break # Perfekter Plan (weder Partner- noch Gegner-Duplikate)
  }
}

# 3. Spielplan lesbar formatieren
final_plan <- data.frame()
for (r in 1:num_rounds) {
  s <- best_schedule[[r]]
  p_str <- paste(setdiff(1:total_players, active_players[[r]]), collapse = ", ")
  
  round_df <- data.frame(
    Runde = r,
    Aussetzer = p_str,
    Court_1 = paste0("S", s[1], "+S", s[2], " vs S", s[3], "+S", s[4]),
    Court_2 = paste0("S", s[5], "+S", s[6], " vs S", s[7], "+S", s[8]),
    Court_3 = paste0("S", s[9], "+S", s[10], " vs S", s[11], "+S", s[12])
  )
  final_plan <- rbind(final_plan, round_df)
}

# --- AUSGABE DES SPIELPLANS ---
cat("\n=== GEGNER-OPTIMIERTER PADEL-TURNIERPLAN ===\n")
print(final_plan, row.names = FALSE)

# --- MATRIZEN-ANALYSE FÜR KONTROLLE ---
cat("\n=== COURT-BEGEGNUNGS-CHECK ===\n")
court_matrix <- matrix(0, nrow = total_players, ncol = total_players)
rownames(court_matrix) <- paste0("S", 1:total_players)
colnames(court_matrix) <- paste0("S", 1:total_players)

for (r in 1:num_rounds) {
  s <- best_schedule[[r]]
  courts <- list(s[1:4], s[5:8], s[9:12])
  for (court in courts) {
    court_matrix[court, court] <- court_matrix[court, court] + 1
  }
}
diag(court_matrix) <- 0
print(as.data.frame(court_matrix))

# --- PARTNER-DUPLIKAT-CHECK ---
cat("\n=== ANALYSE DER ZWEIER-PAARUNGEN ===\n")
all_partnerships <- c()
for (r in 1:num_rounds) {
  s <- best_schedule[[r]]
  courts <- list(s[1:4], s[5:8], s[9:12])
  for (court in courts) {
    all_partnerships <- c(all_partnerships, paste0("S", min(court[1:2]), " & S", max(court[1:2])))
    all_partnerships <- c(all_partnerships, paste0("S", min(court[3:4]), " & S", max(court[3:4])))
  }
}
paarung_counts <- as.data.frame(table(all_partnerships))
colnames(paarung_counts) <- c("Paarung", "Anzahl_Spiele")
doppelte_paarungen <- paarung_counts[paarung_counts$Anzahl_Spiele > 1, ]

if (nrow(doppelte_paarungen) == 0) {
  cat("Perfekt! Es gibt KEINE doppelten Zweier-Paarungen.\n")
} else {
  cat("Folgende Zweier-Paarungen treten mehrfach auf:\n")
  print(doppelte_paarungen, row.names = FALSE)
}

cat("\nScore-Ergebnis (Je niedriger, desto besser):", best_score, "\n")
