############################################################################
# Padel-Turnierplaner mit Partner- und Gegner-Optimierung
# Optimiert mittels Hill-Climbing für 0 Partner-Duplikate
############################################################################

# Settings
num_rounds <- 12
total_players <- 16
pause_groups <- list(13:16, 9:12, 5:8, 1:4)

# Active players per round
active_players <- list()
for (r in 1:num_rounds) {
  group_idx <- ((r - 1) %% 4) + 1
  active_players[[r]] <- setdiff(1:total_players, pause_groups[[group_idx]])
}

# KORRIGIERTE & SYMMETRISCHE Bewertungsfunktion
evaluate_schedule <- function(schedule) {
  # Matrizen zum Mitzählen während der Prüfung
  partner_matrix <- matrix(0, nrow = total_players, ncol = total_players)
  opponent_matrix <- matrix(0, nrow = total_players, ncol = total_players)
  
  for (r in 1:num_rounds) {
    s <- schedule[[r]]
    courts <- list(s[1:4], s[5:8], s[9:12])
    
    for (court in courts) {
      p1 <- court[1:2]
      p2 <- court[3:4]
      
      # Partner IMMER symmetrisch eintragen (beide Richtungen)
      partner_matrix[p1[1], p1[2]] <- partner_matrix[p1[1], p1[2]] + 1
      partner_matrix[p1[2], p1[1]] <- partner_matrix[p1[2], p1[1]] + 1
      partner_matrix[p2[1], p2[2]] <- partner_matrix[p2[1], p2[2]] + 1
      partner_matrix[p2[2], p2[1]] <- partner_matrix[p2[2], p2[1]] + 1
      
      # Gegner symmetrisch eintragen
      for (g1 in p1) {
        for (g2 in p2) {
          opponent_matrix[g1, g2] <- opponent_matrix[g1, g2] + 1
          opponent_matrix[g2, g1] <- opponent_matrix[g2, g1] + 1
        }
      }
    }
  }
  
  # Strafpunkte berechnen
  # Partner-Duplikate massiv bestrafen
  partner_score <- sum(partner_matrix[partner_matrix > 1] - 1) * 1000
  # Gegner-Duplikate einfach bestrafen
  opponent_score <- sum(opponent_matrix[opponent_matrix > 1] - 1)
  
  return(partner_score + opponent_score)
}


# 2. Erweiterte Hill-Climbing-Optimierung für einen perfekten Score von 0
best_score <- Inf
best_schedule <- list()

cat("Suche nach dem perfekten Spielplan (Score = 0)... Bitte warten...\n")

# Die Schleife läuft so lange, bis ein perfekter Score (0) gefunden wird
while (best_score > 0) {
  
  # Generiere einen frischen, zufälligen Startplan (Random Restart)
  current_schedule <- list()
  for (r in 1:num_rounds) {
    current_schedule[[r]] <- sample(active_players[[r]])
  }
  current_score <- evaluate_schedule(current_schedule)
  
  # Lokale Optimierung für diesen Startplan
  no_improvement_counter <- 0
  max_local_steps <- 3000 # Wenn sich nach 3000 Versuchen nichts verbessert, Sackgasse wechseln
  
  while (no_improvement_counter < max_local_steps) {
    if (current_score == 0) {
      best_score <- current_score
      best_schedule <- current_schedule
      break
    }
    
    # Kopiere und mutiere (Spieler-Tausch in einer zufälligen Runde)
    candidate_schedule <- current_schedule
    r_to_mutate <- sample(1:num_rounds, 1)
    swap_idx <- sample(1:12, 2)
    
    tmp <- candidate_schedule[[r_to_mutate]][swap_idx[1]]
    candidate_schedule[[r_to_mutate]][swap_idx[1]] <- candidate_schedule[[r_to_mutate]][swap_idx[2]]
    candidate_schedule[[r_to_mutate]][swap_idx[2]] <- tmp
    
    candidate_score <- evaluate_schedule(candidate_schedule)
    
    # Akzeptiere Verbesserungen ODER gleichwertige Scores (um Plateaus zu überwinden)
    if (candidate_score <= current_score) {
      if (candidate_score < current_score) {
        no_improvement_counter <- 0
      } else {
        no_improvement_counter <- no_improvement_counter + 1
      }
      current_score <- candidate_score
      current_schedule <- candidate_schedule
    } else {
      no_improvement_counter <- no_improvement_counter + 1
    }
  }
  
  # Globalen Bestwert updaten, falls wir näher herangekommen sind
  if (current_score < best_score) {
    best_score <- current_score
    best_schedule <- current_schedule
  }
}

cat("Erfolg! Ein mathematisch perfekter Turnierplan wurde gefunden.\n")
