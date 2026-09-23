############################################################################
# Padel-Turnierplaner mit Partner- und Gegner-Optimierung
# Optimiert mittels Hill-Climbing
############################################################################


# Setup -------------------------------------------------------------------

num_rounds <- 8
total_players <- 16
pause_groups <- list(13:16, 9:12, 5:8, 1:4)

MAX_LOCAL_STEPS <- 80000  # Wie viele Tausch-Versuche wir pro Start zulassen
MAX_ROUNDS <- 256

# Prepare -----------------------------------------------------------------

get_active_players <- function(total_players, pause_groups, num_rounds)  {

  # Active players per round
  active_players <- list()
  for (r in 1:num_rounds) {
    group_idx <- ((r - 1) %% 4) + 1
    active_players[[r]] <- setdiff(1:total_players, pause_groups[[group_idx]])
  }
  active_players
}

setup <- list(
  num_rounds = num_rounds,
  total_players = total_players,
  pause_groups = pause_groups,
  active_players = get_active_players(total_players, pause_groups, num_rounds),
  max_local_steps = MAX_LOCAL_STEPS,
  max_rounds = MAX_ROUNDS
  )


# Evaluate ----------------------------------------------------------------

evaluate_schedule <- function(schedule, setup, describe = FALSE) {
  # Matrizen zum Mitzählen während der Prüfung
  partner_matrix <- matrix(0, nrow = setup$total_players, ncol = setup$total_players)
  opponent_matrix <- matrix(0, nrow = setup$total_players, ncol = setup$total_players)
  
  for (r in 1:setup$num_rounds) {
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
      opponent_matrix[court[1], court[2]] <- opponent_matrix[court[1], court[2]] + 1
      opponent_matrix[court[1], court[3]] <- opponent_matrix[court[1], court[3]] + 1
      opponent_matrix[court[1], court[4]] <- opponent_matrix[court[1], court[4]] + 1
      opponent_matrix[court[2], court[3]] <- opponent_matrix[court[2], court[3]] + 1
      opponent_matrix[court[2], court[4]] <- opponent_matrix[court[2], court[4]] + 1
      opponent_matrix[court[3], court[4]] <- opponent_matrix[court[3], court[4]] + 1

      opponent_matrix[court[2], court[1]] <- opponent_matrix[court[2], court[1]] + 1
      opponent_matrix[court[3], court[1]] <- opponent_matrix[court[3], court[1]] + 1
      opponent_matrix[court[4], court[1]] <- opponent_matrix[court[4], court[1]] + 1
      opponent_matrix[court[3], court[2]] <- opponent_matrix[court[3], court[2]] + 1
      opponent_matrix[court[4], court[2]] <- opponent_matrix[court[4], court[2]] + 1
      opponent_matrix[court[4], court[3]] <- opponent_matrix[court[4], court[3]] + 1
      
    }
  }
  
  # Strafpunkte berechnen
  # Partner-Duplikate massiv bestrafen
  partner_multiple <- sum(partner_matrix[partner_matrix > 1]) / 2
  # Gegner-Duplikate einfach bestrafen
  opponent_multiple <- sum(opponent_matrix[opponent_matrix > 1]) / 2
  opponent_multiple_max <- max(opponent_matrix)
  # Wie oft Gegner-Duplikate max pro Person
  opponent_multiple_pers <- max(rowSums(opponent_matrix > 1))
  
  #return_score <- ((partner_multiple * 1000 + opponent_multiple) + opponent_multiple_max * 100 + opponent_multiple_pers)
  return_score <- ((partner_multiple * 1000 + opponent_multiple) + opponent_multiple_max * 100 + opponent_multiple_pers * 10)
  return_string <- paste("  | same team", partner_multiple, "| same court", opponent_multiple, "| same court max", opponent_multiple_max, "| same court person", opponent_multiple_pers, "\n")
  
  if (describe == TRUE) {  
    cat(return_string)
  }
  
  return(return_score)
}


# Optimize ----------------------------------------------------------------

# 2. Erweiterte Hill-Climbing-Optimierung für einen perfekten Score von 0
best_score <- Inf
best_schedule <- list()
rounds <- 0

cat("Suche nach dem perfekten Spielplan (Score = 0)... Bitte warten...\n")

# Die Schleife läuft so lange, bis ein perfekter Score (0) gefunden wird
while (best_score > 0 && rounds <= MAX_ROUNDS) {
  
  # Generiere einen frischen, zufälligen Startplan (Random Restart)
  current_schedule <- list()
  for (r in 1:num_rounds) {
    current_schedule[[r]] <- sample(active_players[[r]])
  }
  current_score <- evaluate_schedule(current_schedule, setup = setup)
  
  # Lokale Optimierung für diesen Startplan
  no_improvement_counter <- 0
  max_local_steps <- MAX_LOCAL_STEPS # Wenn sich nach 3000 Versuchen nichts verbessert, Sackgasse wechseln
  
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
    
    candidate_score <- evaluate_schedule(candidate_schedule, setup)
    
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
  
  rounds <- rounds + 1
  cat("Runde =", rounds, "| Score =", best_score)
  evaluate_schedule(best_schedule, describe = TRUE, setup = setup)
}


# Output ------------------------------------------------------------------

format_schedule <- function(schedule, setup)  {

  final_plan <- data.frame()
  for (r in 1:setup$num_rounds) {
    
    # scheduled player (excluding Aussetzer)
    s <- schedule[[r]]
  
    # Aussetzer
    a <- setdiff(1:setup$total_players, setup$active_players[[r]])
    
    # combine both
    p <- c(s,a)
    
    #p_str <- paste(setdiff(1:total_players, active_players[[r]]), collapse = ", ")
    
    for (court in seq_len(length(p)/4))  {
    
      round_df <- data.frame(
        round = r,
        court = court,
        t1p1 = p[1 + (court-1)*4],
        t1p2 = p[2 + (court-1)*4],
        vs = "vs",
        t2p1 = p[3 + (court-1)*4],
        t2p2 = p[4 + (court-1)*4]
      )
      final_plan <- rbind(final_plan, round_df)
    }  # for court
  } # for round

  final_plan
  
} # function format_schedule

final_plan <- format_schedule(best_schedule, setup)

# --- AUSGABE DES SPIELPLANS ---
cat("\n=== GEGNER-OPTIMIERTER PADEL-TURNIERPLAN ===\n")
#print(final_plan, row.names = FALSE)
# formatierung funktioniert nicht

best_schedule

# --- PARTNER-DUPLIKAT-CHECK ---
cat("\n=== ANALYSE DER ZWEIER-PAARUNGEN ===\n")
all_partnerships <- c()
for (r in 1:num_rounds) {
  s <- best_schedule[[r]]
  courts <- list(s[1:4], s[5:8], s[9:12])
  for (court in courts) {
    all_partnerships <- c(all_partnerships, paste0("S.", min(court[1:2]), " & S.", max(court[1:2])))
    all_partnerships <- c(all_partnerships, paste0("S.", min(court[3:4]), " & S.", max(court[3:4])))
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

# --- NEU: GEGNER-DUPLIKAT-CHECK ---
cat("\n=== ANALYSE DER GEGNER-WIEDERHOLUNGEN ===\n")
all_opponents <- c()
for (r in 1:num_rounds) {
  s <- best_schedule[[r]]
  courts <- list(s[1:4], s[5:8], s[9:12])
  for (court in courts) {
    p1 <- court[1:2]
    p2 <- court[3:4]
    for (g1 in p1) {
      for (g2 in p2) {
        all_opponents <- c(all_opponents, paste0("S", min(g1, g2), " vs S", max(g1, g2)))
      }
    }
  }
}
gegner_counts <- as.data.frame(table(all_opponents))
colnames(gegner_counts) <- c("Begegnung", "Anzahl_Duelle")
doppelte_gegner <- gegner_counts[gegner_counts$Anzahl_Duelle > 1, ]

if (nrow(doppelte_gegner) == 0) {
  cat("Unglaublich! Es gibt KEINE doppelten Gegner-Begegnungen.\n")
} else {
  cat("Folgende Spieler treten mehrfach gegeneinander an:\n")
  print(doppelte_gegner, row.names = FALSE)
}


cat("\nScore:", best_score, "\n")


