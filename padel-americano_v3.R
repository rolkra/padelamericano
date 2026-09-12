library(openxlsx)

create_americano_balanced <- function(num_courts, num_rounds, player_names = NULL, skill_levels = NULL, iterations = 5000) {
  
  num_players <- num_courts * 4
  
  # --- 1. Spielernamen initialisieren (Default, falls NULL) ---
  if (is.null(player_names)) {
    player_names <- paste("Spieler", 1:num_players)
  } else if (length(player_names) != num_players) {
    stop(paste("Fehler: Für", num_courts, "Courts werden exakt", num_players, 
               "Spieler benötigt. Du hast jedoch", length(player_names), "Namen übergeben."))
  }
  
  # --- 2. Spielstärken initialisieren (Default = 1 für alle, falls NULL) ---
  if (is.null(skill_levels)) {
    skill_levels <- rep(1, num_players)
  } else if (length(skill_levels) != num_players) {
    stop(paste("Fehler: Die Anzahl der Spielstärken (", length(skill_levels), 
               ") muss exakt der Spieleranzahl (", num_players, ") entsprechen."))
  }
  
  message(paste("Generiere Spielplan für", num_players, "Spieler auf", num_courts, "Courts."))
  message("Optimierungs-Fokus: Maximale Durchmischung & maximal ausgeglichene Team-Stärken.")
  
  best_schedule <- NULL
  best_penalty <- Inf
  best_partner_matrix <- NULL
  best_court_matrix <- NULL
  
  # --- 3. Optimierungsschleife ---
  for (it in 1:iterations) {
    current_schedule <- list()
    
    partner_matrix <- matrix(0, nrow = num_players, ncol = num_players)
    court_matrix <- matrix(0, nrow = num_players, ncol = num_players)
    current_penalty <- 0
    
    for (r in 1:num_rounds) {
      active_players <- sample(1:num_players)
      round_games <- list()
      
      for (c in 1:num_courts) {
        idx <- ((c - 1) * 4 + 1):(c * 4)
        court_players <- active_players[idx]
        
        p1 <- court_players[1]; p2 <- court_players[2]
        p3 <- court_players[3]; p4 <- court_players[4]
        
        # --- A. Durchmischungs-Strafen (Court & Partner) ---
        all_pairs <- list(c(p1,p2), c(p1,p3), c(p1,p4), c(p2,p3), c(p2,p4), c(p3,p4))
        court_penalty <- 0
        for (pair in all_pairs) {
          court_penalty <- court_penalty + court_matrix[pair[1], pair[2]]
        }
        
        p_penalty <- partner_matrix[p1, p2] + partner_matrix[p3, p4]
        
        # --- B. NEU: Spielstärke-Ausgleich (Fairness-Penalty) ---
        # Summe Team 1 vs. Summe Team 2
        team1_strength <- skill_levels[p1] + skill_levels[p2]
        team2_strength <- skill_levels[p3] + skill_levels[p4]
        
        # Die Differenz wird quadriert, damit unfaire Spiele extrem hart bestraft werden
        balance_penalty <- (team1_strength - team2_strength)^2
        
        # Gesamte Strafe für diese Runde berechnen und aufaddieren
        current_penalty <- current_penalty + 
          (court_penalty^2 * 5) + 
          (p_penalty^2 * 25) + 
          (balance_penalty * 15) # Gewichtung für faire Matches
        
        # Matrizen aktualisieren
        partner_matrix[p1, p2] <- partner_matrix[p1, p2] + 1
        partner_matrix[p2, p1] <- partner_matrix[p2, p1] + 1
        partner_matrix[p3, p4] <- partner_matrix[p3, p4] + 1
        partner_matrix[p4, p3] <- partner_matrix[p4, p3] + 1
        
        for (pair in all_pairs) {
          court_matrix[pair[1], pair[2]] <- court_matrix[pair[1], pair[2]] + 1
          court_matrix[pair[2], pair[1]] <- court_matrix[pair[2], pair[1]] + 1
        }
        
        round_games[[c]] <- data.frame(
          Runde = r, Court = c,
          Spieler_1 = player_names[p1], Spieler_2 = player_names[p2],
          Spieler_3 = player_names[p3], Spieler_4 = player_names[p4],
          stringsAsFactors = FALSE
        )
      }
      current_schedule[[r]] <- do.call(rbind, round_games)
    }
    
    if (current_penalty < best_penalty) {
      best_penalty <- current_penalty
      best_schedule <- do.call(rbind, current_schedule)
      best_partner_matrix <- partner_matrix
      best_court_matrix <- court_matrix
    }
  }
  
  # --- 4. Detaillierte Durchmischungs-Analyse ausgeben ---
  upper_tri_partner <- best_partner_matrix[upper.tri(best_partner_matrix)]
  upper_tri_court <- best_court_matrix[upper.tri(best_court_matrix)]
  
  partner_counts <- table(upper_tri_partner)
  court_counts <- table(upper_tri_court)
  
  cat("\n======================================================\n")
  cat("                TURNIER-ANALYSE (FINALER PLAN)          \n")
  cat("======================================================\n")
  cat(paste("Paare, die NIE zusammen gespielt haben:      ", ifelse(is.na(partner_counts["0"]), 0, partner_counts["0"]), "\n"))
  cat(paste("Paare, die EXAKT 1x zusammen gespielt haben: ", ifelse(is.na(partner_counts["1"]), 0, partner_counts["1"]), "\n"))
  
  wiederholte_partner <- names(partner_counts)[as.numeric(names(partner_counts)) > 1]
  if(length(wiederholte_partner) > 0) {
    for(w in wiederholte_partner) {
      cat(paste("⚠️ ACHTUNG: Paare, die", w, "x zusammen gespielt haben: ", partner_counts[w], "\n"))
    }
  } else {
    cat("✅ Perfekt: Keine Person musste doppelt mit demselben Partner spielen!\n")
  }
  
  cat("\n--- COURT-Konstellationen (Zusammen auf demselben Platz) ---\n")
  cat(paste("Paare, die NIE am selben Court waren:       ", ifelse(is.na(court_counts["0"]), 0, court_counts["0"]), "\n"))
  cat(paste("Paare, die EXAKT 1x am selben Court waren:  ", ifelse(is.na(court_counts["1"]), 0, court_counts["1"]), "\n"))
  
  wiederholte_courts <- names(court_counts)[as.numeric(names(court_counts)) > 1]
  if(length(wiederholte_courts) > 0) {
    for(w in wiederholte_courts) {
      cat(paste("ℹ️ Info: Paare, die", w, "x am selben Court waren:       ", court_counts[w], "\n"))
    }
  } else {
    cat("✅ Perfekt: Jede Begegnung auf dem Platz war absolut einzigartig!\n")
  }
  
  cat("======================================================\n\n")
  
  final_df <- data.frame(
    Runde = best_schedule$Runde,
    Court = best_schedule$Court,
    `Team 1 - Spieler A` = best_schedule$Spieler_1,
    `Team 1 - Spieler B` = best_schedule$Spieler_2,
    `vs` = "vs.",
    `Team 2 - Spieler A` = best_schedule$Spieler_3,
    `Team 2 - Spieler B` = best_schedule$Spieler_4,
    check.names = FALSE
  )
  
  return(final_df)
}

# --- 5. Stabile Excel-Export Funktion ---
export_schedule_to_excel <- function(schedule_df, filename = "Padel_Americano_Spielplan.xlsx") {
  wb <- createWorkbook()
  addWorksheet(wb, "Spielplan")
  
  header_style <- createStyle(fontName = "Arial", fontSize = 11, fontColour = "#FFFFFF", 
                              fgFill = "#1F497D", halign = "center", textDecoration = "bold")
  data_style <- createStyle(fontName = "Arial", fontSize = 10, halign = "center")
  vs_style <- createStyle(fontName = "Arial", fontSize = 10, halign = "center", fontColour = "#808080", textDecoration = "italic")
  
  writeData(wb, "Spielplan", schedule_df, startRow = 1, startCol = 1, rowNames = FALSE)
  addStyle(wb, "Spielplan", style = header_style, rows = 1, cols = 1:ncol(schedule_df), gridExpand = TRUE)
  addStyle(wb, "Spielplan", style = data_style, rows = 2:(nrow(schedule_df) + 1), cols = c(1,2,3,4,6,7), gridExpand = TRUE)
  addStyle(wb, "Spielplan", style = vs_style, rows = 2:(nrow(schedule_df) + 1), cols = 5, gridExpand = TRUE)
  
  setColWidths(wb, "Spielplan", cols = 1:ncol(schedule_df), widths = "auto")
  saveWorkbook(wb, filename, overwrite = TRUE)
  message(paste("Excel-Datei erfolgreich generiert:", filename))
}

# Spieler und ihre jeweilige Spielstärke (Index-Reihenfolge muss identisch sein!)
meine_spieler <- c("Kristiina P", "Rebecca", "Jasmin", "Filip", "Fiona", "Joseph", "Bianca", "Stefan R", "Kristina M", "Silvio", "Kathrin", "Tim")
meine_staerken <- c(0,             0,         0,            1,      0,        1,       0,       1,     0,           1,        0,         0)

# Plan berechnen (2 Courts = 8 Spieler)
padel_fair_turnier <- create_americano_balanced(
  num_courts = 3,
  num_rounds = 4,
  player_names = meine_spieler,
  skill_levels = meine_staerken,
  iterations = 1000 # Mehr Iterationen helfen, den perfekten Kompromiss aus Fairness und Mix zu finden
)

# Exportieren
export_schedule_to_excel(padel_fair_turnier, filename = "Padel_Faire_Matches.xlsx")
