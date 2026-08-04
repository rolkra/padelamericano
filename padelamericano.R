library(openxlsx)

create_americano_strict_mix <- function(num_courts, num_rounds, iterations = 5000) {
  
  num_players <- num_courts * 4
  message(paste("Generiere Spielplan für", num_players, "Spieler auf", num_courts, "Courts."))
  message("Fokus: Minimierung JEDER doppelten Begegnung auf demselben Court (Partner & Gegner).")
  
  best_schedule <- NULL
  best_penalty <- Inf
  
  for (it in 1:iterations) {
    current_schedule <- list()
    
    # 1. Partner-Matrix: Wie oft waren X und Y im selben Team?
    partner_matrix <- matrix(0, nrow = num_players, ncol = num_players)
    # 2. Court-Matrix: Wie oft waren X und Y überhaupt am selben Platz? (Egal ob Partner/Gegner)
    court_matrix <- matrix(0, nrow = num_players, ncol = num_players)
    
    current_penalty <- 0
    
    for (r in 1:num_rounds) {
      active_players <- sample(1:num_players) # Zufällige Basis-Durchmischung für diese Runde
      round_games <- list()
      
      for (c in 1:num_courts) {
        idx <- ((c - 1) * 4 + 1):(c * 4)
        court_players <- active_players[idx]
        
        p1 <- court_players[1]
        p2 <- court_players[2]
        p3 <- court_players[3]
        p4 <- court_players[4]
        
        # --- Strafberechnung für ALLE Paarungen auf diesem Court ---
        
        # 1. Allgemeine Court-Begegnungen (Jeder mit jedem auf diesem Platz)
        # Wir prüfen alle 6 möglichen Zweier-Kombinationen auf dem Court
        all_pairs <- list(c(p1,p2), c(p1,p3), c(p1,p4), c(p2,p3), c(p2,p4), c(p3,p4))
        court_penalty <- 0
        for (pair in all_pairs) {
          # Jedes Mal, wenn man sich schon mal am selben Platz sah, gibt es Strafpunkte
          court_penalty <- court_penalty + court_matrix[pair[1], pair[2]]
        }
        
        # 2. Spezifische Partner-Begegnungen (Team 1: p1+p2 | Team 2: p3+p4)
        # Doppelte Mitspieler werden extrem hart (zusätzlich) bestraft
        p_penalty <- partner_matrix[p1, p2] + partner_matrix[p3, p4]
        
        # Gesamte Strafe für dieses Spiel berechnen und aufaddieren
        # Quadratische Gewichtung: Wiederholungen wiegen exponentiell schwerer!
        # Partner-Wiederholung wiegt hier durch den Faktor 25 massiv schwerer als reine Court-Begegnung (Faktor 5)
        current_penalty <- current_penalty + (court_penalty^2 * 5) + (p_penalty^2 * 25)
        
        # --- Matrizen für die Zukunft aktualisieren ---
        
        # Partner eintragen
        partner_matrix[p1, p2] <- partner_matrix[p1, p2] + 1
        partner_matrix[p2, p1] <- partner_matrix[p2, p1] + 1
        partner_matrix[p3, p4] <- partner_matrix[p3, p4] + 1
        partner_matrix[p4, p3] <- partner_matrix[p4, p3] + 1
        
        # Alle Court-Begegnungen eintragen
        for (pair in all_pairs) {
          court_matrix[pair[1], pair[2]] <- court_matrix[pair[1], pair[2]] + 1
          court_matrix[pair[2], pair[1]] <- court_matrix[pair[2], pair[1]] + 1
        }
        
        round_games[[c]] <- data.frame(
          Runde = r, Court = c,
          Spieler_1 = paste("Spieler", p1), Spieler_2 = paste("Spieler", p2),
          Spieler_3 = paste("Spieler", p3), Spieler_4 = paste("Spieler", p4),
          stringsAsFactors = FALSE
        )
      }
      current_schedule[[r]] <- do.call(rbind, round_games)
    }
    
    # Wenn dieser Gesamt-Turnierplan besser ist als der bisher beste, speichern wir ihn
    if (current_penalty < best_penalty) {
      best_penalty <- current_penalty
      best_schedule <- do.call(rbind, current_schedule)
    }
    
    # Bei einem Penalty-Score von 0 steht fest: Niemand teilt sich jemals ein zweites Mal einen Court
    if (best_penalty == 0) break
  }
  
  # Formatierung für die Rückgabe
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
  
  message(paste("Optimierung abgeschlossen. Finaler Penalty-Score:", best_penalty))
  return(final_df)
}

# --- Excel-Export Funktion ---
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

out <- create_americano_strict_mix(
  num_courts = 3, 
  num_rounds = 4, 
  iterations = 1000) 

## result

# View(out)
# export_schedule_to_excel(out)  
