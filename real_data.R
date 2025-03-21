# ============================================
# 1. Chargement des Bibliothèques
# ============================================

library(dplyr)
library(changepoint)
# devtools::install_github("lvaudor/hubr") # Décommenter pour installer si nécessaire
library(hubr)
library(Rbeast)
library(strucchange)
library(ecp)
library(cpm)
library(RPostgreSQL)
library(sf)
library(purrr)
library(tidyr)
library(cumSeg)
library(reticulate)
library(readr)
library(ggplot2)
library(VGAM)
library(gridExtra)
library(patchwork)

# ============================================
# 2. Chargement et Préparation des Données
# ============================================

# Importation des données du Drac
Data <- read.csv("data_fct.csv")

# Tri des données selon la colonne 'measure' (assumant que 'measure' est une colonne pertinente)
Data <- arrange(Data, measure)

# Nettoyage des données :
# Suppression des lignes au début et à la fin contenant des NA
while (anyNA(Data[1, ])) {
  Data <- Data[-1, ]
}
while (anyNA(Data[nrow(Data), ])) {
  Data <- Data[-nrow(Data), ]
}

# Suppression des lignes qui n'ont pas au moins deux éléments valides pour l'interpolation
Data <- Data[rowSums(!is.na(Data)) >= 2, ]

# Interpolation des valeurs manquantes pour les colonnes numériques
Data <- Data %>% mutate_if(is.numeric, na.approx)

# Sélection des variables d'intérêt
data <- Data %>% select("active_channel_width", "valley_bottom_width")

# Application de la transformation logarithmique
data$log_AC <- log(data$active_channel_width + 1)
data$log_VB <- log(data$valley_bottom_width + 1)

# Ajout d'une colonne fictive (si nécessaire)
data$rien <- 0

# ============================================
# 3. Définition de la Fonction d'Analyse des Points de Changement
# ============================================

# Fonction pour exécuter l'analyse de point de changement sur une série de valeurs
run_change_point_analysis <- function(values, name) {
  
  # Calcul de la variance et de la taille de la série
  var_value <- var(values)
  N <- length(values)
  ID <- name
  
  # ============================================
  # Méthode PELT
  # ============================================
  cpt_result_pelt <- cpt.mean(
    values,
    method = "PELT",
    penalty = "Manual",
    minseglen = 4,
    pen.value = var_value * log(N)
  )
  cpt_pelt <- cpt_result_pelt@cpts
  cpt_pelt <- cpt_pelt[-length(cpt_pelt)]  # Suppression du dernier point (fin de la série)
  
  # ============================================
  # Méthode BinSeg
  # ============================================
  cpt_result_BinSeg <- cpt.mean(
    values,
    method = "BinSeg",
    Q = 100,
    penalty = "Manual",
    pen.value = var_value * log(N)
  )
  cpt_BinSeg <- cpt_result_BinSeg@cpts
  cpt_BinSeg <- cpt_BinSeg[-length(cpt_BinSeg)]
  
  # ============================================
  # Méthode SegNeigh
  # ============================================
  cpt_result_SegNeigh <- cpt.mean(
    values,
    method = "SegNeigh",
    Q = 100,
    penalty = "Manual",
    pen.value = var_value * log(N)
  )
  cpt_SegNeigh <- cpt_result_SegNeigh@cpts
  cpt_SegNeigh <- cpt_SegNeigh[-length(cpt_SegNeigh)]
  
  # ============================================
  # Méthode Hubert
  # ============================================
  cpt_result_hubert <- Hubert_segmentation(values, alpha = 0.05)
  cpt_hubert <- cpt_result_hubert$locations
  cpt_hubert <- cpt_hubert[-length(cpt_hubert)]  # Suppression du dernier point
  cpt_hubert <- cpt_hubert[-1]                   # Suppression du premier point
  
  # ============================================
  # Méthode CPM 
  # ============================================
  ARL0_value <- round((N * log(N) / 2) - 100, -3)
  ARL0_value <- min(ARL0_value, 10000)
  cpt_result_cpm <- processStream(values, cpmType = "Student", ARL0 = ARL0_value, startup = 20)
  cpt_cpm <- cpt_result_cpm$changePoints
  
  # ============================================
  # Méthode BEAST
  # ============================================
  cpt_result_beast <- beast(
    values,
    season = "none",
    tseg.min = 5,
    tcp.minmax = c(0, 100),
    quiet = 1
  )
  ncp_mode_beast <- cpt_result_beast$trend$ncp_mode
  cp_beast <- cpt_result_beast$trend$cp
  cpt_beast <- cp_beast[1:ncp_mode_beast]
  cpt_beast <- sort(cpt_beast)
  
  # ============================================
  # Méthode Jumpoints
  # ============================================
  cpt_result_jumpoint <- jumpoints(values, output = "2")
  cpt_jumpoint <- as.numeric(cpt_result_jumpoint$psi)
  
  # Compilation des résultats dans un tibble
  results <- tibble(
    ID = ID,
    N = N,
    var = var_value,
    
    # PELT
    cp_pelt = list(cpt_pelt),
    cpt_pelt = length(cpt_pelt),
    
    # BinSeg
    cp_BinSeg = list(cpt_BinSeg),
    cpt_BinSeg = length(cpt_BinSeg),
    
    # SegNeigh
    cp_SegNeigh = list(cpt_SegNeigh),
    cpt_SegNeigh = length(cpt_SegNeigh),
    
    # Hubert
    cp_hubert = list(cpt_hubert),
    cpt_hubert = length(cpt_hubert),
    
    # CPM
    cp_cpm = list(cpt_cpm),
    cpt_cpm = length(cpt_cpm),
    
    # BEAST
    cp_beast = list(cpt_beast),
    cpt_beast = length(cpt_beast),
    
    # Jumpoints
    cp_jumpoint = list(cpt_jumpoint),
    cpt_jumpoint = length(cpt_jumpoint)
  )
  
  return(results)
}

# ============================================
# 4. Exécution de l'Analyse pour Différentes Variables
# ============================================

# Initialisation d'un data.frame pour combiner les résultats
results_combined <- data.frame()

# Analyse pour 'active_channel_width'
values <- data$active_channel_width
name <- "1 - active_channel_width"
resultats_reel_AC <- run_change_point_analysis(values, name)
results_combined <- rbind(results_combined, resultats_reel_AC)

# Analyse pour 'log_active_channel_width'
values <- data$log_AC
name <- "2 - log_active_channel_width"
resultats_reel_log_AC <- run_change_point_analysis(values, name)
results_combined <- rbind(results_combined, resultats_reel_log_AC)

# Analyse pour 'valley_bottom_width'
values <- data$valley_bottom_width
name <- "3 - valley_bottom_width"
resultats_reel_VB <- run_change_point_analysis(values, name)
results_combined <- rbind(results_combined, resultats_reel_VB)

# Analyse pour 'log_valley_bottom_width'
values <- data$log_VB
name <- "4 - log_valley_bottom_width"
resultats_reel_log_VB <- run_change_point_analysis(values, name)
results_combined <- rbind(results_combined, resultats_reel_log_VB)