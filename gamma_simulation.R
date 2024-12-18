# ============================================
# Chargement des Bibliothèques
# ============================================

library(dplyr)
library(changepoint)
# devtools::install_github("lvaudor/hubr") # Décommenter si nécessaire
library(hubr)
library(Rbeast)
library(ecp)
library(cpm)
library(purrr)
library(tidyr)
library(cumSeg)

# ============================================
# Définition des Fonctions Principales
# ============================================

# Fonction principale pour effectuer les simulations
perform_simulation <- function(segment_mean, K, ratio, k1, theta1, delta, log, replicate) {
  
  # Combinaison des paramètres de simulation
  resultats <- crossing(segment_mean, K, ratio, k1, theta1, delta, log)
  
  # Fonction pour générer les séries temporelles simulées
  signal <- function(segment_mean, K, ratio, k1, theta1, delta, log) {  
    
    # Calcul des paramètres dérivés
    theta2 <- theta1 * (ratio^2)
    k2 <- k1 / (ratio^2)
    sd_inter_theorique <- sqrt(k1 * theta1^2)
    sd_intra_theorique <- sqrt(k2 * theta2^2)
    r_theorique <- sd_intra_theorique / sd_inter_theorique 
    N <- K * segment_mean
    nb_k <- K - 1
    
    # Génération des moyennes inter-groupe à partir d'une loi gamma
    mu_k <- rgamma(K, shape = k1, scale = theta1)
    
    # Affichage de la variance inter-groupe observée
    sd_inter_observed <- sqrt(var(mu_k) * K / (K - 1))
    
    # Création d'un tibble avec les paramètres des segments
    tib_seg <- tibble::tibble(
      segment = 1:K,               # Numéro du segment
      mu = mu_k,                   # Moyenne de chaque segment
      k2 = k1 / (ratio^2),         # Paramètre k2 pour la loi gamma intra-groupe
      theta2 = mu_k / k2           # Calcul de theta2 pour chaque segment
    )
    
    # Création d'un vecteur de segments pour chaque observation
    segment_ids <- rep(1:K, each = segment_mean)  # Chaque segment contient N observations
    
    # Création d'un tibble avec toutes les observations
    tib <- tibble::tibble(
      l = 1:(segment_mean * K),        # Index global des observations
      segment = segment_ids             # Numéro de segment pour chaque observation
    ) %>%
      left_join(tib_seg, by = "segment") %>%  # Jointure avec le tibble des segments
      mutate(
        x = rgamma(n(), shape = k2, scale = theta2)  # Génération des données intra-groupe
      )
    
    # Ajout d'une distance entre les segments en remplaçant par des NA
    if (delta > 0) {
      last_segment <- tib$segment[1]  # Initialisation du dernier segment
      
      for (i in 2:nrow(tib)) {  # Boucle pour ajouter des NA entre les segments
        if (!is.na(tib$segment[i]) && tib$segment[i] != last_segment &&
            tib$segment[i] != 1 && !is.na(last_segment)) {
          # Si le segment est différent du dernier segment et si delta est > 0
          end_row <- min(i + delta * N - 1, nrow(tib))  # Fin de la zone à remplacer par des NA
          tib[i:end_row, ] <- NA  # Remplacement par des NA
        }
        last_segment <- tib$segment[i]  # Mise à jour du dernier segment
      }
      
      # Interpolation linéaire des NA
      na_indices <- which(is.na(tib$x))  # Indices des NA
      tib$x <- na.approx(tib$x)  # Interpolation linéaire
      tib$x[na_indices] <- tib$x[na_indices] + rnorm(length(na_indices), 0, mean(theta2))  # Ajout du bruit
      
      tib$l <- na.approx(tib$l)  # Interpolation linéaire
    }
    
    # Calcul de l'écart-type intra-groupe observé pour chaque segment
    record_sd <- tib %>%
      group_by(segment) %>%
      summarise(sd_intra = sd(x) * segment_mean / (segment_mean - 1)) %>%
      pull(sd_intra)  # Extraire les écarts-types
    
    # Moyenne des écarts-types intra-groupe observés
    sd_intra_observed <- mean(record_sd)
    
    # Calcul du rapport sd intra / sd inter
    r_observed <- sd_intra_observed / sd_inter_observed
    
    # Identification des points de changement
    k <- which(diff(na.omit(tib$segment)) != 0) + 1  # +1 pour ajuster l'indice après diff
    
    valid_segments <- tib$segment[!is.na(tib$segment) & tib$segment != 0]
    change_points <- which(diff(valid_segments) != 0) + 1
    k <- which(!is.na(tib$segment) & tib$segment != 0)[change_points]
    k <- k - (delta * N / 2)
    
    # Transformation logarithmique si nécessaire
    if (log == 1) {
      tib$x <- log(tib$x + 1)
      tib$x <- na.approx(tib$x, na.rm = FALSE)  # Interpolation linéaire
    }
    
    # Retour des résultats
    return(list(
      tib = tib,
      sd_intra_observed = sd_intra_observed, 
      sd_inter_observed = sd_inter_observed,
      r_observed = r_observed,
      r_theorique = r_theorique,
      sd_inter_theorique = sd_inter_theorique,
      sd_intra_theorique = sd_intra_theorique,
      k2 = k2,
      theta2 = theta2,
      N = N,
      k = k,
      nb_k = nb_k,
      mu_k = mu_k,
      segment_mean = segment_mean,
      delta = delta,
      log = log,
      K = K,
      ratio = ratio,
      k1 = k1,
      theta1 = theta1
    ))
  }
  
  # Génération des séries simulées en répliquant les paramètres
  series_simulees <- pmap(resultats, function(segment_mean, K, ratio, k1, theta1, delta, log) {
    replicate(replicate, signal(segment_mean = segment_mean, K = K, ratio = ratio, 
                                k1 = k1, theta1 = theta1, delta = delta, log = log), simplify = FALSE)
  })
  
  series_simulees <- flatten(series_simulees)
  
  # Organisation des séries simulées dans un tibble imbriqué
  series_nested <- tibble(
    segment_mean = map_dbl(series_simulees, ~ .$segment_mean),    # segment_mean 
    N = map_dbl(series_simulees, ~ .$N),                          # La longueur de la série
    K = map_dbl(series_simulees, ~ .$K),                          # Le nombre de segments K
    k = map(series_simulees, ~ .$k),                              # Localisation des points de changement k
    ratio = map_dbl(series_simulees, ~ .$ratio),                  # Ratio
    r_observed = map_dbl(series_simulees, ~ .$r_observed),        # Ratio observé
    r_theorique = map_dbl(series_simulees, ~ .$r_theorique),      # Ratio théorique
    delta = map_dbl(series_simulees, ~ .$delta),                  # Ajout de delta 
    log = map_dbl(series_simulees, ~ .$log),                      # Ajout de log
    k1 = map_dbl(series_simulees, ~ .$k1),                        # Ajout du paramètre k1
    k2 = map_dbl(series_simulees, ~ .$k2),                        # Ajout du paramètre k2
    theta1 = map_dbl(series_simulees, ~ .$theta1),                # Ajout du paramètre theta1
    theta2 = map_dbl(series_simulees, ~ .$theta2),                # Ajout du paramètre theta2
    mu_k = map(series_simulees, ~ .$mu_k),                        # Ajout de mu_k
    series = map(series_simulees, ~ .$tib$x),                     # Les séries simulées dans tib$x
    template = map(series_simulees, ~ .$tib$mu)                    # template sans bruit dans tib$mu
  )
  
  # Initialisation du compteur de répliques
  nb_repliques <- 0
  
  # Fonction de segmentation utilisant différentes méthodes
  segmentation <- map(series_simulees, function(serie) {
    values <- serie[["tib"]][["x"]]
    k <- serie[["k"]]
    nb_repliques <<- nb_repliques + 1
    print(paste("Nombre de répliques effectuées :", nb_repliques))
    
    # ============================================
    # Méthode PELT
    # ============================================
    temps_debut <- Sys.time()
    cpt_result <- cpt.mean(values, method = "PELT", penalty = "Manual",
                           minseglen = 4, pen.value = var(values) * log(length(values)))
    temps_fin <- Sys.time()
    time_pelt <- as.numeric(temps_fin - temps_debut, units = "secs")
    pen_pelt <- cpt_result@pen.value
    cpt_pelt <- cpt_result@cpts[-length(cpt_result@cpts)]
    groups <- split(values, findInterval(seq_along(values), cpt_pelt))
    effect_size_pelt <- mean(map_dbl(groups, ~ sd(.x)), na.rm = TRUE)
    segment_mean_pelt <- mean(map_dbl(groups, ~ length(.x)))
    
    # ============================================
    # Méthode SegNeigh
    # ============================================
    temps_debut <- Sys.time()
    cpt_result <- cpt.mean(values, method = "SegNeigh", Q = 100, penalty = "Manual", 
                           pen.value = var(values) * log(length(values)))
    temps_fin <- Sys.time()
    time_SegNeigh <- as.numeric(temps_fin - temps_debut, units = "secs")
    pen_SegNeigh <- cpt_result@pen.value
    cpt_SegNeigh <- cpt_result@cpts[-length(cpt_result@cpts)]
    groups <- split(values, findInterval(seq_along(values), cpt_SegNeigh))
    effect_size_SegNeigh <- mean(map_dbl(groups, ~ sd(.x)), na.rm = TRUE)
    segment_mean_SegNeigh <- mean(map_dbl(groups, ~ length(.x)))
    
    # ============================================
    # Méthode BinSeg
    # ============================================
    temps_debut <- Sys.time()
    cpt_result <- cpt.mean(values, method = "BinSeg", Q = 100, penalty = "Manual", 
                           pen.value = var(values) * log(length(values)))
    temps_fin <- Sys.time()
    time_BinSeg <- as.numeric(temps_fin - temps_debut, units = "secs")
    pen_BinSeg <- cpt_result@pen.value
    cpt_BinSeg <- cpt_result@cpts[-length(cpt_result@cpts)]
    groups <- split(values, findInterval(seq_along(values), cpt_BinSeg))
    effect_size_BinSeg <- mean(map_dbl(groups, ~ sd(.x)), na.rm = TRUE)
    segment_mean_BinSeg <- mean(map_dbl(groups, ~ length(.x)))
    
    # ============================================
    # Méthode Hubert
    # ============================================
    temps_debut <- Sys.time()
    cpt_result <- Hubert_segmentation(values, alpha = 0.05)
    temps_fin <- Sys.time()
    time_hubert <- as.numeric(temps_fin - temps_debut, units = "secs")
    cpt_points <- cpt_result$locations[-length(cpt_result$locations)]
    cpt_hubert <- cpt_points[-1]
    groups <- split(values, findInterval(seq_along(values), cpt_hubert))
    effect_size_hubert <- mean(map_dbl(groups, ~ sd(.x)), na.rm = TRUE)
    segment_mean_hubert <- mean(map_dbl(groups, ~ length(.x)))
    
    # ============================================
    # Méthode CPM (Cumulative Sum)
    # ============================================
    temps_debut <- Sys.time()
    ARL0_value <- round((length(values) * log(length(values)) / 2) - 100, -3)
    ARL0_value <- min(ARL0_value, 10000)
    cpt_result <- processStream(values, cpmType = "Student", ARL0 = ARL0_value, startup = 20)
    temps_fin <- Sys.time()
    time_cpm <- as.numeric(temps_fin - temps_debut, units = "secs")
    cpt_cpm <- cpt_result$changePoints
    groups <- split(values, findInterval(seq_along(values), cpt_cpm))
    effect_size_cpm <- mean(map_dbl(groups, ~ sd(.x)), na.rm = TRUE)
    segment_mean_cpm <- mean(map_dbl(groups, ~ length(.x)))
    
    # ============================================
    # Méthode BEAST
    # ============================================
    temps_debut <- Sys.time()
    cpt_result <- beast(values, season = "none", tseg.min = 4, tcp.minmax = c(0, 100), quiet = 1)
    temps_fin <- Sys.time()
    time_beast <- as.numeric(temps_fin - temps_debut, units = "secs")
    ncp_mode <- cpt_result$trend$ncp_mode
    cp <- sort(cpt_result$trend$cp[1:ncp_mode])
    groups <- split(values, findInterval(seq_along(values), cp))
    effect_size_beast <- mean(map_dbl(groups, ~ sd(.x)), na.rm = TRUE)
    segment_mean_beast <- mean(map_dbl(groups, ~ length(.x)))
    
    # ============================================
    # Méthode Jumpoints
    # ============================================
    temps_debut <- Sys.time()
    cpt_result <- jumpoints(values, output = "2")
    temps_fin <- Sys.time()
    time_jumpoint <- as.numeric(temps_fin - temps_debut, units = "secs")
    cpt_jumpoint <- as.numeric(cpt_result$psi)
    groups <- split(values, findInterval(seq_along(values), cpt_jumpoint))
    effect_size_jumpoint <- mean(map_dbl(groups, ~ sd(.x)), na.rm = TRUE)
    segment_mean_jumpoint <- mean(map_dbl(groups, ~ length(.x)))
    
    # Retour des résultats de segmentation pour toutes les méthodes
    return(list(
      k = k,
      
      # PELT
      time_pelt = time_pelt,
      pen_pelt = pen_pelt,
      cpt_pelt = cpt_pelt,
      segment_mean_pelt = segment_mean_pelt, 
      effect_size_pelt = effect_size_pelt,
      
      # SegNeigh
      time_SegNeigh = time_SegNeigh,
      pen_SegNeigh = pen_SegNeigh,
      cpt_SegNeigh = cpt_SegNeigh,
      segment_mean_SegNeigh = segment_mean_SegNeigh,
      effect_size_SegNeigh = effect_size_SegNeigh,
      
      # BinSeg
      time_BinSeg = time_BinSeg,
      pen_BinSeg = pen_BinSeg,
      cpt_BinSeg = cpt_BinSeg,
      segment_mean_BinSeg = segment_mean_BinSeg,
      effect_size_BinSeg = effect_size_BinSeg,
      
      # Hubert
      time_hubert = time_hubert,
      cpt_hubert = cpt_hubert,
      segment_mean_hubert = segment_mean_hubert,
      effect_size_hubert = effect_size_hubert,
      
      # CPM
      time_cpm = time_cpm,
      cpt_cpm = cpt_cpm,
      segment_mean_cpm = segment_mean_cpm,
      effect_size_cpm = effect_size_cpm,
      
      # BEAST
      time_beast = time_beast,
      cpt_beast = cpt_beast,
      segment_mean_beast = segment_mean_beast,
      effect_size_beast = effect_size_beast,
      
      # Jumpoints
      time_jumpoint = time_jumpoint,
      cpt_jumpoint = cpt_jumpoint,
      segment_mean_jumpoint = segment_mean_jumpoint,
      effect_size_jumpoint = effect_size_jumpoint
    ))
    
  })
  
  # Fonction pour calculer les performances des segmentations
  perf <- function(series_simulees, segmentation) {
    resultats <- uncount(resultats, replicate)
    
    # Ajout des paramètres de simulation aux résultats
    resultats$N <- map(series_simulees, pluck, "N")
    resultats$k <- map(series_simulees, pluck, "k")
    resultats$nb_k <- map(series_simulees, pluck, "nb_k")
    resultats$k2 <- map(series_simulees, pluck, "k2")
    resultats$theta2 <- map(series_simulees, pluck, "theta2")
    resultats$mu_k <- map(series_simulees, pluck, "mu_k")
    resultats$sd_intra_observed <- map(series_simulees, pluck, "sd_intra_observed")
    resultats$sd_inter_observed <- map(series_simulees, pluck, "sd_inter_observed")
    resultats$r_observed <- map(series_simulees, pluck, "r_observed")
    resultats$sd_inter_theorique <- map(series_simulees, pluck, "sd_inter_theorique")
    resultats$sd_intra_theorique <- map(series_simulees, pluck, "sd_intra_theorique")
    resultats$r_theorique <- map(series_simulees, pluck, "r_theorique")
    
    # Ajout des résultats de segmentation pour chaque méthode
    # PELT
    resultats$pen_pelt <- map(segmentation, pluck, "pen_pelt")
    resultats$cpt_pelt <- map(segmentation, pluck, "cpt_pelt")
    resultats$nb_cpt_pelt <- map_dbl(resultats$cpt_pelt, length)
    resultats$time_pelt <- map(segmentation, pluck, "time_pelt")
    resultats$segment_mean_pelt <- map(segmentation, pluck, "segment_mean_pelt")
    resultats$effect_size_pelt <- map(segmentation, pluck, "effect_size_pelt")
    
    # SegNeigh
    resultats$pen_SegNeigh <- map(segmentation, pluck, "pen_SegNeigh")
    resultats$cpt_SegNeigh <- map(segmentation, pluck, "cpt_SegNeigh")
    resultats$nb_cpt_SegNeigh <- map_dbl(resultats$cpt_SegNeigh, length)
    resultats$time_SegNeigh <- map(segmentation, pluck, "time_SegNeigh")
    resultats$segment_mean_SegNeigh <- map(segmentation, pluck, "segment_mean_SegNeigh")
    resultats$effect_size_SegNeigh <- map(segmentation, pluck, "effect_size_SegNeigh")
    
    # BinSeg
    resultats$pen_BinSeg <- map(segmentation, pluck, "pen_BinSeg")
    resultats$cpt_BinSeg <- map(segmentation, pluck, "cpt_BinSeg")
    resultats$nb_cpt_BinSeg <- map_dbl(resultats$cpt_BinSeg, length)
    resultats$time_BinSeg <- map(segmentation, pluck, "time_BinSeg")
    resultats$segment_mean_BinSeg <- map(segmentation, pluck, "segment_mean_BinSeg")
    resultats$effect_size_BinSeg <- map(segmentation, pluck, "effect_size_BinSeg")
    
    # Hubert
    resultats$time_hubert <- map(segmentation, pluck, "time_hubert")
    resultats$cpt_hubert <- map(segmentation, pluck, "cpt_hubert")
    resultats$nb_cpt_hubert <- map_dbl(resultats$cpt_hubert, length)
    resultats$segment_mean_hubert <- map(segmentation, pluck, "segment_mean_hubert")
    resultats$effect_size_hubert <- map(segmentation, pluck, "effect_size_hubert")
    
    # CPM
    resultats$time_cpm <- map(segmentation, pluck, "time_cpm")
    resultats$cpt_cpm <- map(segmentation, pluck, "cpt_cpm")
    resultats$nb_cpt_cpm <- map_dbl(resultats$cpt_cpm, length)
    resultats$segment_mean_cpm <- map(segmentation, pluck, "segment_mean_cpm")
    resultats$effect_size_cpm <- map(segmentation, pluck, "effect_size_cpm")
    
    # BEAST
    resultats$time_beast <- map(segmentation, pluck, "time_beast")
    resultats$cpt_beast <- map(segmentation, pluck, "cpt_beast")
    resultats$nb_cpt_beast <- map_dbl(resultats$cpt_beast, length)
    resultats$segment_mean_beast <- map(segmentation, pluck, "segment_mean_beast")
    resultats$effect_size_beast <- map(segmentation, pluck, "effect_size_beast")
    
    # Jumpoints
    resultats$time_jumpoint <- map(segmentation, pluck, "time_jumpoint")
    resultats$cpt_jumpoint <- map(segmentation, pluck, "cpt_jumpoint")
    resultats$nb_cpt_jumpoint <- map_dbl(resultats$cpt_jumpoint, length)
    resultats$segment_mean_jumpoint <- map(segmentation, pluck, "segment_mean_jumpoint")
    resultats$effect_size_jumpoint <- map(segmentation, pluck, "effect_size_jumpoint")
    
    return(resultats)
  }
  
  # Calcul des performances
  resultats <- perf(series_simulees, segmentation)
  
  # Retour des résultats de la simulation
  return(list(resultats, series_nested))
}

# ============================================
# Configuration des Paramètres de Simulation
# ============================================

# Définition des valeurs des paramètres
segment_mean <- c(50, 100, 200)
K <- c(5, 10, 15)
ratio <- c(0.5, 1, 2)
delta <- c(0, 0.02)
k1 <- c(4)
theta1 <- c(5)
log <- c(0)
replicate <- 1

# Combinaison des paramètres pour les simulations
# resultats <- crossing(segment_mean, K, ratio, k1, theta1, delta, log) # Non utilisé directement

# ============================================
# Exécution de la Simulation
# ============================================

# Mesure du temps d'exécution de la simulation
temps_debut <- Sys.time()
simu <- perform_simulation(segment_mean, K, ratio, k1, theta1, delta, log, replicate)
temps_fin <- Sys.time()
time <- temps_fin - temps_debut
print(time)

# ============================================
# Traitement et Sauvegarde des Résultats
# ============================================

# Fonction pour traiter et sauvegarder les résultats
Result <- function(resultat, R) {
  
  # Extraction des données
  Data <- resultat[[1]]          # Données pour l'analyse
  series_nested <- resultat[[2]] # Séries simulées
  
  # Sauvegarde des données au format RDS
  saveRDS(Data, file = paste0(R, "_data.rds")) 
  saveRDS(series_nested, file = paste0(R, "_serie.rds"))
  
  # Conversion des listes en data.frame
  Data <- as.data.frame(Data)
  series_nested <- as.data.frame(series_nested)
  
  # Fonction pour convertir les colonnes de listes en vecteurs
  process_list_columns <- function(df) {
    for (col in names(df)) {
      if (is.list(df[[col]])) {
        df[[col]] <- sapply(df[[col]], function(x) paste(unlist(x), collapse = ","))
      }
    }
    return(df)
  }
  
  # Application de la fonction de conversion
  Data <- process_list_columns(Data)
  series_nested <- process_list_columns(series_nested)
  
  # Sauvegarde des données au format CSV
  write.csv(Data, file = paste0(R, "_data.csv"), row.names = FALSE)
  write.csv(series_nested, file = paste0(R, "_serie.csv"), row.names = FALSE)
  
}

# Appel de la fonction Result avec les résultats de la simulation
Result(resultat = simu, R = "RR1")
