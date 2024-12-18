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
perform_simulation <- function(segment_mean, K, ratio, delta, heterosced, replicate) {
  
  # Combinaison des paramètres de simulation
  resultats <- crossing(segment_mean, K, ratio, delta, heterosced)
  
  # Fonction pour générer les séries temporelles simulées
  signal <- function(segment_mean, K, ratio, theta = 3, delta, heterosced) {  
    
    # Calcul de la longueur totale de la série et de la marge
    N <- K * segment_mean
    margin <- ceiling(N * delta) # Marge de distance entre les segments
    
    # Génération des points de rupture
    if (K - 1 > (N - margin - 1)) { 
      # Si trop de segments pour la longueur, un segment par point
      k <- seq_len(N - margin - 1)
    } else { 
      # Sinon, échantillonnage des points de rupture avec espacement minimal
      k <- numeric(K - 1)
      while (TRUE) { 
        k <- sort(sample(setdiff(seq_len(N - margin), (N - margin):N), K - 1, replace = FALSE))
        if (all(diff(k) >= margin + 1)) break
      }
    }
    
    # Création des segments avec moyennes et écarts types
    tib_seg <- tibble::tibble(
      segment = 1:K,
      mu = rnorm(K, 0, theta),
      sigma = ratio * theta * (1 - heterosced) + ratio * abs(mu) / sqrt(2 / pi) * heterosced
    )
    
    # Attribution des segments à chaque observation
    series <- rep(0, N)
    series[k] <- 1
    series <- cumsum(series) + 1
    
    # Création de la série complète avec bruit
    tib <- tibble::tibble(
      segment = series,
      l = 1:N
    ) %>% 
      left_join(tib_seg, by = "segment") %>%  
      mutate(
        x = mu + rnorm(N, 0, sd = sigma), # Série avec bruit
        template = mu + rnorm(N, 0, 0)    # Série sans bruit (template)
      )
    
    # Ajout de distance entre les segments en remplaçant par des NA
    if (delta > 0) {
      last_segment <- tib$segment[1]
      for (i in 2:nrow(tib)) {
        if (!is.na(tib$segment[i]) && tib$segment[i] != last_segment 
            && tib$segment[i] != 1 && !is.na(last_segment)) {
          if (delta > 0) {
            end_row <- min(i + delta * N - 1, nrow(tib))
            tib[i:end_row, ] <- NA
          }
        }
        last_segment <- tib$segment[i]
      }
      na_indices <- which(is.na(tib$x))
      tib$x <- na.approx(tib$x) # Interpolation linéaire des NA
      tib$x[na_indices] <- tib$x[na_indices] + 
        rnorm(length(na_indices), 0, mean(tib_seg$sigma))
      
      k <- k + (delta * N / 2)
    }
    
    # Calcul des statistiques globales
    Emu_k <- mean(tib$mu, na.rm = TRUE)
    mu_k <- tib_seg$mu
    Esigma_k_theta <- mean(tib$sigma, na.rm = TRUE) / theta
    Esigma_k <- mean(tib_seg$sigma, na.rm = TRUE)
    sigma_k <- tib_seg$sigma
    
    print(Esigma_k_theta) # Affichage pour le débogage
    
    # Calcul du nombre de ruptures
    nb_k <- K - 1
    
    # Calcul des longueurs des segments
    segment_sizes <- as.numeric(table(tib$segment))
    min_segment <- which.min(segment_sizes)
    min_segment_length <- segment_sizes[min_segment]
    max_segment <- which.max(segment_sizes)
    max_segment_length <- segment_sizes[max_segment]
    
    # Retour des résultats de la simulation
    return(list(
      tib = tib,
      k = k,
      N = N,
      min_segment_length = min_segment_length,
      max_segment_length = max_segment_length,
      nb_k = nb_k,
      Emu_k = Emu_k,
      mu_k = mu_k,
      Esigma_k_theta = Esigma_k_theta,
      Esigma_k = Esigma_k,
      sigma_k = sigma_k,
      theta = theta,
      delta = delta,
      ratio = ratio,
      heterosced = heterosced,
      K = K,
      segment_mean = segment_mean
    ))
  }
  
  # Génération des séries simulées en répliquant les paramètres
  series_simulees <- pmap(resultats, function(segment_mean, K, ratio, delta, heterosced) {
    replicate(replicate, signal(segment_mean = segment_mean, K = K, ratio = ratio, 
                                delta = delta, heterosced = heterosced), simplify = FALSE)
  })
  
  series_simulees <- flatten(series_simulees)
  
  # Organisation des séries simulées dans un tibble imbriqué
  series_nested <- tibble(
    segment_mean = map_dbl(series_simulees, ~ .$segment_mean),
    N = map_dbl(series_simulees, ~ .$N),
    K = map_dbl(series_simulees, ~ .$K),
    ratio = map_dbl(series_simulees, ~ .$ratio),
    delta = map_dbl(series_simulees, ~ .$delta),
    heterosced = map_dbl(series_simulees, ~ .$heterosced),
    series = map(series_simulees, ~ .$tib$x),
    template = map(series_simulees, ~ .$tib$template)
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
    groups <- split(values, findInterval(seq_along(values), cpt_points))
    effect_size_hubert <- mean(map_dbl(groups, ~ sd(.x)), na.rm = TRUE)
    segment_mean_hubert <- mean(map_dbl(groups, ~ length(.x)))
    
    # ============================================
    # Méthode CPM 
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
    resultats$Esigma_k <- map(series_simulees, pluck, "Esigma_k")
    resultats$sigma_k <- map(series_simulees, pluck, "sigma_k")
    resultats$Esigma_k_theta <- map(series_simulees, pluck, "Esigma_k_theta")
    resultats$Emu_k <- map(series_simulees, pluck, "Emu_k")
    resultats$mu_k <- map(series_simulees, pluck, "mu_k")
    resultats$min_segment_length <- map(series_simulees, pluck, "min_segment_length")
    resultats$max_segment_length <- map(series_simulees, pluck, "max_segment_length")
    resultats$theta <- map(series_simulees, pluck, "theta")
    
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
heterosced <- c(0)
replicate <- 1

# Combinaison des paramètres pour les simulations
resultats <- crossing(segment_mean, K, ratio, delta, heterosced)

# ============================================
# Exécution de la Simulation
# ============================================

# Mesure du temps d'exécution de la simulation
temps_debut <- Sys.time()
simu <- perform_simulation(segment_mean, K, ratio, delta, heterosced, replicate)
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
