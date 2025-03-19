# ============================================
# Chargement des Bibliothèques
# ============================================

library(ggplot2)
library(readr)
library(RColorBrewer)
library(dplyr)
library(gridExtra)
library(TSclust)
library(proxy)
library(ggExtra)
library(reshape2)
library(fmsb)
library(aricode)
library(reticulate)
library(tidyr)
library(ggridges)
library(questionr)
library(ggridges)
library(grid)
library(tidyverse)
library(zoo)
library(RPostgres)
library(hubr)
library(ConsensusClusterPlus)
library(cowplot)
library(Rbeast)
library(changepoint)
library(patchwork)


# ============================================
# Ajout et préparation des données
# ============================================

# Ajout de données des change points RDS 
add_data_normal_RDS <- function(){ 
  R1 <- readRDS("Data/normal/RDS/normale1_data.rds") # 100 ietration
  R2 <- readRDS("Data/normal/RDS/normale2_data.rds")
  R3 <- readRDS("Data/normal/RDS/normale3_data.rds")
  R4 <- readRDS("Data/normal/RDS/normale4_data.rds")
  R5 <- readRDS("Data/normal/RDS/normale5_data.rds")
  R6 <- readRDS("Data/normal/RDS/normale6_data.rds")
  R7 <- readRDS("Data/normal/RDS/normale7_data.rds")
  R8 <- readRDS("Data/normal/RDS/normale8_data.rds")
  R9 <- readRDS("Data/normal/RDS/normale9_data.rds")
  R10 <- readRDS("Data/normal/RDS/normale10_data.rds")
  
  
  resultat <- rbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10)
  resultat$id <- 1:nrow(resultat)
  return(resultat)
}
resultat_normal <- add_data_normal_RDS() # loi normal

add_data_gamma_RDS <- function(){
  R1 <- readRDS("Data/gamma/RDS/gamma101_data.rds") # 50 ietration
  R2 <- readRDS("Data/gamma/RDS/gamma102_data.rds")
  R3 <- readRDS("Data/gamma/RDS/gamma103_data.rds")
  R4 <- readRDS("Data/gamma/RDS/gamma104_data.rds")
  R5 <- readRDS("Data/gamma/RDS/gamma105_data.rds")
  R6 <- readRDS("Data/gamma/RDS/gamma106_data.rds")
  R7 <- readRDS("Data/gamma/RDS/gamma107_data.rds")
  R8 <- readRDS("Data/gamma/RDS/gamma108_data.rds")
  R9 <- readRDS("Data/gamma/RDS/gamma109_data.rds")
  R10 <- readRDS("Data/gamma/RDS/gamma110_data.rds")
  R11 <- readRDS("Data/gamma/RDS/gamma111_data.rds")
  R12 <- readRDS("Data/gamma/RDS/gamma112_data.rds")
  R13 <- readRDS("Data/gamma/RDS/gamma113_data.rds")
  R14 <- readRDS("Data/gamma/RDS/gamma114_data.rds")
  R15 <- readRDS("Data/gamma/RDS/gamma115_data.rds")
  R16 <- readRDS("Data/gamma/RDS/gamma116_data.rds")
  R17 <- readRDS("Data/gamma/RDS/gamma117_data.rds")
  R18 <- readRDS("Data/gamma/RDS/gamma118_data.rds")
  R19 <- readRDS("Data/gamma/RDS/gamma119_data.rds")
  R20 <- readRDS("Data/gamma/RDS/gamma120_data.rds")
  
  resultat <- rbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,R13
                    ,R14,R15,R16,R17,R18,R19,R20)
  resultat$id <- 1:nrow(resultat)
  return(resultat)
} # loi gamma
resultat_gamma <- add_data_gamma_RDS()


# Ajout des signaux brutes
add_series_normal_RDS <- function(){
  S1 <- readRDS("Data/normal/RDS/normale1_serie.rds")
  S2 <- readRDS("Data/normal/RDS/normale2_serie.rds")
  S3 <- readRDS("Data/normal/RDS/normale3_serie.rds")
  S4 <- readRDS("Data/normal/RDS/normale4_serie.rds")
  S5 <- readRDS("Data/normal/RDS/normale5_serie.rds")
  S6 <- readRDS("Data/normal/RDS/normale6_serie.rds")
  S7 <- readRDS("Data/normal/RDS/normale7_serie.rds")
  S8 <- readRDS("Data/normal/RDS/normale8_serie.rds")
  S9 <- readRDS("Data/normal/RDS/normale9_serie.rds")
  S10 <- readRDS("Data/normal/RDS/normale10_serie.rds")
  
  serie <- rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10)
  serie$id <- 1:nrow(serie)
  return(serie)
} # loi normal
serie_normal <- add_series_normal_RDS()

add_series_gamma_RDS <- function(){
  S1 <- readRDS("Data/gamma/RDS/gamma101_serie.rds")
  S2 <- readRDS("Data/gamma/RDS/gamma102_serie.rds")
  S3 <- readRDS("Data/gamma/RDS/gamma103_serie.rds")
  S4 <- readRDS("Data/gamma/RDS/gamma104_serie.rds")
  S5 <- readRDS("Data/gamma/RDS/gamma105_serie.rds")
  S6 <- readRDS("Data/gamma/RDS/gamma106_serie.rds")
  S7 <- readRDS("Data/gamma/RDS/gamma107_serie.rds")
  S8 <- readRDS("Data/gamma/RDS/gamma108_serie.rds")
  S9 <- readRDS("Data/gamma/RDS/gamma109_serie.rds")
  S10 <- readRDS("Data/gamma/RDS/gamma110_serie.rds")
  S11 <- readRDS("Data/gamma/RDS/gamma111_serie.rds")
  S12 <- readRDS("Data/gamma/RDS/gamma112_serie.rds")
  S13 <- readRDS("Data/gamma/RDS/gamma113_serie.rds")
  S14 <- readRDS("Data/gamma/RDS/gamma114_serie.rds")
  S15 <- readRDS("Data/gamma/RDS/gamma115_serie.rds")
  S16 <- readRDS("Data/gamma/RDS/gamma116_serie.rds")
  S17 <- readRDS("Data/gamma/RDS/gamma117_serie.rds")
  S18 <- readRDS("Data/gamma/RDS/gamma118_serie.rds")
  S19 <- readRDS("Data/gamma/RDS/gamma119_serie.rds")
  S20 <- readRDS("Data/gamma/RDS/gamma120_serie.rds")
  
  serie <- rbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,
                 S14,S15,S16,S17,S18,S19,S20)
  serie$id <- 1:nrow(serie)
  return(serie)
} # loi gamma
serie_gamma <- add_series_gamma_RDS()


# Préparation avec transformation des données pour l'analyse
prep_data_normal <- function(){ 
  
  # PELT
  pelt <- resultat_normal %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta, theta, k, nb_k, sigma_k, Esigma_k, Esigma_k_theta,
                  Emu_k, mu_k, min_segment_length, max_segment_length, cpt_pelt, nb_cpt_pelt, 
                  time_pelt, segment_mean_pelt, effect_size_pelt)
  colnames(pelt)[17:21] <- c("cpt_loca", "cpt", "time", "segment_mean_algo", "effect_size_algo")
  pelt$algo <- "PELT"
  
  # BinSeg
  BinSeg <- resultat_normal %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta, theta, k, nb_k, sigma_k, Esigma_k, Esigma_k_theta,
                  Emu_k, mu_k, min_segment_length, max_segment_length, cpt_BinSeg, nb_cpt_BinSeg,
                  time_BinSeg, segment_mean_BinSeg, effect_size_BinSeg)
  colnames(BinSeg)[17:21] <- c("cpt_loca", "cpt", "time", "segment_mean_algo", "effect_size_algo")
  BinSeg$algo <- "BinSeg"
  
  # SegNeigh
  SegNeigh <- resultat_normal %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta, theta, k, nb_k, sigma_k, Esigma_k, Esigma_k_theta,
                  Emu_k, mu_k, min_segment_length, max_segment_length, cpt_SegNeigh, nb_cpt_SegNeigh,
                  time_SegNeigh, segment_mean_SegNeigh, effect_size_SegNeigh)
  colnames(SegNeigh)[17:21] <- c("cpt_loca", "cpt", "time", "segment_mean_algo", "effect_size_algo")
  SegNeigh$algo <- "SegNeigh"
  
  # Hubert
  hubert <- resultat_normal %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta, theta, k, nb_k, sigma_k, Esigma_k, Esigma_k_theta,
                  Emu_k, mu_k, min_segment_length, max_segment_length, cpt_hubert, nb_cpt_hubert,
                  time_hubert, segment_mean_hubert, effect_size_hubert)
  colnames(hubert)[17:21] <- c("cpt_loca", "cpt", "time", "segment_mean_algo", "effect_size_algo")
  hubert$algo <- "Hubert"
  
  # CPM
  cpm <- resultat_normal %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta, theta, k, nb_k, sigma_k, Esigma_k, Esigma_k_theta,
                  Emu_k, mu_k, min_segment_length, max_segment_length, cpt_cpm, nb_cpt_cpm,
                  time_cpm, segment_mean_cpm, effect_size_cpm)
  colnames(cpm)[17:21] <- c("cpt_loca", "cpt", "time", "segment_mean_algo", "effect_size_algo")
  cpm$algo <- "CPM"
  
  # BEAST
  beast <- resultat_normal %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta, theta, k, nb_k, sigma_k, Esigma_k, Esigma_k_theta,
                  Emu_k, mu_k, min_segment_length, max_segment_length, cpt_beast, nb_cpt_beast,
                  time_beast, segment_mean_beast, effect_size_beast)
  colnames(beast)[17:21] <- c("cpt_loca", "cpt", "time", "segment_mean_algo", "effect_size_algo")
  beast$algo <- "Beast"
  
  # Jumpoint
  CumSeg <- resultat_normal %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta, theta, k, nb_k, sigma_k, Esigma_k, Esigma_k_theta,
                  Emu_k, mu_k, min_segment_length, max_segment_length, cpt_jumpoint, nb_cpt_jumpoint,
                  time_jumpoint, segment_mean_jumpoint, effect_size_jumpoint)
  colnames(CumSeg)[17:21] <- c("cpt_loca", "cpt", "time", "segment_mean_algo", "effect_size_algo")
  CumSeg$algo <- "CumSeg"
  
  # Combine the data frames
  resultat_normal <- rbind(pelt, cpm, beast, CumSeg, BinSeg, SegNeigh, hubert)
  
  # Ajouter le rand Index 
  rand_index_f <- function(bkps1, bkps2, N) { 
    bkps1 <- c(bkps1, N)
    bkps2 <- c(bkps2, N)
    
    sanity_check <- function(bkps1, bkps2) {
      if (length(bkps1) == 0 || length(bkps2) == 0) {
        stop("Both bkps1 and bkps2 must have at least one breakpoint.")
      }
      if (bkps1[length(bkps1)] != bkps2[length(bkps2)]) {
        stop("The last breakpoint of both bkps1 and bkps2 must be the same.")
      }
    }
    
    sanity_check(bkps1, bkps2)
    n_samples <- bkps1[length(bkps1)]
    bkps1_with_0 <- c(0, bkps1)
    bkps2_with_0 <- c(0, bkps2)
    n_bkps1 <- length(bkps1)
    n_bkps2 <- length(bkps2)
    
    disagreement <- 0
    beginj <- 0  # avoids unnecessary computations
    
    for (index_bkps1 in seq_len(n_bkps1)) {
      start1 <- bkps1_with_0[index_bkps1]
      end1 <- bkps1_with_0[index_bkps1 + 1]
      
      for (index_bkps2 in seq(beginj, n_bkps2)) {
        start2 <- bkps2_with_0[index_bkps2]
        end2 <- bkps2_with_0[index_bkps2 + 1]
        nij <- max(min(end1, end2) - max(start1, start2), 0)
        disagreement <- disagreement + nij * abs(end1 - end2)
        
        # we can skip the rest of the iteration, nij will be 0
        if (end1 < end2) {
          break
        } else {
          beginj <- index_bkps2 + 1
        }
      }
    }
    
    disagreement <- disagreement / (n_samples * (n_samples - 1) / 2)
    return(1.0 - disagreement)
  }
  resultat_normal$rand <- mapply(rand_index_f, resultat_normal$cpt_loca,
                                 resultat_normal$k, resultat_normal$N)
  
  # Ajouter le Jaccard 
  calculate_metrics_with_similarity <- function(real_changes, detected_changes, tolerance) {
    # Initialisation des variables
    TP <- 0
    FP <- 0
    FN <- 0
    used_real_changes <- rep(FALSE, length(real_changes))  # Liste pour suivre les points réels utilisés
    
    # Boucle pour les points détectés
    for (detected in detected_changes) {
      # Chercher un point réel correspondant à ce point détecté
      matched <- FALSE
      for (i in 1:length(real_changes)) {
        if (!used_real_changes[i] && abs(real_changes[i] - detected) <= tolerance) {
          TP <- TP + 1  # Si une correspondance est trouvée, c'est un True Positive
          used_real_changes[i] <- TRUE  # Marquer le point réel comme utilisé
          matched <- TRUE
          break  # Sortir de la boucle dès qu'une correspondance est trouvée
        }
      }
      
      # Si aucun point réel n'a été trouvé pour ce point détecté, c'est un False Positive
      if (!matched) {
        FP <- FP + 1
      }
    }
    
    # Boucle pour les points réels non utilisés (s'il en reste)
    FN <- sum(!used_real_changes)  # Compter les points réels non associés à un point détecté
    
    # Calcul de l'Indice de Jaccard
    jaccard_index <- TP / (TP + FP + FN)  # Similarité de Jaccard
    
    # Calcul de l'Indice Inex
    inex_index <- TP / (TP + FP + FN)  # L'Inex est similaire au Jaccard dans ce cas
    
    # Résultats
    return(jaccard = jaccard_index)
  }
  resultat_normal$jaccard <- mapply(calculate_metrics_with_similarity, 
                                    real_changes = resultat_normal$k,
                                    detected_changes = resultat_normal$cpt_loca,
                                    tolerance = 10)
  
  
  # changement des données en numerique (au lieu de list)
  resultat_normal$nb_k <- unlist(resultat_normal$nb_k)
  resultat_normal$N <- unlist(resultat_normal$N)
  resultat_normal$Esigma_k <- unlist(resultat_normal$Esigma_k)
  resultat_normal$Esigma_k_theta <- unlist(resultat_normal$Esigma_k_theta)
  resultat_normal$Emu_k <- unlist(resultat_normal$Emu_k)
  resultat_normal$max_segment_length <- unlist(resultat_normal$max_segment_length)
  resultat_normal$min_segment_length <- unlist(resultat_normal$min_segment_length)
  resultat_normal$theta <- unlist(resultat_normal$theta)
  resultat_normal$time <- unlist(resultat_normal$time)
  resultat_normal$segment_mean_algo <- unlist(resultat_normal$segment_mean_algo)
  resultat_normal$effect_size_algo <- unlist(resultat_normal$effect_size_algo)
  
  
  # Ajoute une colonne pour le nom de delta
  resultat_normal <- resultat_normal %>%
    mutate(name_delta = ifelse(delta == 0, "abrupt", 
                               ifelse(delta == 0.02, "smooth", NA)))
  
  # Ajoute une colonne pour le nom de ratio
  resultat_normal <- resultat_normal %>%
    mutate(name_ratio = ifelse(ratio == 0.5, "r = 0.5", 
                               ifelse(ratio == 1, "r = 1",
                                      ifelse(ratio == 2, "r = 2", NA))))
  
  
  # Ajoute une colonne pour le nom de segment
  resultat_normal <- resultat_normal %>%
    mutate(name_segment = ifelse(segment_mean == 50, "S = 50", 
                                 ifelse(segment_mean == 100, "S = 100",
                                        ifelse(segment_mean == 200, "S = 200", NA))))
  
  
  # ordre des algo
  resultat_normal$algo <- factor(resultat_normal$algo,levels = c("Beast", "BinSeg", "CPM","CumSeg", "Hubert"
                                                                 , "PELT", "SegNeigh"))
  
  resultat_normal$name_segment <- factor(resultat_normal$name_segment,levels = 
                                           c("S = 50", "S = 100", "S = 200"))
  
  return(resultat_normal)
} # loi normal
resultat_normal <- prep_data_normal()

prep_data_gamma <- function(){ 
  # ne garder que les non log
  # Ajoute une colonne pour le nom de delta
  resultat_gamma <- resultat_gamma %>%
    filter(log == 0)
  
  # PELT
  pelt <- resultat_gamma %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta,log,k1, theta1,k2,theta2, k, nb_k, mu_k,
                  sd_intra_observed, sd_inter_observed, r_observed, sd_intra_theorique,
                  sd_inter_theorique, r_theorique, cpt_pelt, nb_cpt_pelt,time_pelt,
                  segment_mean_pelt, effect_size_pelt)
  colnames(pelt)[21:25] <- c("cpt_loca", "cpt", "time", "segment_mean_algo",
                             "effect_size_algo")
  pelt$algo <- "PELT"
  
  # BinSeg
  BinSeg <- resultat_gamma %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta,log,k1, theta1,k2,theta2, k, nb_k, mu_k,
                  sd_intra_observed, sd_inter_observed, r_observed, sd_intra_theorique,
                  sd_inter_theorique, r_theorique, cpt_BinSeg, nb_cpt_BinSeg,time_BinSeg,
                  segment_mean_BinSeg, effect_size_BinSeg)
  colnames(BinSeg)[21:25] <- c("cpt_loca", "cpt", "time", "segment_mean_algo",
                               "effect_size_algo")
  BinSeg$algo <- "BinSeg"
  
  # SegNeigh
  SegNeigh <- resultat_gamma %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta,log,k1, theta1,k2,theta2, k, nb_k, mu_k,
                  sd_intra_observed, sd_inter_observed, r_observed, sd_intra_theorique,
                  sd_inter_theorique, r_theorique, cpt_SegNeigh, nb_cpt_SegNeigh,
                  time_SegNeigh, segment_mean_SegNeigh, effect_size_SegNeigh)
  colnames(SegNeigh)[21:25] <- c("cpt_loca", "cpt", "time", "segment_mean_algo",
                                 "effect_size_algo")
  SegNeigh$algo <- "SegNeigh"
  
  # Hubert
  hubert <- resultat_gamma %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta,log,k1, theta1,k2,theta2, k, nb_k, mu_k,
                  sd_intra_observed, sd_inter_observed, r_observed, sd_intra_theorique,
                  sd_inter_theorique, r_theorique, cpt_hubert, nb_cpt_hubert,
                  time_hubert, segment_mean_hubert, effect_size_hubert)
  colnames(hubert)[21:25] <- c("cpt_loca", "cpt", "time", "segment_mean_algo",
                               "effect_size_algo")
  hubert$algo <- "Hubert"
  
  # CPM
  cpm <- resultat_gamma %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta,log,k1, theta1,k2,theta2, k, nb_k, mu_k,
                  sd_intra_observed, sd_inter_observed, r_observed, sd_intra_theorique,
                  sd_inter_theorique, r_theorique, cpt_cpm, nb_cpt_cpm,
                  time_cpm, segment_mean_cpm, effect_size_cpm)
  colnames(cpm)[21:25] <- c("cpt_loca", "cpt", "time", "segment_mean_algo",
                            "effect_size_algo")
  cpm$algo <- "CPM"
  
  # BEAST
  beast <- resultat_gamma %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta,log,k1, theta1,k2,theta2, k, nb_k, mu_k,
                  sd_intra_observed, sd_inter_observed, r_observed, sd_intra_theorique,
                  sd_inter_theorique, r_theorique, cpt_beast, nb_cpt_beast,
                  time_beast, segment_mean_beast, effect_size_beast)
  colnames(beast)[21:25] <- c("cpt_loca", "cpt", "time", "segment_mean_algo",
                              "effect_size_algo")
  beast$algo <- "Beast"
  
  # Jumpoint
  CumSeg <- resultat_gamma %>%
    dplyr::select(id,segment_mean,N, K, ratio, delta,log,k1, theta1,k2,theta2, k, nb_k, mu_k,
                  sd_intra_observed, sd_inter_observed, r_observed, sd_intra_theorique,
                  sd_inter_theorique, r_theorique, cpt_jumpoint, nb_cpt_jumpoint,
                  time_jumpoint, segment_mean_jumpoint, effect_size_jumpoint)
  colnames(CumSeg)[21:25] <- c("cpt_loca", "cpt", "time", "segment_mean_algo",
                               "effect_size_algo")
  CumSeg$algo <- "CumSeg"
  
  # Combine the data frames
  resultat_gamma <- rbind(pelt, cpm, beast, CumSeg, BinSeg, SegNeigh, hubert)
  
  # Ajouter le rand Index  
  rand_index_f <- function(bkps1, bkps2, N) { 
    bkps1 <- c(bkps1, N)
    bkps2 <- c(bkps2, N)
    
    sanity_check <- function(bkps1, bkps2) {
      if (length(bkps1) == 0 || length(bkps2) == 0) {
        stop("Both bkps1 and bkps2 must have at least one breakpoint.")
      }
      if (bkps1[length(bkps1)] != bkps2[length(bkps2)]) {
        stop("The last breakpoint of both bkps1 and bkps2 must be the same.")
      }
    }
    
    sanity_check(bkps1, bkps2)
    n_samples <- bkps1[length(bkps1)]
    bkps1_with_0 <- c(0, bkps1)
    bkps2_with_0 <- c(0, bkps2)
    n_bkps1 <- length(bkps1)
    n_bkps2 <- length(bkps2)
    
    disagreement <- 0
    beginj <- 0  # avoids unnecessary computations
    
    for (index_bkps1 in seq_len(n_bkps1)) {
      start1 <- bkps1_with_0[index_bkps1]
      end1 <- bkps1_with_0[index_bkps1 + 1]
      
      for (index_bkps2 in seq(beginj, n_bkps2)) {
        start2 <- bkps2_with_0[index_bkps2]
        end2 <- bkps2_with_0[index_bkps2 + 1]
        nij <- max(min(end1, end2) - max(start1, start2), 0)
        disagreement <- disagreement + nij * abs(end1 - end2)
        
        # we can skip the rest of the iteration, nij will be 0
        if (end1 < end2) {
          break
        } else {
          beginj <- index_bkps2 + 1
        }
      }
    }
    
    disagreement <- disagreement / (n_samples * (n_samples - 1) / 2)
    return(1.0 - disagreement)
  }
  resultat_gamma$rand <- mapply(rand_index_f, resultat_gamma$cpt_loca, resultat_gamma$k, resultat_gamma$N)
  
  # dernier et bonne version
  calculate_metrics_with_similarity <- function(real_changes, detected_changes, tolerance) {
    # Initialisation des variables
    TP <- 0
    FP <- 0
    FN <- 0
    used_real_changes <- rep(FALSE, length(real_changes))  # Liste pour suivre les points réels utilisés
    
    # Boucle pour les points détectés
    for (detected in detected_changes) {
      # Chercher un point réel correspondant à ce point détecté
      matched <- FALSE
      for (i in 1:length(real_changes)) {
        if (!used_real_changes[i] && abs(real_changes[i] - detected) <= tolerance) {
          TP <- TP + 1  # Si une correspondance est trouvée, c'est un True Positive
          used_real_changes[i] <- TRUE  # Marquer le point réel comme utilisé
          matched <- TRUE
          break  # Sortir de la boucle dès qu'une correspondance est trouvée
        }
      }
      
      # Si aucun point réel n'a été trouvé pour ce point détecté, c'est un False Positive
      if (!matched) {
        FP <- FP + 1
      }
    }
    
    # Boucle pour les points réels non utilisés (s'il en reste)
    FN <- sum(!used_real_changes)  # Compter les points réels non associés à un point détecté
    
    # Calcul de l'Indice de Jaccard
    jaccard_index <- TP / (TP + FP + FN)  # Similarité de Jaccard
    
    # Calcul de l'Indice Inex
    inex_index <- TP / (TP + FP + FN)  # L'Inex est similaire au Jaccard dans ce cas
    
    # Résultats
    return(jaccard = jaccard_index)
  }
  resultat_gamma$jaccard <- mapply(calculate_metrics_with_similarity, 
                                   real_changes = resultat_gamma$k,
                                   detected_changes = resultat_gamma$cpt_loca,
                                   tolerance = 10)
  
  # changement en numerique des colonnes (au lieu de list)
  resultat_gamma$nb_k <- as.numeric(resultat_gamma$nb_k)
  resultat_gamma$N <- as.numeric(resultat_gamma$N)
  resultat_gamma$k2 <- as.numeric(resultat_gamma$k2)
  resultat_gamma$theta2 <- as.numeric(resultat_gamma$theta2)
  resultat_gamma$sd_intra_observed <- as.numeric(resultat_gamma$sd_intra_observed)
  resultat_gamma$sd_inter_observed <- as.numeric(resultat_gamma$sd_inter_observed)
  resultat_gamma$r_observed <- as.numeric(resultat_gamma$r_observed)
  resultat_gamma$sd_intra_theorique <- as.numeric(resultat_gamma$sd_intra_theorique)
  resultat_gamma$sd_inter_theorique <- as.numeric(resultat_gamma$sd_inter_theorique)
  resultat_gamma$r_theorique <- as.numeric(resultat_gamma$r_theorique)
  resultat_gamma$time <- as.numeric(resultat_gamma$time)
  resultat_gamma$segment_mean_algo <- as.numeric(resultat_gamma$segment_mean_algo)
  resultat_gamma$effect_size_algo <- as.numeric(resultat_gamma$effect_size_algo)
  
  
  
  # Ajoute une colonne pour le nom de delta
  resultat_gamma <- resultat_gamma %>%
    mutate(name_delta = ifelse(delta == 0, "abrupt", 
                               ifelse(delta == 0.02, "smooth", NA)))
  
  
  # Ajoute une colonne pour le nom de ratio
  resultat_gamma <- resultat_gamma %>%
    mutate(name_ratio = ifelse(ratio == 0.5, "r = 0.5", 
                               ifelse(ratio == 1, "r = 1",
                                      ifelse(ratio == 2, "r = 2", NA))))
  
  # Ajoute une colonne pour le nom de segment
  resultat_gamma <- resultat_gamma %>%
    mutate(name_segment = ifelse(segment_mean == 50, "S = 50", 
                                 ifelse(segment_mean == 100, "S = 100",
                                        ifelse(segment_mean == 200, "S = 200", NA))))
  
  # ordre des algo
  resultat_gamma$algo <- factor(resultat_gamma$algo,levels = c("Beast", "BinSeg", "CPM","CumSeg", "Hubert"
                                                               , "PELT", "SegNeigh"))
  
  # ordre des longueurs de segments
  resultat_gamma$name_segment <- factor(resultat_gamma$name_segment,levels = 
                                          c("S = 50", "S = 100", "S = 200"))
  
  
  return(resultat_gamma)
} # loi gamma
resultat_gamma <- prep_data_gamma()

# division par delta
normal_delta_0 <- resultat_normal %>%
  filter(delta == 0)
normal_delta_002 <- resultat_normal %>%
  filter(delta == 0.02)

# division par delta
gamma_delta_0 <- resultat_gamma %>%
  filter(delta == 0)
gamma_delta_002 <- resultat_gamma %>%
  filter(delta == 0.02)


# ============================================
# Création des figures de l'article
# ============================================

##### Figure 1 #####
### Sélectionner une série aléatoire normal 
# Distribution Normale / r=0.5
serie_normal_05 <- serie_normal[sample(which(serie_normal$ratio == 0.5 & serie_normal$K == 5
                                             & serie_normal$delta == 0.02 &
                                               serie_normal$segment_mean == 200), 1), ]

# Distribution Normale / r=1
serie_normal_1 <- serie_normal[sample(which(serie_normal$ratio == 1 & serie_normal$K == 5
                                            & serie_normal$delta == 0 &
                                              serie_normal$segment_mean == 200), 1), ]

# Distribution Normale / r=2
serie_normal_2 <- serie_normal[sample(which(serie_normal$ratio == 2 & serie_normal$K == 5
                                            & serie_normal$delta == 0 &
                                              serie_normal$segment_mean == 200), 1), ]

# Distribution Gamma / r=0.5
serie_gamma_05 <- serie_gamma[sample(which(serie_gamma$ratio == 0.5 & serie_gamma$K == 5
                                           & serie_gamma$delta == 0.02 
                                           & serie_gamma$log == 0 
                                           & serie_gamma$segment_mean == 200), 1), ]

# Distribution Gamma / r=1
serie_gamma_1 <- serie_gamma[sample(which(serie_gamma$ratio == 1 & serie_gamma$K == 5
                                          & serie_gamma$delta == 0 
                                          & serie_gamma$log == 0 
                                          & serie_gamma$segment_mean == 200), 1), ]

# Distribution Gamma / r=2
serie_gamma_2 <- serie_gamma[sample(which(serie_gamma$ratio == 2 & serie_gamma$K == 5
                                          & serie_gamma$delta == 0  
                                          & serie_gamma$log == 0 
                                          &  serie_gamma$segment_mean == 200), 1), ]

# Compilation des données dans un seul DF
signal <- as.data.frame(serie_normal_05$series)
colnames(signal)[1] <- "Signal"
signal <- cbind(signal,as.data.frame(serie_normal_05$template))
colnames(signal)[2] <- "Template"
info <- resultat_normal[resultat_normal$id %in% serie_normal_05$id, ]
signal$mu_0 <- info$mu_0[1]
signal <- signal %>%
  group_by(Template) %>%
  mutate(
    q2.5 = ifelse(all(is.na(Template)), NA, quantile(Signal, probs = 0.025, na.rm = TRUE)),   # Quantile 2.5%
    q97.5 = ifelse(all(is.na(Template)), NA, quantile(Signal, probs = 0.975, na.rm = TRUE))  # Quantile 97.5%
  )
signal$ID <- "r = 0.5*"
signal$index <- 1:nrow(signal)
signal_normal <- signal

signal <- as.data.frame(serie_normal_1$series)
colnames(signal)[1] <- "Signal"
signal <- cbind(signal,as.data.frame(serie_normal_1$template))
colnames(signal)[2] <- "Template"
info <- resultat_normal[resultat_normal$id %in% serie_normal_1$id, ]
signal$mu_0 <- info$mu_0[1]
signal <- signal %>%
  group_by(Template) %>%
  mutate(
    q2.5 = ifelse(all(is.na(Template)), NA, quantile(Signal, probs = 0.025, na.rm = TRUE)),   # Quantile 2.5%
    q97.5 = ifelse(all(is.na(Template)), NA, quantile(Signal, probs = 0.975, na.rm = TRUE))  # Quantile 97.5%
  )
signal$ID <- "r = 1"
signal$index <- 1:nrow(signal)
signal_normal <- rbind(signal_normal, signal)

signal <- as.data.frame(serie_normal_2$series)
colnames(signal)[1] <- "Signal"
signal <- cbind(signal,as.data.frame(serie_normal_2$template))
colnames(signal)[2] <- "Template"
info <- resultat_normal[resultat_normal$id %in% serie_normal_2$id, ]
signal$mu_0 <- info$mu_0[1]
signal <- signal %>%
  group_by(Template) %>%
  mutate(
    q2.5 = ifelse(all(is.na(Template)), NA, quantile(Signal, probs = 0.025, na.rm = TRUE)),   # Quantile 2.5%
    q97.5 = ifelse(all(is.na(Template)), NA, quantile(Signal, probs = 0.975, na.rm = TRUE))  # Quantile 97.5%
  )
signal$ID <- "r = 2"
signal$index <- 1:nrow(signal)
signal_normal <- rbind(signal_normal, signal)

signal <- as.data.frame(serie_gamma_05$series)
colnames(signal)[1] <- "Signal"
signal <- cbind(signal,as.data.frame(serie_gamma_05$template))
colnames(signal)[2] <- "Template"
info <- resultat_gamma[resultat_gamma$id %in% serie_gamma_05$id, ]
signal$mu_0 <- info$mu_0[1]
signal <- signal %>%
  group_by(Template) %>%
  mutate(
    q2.5 = ifelse(all(is.na(Template)), NA, quantile(Signal, probs = 0.025, na.rm = TRUE)),   # Quantile 2.5%
    q97.5 = ifelse(all(is.na(Template)), NA, quantile(Signal, probs = 0.975, na.rm = TRUE))  # Quantile 97.5%
  )
signal$ID <- "r = 0.5*"
signal$index <- 1:nrow(signal)
signal_gamma <- signal

signal <- as.data.frame(serie_gamma_1$series)
colnames(signal)[1] <- "Signal"
signal <- cbind(signal,as.data.frame(serie_gamma_1$template))
colnames(signal)[2] <- "Template"
info <- resultat_gamma[resultat_gamma$id %in% serie_gamma_1$id, ]
signal$mu_0 <- info$mu_0[1]
signal <- signal %>%
  group_by(Template) %>%
  mutate(
    q2.5 = ifelse(all(is.na(Template)), NA, quantile(Signal, probs = 0.025, na.rm = TRUE)),   # Quantile 2.5%
    q97.5 = ifelse(all(is.na(Template)), NA, quantile(Signal, probs = 0.975, na.rm = TRUE))  # Quantile 97.5%
  )
signal$ID <- "r = 1"
signal$index <- 1:nrow(signal)
signal_gamma <- rbind(signal_gamma, signal)

signal <- as.data.frame(serie_gamma_2$series)
colnames(signal)[1] <- "Signal"
signal <- cbind(signal,as.data.frame(serie_gamma_2$template))
colnames(signal)[2] <- "Template"
info <- resultat_gamma[resultat_gamma$id %in% serie_gamma_2$id, ]
signal$mu_0 <- info$mu_0[1]
signal <- signal %>%
  group_by(Template) %>%
  mutate(
    q2.5 = ifelse(all(is.na(Template)), NA, quantile(Signal, probs = 0.025, na.rm = TRUE)),   # Quantile 2.5%
    q97.5 = ifelse(all(is.na(Template)), NA, quantile(Signal, probs = 0.975, na.rm = TRUE))  # Quantile 97.5%
  )
signal$ID <- "r = 2"
signal$index <- 1:nrow(signal)
signal_gamma <- rbind(signal_gamma, signal)

# Création du graphique combiné 
h1 <- ggplot() +
  geom_line(data = signal_normal, aes(x = index, y = Signal), color = "black", size = 0.7) +  # Ligne pour Signal
  geom_line(data = signal_normal, aes(x = index, y = mu_0), color = "#0C3C78", size = 1) + # Ligne pour Template
  geom_line(data = signal_normal, aes(x = index, y = Template), color = "#E42F45", size = 1) + # Ligne pour Template
  geom_line(data = signal_normal, aes(x = index, y = q2.5), color = "#0C3C78", size = 1,linetype = "dashed") + # Ligne pour Template
  geom_line(data = signal_normal, aes(x = index, y = q97.5), color = "#0C3C78", size = 1,linetype = "dashed") + # Ligne pour Template
  facet_grid(ID ~ ., scales = "free_y") +  # Facettes avec titres à gauche
  theme_minimal() +
  labs(title = "a", x = NULL, y = NULL) +  # Étiquette personnalisée
  theme(
    axis.text.x = element_blank(), 
    axis.text.y.left = element_text(size = 16),
    title = element_text(size = 22),
    axis.ticks.x = element_blank(),  
    panel.grid = element_blank(),
    strip.text.y = element_text(size = 18, angle = 0, hjust = 0.5, vjust = 0.5),  # Ajustement explicite
    strip.background = element_blank(),  # Enlever le fond des facettes
    panel.spacing = unit(2, "lines")  # Espacement entre les panels
  )

h2 <- ggplot() +
  geom_line(data = signal_gamma, aes(x = index, y = Signal), color = "black", size = 0.7) +  # Ligne pour Signal
  geom_line(data = signal_gamma, aes(x = index, y = mu_0), color = "#0C3C78", size = 1) + # Ligne pour Template
  geom_line(data = signal_gamma, aes(x = index, y = Template), color = "#E42F45", size = 1) + # Ligne pour Template
  geom_line(data = signal_gamma, aes(x = index, y = q2.5), color = "#0C3C78", size = 1,linetype = "dashed") + # Ligne pour Template
  geom_line(data = signal_gamma, aes(x = index, y = q97.5), color = "#0C3C78", size = 1,linetype = "dashed") + # Ligne pour Template
  facet_wrap(~ ID, ncol = 1, scales = "free_y") +  # Facettes pour chaque méthode
  theme_minimal() +
  labs(title = "b", x = NULL, y = NULL) +  # Étiquette personnalisée  
  theme(
    axis.text.x = element_blank(),
    axis.text.y.right = element_text(size = 16),
    title = element_text(size = 22),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    strip.text  = element_blank(),
    axis.text.y = element_text(hjust = 1),
    panel.spacing = unit(2, "lines")  # Espacement entre les panels
  ) +
  scale_y_continuous(position = "right")  # Placer l'axe Y à droite

h1 +h2

##### Figure 2 #####
time <- resultat_normal %>%
  dplyr::select(time,algo,N)
time <- rbind(time, resultat_gamma %>%
                dplyr::select(time,algo,N))

ggplot(time, aes(x = as.factor(N), y = time, fill = algo, colour = algo)) + 
  geom_boxplot(outlier.shape = NA, show.legend = TRUE, width = 0.5) +  # Augmenter la largeur
  labs(x = "Signal length", y = "Time (second)",title = NULL)+
  theme_bw()+
  scale_y_log10(
    breaks = c(0.001,0.01, 0.1,1, 10,50),            # Ticks majeurs aux valeurs spécifiées
    minor_breaks = c(seq(0.001, 0.01, by = 0.001),
                     seq(0.01, 0.1, by = 0.01),
                     seq(0.1, 1, by = 1),
                     seq(1, 10, by = 1),
                     seq(10, 50, by = 10)),
    labels = function(x) {
      ifelse(x < 1, format(x, nsmall = 1, scientific = FALSE), round(x))  # Enlève les zéros inutiles pour les décimaux < 1
    }
  ) +
  annotation_logticks(sides = "l", size = 0.25) +  # Ajoute des ticks logarithmiques uniquement sur l'axe gauche
  theme(    axis.text.y = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            axis.title.y = element_text(size = 24, margin = margin(r = 10)),  # Écarte le titre de l'axe Y
            axis.title.x = element_text(size = 24, margin = margin(t = 10)),
            legend.text = element_text(size = 18),  # Augmente la taille du texte dans la légende
            legend.title = element_text(size = 20)# Augmente la taille des labels de l'axe Y
  )+
  scale_color_brewer(n="Algorithms",palette = "Dark2")+
  scale_fill_brewer(n="Algorithms",palette = "Dark2")


##### Figure 3 #####
# Mise en forme des données : Sélection des données
combined <- resultat_normal %>%
  dplyr::select(segment_mean,name_segment,segment_mean_algo,algo,
                name_ratio,ratio,delta,K,cpt,rand,jaccard)
combined$ID <- "Normal"
combined2 <-  resultat_gamma %>%
  dplyr::select(segment_mean,name_segment,segment_mean_algo,algo,
                name_ratio,ratio,delta,K,cpt,rand,jaccard)
combined2$ID <- "Gamma"
combined <- rbind(combined,combined2)
combined$ID <- factor(combined$ID,levels = c("Normal","Gamma"))

combined_delta_0 <- combined %>%
  dplyr::filter(delta == 0)

# Création du graphique
ggplot(combined_delta_0, aes(x = factor(ratio), y = segment_mean_algo ,fill = factor(algo))) +
  geom_boxplot(color = "black", outliers = FALSE,show.legend = TRUE) +
  geom_hline(aes(yintercept = segment_mean),linetype = "dashed",show.legend = FALSE)+
  facet_grid(name_segment~ID) +
  theme_bw() +
  scale_y_log10(
    breaks = c(10,50, 100,200, 1000),            # Ticks majeurs aux valeurs spécifiées
    minor_breaks = c(seq(0, 100, by = 10), seq(100, 1000, by = 100)) # Ticks mineurs
  ) +
  theme(
    # axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 24, margin = margin(r = 10)),  
    axis.title.x = element_text(size = 24, margin = margin(t = 10)),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 20),
    strip.text = element_text(size = 16)
  )+ 
  annotation_logticks(sides = "l",size = 0.25 ) + 
  labs(y = "Mean segment length", x = expression(italic(r)~ "ratio"), title = NULL) +
  scale_fill_brewer(n="Algorithms",palette = "Dark2")


# smooth change 
ggplot(combined_delta_002, aes(x = factor(ratio), y = segment_mean_algo ,fill = factor(algo))) +
  geom_boxplot(color = "black", outliers = FALSE,show.legend = TRUE) +
  geom_hline(aes(yintercept = segment_mean),linetype = "dashed",show.legend = FALSE)+
  facet_grid(name_segment~ID) +
  theme_bw() +
  scale_y_log10(
    breaks = c(10,50, 100,200, 1000),            # Ticks majeurs aux valeurs spécifiées
    minor_breaks = c(seq(0, 100, by = 10), seq(100, 1000, by = 100)) # Ticks mineurs
  ) +
  theme(
    # axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 24, margin = margin(r = 10)),  
    axis.title.x = element_text(size = 24, margin = margin(t = 10)),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 20),
    strip.text = element_text(size = 16)
  )+ 
  annotation_logticks(sides = "l",size = 0.25 ) + 
  labs(y = "Mean segment length", x = expression(italic(r)~ "ratio"), title = NULL) +
  scale_fill_brewer(n="Algorithms",palette = "Dark2")


##### Figure 4 #####
# Mise en forme des données : Sélection des données
combined <- resultat_normal %>%
  dplyr::select(segment_mean,name_segment,segment_mean_algo,algo,
                name_ratio,ratio,delta,K,cpt,rand,jaccard)
combined$ID <- "Normal"
combined2 <-  resultat_gamma %>%
  dplyr::select(segment_mean,name_segment,segment_mean_algo,algo,
                name_ratio,ratio,delta,K,cpt,rand,jaccard)
combined2$ID <- "Gamma"
combined <- rbind(combined,combined2)
combined$ID <- factor(combined$ID,levels = c("Normal","Gamma"))

# Sélection uniquement des données avec Delta 0.02
combined_delta_002 <- combined %>%
  dplyr::filter(delta == 0.02)

# Création du graphique
ggplot(combined_delta_002, aes(x = factor(ratio), y = segment_mean_algo ,fill = factor(algo))) +
  geom_boxplot(color = "black", outliers = FALSE,show.legend = TRUE) +
  geom_hline(aes(yintercept = segment_mean),linetype = "dashed",show.legend = FALSE)+
  facet_grid(name_segment~ID) +
  theme_bw() +
  scale_y_log10(
    breaks = c(10,50, 100,200, 1000),            # Ticks majeurs aux valeurs spécifiées
    minor_breaks = c(seq(0, 100, by = 10), seq(100, 1000, by = 100)) # Ticks mineurs
  ) +
  theme(
    # axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 24, margin = margin(r = 10)),  
    axis.title.x = element_text(size = 24, margin = margin(t = 10)),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 20),
    strip.text = element_text(size = 16)
  )+ 
  annotation_logticks(sides = "l",size = 0.25 ) + 
  labs(y = "Mean segment length", x = expression(italic(r)~ "ratio"), title = NULL) +
  scale_fill_brewer(n="Algorithms",palette = "Dark2")



##### Figure 5 #####
# Mise en forme des données : Sélection des données
combined <- resultat_normal %>%
  dplyr::select(segment_mean,name_segment,segment_mean_algo,algo,
                name_ratio,ratio,delta,K,cpt,rand,jaccard)
combined$ID <- "Normal"
combined2 <-  resultat_gamma %>%
  dplyr::select(segment_mean,name_segment,segment_mean_algo,algo,
                name_ratio,ratio,delta,K,cpt,rand,jaccard)
combined2$ID <- "Gamma"
combined <- rbind(combined,combined2)
combined$ID <- factor(combined$ID,levels = c("Normal","Gamma"))

combined_delta_0 <- combined %>%
  dplyr::filter(delta == 0)

# Création du graphique
ggplot(combined_delta_0, aes(x = factor(ratio), y = rand ,fill = factor(algo))) +
  geom_boxplot(color = "black", outliers = FALSE,show.legend = TRUE) +
  facet_grid(name_segment~ID) +
  theme_bw() +
  labs(y = "Rand Index", x = expression(italic(r)~ "ratio"), title = NULL) +
  theme(
    # axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 24, margin = margin(r = 10)),  
    axis.title.x = element_text(size = 24, margin = margin(t = 10)),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 20),
    strip.text = element_text(size = 16)
  )+ 
  scale_fill_brewer(n="Algorithms",palette = "Dark2")


##### Figure 6 #####
# Mise en forme des données : Sélection des données
combined <- resultat_normal %>%
  dplyr::select(segment_mean,name_segment,segment_mean_algo,algo,
                name_ratio,ratio,delta,K,cpt,rand,jaccard)
combined$ID <- "Normal"
combined2 <-  resultat_gamma %>%
  dplyr::select(segment_mean,name_segment,segment_mean_algo,algo,
                name_ratio,ratio,delta,K,cpt,rand,jaccard)
combined2$ID <- "Gamma"
combined <- rbind(combined,combined2)
combined$ID <- factor(combined$ID,levels = c("Normal","Gamma"))

combined_delta_0 <- combined %>%
  dplyr::filter(delta == 0)

# Création du graphique
ggplot(combined_delta_0, aes(x = factor(ratio), y = jaccard ,fill = factor(algo))) +
  geom_boxplot(color = "black", outliers = FALSE,show.legend = TRUE) +
  facet_grid(name_segment~ID) +
  theme_bw() +
  labs(y = "Jaccard Index", x = expression(italic(r)~ "ratio"), title = NULL) +
  theme(
    # axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 24, margin = margin(r = 10)),  
    axis.title.x = element_text(size = 24, margin = margin(t = 10)),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.text = element_text(size = 18),  
    legend.title = element_text(size = 20),
    strip.text = element_text(size = 16)
  )+ 
  scale_fill_brewer(n="Algorithms",palette = "Dark2")


##### Ensambling (pour Figure 7) #####
# ensambling pour AC
extract_cp_matrix <- function(resultats_reel_AC) {
  cp_columns <- resultats_reel_AC %>%
    select(starts_with("cp_"))
  
  # Déterminer la longueur de la série temporelle
  # Ici, supposons que N représente la longueur
  N <- resultats_reel_AC$N[1]
  
  # Initialiser la matrice binaire
  cp_matrix <- matrix(0, nrow = N, ncol = ncol(cp_columns))
  colnames(cp_matrix) <- names(cp_columns)
  
  # Remplir la matrice
  for (i in 1:ncol(cp_columns)) {
    cp_list <- cp_columns[[i]][[1]]
    cp_matrix[cp_list, i] <- 1
  }
  
  return(cp_matrix)
}
cp_matrix <- extract_cp_matrix(resultats_reel_AC)

cp_matrix_t <- t(cp_matrix)
max_clusters <- 20

title <- "results_AC"

# Exécuter ConsensusClusterPlus
consensus_results_AC <- ConsensusClusterPlus(
  cp_matrix_t,
  maxK = max_clusters,
  reps = 1000, # Nombre de rééchantillonnages
  pItem = 0.8,  # Proportion d'objets à échantillonner à chaque itération
  pFeature = 1, # Proportion de features à échantillonner à chaque itération
  clusterAlg = "hc", # Algorithme de clustering, par exemple 'hc' pour hierarchical clustering
  distance = "euclidean",
  seed = 1234,
  plot = "png",
  title = title # Générer des graphiques de consensus
)

optimal_k <- 3
final_clusters <- consensus_results_AC[[optimal_k]]$consensusClass

consensus_change_points <- which(diff(final_clusters) != 0) + 1
diff_points <- diff(consensus_change_points)
new_sequence <- c(TRUE, diff_points != 1)
ensambling_AC <- consensus_change_points[new_sequence]
print(ensambling_AC)

# pour log_AC
extract_cp_matrix <- function(resultats_reel_log_AC) {
  cp_columns <- resultats_reel_log_AC %>%
    select(starts_with("cp_"))
  
  # Déterminer la longueur de la série temporelle
  # Ici, supposons que N représente la longueur
  N <- resultats_reel_log_AC$N[1]
  
  # Initialiser la matrice binaire
  cp_matrix <- matrix(0, nrow = N, ncol = ncol(cp_columns))
  colnames(cp_matrix) <- names(cp_columns)
  
  # Remplir la matrice
  for (i in 1:ncol(cp_columns)) {
    cp_list <- cp_columns[[i]][[1]]
    cp_matrix[cp_list, i] <- 1
  }
  
  return(cp_matrix)
}
cp_matrix <- extract_cp_matrix(resultats_reel_log_AC)

cp_matrix_t <- t(cp_matrix)
max_clusters <- 20

title <- "results_log_AC"

# Exécuter ConsensusClusterPlus
consensus_results_log_AC <- ConsensusClusterPlus(
  cp_matrix_t,
  maxK = max_clusters,
  reps = 1000, # Nombre de rééchantillonnages
  pItem = 0.8,  # Proportion d'objets à échantillonner à chaque itération
  pFeature = 1, # Proportion de features à échantillonner à chaque itération
  clusterAlg = "hc", # Algorithme de clustering, par exemple 'hc' pour hierarchical clustering
  distance = "euclidean",
  seed = 1234,
  plot = "png",
  title = title # Générer des graphiques de consensus
)

optimal_k <- 7
final_clusters <- consensus_results_log_AC[[optimal_k]]$consensusClass

consensus_change_points <- which(diff(final_clusters) != 0) + 1
diff_points <- diff(consensus_change_points)
new_sequence <- c(TRUE, diff_points != 1)
ensambling_log_AC <- consensus_change_points[new_sequence]
print(ensambling_log_AC)



# ensambling pour VB
extrVBt_cp_matrix <- function(resultats_reel_VB) {
  cp_columns <- resultats_reel_VB %>%
    select(starts_with("cp_"))
  
  # Déterminer la longueur de la série temporelle
  # Ici, supposons que N représente la longueur
  N <- resultats_reel_VB$N[1]
  
  # Initialiser la matrice binaire
  cp_matrix <- matrix(0, nrow = N, ncol = ncol(cp_columns))
  colnames(cp_matrix) <- names(cp_columns)
  
  # Remplir la matrice
  for (i in 1:ncol(cp_columns)) {
    cp_list <- cp_columns[[i]][[1]]
    cp_matrix[cp_list, i] <- 1
  }
  
  return(cp_matrix)
}
cp_matrix <- extrVBt_cp_matrix(resultats_reel_VB)

cp_matrix_t <- t(cp_matrix)
max_clusters <- 20

title <- "results_VB"

# Exécuter ConsensusClusterPlus
consensus_results_VB <- ConsensusClusterPlus(
  cp_matrix_t,
  maxK = max_clusters,
  reps = 1000, # Nombre de rééchantillonnages
  pItem = 0.8,  # Proportion d'objets à échantillonner à chaque itération
  pFeature = 1, # Proportion de features à échantillonner à chaque itération
  clusterAlg = "hc", # Algorithme de clustering, par exemple 'hc' pour hierarchical clustering
  distance = "euclidean",
  seed = 1234,
  plot = "png",
  title = title # Générer des graphiques de consensus
)

optimal_k <- 7
final_clusters <- consensus_results_VB[[optimal_k]]$consensusClass

consensus_change_points <- which(diff(final_clusters) != 0) + 1
diff_points <- diff(consensus_change_points)
new_sequence <- c(TRUE, diff_points != 1)
ensambling_VB <- consensus_change_points[new_sequence]
print(ensambling_VB)

# pour log_VB
extrVBt_cp_matrix <- function(resultats_reel_log_VB) {
  cp_columns <- resultats_reel_log_VB %>%
    select(starts_with("cp_"))
  
  # Déterminer la longueur de la série temporelle
  # Ici, supposons que N représente la longueur
  N <- resultats_reel_log_VB$N[1]
  
  # Initialiser la matrice binaire
  cp_matrix <- matrix(0, nrow = N, ncol = ncol(cp_columns))
  colnames(cp_matrix) <- names(cp_columns)
  
  # Remplir la matrice
  for (i in 1:ncol(cp_columns)) {
    cp_list <- cp_columns[[i]][[1]]
    cp_matrix[cp_list, i] <- 1
  }
  
  return(cp_matrix)
}
cp_matrix <- extrVBt_cp_matrix(resultats_reel_log_VB)

cp_matrix_t <- t(cp_matrix)
max_clusters <- 20

title <- "results_log_VB"

# Exécuter ConsensusClusterPlus
consensus_results_log_VB <- ConsensusClusterPlus(
  cp_matrix_t,
  maxK = max_clusters,
  reps = 1000, # Nombre de rééchantillonnages
  pItem = 0.8,  # Proportion d'objets à échantillonner à chaque itération
  pFeature = 1, # Proportion de features à échantillonner à chaque itération
  clusterAlg = "hc", # Algorithme de clustering, par exemple 'hc' pour hierarchical clustering
  distance = "euclidean",
  seed = 1234,
  plot = "png",
  title = title # Générer des graphiques de consensus
)

optimal_k <- 5
final_clusters <- consensus_results_log_VB[[optimal_k]]$consensusClass

consensus_change_points <- which(diff(final_clusters) != 0) + 1
diff_points <- diff(consensus_change_points)
new_sequence <- c(TRUE, diff_points != 1)
ensambling_log_VB <- consensus_change_points[new_sequence]
print(ensambling_log_VB)


##### Figure 7 #####


label <- c("Beast", "BinSeg", "CPM","CumSeg", "Hubert", "PELT", "SegNeigh", "", "Consensus")

N <- resultats_reel_AC$N

data_comb <- data.frame(x = rep(1:N, times = length(label)),  # Répéter x N fois pour chaque ID
                        y = rep(1, N * length(label)),  # Répéter y N fois pour chaque ID
                        ID = rep(label, each = N))  # Répéter chaque label N fois

data_comb$ID <- factor(data_comb$ID, levels = label)

encoches_combined_AC <- rbind(
  data.frame(x = unlist(resultats_reel_AC$cp_beast), y = rep(1, length(unlist(resultats_reel_AC$cp_beast))), ID = rep(label[1], length(unlist(resultats_reel_AC$cp_beast)))),
  data.frame(x = unlist(resultats_reel_AC$cp_BinSeg), y = rep(1, length(unlist(resultats_reel_AC$cp_BinSeg))), ID = rep(label[2], length(unlist(resultats_reel_AC$cp_BinSeg)))),
  data.frame(x = unlist(resultats_reel_AC$cp_cpm), y = rep(1, length(unlist(resultats_reel_AC$cp_cpm))), ID = rep(label[3], length(unlist(resultats_reel_AC$cp_cpm)))),
  data.frame(x = unlist(resultats_reel_AC$cp_hubert), y = rep(1, length(unlist(resultats_reel_AC$cp_hubert))), ID = rep(label[4], length(unlist(resultats_reel_AC$cp_hubert)))),
  data.frame(x = unlist(resultats_reel_AC$cp_jumpoint), y = rep(1, length(unlist(resultats_reel_AC$cp_jumpoint))), ID = rep(label[5], length(unlist(resultats_reel_AC$cp_jumpoint)))),
  data.frame(x = unlist(resultats_reel_AC$cp_pelt), y = rep(1, length(unlist(resultats_reel_AC$cp_pelt))), ID = rep(label[6], length(unlist(resultats_reel_AC$cp_pelt)))),
  data.frame(x = unlist(resultats_reel_AC$cp_SegNeigh), y = rep(1, length(unlist(resultats_reel_AC$cp_SegNeigh))), ID = rep(label[7], length(unlist(resultats_reel_AC$cp_SegNeigh)))),
  data.frame(x = NA, y = NA, ID = rep(label[8], 1)),
  data.frame(x = ensambling_AC, y = rep(1,length(ensambling_AC)), ID = rep(label[9], length(ensambling_AC)))
)

encoches_combined_log_AC <- rbind(
  data.frame(x = unlist(resultats_reel_log_AC$cp_beast), y = rep(1, length(unlist(resultats_reel_log_AC$cp_beast))), ID = rep(label[1], length(unlist(resultats_reel_log_AC$cp_beast)))),
  data.frame(x = unlist(resultats_reel_log_AC$cp_BinSeg), y = rep(1, length(unlist(resultats_reel_log_AC$cp_BinSeg))), ID = rep(label[2], length(unlist(resultats_reel_log_AC$cp_BinSeg)))),
  data.frame(x = unlist(resultats_reel_log_AC$cp_cpm), y = rep(1, length(unlist(resultats_reel_log_AC$cp_cpm))), ID = rep(label[3], length(unlist(resultats_reel_log_AC$cp_cpm)))),
  data.frame(x = unlist(resultats_reel_log_AC$cp_hubert), y = rep(1, length(unlist(resultats_reel_log_AC$cp_hubert))), ID = rep(label[4], length(unlist(resultats_reel_log_AC$cp_hubert)))),
  data.frame(x = unlist(resultats_reel_log_AC$cp_jumpoint), y = rep(1, length(unlist(resultats_reel_log_AC$cp_jumpoint))), ID = rep(label[5], length(unlist(resultats_reel_log_AC$cp_jumpoint)))),
  data.frame(x = unlist(resultats_reel_log_AC$cp_pelt), y = rep(1, length(unlist(resultats_reel_log_AC$cp_pelt))), ID = rep(label[6], length(unlist(resultats_reel_log_AC$cp_pelt)))),
  data.frame(x = unlist(resultats_reel_log_AC$cp_SegNeigh), y = rep(1, length(unlist(resultats_reel_log_AC$cp_SegNeigh))), ID = rep(label[7], length(unlist(resultats_reel_log_AC$cp_SegNeigh)))),
  data.frame(x = NA, y = NA, ID = rep(label[8], 1)),
  data.frame(x = ensambling_log_AC, y = rep(1,length(ensambling_log_AC)), ID = rep(label[9], length(ensambling_log_AC)))
)

encoches_combined_VB <- rbind(
  data.frame(x = unlist(resultats_reel_VB$cp_beast), y = rep(1, length(unlist(resultats_reel_VB$cp_beast))), ID = rep(label[1], length(unlist(resultats_reel_VB$cp_beast)))),
  data.frame(x = unlist(resultats_reel_VB$cp_BinSeg), y = rep(1, length(unlist(resultats_reel_VB$cp_BinSeg))), ID = rep(label[2], length(unlist(resultats_reel_VB$cp_BinSeg)))),
  data.frame(x = unlist(resultats_reel_VB$cp_cpm), y = rep(1, length(unlist(resultats_reel_VB$cp_cpm))), ID = rep(label[3], length(unlist(resultats_reel_VB$cp_cpm)))),
  data.frame(x = unlist(resultats_reel_VB$cp_hubert), y = rep(1, length(unlist(resultats_reel_VB$cp_hubert))), ID = rep(label[4], length(unlist(resultats_reel_VB$cp_hubert)))),
  data.frame(x = unlist(resultats_reel_VB$cp_jumpoint), y = rep(1, length(unlist(resultats_reel_VB$cp_jumpoint))), ID = rep(label[5], length(unlist(resultats_reel_VB$cp_jumpoint)))),
  data.frame(x = unlist(resultats_reel_VB$cp_pelt), y = rep(1, length(unlist(resultats_reel_VB$cp_pelt))), ID = rep(label[6], length(unlist(resultats_reel_VB$cp_pelt)))),
  data.frame(x = unlist(resultats_reel_VB$cp_SegNeigh), y = rep(1, length(unlist(resultats_reel_VB$cp_SegNeigh))), ID = rep(label[7], length(unlist(resultats_reel_VB$cp_SegNeigh)))),
  data.frame(x = NA, y = NA, ID = rep(label[8], 1)),
  data.frame(x = ensambling_VB, y = rep(1,length(ensambling_VB)), ID = rep(label[9], length(ensambling_VB)))
)

encoches_combined_log_VB <- rbind(
  data.frame(x = unlist(resultats_reel_log_VB$cp_beast), y = rep(1, length(unlist(resultats_reel_log_VB$cp_beast))), ID = rep(label[1], length(unlist(resultats_reel_log_VB$cp_beast)))),
  data.frame(x = unlist(resultats_reel_log_VB$cp_BinSeg), y = rep(1, length(unlist(resultats_reel_log_VB$cp_BinSeg))), ID = rep(label[2], length(unlist(resultats_reel_log_VB$cp_BinSeg)))),
  data.frame(x = unlist(resultats_reel_log_VB$cp_cpm), y = rep(1, length(unlist(resultats_reel_log_VB$cp_cpm))), ID = rep(label[3], length(unlist(resultats_reel_log_VB$cp_cpm)))),
  data.frame(x = unlist(resultats_reel_log_VB$cp_hubert), y = rep(1, length(unlist(resultats_reel_log_VB$cp_hubert))), ID = rep(label[4], length(unlist(resultats_reel_log_VB$cp_hubert)))),
  data.frame(x = unlist(resultats_reel_log_VB$cp_jumpoint), y = rep(1, length(unlist(resultats_reel_log_VB$cp_jumpoint))), ID = rep(label[5], length(unlist(resultats_reel_log_VB$cp_jumpoint)))),
  data.frame(x = unlist(resultats_reel_log_VB$cp_pelt), y = rep(1, length(unlist(resultats_reel_log_VB$cp_pelt))), ID = rep(label[6], length(unlist(resultats_reel_log_VB$cp_pelt)))),
  data.frame(x = unlist(resultats_reel_log_VB$cp_SegNeigh), y = rep(1, length(unlist(resultats_reel_log_VB$cp_SegNeigh))), ID = rep(label[7], length(unlist(resultats_reel_log_VB$cp_SegNeigh)))),
  data.frame(x = NA, y = NA, ID = rep(label[8], 1)),
  data.frame(x = ensambling_log_VB, y = rep(1,length(ensambling_log_VB)), ID = rep(label[9], length(ensambling_log_VB)))
)


distance_min <- (min(Data$measure/1000))
distance_max <- (max(Data$measure/1000))
ticks_labels <- seq(floor(distance_min / 10) * 10, ceiling(distance_max / 10) * 10, by = 30)
ticks_positions <- approx(x = Data$measure/1000, y = 1:N, xout = ticks_labels)$y  # Interpolation des indices


Data$ID_AC <- "Active Channel Width (m)"
Data$ID_log_AC <- "Log transform"
Data$ID_VB <- "Valley Bottom Width (m)"
Data$ID_log_VB <- "Log transform"


ac1 <- ggplot(Data, aes(x = 1:N, y = active_channel_width))+
  geom_line(size = 0.8)+
  facet_wrap(~ ID_AC, ncol = 1, strip.position = "left", switch = "y")+
  theme_minimal()+
  theme_minimal() +
  theme(
    panel.border = element_blank(), # Supprime les bordures
    panel.grid = element_blank(),   # Supprime les grilles
    axis.line.y = element_line(color = "black", size = 0.6), # Ajoute une ligne verticale comme axe Y
    axis.ticks.y = element_line(color = "black"), # Ajoute des ticks sur l'axe Y
    axis.text.y = element_text(size = 16),  # Affiche les étiquettes sur l'axe Y
    axis.title.y = element_text(size = 16), # Ajoute un titre à l'axe Y
    strip.text.y.left = element_text(
      hjust = 0.5, 
      vjust = 0.5, 
      size = 16,
      margin = margin(r = 5, l = 15)
    ),
    strip.placement = "outside",  # Place les labels des facettes à l'extérieur
    axis.text.x = element_blank(), # Supprime les étiquettes (textes) sur l'axe X
    axis.ticks.x = element_blank(), # Supprime les ticks de l'axe X
    axis.title.x = element_blank(), # Supprime le titre de l'axe X
  ) +
  scale_x_continuous(
    breaks = ticks_positions,  # Positions interpolées des ticks
    labels = ticks_labels,  # Étiquettes correspondant aux distances
    expand = c(0, 0)  # Assure que l'axe commence à x = 0
  ) +
  labs(x = NULL, y = NULL)

ac1_log <- ggplot(Data, aes(x = 1:N, y = log(active_channel_width)+1))+
  geom_line(size = 0.8)+
  facet_wrap(~ ID_log_AC, ncol = 1, strip.position = "left", switch = "y")+
  theme_minimal() +
  theme(
    panel.border = element_blank(), # Supprime les bordures
    panel.grid = element_blank(),   # Supprime les grilles
    axis.line.y = element_line(color = "black", size = 0.5), # Ajoute une ligne verticale comme axe Y
    axis.ticks.y = element_line(color = "black"), # Ajoute des ticks sur l'axe Y
    axis.text.y = element_text(size = 16),  # Affiche les étiquettes sur l'axe Y
    axis.title.y = element_text(size = 16), # Ajoute un titre à l'axe Y
    strip.text.y.left = element_text(
      hjust = 0.5, 
      vjust = 0.5, 
      size = 16,
      margin = margin(r = 5, l = 25)
    ),
    strip.placement = "outside",  # Place les labels des facettes à l'extérieur
    axis.text.x = element_blank(), # Supprime les étiquettes (textes) sur l'axe X
    axis.ticks.x = element_blank(), # Supprime les ticks de l'axe X
    axis.title.x = element_blank() # Supprime le titre de l'axe x
  ) +
  scale_x_continuous(
    breaks = ticks_positions,  # Positions interpolées des ticks
    labels = ticks_labels,  # Étiquettes correspondant aux distances
    expand = c(0, 0)  # Assure que l'axe commence à x = 0
  ) +
  labs(x = NULL, y = NULL)



ac2 <- ggplot() +
  geom_line(data = data_comb, aes(x = x, y = y), color = "black", size = 0.8) +
  geom_segment(data = encoches_combined_AC, aes(x = x, xend = x, y = y - 0.6, yend = y + 0.6), color = "red", size = 1.5) +
  facet_wrap(~factor(ID, levels = label), ncol = 1, strip.position = "left")  +
  theme_void() +
  theme(
    panel.border = element_blank(),
    axis.title.x = element_text(size = 18, color = "black", margin = margin(t = 10)),
    axis.text.Y = element_blank(),
    axis.text.x = element_text(size = 16, color = "black"),  # Ajouter le texte pour l'axe x
    axis.ticks.y = element_blank(),
    strip.text.y.left = element_text(
      angle = 0, 
      hjust = 0,    # Alignement horizontal vers la gauche
      vjust = 0.5,  # Alignement vertical centré
      size = 16,
      margin = margin(r = 5, l = 15)  # Décalage vers la gauche
    ),
    strip.placement = "outside",  # S'assurer que les labels des facettes sont à l'extérieur
    strip.background = element_blank(),  # Supprimer le fond de la strip
  ) +
  ylim(0, 2) +
  scale_x_continuous(
    breaks = ticks_positions,  # Positions interpolées des ticks
    labels = ticks_labels,  # Étiquettes correspondant aux distances
    expand = c(0, 0)  # Assure que l'axe commence à x = 0
  ) +
  # Ajouter un carré blanc sur la facette "rien" pour cacher la ligne noire
  geom_line(data = data.frame(ID = "", x = data_comb$x, y = data_comb$y),
            aes(x = x, y = y), color = "white", size = 0.8) +
  labs(x = "Distance from downstream (km)") 



ac2_log <- ggplot() +
  geom_line(data = data_comb, aes(x = x, y = y), color = "black", size = 0.8) +
  geom_segment(data = encoches_combined_log_AC, aes(x = x, xend = x, y = y - 0.6, yend = y + 0.6), color = "red", size = 1.5) +
  facet_wrap(~factor(ID, levels = label), ncol = 1, strip.position = "left")  +
  theme_void() +
  theme(
    panel.border = element_blank(),
    axis.title.x = element_text(size = 18, color = "black", margin = margin(t = 10)),
    axis.text.Y = element_blank(),
    axis.text.x = element_text(size = 16, color = "black"),  # Ajouter le texte pour l'axe x
    axis.ticks.y = element_blank(),
    strip.text.y.left = element_text(
      angle = 0, 
      hjust = 0,    # Alignement horizontal vers la gauche
      vjust = 0.5,  # Alignement vertical centré
      size = 16,
      margin = margin(r = 5, l = 15)  # Décalage vers la gauche
    ),
    strip.placement = "outside",  # S'assurer que les labels des facettes sont à l'extérieur
    strip.background = element_blank(),  # Supprimer le fond de la strip
  ) +
  ylim(0, 2) +
  scale_x_continuous(
    breaks = ticks_positions,  # Positions interpolées des ticks
    labels = ticks_labels,  # Étiquettes correspondant aux distances
    expand = c(0, 0)  # Assure que l'axe commence à x = 0
  ) +
  # Ajouter un carré blanc sur la facette "rien" pour cacher la ligne noire
  geom_line(data = data.frame(ID = "", x = data_comb$x, y = data_comb$y),
            aes(x = x, y = y), color = "white", size = 0.8) +
  labs(x = "Distance from downstream (km)") 




# idem pour VB
vb1 <- ggplot(Data, aes(x = 1:N, y = valley_bottom_width))+
  geom_line(size = 0.8)+
  facet_wrap(~ ID_VB, ncol = 1, strip.position = "left", switch = "y")+
  theme_minimal()+
  theme_minimal() +
  theme(
    panel.border = element_blank(), # Supprime les bordures
    panel.grid = element_blank(),   # Supprime les grilles
    axis.line.y = element_line(color = "black", size = 0.6), # Ajoute une ligne verticale comme axe Y
    axis.ticks.y = element_line(color = "black"), # Ajoute des ticks sur l'axe Y
    axis.text.y = element_text(size = 16),  # Affiche les étiquettes sur l'axe Y
    axis.title.y = element_text(size = 16), # Ajoute un titre à l'axe Y
    strip.text.y.left = element_text(
      hjust = 0.5, 
      vjust = 0.5, 
      size = 16,
      margin = margin(r = 5, l = 15)
    ),
    strip.placement = "outside",  # Place les labels des facettes à l'extérieur
    axis.text.x = element_blank(), # Supprime les étiquettes (textes) sur l'axe X
    axis.ticks.x = element_blank(), # Supprime les ticks de l'axe X
    axis.title.x = element_blank(), # Supprime le titre de l'axe X
  ) +
  scale_x_continuous(
    breaks = ticks_positions,  # Positions interpolées des ticks
    labels = ticks_labels,  # Étiquettes correspondant aux distances
    expand = c(0, 0)  # Assure que l'axe commence à x = 0
  ) +
  labs(x = NULL, y = NULL)

vb1_log <- ggplot(Data, aes(x = 1:N, y = log(valley_bottom_width)+1))+
  geom_line(size = 0.8)+
  facet_wrap(~ ID_log_VB, ncol = 1, strip.position = "left", switch = "y")+
  theme_minimal() +
  theme(
    panel.border = element_blank(), # Supprime les bordures
    panel.grid = element_blank(),   # Supprime les grilles
    axis.line.y = element_line(color = "black", size = 0.5), # Ajoute une ligne verticale comme axe Y
    axis.ticks.y = element_line(color = "black"), # Ajoute des ticks sur l'axe Y
    axis.text.y = element_text(size = 16),  # Affiche les étiquettes sur l'axe Y
    axis.title.y = element_text(size = 16), # Ajoute un titre à l'axe Y
    strip.text.y.left = element_text(
      hjust = 0.5, 
      vjust = 0.5, 
      size = 16,
      margin = margin(r = 5, l = 25)
    ),
    strip.placement = "outside",  # Place les labels des facettes à l'extérieur
    axis.text.x = element_blank(), # Supprime les étiquettes (textes) sur l'axe X
    axis.ticks.x = element_blank(), # Supprime les ticks de l'axe X
    axis.title.x = element_blank() # Supprime le titre de l'axe x
  ) +
  scale_x_continuous(
    breaks = ticks_positions,  # Positions interpolées des ticks
    labels = ticks_labels,  # Étiquettes correspondant aux distances
    expand = c(0, 0)  # Assure que l'axe commence à x = 0
  ) +
  labs(x = NULL, y = NULL)



vb2 <- ggplot() +
  geom_line(data = data_comb, aes(x = x, y = y), color = "black", size = 0.8) +
  geom_segment(data = encoches_combined_VB, aes(x = x, xend = x, y = y - 0.6, yend = y + 0.6), color = "red", size = 1.5) +
  facet_wrap(~factor(ID, levels = label), ncol = 1, strip.position = "left")  +
  theme_void() +
  theme(
    panel.border = element_blank(),
    axis.title.x = element_text(size = 18, color = "black", margin = margin(t = 10)),
    axis.text.Y = element_blank(),
    axis.text.x = element_text(size = 16, color = "black"),  # Ajouter le texte pour l'axe x
    axis.ticks.y = element_blank(),
    strip.text.y.left = element_text(
      angle = 0, 
      hjust = 0,    # Alignement horizontal vers la gauche
      vjust = 0.5,  # Alignement vertical centré
      size = 16,
      margin = margin(r = 5, l = 15)  # Décalage vers la gauche
    ),
    strip.placement = "outside",  # S'assurer que les labels des facettes sont à l'extérieur
    strip.background = element_blank(),  # Supprimer le fond de la strip
  ) +
  ylim(0, 2) +
  scale_x_continuous(
    breaks = ticks_positions,  # Positions interpolées des ticks
    labels = ticks_labels,  # Étiquettes correspondant aux distances
    expand = c(0, 0)  # Assure que l'axe commence à x = 0
  ) +
  # Ajouter un carré blanc sur la facette "rien" pour cacher la ligne noire
  geom_line(data = data.frame(ID = "", x = data_comb$x, y = data_comb$y),
            aes(x = x, y = y), color = "white", size = 0.8) +
  labs(x = "Distance from downstream (km)") 



vb2_log <- ggplot() +
  geom_line(data = data_comb, aes(x = x, y = y), color = "black", size = 0.8) +
  geom_segment(data = encoches_combined_log_VB, aes(x = x, xend = x, y = y - 0.6, yend = y + 0.6), color = "red", size = 1.5) +
  facet_wrap(~factor(ID, levels = label), ncol = 1, strip.position = "left")  +
  theme_void() +
  theme(
    panel.border = element_blank(),
    axis.title.x = element_text(size = 18, color = "black", margin = margin(t = 10)),
    axis.text.Y = element_blank(),
    axis.text.x = element_text(size = 16, color = "black"),  # Ajouter le texte pour l'axe x
    axis.ticks.y = element_blank(),
    strip.text.y.left = element_text(
      angle = 0, 
      hjust = 0,    # Alignement horizontal vers la gauche
      vjust = 0.5,  # Alignement vertical centré
      size = 16,
      margin = margin(r = 5, l = 15)  # Décalage vers la gauche
    ),
    strip.placement = "outside",  # S'assurer que les labels des facettes sont à l'extérieur
    strip.background = element_blank(),  # Supprimer le fond de la strip
  ) +
  ylim(0, 2) +
  scale_x_continuous(
    breaks = ticks_positions,  # Positions interpolées des ticks
    labels = ticks_labels,  # Étiquettes correspondant aux distances
    expand = c(0, 0)  # Assure que l'axe commence à x = 0
  ) +
  # Ajouter un carré blanc sur la facette "rien" pour cacher la ligne noire
  geom_line(data = data.frame(ID = "", x = data_comb$x, y = data_comb$y),
            aes(x = x, y = y), color = "white", size = 0.8) +
  labs(x = "Distance from downstream (km)") 



k1 <- ac1 / ac2 
k1
k2 <- ac1_log / ac2_log
k2

plot_grid(k1, k2, ncol = 1, labels = c("a", "b"), label_size = 25)


v1 <- vb1 / vb2
v1
v2 <- vb1_log / vb2_log
v2

plot_grid(v1, v2, ncol = 1, labels = c("a", "b"), label_size = 25)




##### Figure 9 #####
# Génération des data
generate_segmented_series <- function(mean_central, variance, segment_length = 100, transition_length) {
  set.seed(123)  # Pour la reproductibilité
  
  # Générer 4 moyennes différentes autour de la moyenne centrale
  segment_means <- mean_central + runif(4, -1, 1)
  
  # Générer les 4 segments avec rnorm
  segment1 <- rnorm(segment_length, mean = segment_means[1], sd = sqrt(variance))
  segment2 <- rnorm(segment_length, mean = segment_means[2], sd = sqrt(variance))
  segment3 <- rnorm(segment_length, mean = segment_means[3], sd = sqrt(variance))
  segment4 <- rnorm(segment_length, mean = segment_means[4], sd = sqrt(variance))
  
  # Fonction pour créer une transition linéaire entre deux segments
  create_transition <- function(start, end, length) {
    seq(start, end, length.out = length) + rnorm(length, mean = 0, sd = sqrt(variance) / 2)
  }
  
  # Ajouter des transitions douces entre les segments
  transition1 <- create_transition(tail(segment1, 1), head(segment2, 1), transition_length)
  transition2 <- create_transition(tail(segment2, 1), head(segment3, 1), transition_length)
  transition3 <- create_transition(tail(segment3, 1), head(segment4, 1), transition_length)
  
  # Concaténer les segments et les transitions
  series <- c(segment1, transition1, segment2, transition2, segment3, transition3, segment4)
  
  return(series)
}

# Exemple d'utilisation
serie_0 <- generate_segmented_series(mean_central = 0, variance = 0, transition_length = 20)
serie_05 <- generate_segmented_series(mean_central = 0, variance = 0.05, transition_length = 20)

# Calcul des points de changement par chaque algorithme
#BEAST
cpt_result <- beast(serie_0, season = "none", tcp.minmax = c(0, 150), quiet = 1)
ncp_mode_beast <- cpt_result$trend$ncp_mode
cp_beast <- cpt_result$trend$cp
cpt_beast <- cp_beast[1:ncp_mode_beast]
cpt_beast <- sort(cpt_beast)

#pelt
cpt_result <- cpt.mean(serie_0, method = "PELT", penalty = "Manual",
                       pen.value = var(serie_0) * log(length(serie_0)))
cpt_pelt <- cpt_result@cpts
cpt_pelt <- cpt_pelt[-length(cpt_pelt)]


df_0 <- data.frame(
  Time = 1:length(serie_0),
  Value = serie_0
)

# Points de rupture sous forme de dataframe
breakpoints_0 <- bind_rows(
  data.frame(Time = cpt_beast, method = "BEAST"),
  data.frame(Time = cpt_pelt, method = "PELT"),
)


#BEAST
cpt_result <- beast(serie_05, season = "none", tcp.minmax = c(0, 150), quiet = 1)
ncp_mode_beast <- cpt_result$trend$ncp_mode
cp_beast <- cpt_result$trend$cp
cpt_beast <- cp_beast[1:ncp_mode_beast]
cpt_beast <- sort(cpt_beast)

#pelt
#pelt
cpt_result <- cpt.mean(serie_05, method = "PELT", penalty = "Manual",
                       pen.value = var(serie_05) * log(length(serie_05)))
cpt_pelt <- cpt_result@cpts
cpt_pelt <- cpt_pelt[-length(cpt_pelt)]

df_05 <- data.frame(
  Time = 1:length(serie_05),
  Value = serie_05
)

# Points de rupture sous forme de dataframe
breakpoints_05 <- bind_rows(
  data.frame(Time = cpt_beast, method = "BEAST"),
  data.frame(Time = cpt_pelt, method = "PELT"),
)


# Déterminer les bornes de l'axe Y (les mêmes pour les deux graphes)
min_y <- min(df_0$Value, df_05$Value)
max_y <- max(df_0$Value, df_05$Value)

# Graphique 1
a <- ggplot(df_0, aes(x = Time, y = Value)) +
  geom_line(color = "black") +
  geom_vline(data = breakpoints_0, aes(xintercept = Time), color = "red", 
             linetype = "dashed", size = 1.2) +  
  facet_wrap(~method, scales = "free_x", strip.position = "left", ncol = 1) +  
  labs(x = NULL, y = NULL) +  # Supprime les labels des axes
  ylim(min_y, max_y) +  # Fixe les bornes de Y
  theme_void() +  # Supprime tous les éléments du thème
  theme(
    strip.text = element_text(size = 14, face = "bold", margin = margin(r = 10)),  # Décale vers la gauche
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Ajoute un cadre autour de chaque facette
  )

# Graphique 2
b <- ggplot(df_05, aes(x = Time, y = Value)) +
  geom_line(color = "black") +
  geom_vline(data = breakpoints_05, aes(xintercept = Time), color = "red", 
             linetype = "dashed", size = 1.2) +  
  facet_wrap(~method, scales = "free_x", strip.position = "left", ncol = 1) +  
  labs(x = NULL, y = NULL) +  # Supprime les labels des axes
  ylim(min_y, max_y) +  # Fixe les bornes de Y
  theme_void() +  # Supprime tous les éléments du thème
  theme(
    strip.text = element_blank(),  # Décale vers la gauche
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Ajoute un cadre autour de chaque facette
  )

# Combinaison des graphiques avec même hauteur et axe aligné
a + b + plot_annotation(tag_levels = 'A')

