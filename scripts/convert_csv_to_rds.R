#_______________________________________________________________________________
# Title   : convert_csv_to_rds.R
# Object  : Convertir les CSV lus par l'application Shiny en fichiers .rds
#           compresses, pour un deploiement Shinylive (R dans le navigateur).
#
#           Le .rds stocke le data.frame DEJA parse (types, delimiteur et lignes
#           d'en-tete ignorees compris), exactement comme read_delim() le
#           produisait. L'app n'a donc plus qu'a faire readRDS() : plus de
#           parsing CSV cote navigateur -> chargement bien plus rapide et plus
#           leger.
#
# Usage   : depuis la racine du depot :
#             Rscript scripts/convert_csv_to_rds.R
#
#           Lit les CSV depuis  data_source/
#           Ecrit les RDS dans  app_shiny/   (le dossier exporte par Shinylive)
#_______________________________________________________________________________

suppressPackageStartupMessages({
  library(readr)
})

# ── Parametrage des chemins ────────────────────────────────────────────────----
SRC_DIR <- "data_source"   # ou se trouvent les CSV bruts
APP_DIR <- "app_shiny"     # racine de l'app (= dossier exporte par Shinylive)
COMPRESS <- "xz"           # "xz" = plus petit ; "gzip" = plus rapide a ecrire

# ── Table des fichiers a convertir ─────────────────────────────────────────----
# Chaque entree reproduit EXACTEMENT l'appel read_delim() de app.R (meme
# delimiteur, memes lignes ignorees) pour garantir des types de colonnes
# identiques. rel = chemin relatif commun aux deux arborescences.
jobs <- list(
  list(rel = "output/data_modif/PHYTOBS_DOME_PP_FR.csv", delim = ",", skip = 0),
  list(rel = "output/data_modif/SOMLIT_DOME_PP_FR.csv",  delim = ",", skip = 0),
  list(rel = "output/data_modif/PNMI_DOME_PP_FR.csv",    delim = ",", skip = 0),
  list(rel = "output/data_modif/ROSCOFF_DOME_PP_FR.csv", delim = ",", skip = 0),
  # REPHY_DOME_PP_FR.csv est absent du depot (gitignore) : convertie si presente
  list(rel = "output/data_modif/REPHY_DOME_PP_FR.csv",   delim = ",", skip = 0),
  list(rel = "output/data_modif/PNMI_DOME_ZP_FR.csv",    delim = ";", skip = 0),
  list(rel = "output/data_modif/REPHY_OCEAN.csv",        delim = ",", skip = 32),
  list(rel = "output/data_modif/SOMLIT_OCEAN.csv",       delim = ",", skip = 18),
  list(rel = "data/Additional_data/BODC_QUADRIGE_DIAS_JY.csv",     delim = ";", skip = 0),
  list(rel = "data/Additional_data/Taxonomy_correspondance_rank.csv", delim = ";", skip = 0)
)

# ── Conversion ─────────────────────────────────────────────────────────────----
convert_one <- function(job) {
  in_path  <- file.path(SRC_DIR, job$rel)
  out_path <- file.path(APP_DIR, sub("\\.csv$", ".rds", job$rel))

  if (!file.exists(in_path)) {
    message(sprintf("  [SKIP]    %s (introuvable)", job$rel))
    return(invisible(NULL))
  }

  df <- read_delim(
    in_path,
    delim         = job$delim,
    escape_double = FALSE,
    trim_ws       = TRUE,
    skip          = job$skip,
    show_col_types = FALSE
  )

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, out_path, compress = COMPRESS)

  csv_mb <- file.size(in_path)  / 1024^2
  rds_mb <- file.size(out_path) / 1024^2
  message(sprintf("  [OK]      %-45s  %6.1f Mo CSV -> %6.2f Mo RDS  (%d lignes)",
                  basename(job$rel), csv_mb, rds_mb, nrow(df)))
  invisible(rds_mb)
}

# ── Fond de carte : pre-calcul pour supprimer la dependance maps/mapdata ────----
# map_data("worldHires") n'est pas garanti d'etre disponible sous webR (packages
# C). On genere donc le fond de carte une fois ici (recadre autour de la France
# elargie) et l'app se contente d'un readRDS(). Necessite maps + mapdata EN LOCAL.
convert_worldmap <- function() {
  out_path <- file.path(APP_DIR, "data", "worldmap_hires_france.rds")
  ok <- requireNamespace("maps", quietly = TRUE) &&
        requireNamespace("mapdata", quietly = TRUE) &&
        requireNamespace("ggplot2", quietly = TRUE)
  if (!ok) {
    message("  [SKIP]    worldmap_hires_france.rds : installez maps + mapdata + ggplot2")
    return(invisible(NULL))
  }

  wm <- ggplot2::map_data("worldHires")

  # Garder tout groupe (polygone) ayant au moins un point dans la fenetre
  # elargie -> polygones intacts, aucun artefact de decoupe.
  box <- list(xmin = -12, xmax = 12, ymin = 40, ymax = 53)
  in_box <- wm$long >= box$xmin & wm$long <= box$xmax &
            wm$lat  >= box$ymin & wm$lat  <= box$ymax
  keep_groups <- unique(wm$group[in_box])
  wm <- wm[wm$group %in% keep_groups, ]

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(wm, out_path, compress = COMPRESS)
  message(sprintf("  [OK]      worldmap_hires_france.rds  ->  %6.2f Mo RDS  (%d points)",
                  file.size(out_path) / 1024^2, nrow(wm)))
  invisible(NULL)
}

# ── Execution ──────────────────────────────────────────────────────────────----
message("Conversion CSV -> RDS")
message("  source : ", normalizePath(SRC_DIR, mustWork = FALSE))
message("  cible  : ", normalizePath(APP_DIR, mustWork = FALSE))
message("")

invisible(lapply(jobs, convert_one))
convert_worldmap()

message("")
message("Termine. Les .rds sont dans ", APP_DIR, "/ (pret pour Shinylive).")
