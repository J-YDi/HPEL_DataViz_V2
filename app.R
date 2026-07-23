#_______________________________________________________________________________
# Title   : Shinyapp.R — Interface Shiny - HPEL DATA 
# Authors : Jean-Yves Dias
# Date    : 2026
# Object  : Visualisation des series temporelles et effort d'echantillonnage
#           des donnees HPEL
# Version : R 4.5.0
#_______________________________________________________________________________

# Packages ─────────────────────────────────────────────────────────────────----

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(lubridate)
library(DT)

# Les donnees sont pre-parsees en .rds (voir scripts/convert_csv_to_rds.R).
# On lit donc avec readRDS() : plus de parsing CSV cote navigateur (Shinylive),
# et plus besoin des packages readr / maps / mapdata sous webR.

# Constantes ───────────────────────────────────────────────────────────────----

FRANCE_XLIM <- c(-5.5, 10.0)
FRANCE_YLIM <- c(40.8, 51.8)

DATASET_COLORS <- c(
  "PHYTOBS" = "#1f77b4",
  "REPHY"   = "#e8700a",
  "SOMLIT"  = "#2ca02c",
  "PNMI"    = "#d62728",
  "SBR"     = "#9467bd"
)

MOIS_LETTRES <- setNames(LETTERS[1:12], 1:12)

# Chargement codes BODC parametres hydro
BODC_DATA <- tryCatch(
  readRDS("data/Additional_data/BODC_QUADRIGE_DIAS_JY.rds"),
  error = function(e) NULL
)


# Palette taxonomie
TAXONOMY_COLORS <- c(
  "Superdomain" = "gray0", "Kingdom" = "blue","Subkingdom" = "maroon3", "Phylum" = "darkorange",
  "Subphylum" = "springgreen3", "Infraphylum" = "cyan", "Gigaclass" = "green", "Class" = "red",
  "Subclass" = "mediumpurple", "Superorder" = "dodgerblue", "Order" = "magenta","Infraorder" = "pink",
  "Suborder" = "lightsteelblue", "Family" = "gold", "Subfamily" = "brown",
  "Genus" = "violetred1", "Subgenus" = "tan", "Species" = "navy",
  "Forma" = "grey", "Variety" = "orange"
)

# ── Inutile en vFinal──────────────────────────────────────────────────────────

assign_region <- function(df) {
  df %>% mutate(REGION = case_when(
    LATIT >= 48.5                  ~ "Manche",
    LATIT <= 44   & LONGI >= 2     ~ "Méditerranée",
    LATIT <= 48.5 & LONGI <= 0     ~ "Atlantique",
    TRUE                           ~ "Autre"
  ))
}

# ── Chargement donnees Phytoplancton ──────────────────────────────────────────

load_phyto <- function(base_path = "output/data_modif") {

  read_safe <- function(path) {
    if (!file.exists(path)) return(NULL)
    readRDS(path)
  }

  PHYTOBS <- read_safe(file.path(base_path, "PHYTOBS_DOME_PP_FR.rds"))
  if (!is.null(PHYTOBS)) {
    PHYTOBS <- PHYTOBS %>% # Besoin de renommer les stations...
      mutate(STATN = case_when(
        STATN == "Marseille SOFCOM Frioul"  ~ "Frioul",
        STATN == "SOMLIT-WX-C"             ~ "Point C",
        STATN == "goulbrest"               ~ "Portzic",
        STATN == "SOMLIT-Astan"            ~ "Astan",
        STATN == "SOMLIT-Antioche"         ~ "Antioche",
        STATN == "SOMLIT - Bouee13"        ~ "Bouee 13",
        STATN == "Rade de Brest - Lanveoc" ~ "Lanveoc",
        STATN == "SOLA"                    ~ "Sola",
        TRUE                               ~ STATN
      ), DATA = "PHYTOBS")
  }

  REPHY   <- read_safe(file.path(base_path, "REPHY_DOME_PP_FR.rds"))
  if (!is.null(REPHY))   { REPHY$DATA <- "REPHY"; REPHY$SMPNO <- as.character(REPHY$SMPNO) }

  SOMLIT  <- read_safe(file.path(base_path, "SOMLIT_DOME_PP_FR.rds"))
  if (!is.null(SOMLIT))  { SOMLIT$DATA <- "SOMLIT"; SOMLIT$QFLAG <- NA } # Colonne manquante

  PNMI_PP    <- read_safe(file.path(base_path, "PNMI_DOME_PP_FR.rds"))
  if (!is.null(PNMI_PP))    PNMI_PP$DATA    <- "PNMI"

  ROSCOFF <- read_safe(file.path(base_path, "ROSCOFF_DOME_PP_FR.rds"))
  if (!is.null(ROSCOFF)) ROSCOFF$DATA <- "SBR"

  dfs <- Filter(Negate(is.null), list(PHYTOBS, REPHY, SOMLIT, PNMI_PP, ROSCOFF))
  if (length(dfs) == 0) return(NULL)

  # Harmoniser les types avant bind_rows
  dfs <- lapply(dfs, function(df) mutate(df, across(everything(), as.character)))
  # Combiner les donnees
  bind_rows(dfs) %>%
    select(STATN, LATIT, LONGI, DATA, SDATE, MXDEP, CRUIS, SPECI, VALUE, RESP_RESULTAT) %>%
    distinct() %>%
    mutate(
      LONGI = round(as.numeric(LONGI), 3),
      LATIT = round(as.numeric(LATIT), 3),
      VALUE = as.numeric(VALUE),
      MXDEP = as.numeric(MXDEP),
      CRUIS = as.numeric(CRUIS),
      SDATE = as.character(SDATE),
      RESP_RESULTAT = as.character(RESP_RESULTAT)
    ) %>%
    filter(MXDEP <= 5) %>% #On garde uniquement les donnees de surface
    group_by(STATN) %>%
    mutate(
      first_longi = first(LONGI),
      first_latit = first(LATIT)
    ) %>%
    ungroup() %>%
    assign_region()
}

# ── Chargement donnees Zooplancton ────────────────────────────────────────────

load_zoo <- function(base_path = "output/data_modif") {
  path <- file.path(base_path, "PNMI_DOME_ZP_FR.rds")
  if (!file.exists(path)) return(NULL)

  readRDS(path) %>%
    mutate(DATA = "PNMI") %>%
    select(STATN, LATIT, LONGI, DATA, SDATE, MXDEP, CRUIS, SPECI, VALUE, RESP_RESULTAT) %>%
    distinct() %>%
    mutate(
      LONGI = round(as.numeric(LONGI), 3),
      LATIT = round(as.numeric(LATIT), 3),
      VALUE = as.numeric(VALUE),
      MXDEP = as.numeric(MXDEP),
      CRUIS = as.numeric(CRUIS),
      SDATE = as.character(SDATE),
      RESP_RESULTAT = as.character(RESP_RESULTAT)
    ) %>%
    filter(MXDEP <= 5) %>% #On garde que les donnees de surface
    group_by(STATN) %>%
    mutate(
      first_longi = first(LONGI),
      first_latit = first(LATIT)
    ) %>%
    ungroup() %>%
    assign_region()
}

# ── Chargement donnees Hydrologie ─────────────────────────────────────────────

load_hydro <- function(base_path = "output/data_modif") {

  # Les lignes d'en-tete (skip = 32 / 18) sont deja retirees a la conversion RDS.
  read_safe <- function(path) {
    if (!file.exists(path)) return(NULL)
    readRDS(path)
  }

  # Charger les donnees hydrologie
  REPHY <- read_safe(file.path(base_path, "REPHY_OCEAN.rds"))
  if (!is.null(REPHY)) {
    REPHY <- REPHY %>%
      mutate(DATA = "REPHY", Station = sub("_.*", "", Station)) #Recuperer les noms de stations corrects
  }

  SOMLIT <- read_safe(file.path(base_path, "SOMLIT_OCEAN.rds"))
  if (!is.null(SOMLIT)) {
    SOMLIT <- SOMLIT %>%
      mutate(DATA = "SOMLIT", Station = sub("_.*", "", Station)) #Idem
  }

  dfs <- Filter(Negate(is.null), list(REPHY, SOMLIT))
  if (length(dfs) == 0) return(NULL)

  # Extraire les coordonnees avant de coercer en character
  coords <- bind_rows(dfs) %>%
    select(Station, `Latitude [degrees_north]`, `Longitude [degrees_east]`) %>%
    distinct() %>%
    rename(STATN = Station, LATIT = `Latitude [degrees_north]`, LONGI = `Longitude [degrees_east]`) %>% #Harmoniser avec DOME
    mutate(
      LATIT = as.numeric(LATIT),
      LONGI = as.numeric(LONGI)
    )

  # Charger la table de correspondance des parametres
  param_file <- "data/Additional_data/BODC_QUADRIGE_DIAS_JY.rds"
  if (!file.exists(param_file)) {
    Param <- data.frame(METH = character(), PARAM = character(), stringsAsFactors = FALSE)
  } else {
    Param <- readRDS(param_file)
  }

  # Coercer tous les types a character avant bind_rows pour eviter les conflits
  dfs <- lapply(dfs, function(df) mutate(df, across(everything(), as.character)))

  # Preparer chaque dataframe apres bind_rows
  prepare_hydro <- function(df, param_table) {
    # Identifier les colonnes infos echantillons
    meta_cols <- c("Station", "DATA", "yyyy-mm-ddThh:mm:ss.sss", "Cruise", "Depth [m]")
    
    # Garder seulement les infos echantillons
    keep_cols <- intersect(colnames(df), meta_cols)
    
    # Identifier les colonnes de parametres
    param_cols <- setdiff(colnames(df), keep_cols)
    
    # Selectionner et faire le pivot
    df <- df %>%
      select(all_of(keep_cols), all_of(param_cols)) %>%
      pivot_longer(
        cols = -all_of(keep_cols),
        names_to = "METH",
        values_to = "VALUE"
      ) %>%
      filter(!is.na(VALUE), VALUE != "")
    
    # Joindre avec la table de parametres
    if (nrow(param_table) > 0 && "METH" %in% colnames(param_table)) {
      df <- left_join(df, param_table, by = "METH")
    } else {
      df <- df %>% mutate(PARAM = METH)
    }
    
    df
  }

  # Fusionner les donnees
  HYDRO <- bind_rows(dfs) %>%
    prepare_hydro(Param)

  if (nrow(HYDRO) == 0) return(NULL)

  HYDRO <- HYDRO %>%
    mutate(
      VALUE = as.numeric(VALUE),
      CRUIS = as.numeric(Cruise), #On converti comme DOME
      MXDEP = as.numeric(`Depth [m]`) #On converti comme DOME
    ) %>%
    filter(!is.na(MXDEP), MXDEP <= 5) %>% #On garde uniquement la surface aussi
    select(STATN = Station, DATA, SDATE = `yyyy-mm-ddThh:mm:ss.sss`,
           METH,PARAM, MXDEP, CRUIS, VALUE) %>%
    distinct() %>%
    # Joindre avec les coordonnees
    left_join(coords, by = "STATN") %>%
    filter(!is.na(LATIT), !is.na(LONGI)) %>%
    mutate(
      LATIT = round(as.numeric(LATIT), 3),
      LONGI = round(as.numeric(LONGI), 3),
      first_longi = LONGI,
      first_latit = LATIT
    ) %>%
    group_by(STATN) %>%
    mutate(first_longi = first(LONGI), first_latit = first(LATIT)) %>%
    ungroup() %>%
    assign_region()

  HYDRO
}
# Faire correspondre les donnees au choix dans l'app
TYPE_CONFIG <- list(
  "Phytoplancton" = list(loader = load_phyto, datasets = c("PHYTOBS","REPHY","SOMLIT","PNMI","SBR")),
  "Zooplancton"   = list(loader = load_zoo,   datasets = c("PNMI")),
  "Hydrologie"    = list(loader = load_hydro, datasets = c("REPHY","SOMLIT"))
)

# Fond de carte ─────────────────────---────────────────────────────────────────

# Fond de carte pre-calcule (worldHires recadre France) -> pas de maps/mapdata
# necessaires sous webR. Genere par scripts/convert_csv_to_rds.R.
WORLDMAP <- readRDS("data/worldmap_hires_france.rds")

# Fonction faire la carte ──────────────────────────────────────────────────────

make_map <- function(data, sel_datasets, xlim, ylim, show_labels) {

  # Filtrer par dataset
  if (!is.null(sel_datasets) && length(sel_datasets) > 0) {
    sel_datasets <- sel_datasets[sel_datasets != ""]
    data <- data %>% filter(DATA %in% sel_datasets)
  } else {
    # Si aucun dataset selectionne, retourner une carte vide
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data or wait...",
                               size = 5, color = "gray50") + theme_void())
  }
  # Carte
  p <- ggplot() +
    geom_polygon(
      data  = WORLDMAP,
      aes(x = long, y = lat, group = group),
      fill  = "grey78", color = "grey30", linewidth = 0.2
    ) +
    coord_fixed(
      xlim  = xlim,
      ylim  = ylim,
      ratio = 1.4,
      expand = FALSE
    ) +
    labs(x = "Longitude (°E)", y = "Latitude (°N)", colour = "Jeu de données") +
    theme_gdocs() +
    theme(
      panel.background  = element_rect(fill = "#cce5f6"),
      panel.grid.major  = element_line(color = "grey60", linewidth = 0.15),
      panel.border      = element_rect(color = "grey40", fill = NA, linewidth = 0.4),
      legend.background = element_rect(fill = "white"),
      legend.key        = element_rect(fill = "white"),
      legend.text       = element_text(size = 13),
      legend.title      = element_text(size = 13, face = "bold"),
      legend.position   = "bottom",
      axis.text         = element_text(size = 11),
      axis.title        = element_text(size = 12)
    ) +
    guides(colour = guide_legend(override.aes = list(size = 6), direction = "horizontal"))

  sea_labels <- data.frame(
    x     = c(-3.2, -2.5, 5.5),
    y     = c(45.5, 50.3, 42.4),
    label = c("Océan Atlantique", "Manche", "Mer Méditerranée"),
    angle = c(-45, 20, 8)
  ) %>% filter(x >= xlim[1], x <= xlim[2], y >= ylim[1], y <= ylim[2])

  if (nrow(sea_labels) > 0) {
    p <- p + geom_text(
      data = sea_labels,
      aes(x = x, y = y, label = label, angle = angle),
      color = "steelblue4", fontface = "italic", size = 4.5,
      inherit.aes = FALSE
    )
  }

  if (is.null(data) || nrow(data) == 0) return(p)

  data_visible <- data[data$first_longi >= xlim[1] & data$first_longi <= xlim[2] &
                       data$first_latit >= ylim[1] & data$first_latit <= ylim[2], ]

  if (nrow(data_visible) == 0) return(p)

  p <- p + geom_point(
    data  = data_visible,
    aes(x = first_longi, y = first_latit, colour = DATA),
    size  = 3.5, alpha = 0.85
  )

  if (show_labels) {
    p <- p + geom_text_repel(
      data          = data_visible %>% distinct(STATN, DATA, first_longi, first_latit),
      aes(x = first_longi, y = first_latit, label = STATN, colour = DATA),
      size          = 3.2,
      max.overlaps  = Inf,
      box.padding   = 0.4,
      point.padding = 0.3,
      force         = 1.2,
      segment.color = "grey50",
      segment.size  = 0.3,
      show.legend   = FALSE
    )
  }

  p
}

# Fonction faire Heatmap echantillonnage plancton ───────────────────────────────

make_sampling_plancton <- function(data, sel_datasets, sel_stations, xlim, ylim, year_range) {

  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data",
                               size = 6, color = "gray50") + theme_void())
  }

  # Si aucun dataset selectionne, retourner vide
  if (is.null(sel_datasets) || length(sel_datasets) == 0 || all(sel_datasets == "")) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucun jeu de donnees selectionne ou patience...",
                               size = 5, color = "gray50") + theme_void())
  }

  # Filtrer par jeux de donnees - supprimer les vides
  sel_datasets <- sel_datasets[sel_datasets != ""]
  data <- data %>% filter(DATA %in% sel_datasets)

  # Filtrer coordonnees GPS en lien avec la carte
  data <- data %>%
    filter(first_longi >= xlim[1], first_longi <= xlim[2],
           first_latit >= ylim[1], first_latit <= ylim[2])

  # Filtrer stations
  if (!is.null(sel_stations) && length(sel_stations) > 0) {
    data <- data %>% filter(STATN %in% sel_stations)
  }

  # Filtrer annees
  if (!is.null(year_range) && length(year_range) == 2) {
    data <- data %>% filter(CRUIS >= year_range[1], CRUIS <= year_range[2])
  }

  if (nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucune donnee pour cette selection",
                               size = 5, color = "gray50") + theme_void())
  }

  # Preparer les dates
  data <- data %>%
    mutate(
      SDATE = as.Date(as.character(SDATE), format = "%Y%m%d"),
      month = month(SDATE),
      year = CRUIS
    ) %>%
    filter(!is.na(year), !is.na(month))

  if (nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Pas de dates valides",
                               size = 5, color = "gray50") + theme_void())
  }

  start_year <- min(data$year, na.rm = TRUE)
  end_year <- max(data$year, na.rm = TRUE)

  # Stations dans l'ordre par latitude
  station_order <- data %>%
    distinct(STATN, first_latit) %>%
    arrange(desc(first_latit)) %>%
    pull(STATN)

  data$STATN <- factor(data$STATN, levels = station_order)

  # Agregation : effort d'echantillonnage par station, annee, mois
  # Compter le nombre de dates distinctes
  fq <- data %>%
    group_by(STATN, year, month) %>%
    summarise(n = n_distinct(SDATE), .groups = "drop") %>%
    complete(STATN, year = start_year:end_year, month = 1:12, fill = list(n = 0))

  fq <- fq %>%
    mutate(
      lettres = MOIS_LETTRES[as.character(month)],
      sampling = paste(year, lettres, sep = "-"),
      sampling_effort = as.integer(n)
    )

  # Reperes annuels
  vertical <- paste0(start_year:end_year, "-L")
  year_half <- paste0(start_year:end_year, "-F")
  year_labels <- data.frame(sampling = year_half, year = start_year:end_year)

  y_max <- length(unique(fq$STATN)) + 0.5

  # Creer palette dynamique basee sur le max des donnees
  max_effort <- max(fq$sampling_effort, na.rm = TRUE)
  
  # Palette fixe pour 0-10
  base_colors <- c(
    "0" = "grey90", "1" = "springgreen", "2" = "palegreen4", "3" = "maroon2",
    "4" = "purple", "5" = "darkgoldenrod1", "6" = "chocolate", "7" = "cornflowerblue",
    "8" = "dodgerblue", "9" = "red", "10" = "darkred"
  )
  
  # Si max > 10, ajouter des couleurs supplementaires via gradient
  if (max_effort > 10) {
    extra_colors <- colorRampPalette(c("darkred", "black"))(max_effort - 10)
    for (i in 11:max_effort) {
      base_colors[as.character(i)] <- extra_colors[i - 10]
    }
  }
  
  # Convertir sampling_effort en factor pour que scale_fill_manual fonctionne
  fq <- fq %>% mutate(sampling_effort = factor(sampling_effort, levels = as.character(0:max_effort)))

  # Graphique
  p <- ggplot(fq, aes(x = sampling, y = STATN, fill = sampling_effort)) +
    geom_tile(alpha = 0.60) +
    geom_vline(xintercept = vertical, colour = "grey40", linetype = "dashed", linewidth = 1) +
    geom_text(data = year_labels,
              aes(x = sampling, y = y_max, label = year, fontface = "bold"),
              inherit.aes = FALSE, colour = "black", size = 3) +
    scale_x_discrete(labels = rep(1:12, length(start_year:end_year))) +
    theme_classic(base_size = 12) +
    theme(
      axis.text.x = element_text(size = 5),
      axis.title = element_blank(),
      legend.position = "top"
    ) +
    guides(fill = guide_legend(nrow = 1))

  p
}


# Fonction faire Heatmap echantillonnage Hydrologie ─────────────────────────

make_sampling_hydro <- function(data, sel_datasets, sel_stations, sel_params, xlim, ylim, year_range) {

  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Pas de données",
                               size = 6, color = "gray50") + theme_void())
  }

  # Filtrer par dataset
  if (!is.null(sel_datasets) && length(sel_datasets) > 0) {
    data <- data %>% filter(DATA %in% sel_datasets)
  }
  
  # Filtrer par coordonnees GPS lien avec carte
  data <- data %>%
    filter(first_longi >= xlim[1], first_longi <= xlim[2],
           first_latit >= ylim[1], first_latit <= ylim[2])

  # Filtrer par station
  if (!is.null(sel_stations) && length(sel_stations) > 0) {
    data <- data %>% filter(STATN %in% sel_stations)
  }

  # Filtrer par parametres
  if (!is.null(sel_params) && length(sel_params) > 0) {
    data <- data %>% filter(PARAM %in% sel_params)
  }

  # Filtrer par annee
  if (!is.null(year_range) && length(year_range) == 2) {
    data <- data %>% filter(CRUIS >= year_range[1], CRUIS <= year_range[2])
  }

  if (nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucune donnee pour cette selection",
                               size = 5, color = "gray50") + theme_void())
  }

  # Preparer les dates
  data <- data %>%
    mutate(
      SDATE = parse_date_time(SDATE, orders = c("ymd_HMS", "ymd")),
      month = month(SDATE),
      year = year(SDATE)
    ) %>%
    filter(!is.na(year), !is.na(month))

  if (nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Pas de dates valides",
                               size = 5, color = "gray50") + theme_void())
  }

  start_year <- year_range[1]
  end_year <- year_range[2]

  # Agregation : effort d'echantillonnage par station, parametre, annee, mois
  fq <- data %>%
    group_by(STATN, PARAM, year, month) %>%
    summarise(n = n(), .groups = "drop") %>%
    complete(STATN, PARAM, year = start_year:end_year, month = 1:12, fill = list(n = 0))

  fq <- fq %>%
    mutate(
      lettres = MOIS_LETTRES[as.character(month)],
      sampling = paste(year, lettres, sep = "-"),
      sampling_effort = n
    )

  # Reperes annuels
  vertical <- paste0(start_year:end_year, "-L")
  year_half <- paste0(start_year:end_year, "-F")
  year_labels <- data.frame(sampling = year_half, year = start_year:end_year)

  # Graphique avec facets par station et parametre
  p <- ggplot(fq, aes(x = sampling, y = PARAM, fill = factor(sampling_effort))) +
    geom_tile(alpha = 0.60) +
    scale_fill_manual(
      values = c("0" = "grey90", "1" = "springgreen", "2" = "palegreen4", "3" = "maroon2",
                 "4" = "purple", "5" = "darkgoldenrod1", "6" = "chocolate", "7" = "cornflowerblue",
                 "8" = "dodgerblue", "9" = "red", "10" = "darkred"),
      name = "Échantillons",
      na.value = "white"
    ) +
    geom_vline(xintercept = vertical, colour = "grey40", linetype = "dashed", linewidth = 1) +
    geom_text(data = year_labels,
              aes(x = sampling, y = Inf, label = year, fontface = "bold", vjust = 2),
              inherit.aes = FALSE, colour = "black", size = 2.5) +
    scale_x_discrete(labels = rep(1:12, length(start_year:end_year))) +
    facet_wrap(~STATN, ncol = 1, scales = "fixed") +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(size = 4),
      axis.title = element_blank(),
      legend.position = "top",
      strip.text = element_text(size = 10, face = "bold")
    ) +
    guides(fill = guide_legend(nrow = 1))

  p
}

# Fonction pour niveau Taxonomique Plancton ────────────────────────────────

make_niv_taxo <- function(data, sel_datasets, sel_stations, xlim, ylim, year_range) {

  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Pas de données",
                               size = 6, color = "gray50") + theme_void())
  }

  # Si aucun dataset selectionne, retourner vide
  if (is.null(sel_datasets) || length(sel_datasets) == 0 || all(sel_datasets == "")) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucun jeu de données sélectionné",
                               size = 5, color = "gray50") + theme_void())
  }

  # Filtrer par jeux de donnees - supprimer les vides
  sel_datasets <- sel_datasets[sel_datasets != ""]
  
  data <- data %>% filter(DATA %in% sel_datasets)

  # Filtrer coordonnees GPS en lien avec la carte
  data <- data %>%
    filter(first_longi >= xlim[1], first_longi <= xlim[2],
           first_latit >= ylim[1], first_latit <= ylim[2])

  # Filtrer stations
  if (!is.null(sel_stations) && length(sel_stations) > 0) {
    data <- data %>% filter(STATN %in% sel_stations)
  }

  # Filtrer annees
  if (!is.null(year_range) && length(year_range) == 2) {
    data <- data %>% filter(CRUIS >= year_range[1], CRUIS <= year_range[2])
  }

  if (nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucune donnée",
                               size = 5, color = "gray50") + theme_void())
  }

  # Graphique 
  p <- ggplot(data, aes(x = date, y = PROP, fill = NIVEAU_TAX)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~STATN, ncol = 1) +
    scale_fill_manual(values = TAXONOMY_COLORS, na.value = "white") +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom",
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7, face = "bold"),
      strip.text = element_text(size = 10, face = "bold")
    ) +
    geom_text(aes(x = date, y = 1.05, label = NB_DATES), size = 2.2, inherit.aes = FALSE) +
    scale_x_date(breaks = "month", date_labels = "%Y-%m") +
    labs(y = "Proportion", x = "Month", fill = "Rang taxonomique")

  p
}

# Fonction pour viz Richesse Specifique Plancton ────────────────────────────

make_richesse_plancton <- function(data, sel_datasets, sel_stations, xlim, ylim, year_range) {

  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Pas de données",
                               size = 6, color = "gray50") + theme_void())
  }

  # Si aucun dataset selectionne, retourner vide
  if (is.null(sel_datasets) || length(sel_datasets) == 0 || all(sel_datasets == "")) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucun jeu de données sélectionné",
                               size = 5, color = "gray50") + theme_void())
  }

  # Filtrer par jeux de donnees - supprimer les vides
  sel_datasets <- sel_datasets[sel_datasets != ""]

  # Filtrer par jeux de donnees
  data <- data %>% filter(DATA %in% sel_datasets)

  # Filtrer coordonnees GPS
  data <- data %>%
    filter(first_longi >= xlim[1], first_longi <= xlim[2],
           first_latit >= ylim[1], first_latit <= ylim[2])

  # Filtrer stations
  if (!is.null(sel_stations) && length(sel_stations) > 0) {
    data <- data %>% filter(STATN %in% sel_stations)
  }

  # Filtrer annees
  if (!is.null(year_range) && length(year_range) == 2) {
    data <- data %>% filter(CRUIS >= year_range[1], CRUIS <= year_range[2])
  }

  if (nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucune donnee",
                               size = 5, color = "gray50") + theme_void())
  }
  
  # Verif format de la date
  data <- data %>%
    mutate(SDATE = as.Date(as.character(SDATE), format = "%Y%m%d"))

  # Moyenne annuelle
  subdata_y <- data %>%
    mutate(Year = year(SDATE)) %>%
    group_by(STATN, Year) %>%
    summarise(Rspe = mean(Rspe, na.rm = TRUE), .groups = "drop") %>%
    mutate(SDATE = as.Date(paste0(Year, "-06-15")))

  # Graphique
  p <- ggplot(data) +
    geom_point(aes(y = Rspe, x = as.Date(SDATE), color = RESP_RESULTAT), size = 2) +
    geom_line(aes(y = Rspe, x = as.Date(SDATE), color = RESP_RESULTAT), size = 0.8, alpha = 0.6) +
    geom_line(data = subdata_y, aes(y = Rspe, x = as.Date(SDATE)), size = 1, linetype = "dashed", alpha = 0.5, color = "gray50") +
    geom_point(data = subdata_y, aes(y = Rspe, x = as.Date(SDATE)), size = 2.5, shape = 24, alpha = 0.5, color = "darkred") +
    scale_x_date(breaks = "3 months", date_labels = "%Y-%m") +
    scale_color_manual(
      values = c(
        "Ana Maria Hapette" = "#e41a1c",
        "Beatriz Beker" = "#377eb8",
        "Cécile Klein" = "#4daf4a",
        "Gaspard Delebecq" = "#984ea3",
        "NA" = "black"
      ),
      na.value = "black",
      name = "RESP_RESULTAT"
    ) +
    labs(y = "Nombre taxons identifies", x = "") +
    facet_wrap(~STATN, ncol = 1, scales = "free_y") +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 7, angle = 90),
      legend.position = "bottom",
      strip.text = element_text(size = 10, face = "bold")
    )

  p
}

# Fonction pour composition taxonomique plancton ────────────────────────────

make_taxonomy_plancton <- function(data, sel_datasets, sel_stations, xlim, ylim, year_range) {

  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Pas de données",
                               size = 6, color = "gray50") + theme_void())
  }

  # Filtrer par jeux de donnees
  if (!is.null(sel_datasets) && length(sel_datasets) > 0) {
    data <- data %>% filter(DATA %in% sel_datasets)
  }

  # Filtrer coordonnees GPS, lien avec la carte
  data <- data %>%
    filter(first_longi >= xlim[1], first_longi <= xlim[2],
           first_latit >= ylim[1], first_latit <= ylim[2])

  # Filtrer stations
  if (!is.null(sel_stations) && length(sel_stations) > 0) {
    data <- data %>% filter(STATN %in% sel_stations)
  }

  # Filtrer annees
  if (!is.null(year_range) && length(year_range) == 2) {
    data <- data %>% filter(CRUIS >= year_range[1], CRUIS <= year_range[2])
  }

  if (nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucune donnée",
                               size = 5, color = "gray50") + theme_void())
  }

  # Ordonner les niveaux taxonomiques
  tax_levels <- c("Superdomain", "Kingdom","Subkingdom","Phylum", "Subphylum", "Infraphylum","Gigaclass",
                  "Class", "Subclass", "Superorder", "Order", "Suborder","Infraorder",
                  "Family", "Subfamily", "Genus", "Subgenus", "Species", "Forma", "Variety")
  data$NIVEAU_TAX <- factor(data$NIVEAU_TAX,
                            levels = intersect(tax_levels, unique(data$NIVEAU_TAX)))

  # Graphique
  p <- ggplot(data, aes(x = date, y = PROP, fill = NIVEAU_TAX)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~STATN, ncol = 1) +
    scale_fill_manual(values = TAXONOMY_COLORS, na.value = "white") +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 5, angle = 90),
      legend.position = "bottom",
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7, face = "bold"),
      strip.text = element_text(size = 10, face = "bold")
    ) +
    labs(y = "% Identification taxo", x = "", fill = "Rang taxonomique") +
    guides(fill = guide_legend(nrow = 2))

  p
}
# Interface
ui <- page_sidebar(
  title = tags$div(
    tags$img(src = "logo.png", height = "40px", style = "vertical-align: middle; margin-right: 10px;"),
    "Données HPEL - Visualisation/effort d'échantillonnage",
    style = "display: flex; align-items: center;"
  ),
  # Theme Bootstrap 5 standard, SANS police Google : les presets bootswatch
  # (ex. "lux") embarquent une police telechargee via curl au demarrage, ce qui
  # echoue sous webR/Shinylive. On garde donc un theme sans dependance reseau.
  theme = bs_theme(version = 5),

  sidebar = sidebar(
    width = 320,

    # Selection type de donnees
    selectInput("type_data", tags$b("Type de données"),
                choices = names(TYPE_CONFIG), selected = "Phytoplancton"),

    hr(),

    # Filtres carte
    h6("Coordonnees", style = "font-weight: bold; margin-bottom: 10px; color: #333;"),

    tags$b("Longitude (°E)"),
    fluidRow(
      column(6, numericInput("xmin", "Min", value = -5.5, min = -10, max = 15, step = 0.5)),
      column(6, numericInput("xmax", "Max", value = 10.0, min = -10, max = 15, step = 0.5))
    ),

    tags$b("Latitude (°N)"),
    fluidRow(
      column(6, numericInput("ymin", "Min", value = 40.8, min = 35, max = 55, step = 0.5)),
      column(6, numericInput("ymax", "Max", value = 51.8, min = 35, max = 55, step = 0.5))
    ),

    actionButton("reset_view", "Reinitialiser coordonnees",
                 icon = icon("arrows-rotate"),
                 class = "btn-outline-secondary btn-sm mt-1 w-100"),

    checkboxInput("show_labels", "Afficher noms stations (Carte)", value = TRUE),

    hr(),

    # Filtres temporels
    h6("Periode", style = "font-weight: bold; margin-bottom: 10px; color: #333;"),

    uiOutput("ui_year_filter"),

    hr(),

    # Filtres echantillonnage
    h6("Filtres", style = "font-weight: bold; margin-bottom: 10px; color: #333;"),

    uiOutput("ui_datasets_select"),
    uiOutput("ui_stations_select"),
    uiOutput("ui_params_select"),

    hr(),

    uiOutput("info_box"),
    
    hr(),
    
    tags$footer(
      tags$small(
        "© 2026 - Jean-Yves Dias, Eric Goberville, Dorothée Vincent",
        tags$br(),
        "INDIBIO Project"
      ),
      style = "text-align: center; color: #666; font-size: 10px; margin-top: 20px; padding-top: 10px;"
    )
  ),

  navset_card_tab(
    nav_panel(
      "Maps",
      card(
        full_screen = TRUE,
        card_body(padding = 0,
          plotOutput("map_plot", height = "680px"))
      )
    ),

    nav_panel(
      "Sampling plankton",
      card(
        full_screen = TRUE,
        card_body(
          plotOutput("sampling_plot", height = "680px"))
      )
    ),

    nav_panel(
      "Sampling hydrology",
      card(
        full_screen = TRUE,
        card_body(
          plotOutput("sampling_hydro_plot", height = "680px"))
      )
    ),

    nav_panel(
      "TS Hydro",
      card(
        full_screen = TRUE,
        card_body(
          plotOutput("viz_hydro_plot", height = "680px"))
      )
    ),

    nav_panel(
      "Codes Methodes",
      card(
        full_screen = TRUE,
        card_body(
          dataTableOutput("bodc_table")
        )
      )
    ),

    nav_panel(
      "TS plankton",
      card(
        full_screen = TRUE,
        card_body(
          plotOutput("viz_plancton_plot", height = "680px")
        )
      )
    ),

    nav_panel(
      "Identification taxo plankton",
      card(
        full_screen = TRUE,
        card_body(
          plotOutput("niv_taxo_plot", height = "680px")
        )
      )
    )
  )
)

# Fonction pour visualisation Hydrologie 

make_viz_hydro <- function(data, sel_datasets, sel_stations, sel_params, xlim, ylim, year_range) {

  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Pas de donnees",
                               size = 6, color = "gray50") + theme_void())
  }

  # Si aucun dataset selectionne, retourner vide
  if (is.null(sel_datasets) || length(sel_datasets) == 0 || all(sel_datasets == "")) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucun jeu de donnees selectionne",
                               size = 5, color = "gray50") + theme_void())
  }

  # Filtrer par dataset
  sel_datasets <- sel_datasets[sel_datasets != ""]
  data <- data %>% filter(DATA %in% sel_datasets)

  # Filtrer par parametres
  if (!is.null(sel_params) && length(sel_params) > 0) {
    sel_params <- sel_params[sel_params != ""]
    data <- data %>% filter(PARAM %in% sel_params)
  }

  # Filtrer region GPS
  data <- data %>%
    filter(LONGI >= xlim[1], LONGI <= xlim[2],
           LATIT >= ylim[1], LATIT <= ylim[2])

  # Filtrer stations
  if (!is.null(sel_stations) && length(sel_stations) > 0) {
    data <- data %>% filter(STATN %in% sel_stations)
  }

  # Filtrer annees
  if (!is.null(year_range) && length(year_range) == 2) {
    data <- data %>% mutate(year = as.numeric(CRUIS))
    data <- data %>% filter(year >= year_range[1], year <= year_range[2])
  }

  if (nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Aucune donnee pour cette selection",
                               size = 5, color = "gray50") + theme_void())
  }

  # Preparer les donnees
  data <- data %>%
    mutate(
      DATE = as.Date(SDATE),
      VALUE = as.numeric(VALUE)
    ) %>%
    filter(!is.na(DATE), !is.na(VALUE))

  if (nrow(data) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Pas de dates valides",
                               size = 5, color = "gray50") + theme_void())
  }

  # Graphique 
  p <- ggplot(data, aes(x = DATE, y = VALUE, colour = METH)) +
    geom_point(size = 2, alpha = 0.4) +
    geom_line(size = 1, alpha = 0.3) +
    scale_x_date(breaks = "month", date_labels = "%Y-%m") +
    labs(x = "Date", y = "Valeur", colour = "Methode") +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(size = 5, angle = 90),
      legend.position = "bottom"
    ) +
    facet_wrap(~ PARAM + STATN, scales = "free_y")

  p
}

# Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Charger les donnees brutes selon le type
  all_stations <- reactive({
    type <- req(input$type_data)
    withProgress(message = "Chargement des données...", value = 0.5, {
      TYPE_CONFIG[[type]]$loader()
    })
  })

  # Reset vue
  observeEvent(input$reset_view, {
    updateNumericInput(session, "xmin", value = FRANCE_XLIM[1])
    updateNumericInput(session, "xmax", value = FRANCE_XLIM[2])
    updateNumericInput(session, "ymin", value = FRANCE_YLIM[1])
    updateNumericInput(session, "ymax", value = FRANCE_YLIM[2])
  })

  # Limites X/Y validées
  xlim <- reactive({
    xmin <- input$xmin
    xmax <- input$xmax
    if (is.null(xmin) || is.null(xmax) || xmin >= xmax) return(FRANCE_XLIM)
    c(xmin, xmax)
  })

  ylim <- reactive({
    ymin <- input$ymin
    ymax <- input$ymax
    if (is.null(ymin) || is.null(ymax) || ymin >= ymax) return(FRANCE_YLIM)
    c(ymin, ymax)
  })

  # ── Selecteurs dynamiques
  available_datasets <- reactive({
    TYPE_CONFIG[[input$type_data]]$datasets
  })

  available_stations <- reactive({
    sta <- all_stations()
    if (is.null(sta) || nrow(sta) == 0) return(character())
    sort(unique(sta$STATN))
  })

  available_params <- reactive({
    sta <- all_stations()
    if (is.null(sta) || nrow(sta) == 0) return(character())
    if ("PARAM" %in% colnames(sta)) {
      sort(unique(sta$PARAM[!is.na(sta$PARAM)]))
    } else {
      character()
    }
  })

  available_years <- reactive({
    sta <- all_stations()
    if (is.null(sta) || nrow(sta) == 0) return(c(2000, 2025))
    years <- as.numeric(sta$CRUIS[!is.na(sta$CRUIS)])
    if (length(years) == 0) return(c(2000, 2025))
    c(min(years, na.rm = TRUE), max(years, na.rm = TRUE))
  })

  # UI : Filtre annee
  output$ui_year_filter <- renderUI({
    year_range <- available_years()
    sliderInput("year_range", "Années",
                min = year_range[1], max = year_range[2],
                value = year_range,
                sep = "", step = 1)
  })

  # UI : Jeux de donnees
  output$ui_datasets_select <- renderUI({
    choices <- available_datasets()
    checkboxGroupInput("sel_datasets", tags$b("Jeux de données"),
                       choices = choices,
                       selected = choices)
  })

  # UI : Stations avec "Tout selectionner"
  output$ui_stations_select <- renderUI({
    choices <- available_stations()
    if (length(choices) == 0) {
      return(tags$p("Aucune station disponible", style = "color: #888;"))
    }

    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
        tags$b("Stations"),
        actionButton("toggle_all_stations", "Tout (dé)cocher", class = "btn-sm btn-outline-primary",
                     style = "font-size: 11px; padding: 4px 8px;")
      ),
      checkboxGroupInput("sel_stations", NULL,
                         choices = choices,
                         selected = choices[1:max(length(choices))])
    )
  })

  # UI : Parametres (Hydrologie uniquement)
  output$ui_params_select <- renderUI({
    if (input$type_data != "Hydrologie") return(NULL)

    choices <- available_params()
    if (length(choices) == 0) {
      return(tags$p("Aucun parametre disponible", style = "color: #888;"))
    }

    tagList(
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
        tags$b("Paramètres"),
        actionButton("toggle_all_params", "Tout (dé)cocher", class = "btn-sm btn-outline-primary",
                     style = "font-size: 11px; padding: 4px 8px;")
      ),
      checkboxGroupInput("sel_params", NULL,
                         choices = choices,
                         selected = c())
    )
  })

  # Toggle stations
  observeEvent(input$toggle_all_stations, {
    all_choices <- available_stations()
    current_sel <- input$sel_stations %||% character()

    if (length(current_sel) == length(all_choices)) {
      # Tout est coche → décocher tout
      updateCheckboxGroupInput(session, "sel_stations", selected = character())
    } else {
      # Sinon → tout cocher
      updateCheckboxGroupInput(session, "sel_stations", selected = all_choices)
    }
  })

  # Calcul Rspe et rank pour Viz Plancton
  # NB : version sans pivot_wider. L'ancienne creait une matrice large (1 colonne
  # par espece, des milliers) saturant la memoire sous webR ("cannot allocate
  # vector"). On calcule ici la meme chose par regroupement, en format long.
  plancton_with_rspe <- reactive({
    data <- all_stations()
    if (is.null(data) || nrow(data) == 0) return(NULL)

    # Colonnes identifiant un echantillon (tout sauf l'espece et sa valeur)
    id_cols <- setdiff(colnames(data), c("SPECI", "VALUE"))

    # Valeur moyenne par echantillon et par espece, puis on ne garde que
    # les especes reellement presentes (valeur non nulle)
    agg <- data %>%
      group_by(across(all_of(id_cols)), SPECI) %>%
      summarise(sp_value = mean(VALUE), .groups = "drop") %>%
      filter(!is.na(sp_value), sp_value != 0)

    if (nrow(agg) == 0) return(NULL)

    # Richesse specifique = nombre d'especes non nulles par echantillon
    agg %>%
      group_by(across(all_of(id_cols))) %>%
      mutate(Rspe = n()) %>%
      ungroup() %>%
      rename(rank = SPECI)
  })

  # Toggle parametres
  observeEvent(input$toggle_all_params, {
    all_choices <- available_params()
    current_sel <- input$sel_params %||% character()

    if (length(current_sel) == length(all_choices)) {
      updateCheckboxGroupInput(session, "sel_params", selected = character())
    } else {
      updateCheckboxGroupInput(session, "sel_params", selected = all_choices)
    }
  })


  # ── Preparation donnees Niv_Taxo
  niv_taxo_data <- reactive({
    type <- req(input$type_data)
    if (type != "Phytoplancton" && type != "Zooplancton") return(NULL)

    # Charger les donnees brutes
    Phyto <- all_stations()
    if (is.null(Phyto) || nrow(Phyto) == 0) return(NULL)

    # Charger la taxonomie appropriee
    tax_file <- "data/Additional_data/Taxonomy_correspondance_rank.rds"

    Taxonomy_correspondance <- readRDS(tax_file)
    colnames(Taxonomy_correspondance)[1] <- "SPECI"
    
    # Convertir SPECI en numeric pour que ça match avec Phyto (qui est double)
    Taxonomy_correspondance$SPECI <- as.numeric(Taxonomy_correspondance$SPECI)
    Phyto$SPECI <- as.numeric(Phyto$SPECI)

    # Left join avec taxonomie
    Phyto <- left_join(Phyto, Taxonomy_correspondance, by = "SPECI")
    # Filtrer les lignes avec rank
    Phyto <- filter(Phyto, !is.na(rank), rank != "")
    
    if (nrow(Phyto) == 0) return(NULL)

    # Preparer : COUNT = 1 par observation
    subdata <- Phyto %>%
      select(STATN, SDATE, CRUIS, DATA, first_longi, first_latit, rank) %>%
      mutate(
        COUNT = 1,
        SDATE = as.Date(as.character(SDATE), format = "%Y%m%d")
      )

    # Pivot wider par rank
    subdata <- pivot_wider(subdata, names_from = rank, values_from = COUNT, values_fn = sum)

    # Pivot longer - garder DATA dans les colonnes non-pivotees
    subdata <- pivot_longer(subdata,
                           cols = -c(STATN, SDATE, CRUIS, DATA, first_longi, first_latit),
                           names_to = "NIVEAU_TAX",
                           values_to = "VALEUR") %>%
      filter(!is.na(VALEUR), VALEUR > 0)
    

    if (nrow(subdata) == 0) return(NULL)

    # Compter dates distinctes
    nb_dates <- subdata %>%
      mutate(Month = month(SDATE)) %>%
      group_by(Month, STATN, CRUIS) %>%
      summarise(NB_DATES = n_distinct(SDATE), .groups = "drop")

    # Agregation mensuelle - garder DATA, first_longi, first_latit
    subdata_month <- subdata %>%
      mutate(Month = month(SDATE)) %>%
      group_by(Month, CRUIS, NIVEAU_TAX, STATN, DATA, first_longi, first_latit) %>%
      summarise(VALEUR = mean(VALEUR, na.rm = TRUE), .groups = "drop") %>%
      group_by(Month, STATN, CRUIS) %>%
      mutate(PROP = VALEUR / sum(VALEUR, na.rm = TRUE)) %>%
      ungroup() %>%
      left_join(nb_dates, by = c("Month", "STATN", "CRUIS"))
    

    # Ordonner les niveaux taxonomiques
    tax_levels <- c("Superdomain", "Kingdom","Subkingdom", "Phylum", "Subphylum", "Infraphylum","Gigaclass",
                    "Class", "Subclass", "Superorder", "Order", "Suborder","Infraorder",
                    "Family", "Subfamily", "Genus", "Subgenus", "Species", "Forma", "Variety")
    subdata_month$NIVEAU_TAX <- factor(subdata_month$NIVEAU_TAX,
                                        levels = intersect(tax_levels, unique(subdata_month$NIVEAU_TAX)))

    # Creer la date
    subdata_month <- subdata_month %>%
      mutate(date = as.Date(paste(CRUIS, Month, "01", sep = "-")))


    subdata_month
  })


    # Carte
  output$map_plot <- renderPlot({
    make_map(
      data = all_stations(),
      sel_datasets = input$sel_datasets,
      xlim = xlim(),
      ylim = ylim(),
      show_labels = isTRUE(input$show_labels)
    )
  }, res = 110)

  # Echantillonnage Plancton
  output$sampling_plot <- renderPlot({
    data <- all_stations()
    sel_datasets <- input$sel_datasets
    sel_stations <- input$sel_stations %||% available_stations()
    year_rng <- input$year_range %||% available_years()

    make_sampling_plancton(data, sel_datasets, sel_stations, xlim(), ylim(), year_rng)
  }, res = 110)

  # ──Echantillonnage Hydrologie
  output$sampling_hydro_plot <- renderPlot({
    data <- all_stations()
    sel_datasets <- input$sel_datasets
    sel_stations <- input$sel_stations %||% available_stations()
    sel_params <- input$sel_params
    year_rng <- input$year_range %||% available_years()

    make_sampling_hydro(data, sel_datasets, sel_stations, sel_params, xlim(), ylim(), year_rng)
  }, res = 110)

  # Visualisation Hydrologie
  output$viz_hydro_plot <- renderPlot({
    data <- all_stations()
    sel_datasets <- input$sel_datasets
    sel_stations <- input$sel_stations %||% available_stations()
    sel_params <- input$sel_params
    year_rng <- input$year_range %||% available_years()

    make_viz_hydro(data, sel_datasets, sel_stations, sel_params, xlim(), ylim(), year_rng)
  }, res = 110)

  # Visualisation Plancton (Richesse + Taxonomie)
  output$viz_plancton_plot <- renderPlot({
    data <- plancton_with_rspe()  # Donnees avec Rspe calculee
    sel_datasets <- input$sel_datasets
    sel_stations <- input$sel_stations %||% available_stations()
    year_rng <- input$year_range %||% available_years()

    make_richesse_plancton(data, sel_datasets, sel_stations, xlim(), ylim(), year_rng)
  }, res = 110)

  # Niveau Taxonomique
  output$niv_taxo_plot <- renderPlot({
    data <- niv_taxo_data()
    sel_datasets <- input$sel_datasets
    sel_stations <- input$sel_stations %||% available_stations()
    year_rng <- input$year_range %||% available_years()

    make_niv_taxo(data, sel_datasets, sel_stations, xlim(), ylim(), year_rng)
  }, res = 110)

  # Table BODC (Codes Methodes)
  output$bodc_table <- renderDataTable({
    if (is.null(BODC_DATA)) {
      return(data.frame(Message = "Fichier BODC non trouve"))
    }
    BODC_DATA
  }, options = list(
    pageLength = 25,
    searchHighlight = TRUE,
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
  ))

  # ── Info box
  output$info_box <- renderUI({
    sta <- all_stations()
    xl  <- xlim()
    yl  <- ylim()

    if (!is.null(sta) && nrow(sta) > 0) {
      sta_visible <- sta %>%
        filter(first_longi >= xl[1], first_longi <= xl[2],
               first_latit >= yl[1], first_latit <= yl[2])
    } else {
      sta_visible <- data.frame()
    }

    n_total   <- if (is.null(sta)) 0L else length(unique(sta$STATN))
    n_visible <- length(unique(sta_visible$STATN))

    tagList(
      tags$small(
        tags$b("Stations totales : "),
        tags$span(n_total, style = "color:#2c86c8;font-weight:bold;font-size:1.1em")
      ),
      tags$br(),
      tags$small(
        tags$b("Visibles dans la fenetre : "),
        tags$span(n_visible, style = "color:#27ae60;font-weight:bold;font-size:1.1em")
      )
    )
  })
}

shinyApp(ui = ui, server = server)
