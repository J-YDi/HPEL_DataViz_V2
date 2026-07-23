# HPEL Data Viz — version Shinylive

Application Shiny de visualisation des séries temporelles et de l'effort
d'échantillonnage des données HPEL (phytoplancton, zooplancton, hydrologie),
**convertie pour tourner sans serveur** grâce à
[Shinylive](https://posit-dev.github.io/r-shinylive/).

Avec Shinylive, R est compilé en WebAssembly (webR) et **s'exécute
entièrement dans le navigateur du visiteur**. Il n'y a plus de serveur : la RAM
utilisée est celle de la machine du visiteur (limite ~4 Go par onglet), et non
le 1 Go de shinyapps.io qui bloquait l'application. Le site est purement
statique et hébergé gratuitement sur GitHub Pages.

## Ce qui change par rapport à la version d'origine

- Les données CSV lues par l'app sont converties en **`.rds` compressés**
  (données déjà parsées) → chargement beaucoup plus rapide et léger dans le
  navigateur, plus de parsing CSV côté client.
- Le fond de carte `worldHires` est **pré-calculé en `.rds`**, ce qui supprime
  la dépendance aux packages `maps`/`mapdata` (code C, pas toujours disponibles
  sous webR).
- L'app ne charge plus `readr`, `maps`, `mapdata` : uniquement des packages
  disponibles sous webR.

## Structure

```
.
├── app_shiny/                     # L'application (dossier exporté par Shinylive)
│   ├── app.R                      #   lit les .rds via readRDS()
│   ├── www/logo.png
│   ├── data/
│   │   ├── worldmap_hires_france.rds   (généré)
│   │   └── Additional_data/*.rds        (généré)
│   └── output/data_modif/*.rds          (généré)
├── data_source/                   # CSV bruts (source ; non versionnés par défaut)
│   ├── output/data_modif/*.csv
│   └── data/Additional_data/*.csv
├── scripts/
│   └── convert_csv_to_rds.R       # CSV -> RDS (+ fond de carte)
└── .github/workflows/deploy.yml   # Build Shinylive + publication GitHub Pages
```

## Mise en route

### 1. Générer les données `.rds`

Depuis la racine du dépôt, avec R installé localement (packages `readr`,
`maps`, `mapdata`, `ggplot2`) :

```bash
Rscript scripts/convert_csv_to_rds.R
```

Cela lit les CSV de `data_source/` et écrit les `.rds` dans `app_shiny/`.

### 2. Tester en local (optionnel)

```r
# Version serveur classique (rapide pour vérifier) :
shiny::runApp("app_shiny")

# OU version Shinylive locale (aperçu identique au navigateur) :
# install.packages("shinylive")
shinylive::export("app_shiny", "site")
httpuv::runStaticServer("site")   # puis ouvrir l'URL affichée
```

### 3. Publier sur GitHub Pages

1. Créez le dépôt sur GitHub et poussez ce dossier.
2. Committez les `.rds` générés à l'étape 1 (ils sont légers ; les CSV de
   `data_source/` restent en local via `.gitignore`).
3. Dans **Settings → Pages**, choisissez **Source : GitHub Actions**.
4. Chaque push sur `main` reconstruit et publie le site automatiquement
   (workflow `.github/workflows/deploy.yml`). L'URL apparaît dans l'onglet
   **Actions** et dans **Settings → Pages**.

## Bon à savoir

- **Tout est public** côté navigateur (code + données `.rds`). N'y mettez rien
  de confidentiel.
- **Premier chargement** un peu long : le navigateur télécharge R (webR) et les
  packages. Ensuite tout est en cache.
- Si un package venait à manquer sous webR au build, le message d'erreur de
  `shinylive::export` l'indiquera ; on adaptera alors le code.

Contact : jean-yves.dias@sorbonne-universite.fr
