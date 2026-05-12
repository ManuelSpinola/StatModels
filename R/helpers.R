# ============================================================
# helpers.R — Funciones y objetos compartidos entre módulos
# StatModels — Modelos estadísticos avanzados
# Paleta: Tableau Color Blind (coherente con StatSuite)
# ============================================================

library(shiny)
library(shinyjs)
library(bslib)
library(bsicons)
library(tidyverse)
library(DT)
library(palmerpenguins)
library(readxl)
library(car)
# GLM
library(glmmTMB)
library(poissonreg)   # engine Poisson para tidymodels
# Ecosistema tidymodels (para validación cruzada)
library(tidymodels)   # carga rsample, recipes, parsnip, workflows, tune, yardstick
library(purrr)
# Ecosistema easystats
library(performance)
library(parameters)
library(modelbased)
library(marginaleffects)  # requerido por modelbased::estimate_relation()
library(see)

# ── Paleta de colores (idéntica a StatSuite) ───────────────
colores <- list(
  fondo       = "#F4F7FB",
  primario    = "#1170AA",
  acento      = "#FC7D0B",
  secundario  = "#5FA2CE",
  texto       = "#57606C",
  exito       = "#5FA2CE",
  advertencia = "#F1CE63",
  peligro     = "#C85200",
  borde       = "#C8D9EC",

  tableau = c(
    "#1170AA", "#FC7D0B", "#A3ACB9", "#57606C",
    "#C85200", "#7BC8ED", "#5FA2CE", "#F1CE63",
    "#9F8B75", "#B85A0D"
  )
)

# ── Tema visual (idéntico a StatSuite) ─────────────────────
tema_app <- bs_theme(
  version      = 5,
  bg           = colores$fondo,
  fg           = colores$texto,
  primary      = colores$primario,
  secondary    = colores$secundario,
  success      = colores$exito,
  danger       = colores$peligro,
  warning      = colores$advertencia,
  base_font    = font_google("Nunito"),
  heading_font = font_google("Nunito", wght = 700),
  bootswatch   = NULL
) |>
  bs_add_rules("
  .navbar { background-color: #1170AA !important; }
  .navbar-brand { color: #ffffff !important; display: flex !important;
                  align-items: center !important;
                  padding-top: 0 !important; padding-bottom: 0 !important; }
  .navbar .nav-link { color: #ffffff !important; }
  .navbar .nav-link.active { border-bottom: 2px solid #FC7D0B; }
  .btn-primary { background-color: #FC7D0B; border-color: #FC7D0B; color: #ffffff; }
  .btn-primary:hover { background-color: #d4680a; border-color: #d4680a; }
  .card-header { background-color: #EEF3FA; font-weight: 700;
                 border-bottom: 2px solid #C8D9EC; }
  .navbar-brand { display: flex !important; align-items: center !important;
                  padding-top: 0 !important; padding-bottom: 0 !important; }

  /* Semáforo de supuestos */
  .sem-ok   { background: #f0f9f5; border-left: 4px solid #5FA2CE; }
  .sem-warn { background: #fffbf0; border-left: 4px solid #F1CE63; }
  .sem-bad  { background: #fff0f2; border-left: 4px solid #C85200; }

  /* Quiz didáctico */
  .quiz-opt { border: 1px solid #C8D9EC; border-radius: 8px;
              padding: 0.6rem 1rem; margin-bottom: 0.4rem;
              cursor: pointer; background: #ffffff;
              transition: border-color 0.15s; font-size: 0.88rem; }
  .quiz-opt:hover { border-color: #1170AA; }
  .quiz-correct   { border-color: #5FA2CE !important;
                    background: #f0f9f5 !important; }
  .quiz-wrong     { border-color: #C85200 !important;
                    background: #fff0f2 !important; }

  /* Código R */
  .codigo-bloque { background: #1e1e2e; color: #cdd6f4;
                   border-radius: 8px; padding: 1rem;
                   font-family: 'Fira Code', monospace;
                   font-size: 0.82rem; line-height: 1.7;
                   overflow-x: auto; white-space: pre; }
")

# ── Escalas ggplot2 (Tableau Color Blind) ─────────────────
scale_fill_tableau_cb <- function(...) {
  scale_fill_manual(values = colores$tableau, ...)
}
scale_color_tableau_cb <- function(...) {
  scale_color_manual(values = colores$tableau, ...)
}

# ── Encabezado estándar de scripts R ──────────────────────
# Usada por todos los módulos de StatSuite que generan código R.
encabezado_script <- function(app, modulo) {
  paste0(
    "# ============================================\n",
    "# ", app, " \u00b7 StatSuite\n",
    "# M\u00f3dulo: ", modulo, "\n",
    "# Generado: ", format(Sys.Date(), "%Y-%m-%d"), "\n",
    "# Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 UNA \u00b7 Costa Rica\n",
    "# ============================================\n\n"
  )
}
