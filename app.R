# ============================================================
# app.R — Punto de entrada de StatModels
#
# Este archivo SOLO:
#   1. Carga librerías y helpers compartidos
#   2. Carga los módulos
#   3. Define ui y server ensamblando los módulos
#
# La lógica de cada módulo vive en modules/mod_*.R
# Las funciones y estilos compartidos viven en R/helpers.R
#
# StatSuite:
#   StatDesign  — Diseño de estudios y muestreo
#   StatFlow    — Primeros análisis y visualización
#   StatGeo     — Análisis espacial y mapas
#   StatMonitor — Monitoreo poblacional
#   StatModels  — Modelos estadísticos          ← esta app
# ============================================================

# ── 1. Librerías y helpers ─────────────────────────────────
source("R/helpers.R")

# ── 2. Módulos ─────────────────────────────────────────────
source("modules/mod_lm.R")
source("modules/mod_glm.R")
# source("modules/mod_gam.R")      # próximamente
# source("modules/mod_lmm.R")      # próximamente
source("modules/mod_acerca_de.R")

# ── Helper interno: pantalla "próximamente" ────────────────
# Reutilizable para todos los módulos pendientes.
proximamente_ui <- function(icono, titulo, subtitulo, datasets) {
  div(
    class = "py-5 px-3",
    style = "max-width: 620px; margin: 0 auto;",
    div(
      class = "text-center mb-4",
      bs_icon(icono, size = "3em",
              style = paste0("color:", colores$secundario)),
      h4(
        class = "mt-3 mb-2",
        style = paste0("color:", colores$primario, "; font-weight:700;"),
        titulo
      ),
      p(class = "text-muted", subtitulo)
    ),
    div(
      class = "alert mb-3",
      style = paste0(
        "background:", colores$fondo, ";",
        "border-left: 4px solid ", colores$acento, ";"
      ),
      bs_icon("hourglass-split", class = "me-2",
              style = paste0("color:", colores$acento)),
      strong("En desarrollo."),
      " Este m\u00f3dulo estar\u00e1 disponible en una pr\u00f3xima versi\u00f3n de StatModels."
    ),
    card(
      card_header(
        bs_icon("database", class = "me-1"),
        "Datasets previstos"
      ),
      card_body(
        p(class = "small text-muted mb-0",
          bs_icon("circle-fill", size = "0.5em", class = "me-1"),
          datasets)
      )
    )
  )
}

# ── 3. UI ──────────────────────────────────────────────────
ui <- page_navbar(
  header = shinyjs::useShinyjs(),
  title = div(
    style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
    img(src = "hexsticker_StatModels.png", height = "38px"),
    span("StatModels", style = "font-weight: 600;")
  ),
  theme  = tema_app,
  lang   = "es",
  footer = div(
    class = "text-center text-muted small py-2",
    style = paste0("border-top: 1px solid ", colores$borde, ";"),
    "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
  ),

  # ── Módulo activo ─────────────────────────────────────────
  nav_panel(
    title = "Modelo lineal general (LM)",
    icon  = bs_icon("graph-up"),
    mod_lm_ui("lm")
  ),

  # ── Próximamente ──────────────────────────────────────────
  nav_panel(
    title = "Modelo lineal generalizado (GLM)",
    icon  = bs_icon("toggles"),
    mod_glm_ui("glm")
  ),

  nav_panel(
    title = "Modelo aditivo generalizado (GAM)",
    icon  = bs_icon("bezier2"),
    proximamente_ui(
      icono     = "bezier2",
      titulo    = "Modelo aditivo generalizado (GAM)",
      subtitulo = paste0(
        "Reemplaza los efectos lineales por funciones suaves no param\u00e9tricas. ",
        "Ideal cuando la relaci\u00f3n entre X e Y no es lineal. ",
        "Paquete: mgcv."
      ),
      datasets  = "palmerpenguins \u00b7 mgcv::gamSim()"
    )
  ),

  nav_panel(
    title = "Modelos mixtos (LMM / GLMM)",
    icon  = bs_icon("diagram-3"),
    proximamente_ui(
      icono     = "diagram-3",
      titulo    = "Modelos mixtos (LMM / GLMM)",
      subtitulo = paste0(
        "Modelan datos con estructura jer\u00e1rquica o medidas repetidas. ",
        "Combinan efectos fijos (poblacionales) y efectos aleatorios ",
        "(individuos, sitios, grupos). Paquete: lme4."
      ),
      datasets  = "lme4::sleepstudy \u00b7 lme4::cbpp"
    )
  ),

  nav_spacer(),

  nav_panel(
    title = "Acerca de",
    icon  = bs_icon("info-circle"),
    mod_acerca_de_ui("acerca_de")
  ),

  nav_item(
    tags$span(class = "text-white-50 small", "StatModels v1.0")
  )
)

# ── 4. Server ──────────────────────────────────────────────
server <- function(input, output, session) {
  mod_lm_server("lm")
  mod_glm_server("glm")
  mod_acerca_de_server("acerca_de")

  # Handler para actualizar estilos de tarjetas de familia GLM
  session$onSessionEnded(function() {})
}

# ── 5. Lanzar ──────────────────────────────────────────────
shinyApp(ui, server)
