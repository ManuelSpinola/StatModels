# ============================================================
# mod_acerca_de.R — Información sobre StatModels
# StatModels · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

mod_acerca_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "py-4 px-3",
      style = "max-width: 780px; margin: 0 auto;",

      h4(
        bs_icon("info-circle", class = "me-2"),
        "Acerca de StatModels",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(class = "text-muted mb-4",
        "StatModels es el módulo de modelado estadístico de StatSuite, ",
        "desarrollado en el ICOMVIS de la Universidad Nacional, Costa Rica. ",
        "Inspirado en la filosofía de JASP y jamovi: accesible, didáctico, ",
        "y con código R reproducible para quienes quieran profundizar."
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(bs_icon("collection", class = "me-1"),
                      "StatSuite — Ecosistema completo"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("StatDesign"), " — Diseño de estudios y muestreo"),
              tags$li(strong("StatFlow"),   " — Primeros análisis y visualización"),
              tags$li(strong("StatGeo"),    " — Análisis espacial y mapas"),
              tags$li(strong("StatMonitor")," — Monitoreo poblacional"),
              tags$li(strong("StatModels"), " — Modelos estadísticos ← aquí")
            )
          )
        ),

        card(
          card_header(bs_icon("box-seam", class = "me-1"),
                      "Ecosistema R utilizado"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("tidymodels"), " — flujo de trabajo unificado"),
              tags$li(strong("parameters"),  " — tabla de coeficientes"),
              tags$li(strong("performance"), " — diagnóstico y bondad de ajuste"),
              tags$li(strong("effectsize"),  " — tamaños del efecto"),
              tags$li(strong("see"),         " — visualización easystats"),
              tags$li(strong("palmerpenguins"), " — datos reales para LM")
            )
          )
        )
      ),

      div(
        class = "alert alert-info small mt-3",
        bs_icon("envelope", class = "me-1"),
        "Contacto: ",
        tags$a(href = "mailto:manuel.spinola@una.ac.cr",
               "manuel.spinola@una.ac.cr")
      )
    )
  )
}

mod_acerca_de_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # sin lógica reactiva por ahora
  })
}
