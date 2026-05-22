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
        "StatModels es el m\u00f3dulo de modelado estad\u00edstico de StatSuite, ",
        "desarrollado en el ICOMVIS de la Universidad Nacional, Costa Rica. ",
        "Surge de m\u00e1s de 20 a\u00f1os de ense\u00f1anza de estad\u00edstica y ciencia de datos, ",
        "y de la posibilidad de materializar ese conocimiento en aplicaciones ",
        "interactivas accesibles para estudiantes e investigadores."
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(bs_icon("collection", class = "me-1"),
                      "StatSuite \u2014 Ecosistema completo"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("StatDesign"), " \u2014 Dise\u00f1o de estudios y muestreo"),
              tags$li(strong("StatFlow"),   " \u2014 Primeros an\u00e1lisis y visualizaci\u00f3n"),
              tags$li(strong("StatGeo"),    " \u2014 An\u00e1lisis espacial y mapas"),
              tags$li(strong("StatMonitor")," \u2014 Monitoreo poblacional"),
              tags$li(strong("StatModels"), " \u2014 Modelos estad\u00edsticos \u2190 aqu\u00ed")
            )
          )
        ),

        card(
          card_header(bs_icon("box-seam", class = "me-1"),
                      "Ecosistema R utilizado"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("lme4"), " + ", strong("lmerTest"),
                      " \u2014 modelos lineales mixtos (LMM)"),
              tags$li(strong("glmmTMB"),
                      " \u2014 modelos generalizados mixtos (GLMM)"),
              tags$li(strong("mgcv"),
                      " \u2014 modelos aditivos generalizados (GAM)"),
              tags$li(strong("parameters"),  " \u2014 tabla de coeficientes"),
              tags$li(strong("performance"), " \u2014 diagn\u00f3stico y bondad de ajuste"),
              tags$li(strong("modelbased"),  " \u2014 efectos marginales y contrastes"),
              tags$li(strong("tidymodels"),  " \u2014 flujo de trabajo unificado")
            )
          )
        )
      ),

      # Desarrollo
      card(
        class = "mt-3",
        card_header(bs_icon("code-slash", class = "me-1"),
                    "Desarrollo"),
        card_body(
          p(class = "small mb-2",
            bs_icon("person-fill", class = "me-1"),
            strong("Autor:"), " Manuel Sp\u00ednola \u2014 ICOMVIS, ",
            "Universidad Nacional, Costa Rica."),
          p(class = "small mb-2",
            bs_icon("robot", class = "me-1"),
            strong("Asistencia en desarrollo:"), " StatModels fue desarrollado ",
            "con asistencia de ", strong("Claude (Anthropic)"),
            " para la estructura de m\u00f3dulos, interfaz de usuario, ",
            "l\u00f3gica del servidor."),
          p(class = "small mb-0",
            bs_icon("building", class = "me-1"),
            strong("Instituci\u00f3n:"), " Instituto Internacional en ",
            "Conservaci\u00f3n y Manejo de Vida Silvestre (ICOMVIS), ",
            "Universidad Nacional de Costa Rica.")
        )
      ),

      div(
        class = "alert alert-info small mt-3 mb-0",
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
    # sin lógica reactiva
  })
}
