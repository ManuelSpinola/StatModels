#' Application UI
#'
#' @return A Shiny UI object.
#' @noRd
app_ui <- function() {

  # ── Recursos estáticos ──────────────────────────────────
  golem::add_resource_path(
    "www",
    system.file("app/www", package = "StatModels")
  )

  # ── UI principal ─────────────────────────────────────────
  page_navbar(
    header = shinyjs::useShinyjs(),
    title  = div(
      style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
      img(src = "www/hexsticker_StatModels.png", height = "38px"),
      span("StatModels", style = "font-weight: 600;")
    ),
    theme  = tema_app,
    lang   = "es",
    footer = div(
      class = "text-center text-muted small py-2",
      style = paste0("border-top: 1px solid ", colores$borde, ";"),
      "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
    ),

    # ── Módulos activos ───────────────────────────────────
    nav_panel(
      title = "Modelo lineal general (LM)",
      icon  = bs_icon("graph-up"),
      mod_lm_ui("lm")
    ),

    nav_panel(
      title = "Modelo lineal generalizado (GLM)",
      icon  = bs_icon("toggles"),
      mod_glm_ui("glm")
    ),

    # ── Próximamente ──────────────────────────────────────
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
}
