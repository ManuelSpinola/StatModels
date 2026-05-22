#' Application UI
#'
#' @return A Shiny UI object.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import shinyjs
#' @noRd
app_ui <- function() {

  golem::add_resource_path(
    "www",
    system.file("app/www", package = "StatModels")
  )

  bslib::page_navbar(
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
    bslib::nav_panel(
      title = "Modelo lineal general (LM)",
      icon  = bsicons::bs_icon("graph-up"),
      mod_lm_ui("lm")
    ),

    bslib::nav_panel(
      title = "Modelo lineal generalizado (GLM)",
      icon  = bsicons::bs_icon("toggles"),
      mod_glm_ui("glm")
    ),

    bslib::nav_panel(
      title = "Modelo aditivo generalizado (GAM)",
      icon  = bsicons::bs_icon("bezier2"),
      mod_gam_ui("gam")
    ),

    bslib::nav_panel(
      title = "Modelo lineal mixto (LMM)",
      icon  = bsicons::bs_icon("diagram-3"),
      mod_lmm_ui("lmm")
    ),

    bslib::nav_panel(
      title = "Modelo lineal generalizado mixto (GLMM)",
      icon  = bsicons::bs_icon("diagram-3-fill"),
      mod_glmm_ui("glmm")
    ),

    bslib::nav_spacer(),

    bslib::nav_panel(
      title = "Acerca de",
      icon  = bsicons::bs_icon("info-circle"),
      mod_acerca_de_ui("acerca_de")
    ),

    bslib::nav_item(
      tags$span(class = "text-white-50 small", "StatModels v1.0")
    )
  )
}
