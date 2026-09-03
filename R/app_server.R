#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024^2) # 30 MB

  mod_lm_server("lm")
  mod_glm_server("glm")
  mod_gam_server("gam")
  mod_lmm_server("lmm")
  mod_glmm_server("glmm")
  mod_acerca_de_server("acerca_de")

  session$onSessionEnded(function() {})
}
