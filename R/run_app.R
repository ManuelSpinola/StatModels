#' Run the StatModels Shiny Application
#'
#' @description Launches the StatModels interactive statistical modeling
#'   platform in your default web browser.
#'
#' @param ... Arguments passed to \code{\link[golem]{with_golem_options}}.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#'   StatModels::run_app()
#' }
#'
#' @export
run_app <- function(...) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui     = app_ui(),
      server = app_server
    ),
    golem_opts = list(...)
  )
}
