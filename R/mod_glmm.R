# ============================================================
# mod_glmm.R — Modelo Lineal Generalizado Mixto (GLMM)
# StatModels · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Familias: Poisson, Binomial negativa, Binomial
# Motor:    glmmTMB
# Ecosistema: easystats (parameters, performance, modelbased)
# Datos:    ranas_glmm.rda (simulado) / aves_glmm.rda (simulado)
#
# Filosofía: didáctico, sin conocimiento previo de programación
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_glmm_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("diagram-3-fill", class = "me-2"),
        "Modelo Lineal Generalizado Mixto (GLMM)",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Combina la flexibilidad del GLM (familias no gaussianas) con la ",
        "estructura jer\u00e1rquica del LMM (efectos aleatorios). ",
        "Ideal para ", strong("conteos, proporciones o presencia/ausencia"),
        " con datos agrupados. Motor: ", strong("glmmTMB"),
        " \u00b7 inferencia con ", strong("parameters"),
        " \u00b7 diagn\u00f3stico con ", strong("performance"), "."
      )
    ),

    navset_card_tab(

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "\u00bfQu\u00e9 es?"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Del GLM al GLMM"),
          p(class = "small text-muted mb-3",
            "El GLM maneja respuestas no gaussianas (conteos, proporciones) ",
            "pero asume que todas las observaciones son independientes. ",
            "El GLMM agrega ", strong("efectos aleatorios"),
            " para manejar datos agrupados o con medidas repetidas, ",
            "exactamente como el LMM pero con familias de distribuci\u00f3n no normales."),

          layout_columns(
            col_widths = c(6, 6),
            div(
              class = "card-muestreo mb-3",
              style = paste0("border-left: 4px solid ", colores$primario, ";"),
              p(class = "small fw-bold mb-1", "GLM"),
              tags$ul(class = "small mb-0",
                tags$li("Respuesta no gaussiana \u2714"),
                tags$li("Observaciones independientes \u2714"),
                tags$li("Sin estructura jer\u00e1rquica \u2718")
              )
            ),
            div(
              class = "card-muestreo mb-3",
              style = paste0("border-left: 4px solid ", colores$acento, ";"),
              p(class = "small fw-bold mb-1", "GLMM"),
              tags$ul(class = "small mb-0",
                tags$li("Respuesta no gaussiana \u2714"),
                tags$li("Observaciones agrupadas \u2714"),
                tags$li("Efectos aleatorios por grupo \u2714")
              )
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Familias disponibles en glmmTMB"),

          div(
            style = "overflow-x: auto;",
            tags$table(
              class = "table table-sm table-bordered small mb-3",
              style = "background: #ffffff;",
              tags$thead(
                tags$tr(
                  tags$th(style = paste0("background:", colores$primario,
                                         " !important; color:#fff !important;"),
                          "Familia"),
                  tags$th(style = paste0("background:", colores$primario,
                                         " !important; color:#fff !important;"),
                          "Variable Y"),
                  tags$th(style = paste0("background:", colores$primario,
                                         " !important; color:#fff !important;"),
                          "Enlace"),
                  tags$th(style = paste0("background:", colores$primario,
                                         " !important; color:#fff !important;"),
                          "Ejemplo ecol\u00f3gico")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(tags$span(class = "badge",
                                    style = paste0("background:", colores$primario),
                                    "Poisson")),
                  tags$td("Conteos (0, 1, 2\u2026) sin sobredispersi\u00f3n"),
                  tags$td(code("log")),
                  tags$td("Conteo de aves en puntos de conteo dentro de fragmentos")
                ),
                tags$tr(style = paste0("background:", colores$fondo),
                  tags$td(tags$span(class = "badge",
                                    style = paste0("background:", colores$acento),
                                    "Binomial negativa")),
                  tags$td("Conteos sobredispersos"),
                  tags$td(code("log")),
                  tags$td("Abundancia de aves con sobredispersi\u00f3n (aves_glmm)")
                ),
                tags$tr(
                  tags$td(tags$span(class = "badge",
                                    style = paste0("background:", colores$secundario),
                                    "Binomial")),
                  tags$td("Presencia/ausencia (0/1)"),
                  tags$td(code("logit")),
                  tags$td("Presencia/ausencia de rana en charca (ranas_glmm)")
                ),
                tags$tr(style = paste0("background:", colores$fondo),
                  tags$td(tags$span(class = "badge",
                                    style = paste0("background:", colores$peligro),
                                    "ZIP")),
                  tags$td("Conteos con exceso de ceros"),
                  tags$td(code("log")),
                  tags$td("Conteos con exceso de ceros — datos propios")
                ),
                tags$tr(
                  tags$td(tags$span(class = "badge",
                                    style = "background:#6B3FA0",
                                    "ZINB")),
                  tags$td("Conteos sobredispersos + exceso de ceros"),
                  tags$td(code("log")),
                  tags$td("Conteos sobredispersos + exceso de ceros — datos propios")
                )
              )
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "El offset: estandarizar el esfuerzo de muestreo"),
          p(class = "small mb-2",
            "Cuando el esfuerzo de muestreo var\u00eda entre observaciones ",
            "(distinto tiempo, \u00e1rea o n\u00famero de trampas), los conteos no son ",
            "comparables directamente. El offset corrige esto:"),
          div(class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("info-circle", class = "me-1"),
              "Al agregar ", code("offset(log(esfuerzo))"),
              " el modelo estima tasas (conteo por unidad de esfuerzo) ",
              "en lugar de conteos crudos. Por ejemplo, si unas parcelas ",
              "son m\u00e1s grandes o las visitas m\u00e1s largas, el offset ",
              "estandariza para comparar en la misma escala.")
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 2: Fundamentos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("journal-bookmark", class = "me-1"),
                        "Fundamentos"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Supuestos del GLMM"),
          p(class = "small text-muted mb-3",
            "El GLMM hereda los supuestos del GLM y agrega los del LMM. ",
            "Los m\u00e1s importantes en la pr\u00e1ctica ecol\u00f3gica:"),

          # Supuesto 1: Familia correcta
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$primario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("toggles",
                        style = paste0("color:", colores$primario, "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$primario, "; font-weight:700;"),
                   "1. Familia de distribuci\u00f3n correcta")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("\u00bfQu\u00e9 significa?")),
                p(class = "small text-muted mb-0",
                  "La distribuci\u00f3n elegida debe corresponder al tipo de Y. ",
                  "Poisson para conteos con varianza \u2248 media, ",
                  "binomial negativa si hay sobredispersi\u00f3n, ",
                  "ZIP o ZINB si hay exceso de ceros.")
              ),
              div(
                p(class = "small mb-1", strong("\u00bfC\u00f3mo verificarlo?")),
                p(class = "small text-muted mb-1",
                  code("check_overdispersion()"), " y ",
                  code("check_zeroinflation()"), " de performance."),
                div(class = "alert alert-warning small py-1 px-2 mb-0",
                    bs_icon("exclamation-triangle", class = "me-1"),
                    strong("Si falla:"), " cambia la familia en Ajustar modelo.")
              )
            )
          ),

          # Supuesto 2: Normalidad efectos aleatorios
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$secundario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("diagram-3",
                        style = paste0("color:", colores$secundario, "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$secundario, "; font-weight:700;"),
                   "2. Normalidad de los efectos aleatorios")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("\u00bfQu\u00e9 significa?")),
                p(class = "small text-muted mb-0",
                  "Los efectos aleatorios (BLUPs) deben distribuirse normalmente ",
                  "alrededor de cero. Con pocos grupos (\u22655-6) desviaciones leves ",
                  "son esperables.")
              ),
              div(
                p(class = "small mb-1", strong("\u00bfC\u00f3mo verificarlo?")),
                p(class = "small text-muted mb-1",
                  "QQ-plot de los BLUPs en la pesta\u00f1a Diagn\u00f3stico. ",
                  "Los puntos deben seguir la diagonal."),
                div(class = "alert alert-warning small py-1 px-2 mb-0",
                    bs_icon("exclamation-triangle", class = "me-1"),
                    strong("Si falla:"), " revisa si hay outliers por grupo.")
              )
            )
          ),

          # Supuesto 3: Sobredispersión
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$acento, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("arrows-expand",
                        style = paste0("color:", colores$acento, "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$acento, "; font-weight:700;"),
                   "3. Sobredispersi\u00f3n (solo Poisson)")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("\u00bfQu\u00e9 significa?")),
                p(class = "small text-muted mb-0",
                  "En Poisson la varianza debe ser igual a la media. ",
                  "Si la varianza real es mucho mayor, los errores est\u00e1ndar ",
                  "son demasiado peque\u00f1os y hay falsos positivos.")
              ),
              div(
                p(class = "small mb-1", strong("\u00bfC\u00f3mo verificarlo?")),
                p(class = "small text-muted mb-1",
                  code("check_overdispersion()"), " de performance. ",
                  "Estad\u00edstico > 1.5 indica sobredispersi\u00f3n."),
                div(class = "alert alert-warning small py-1 px-2 mb-0",
                    bs_icon("exclamation-triangle", class = "me-1"),
                    strong("Si falla:"), " cambia a binomial negativa (NB).")
              )
            )
          ),

          # Supuesto 4: Inflación de ceros
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$peligro, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("0-circle",
                        style = paste0("color:", colores$peligro, "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$peligro, "; font-weight:700;"),
                   "4. Inflaci\u00f3n de ceros")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("\u00bfQu\u00e9 significa?")),
                p(class = "small text-muted mb-0",
                  "Hay dos mecanismos que generan ceros: ",
                  "(1) el proceso de conteo (Poisson/NB) y ",
                  "(2) un proceso separado que produce ceros adicionales ",
                  "(ej. sitio inapropiado donde la especie nunca ocurre). ",
                  "ZIP y ZINB modelan ambos procesos simult\u00e1neamente.")
              ),
              div(
                p(class = "small mb-1", strong("\u00bfC\u00f3mo verificarlo?")),
                p(class = "small text-muted mb-1",
                  code("check_zeroinflation()"), " de performance compara ",
                  "ceros observados vs. predichos."),
                div(class = "alert alert-warning small py-1 px-2 mb-0",
                    bs_icon("exclamation-triangle", class = "me-1"),
                    strong("Si falla:"), " usa ZIP (Poisson) o ZINB (NB).")
              )
            )
          ),

          # Supuesto 5: Singular fit
          div(
            class = "card-muestreo mb-0",
            style = paste0("border-left: 4px solid ", colores$texto, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("exclamation-triangle",
                        style = paste0("color:", colores$texto, "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$texto, "; font-weight:700;"),
                   "5. Singular fit")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("\u00bfQu\u00e9 significa?")),
                p(class = "small text-muted mb-0",
                  "El modelo intenta estimar m\u00e1s par\u00e1metros de los que ",
                  "los datos soportan. La varianza de alg\u00fan efecto aleatorio ",
                  "se estima en 0 o la correlaci\u00f3n entre efectos es exactamente \u00b11.")
              ),
              div(
                p(class = "small mb-1", strong("\u00bfC\u00f3mo verificarlo?")),
                p(class = "small text-muted mb-1",
                  "Se detecta autom\u00e1ticamente al ajustar el modelo."),
                div(class = "alert alert-warning small py-1 px-2 mb-0",
                    bs_icon("exclamation-triangle", class = "me-1"),
                    strong("Soluci\u00f3n:"), " simplificar la estructura aleatoria ",
                    "(ej. eliminar la pendiente aleatoria).")
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 3: Los datos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("table", class = "me-1"), "Los datos"),
        card_body(

          navset_pill(

            # Sub-tab 1: Cargar datos
            nav_panel(
              title = tagList(bs_icon("database", class = "me-1"),
                              "Cargar datos"),
              br(),
              layout_columns(
                col_widths = c(4, 8),
                fill = FALSE,

                card(
                  card_header(bs_icon("database", class = "me-1"),
                              "Fuente de datos"),
                  card_body(
                    p(class = "small fw-bold text-muted mb-2",
                      bs_icon("toggles", class = "me-1"),
                      "\u00bfQu\u00e9 tipo de variable respuesta tienes?"),

                    # Tarjetas de familia
                    tags$div(
                      style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:8px; margin-bottom:12px;",

                      # Poisson
                      tags$div(
                        id = ns("card_poisson"),
                        style = "background:#E1F5EE; border:2px solid #0F6E56; border-radius:10px; padding:10px 12px; cursor:pointer;",
                        onclick = paste0("Shiny.setInputValue('", ns("familia_datos"), "', 'poisson', {priority:'event'})"),
                        tags$div(style = "font-size:18px; color:#0F6E56;", bs_icon("bar-chart-steps")),
                        tags$p(style = "font-size:13px; font-weight:500;", "Poisson"),
                        tags$p(style = "font-size:11px; color:#555; margin:0;", "Conteos sin sobredispersi\u00f3n"),
                        tags$span(style = "font-size:10px; background:#0F6E56; color:#fff; padding:1px 6px; border-radius:4px;", "log")
                      ),

                      # Binomial negativa
                      tags$div(
                        id = ns("card_nbinom2"),
                        style = "background:var(--color-background-secondary); border:1.5px solid var(--color-border-tertiary); border-radius:10px; padding:10px 12px; cursor:pointer;",
                        onclick = paste0("Shiny.setInputValue('", ns("familia_datos"), "', 'nbinom2', {priority:'event'})"),
                        tags$div(style = "font-size:18px; color:#853F0B;", bs_icon("graph-up")),
                        tags$p(style = "font-size:13px; font-weight:500;", "Binomial negativa"),
                        tags$p(style = "font-size:11px; color:#555; margin:0;", "Conteos sobredispersos"),
                        tags$span(style = "font-size:10px; background:#853F0B; color:#fff; padding:1px 6px; border-radius:4px;", "log")
                      ),

                      # Binomial
                      tags$div(
                        id = ns("card_binomial"),
                        style = "background:var(--color-background-secondary); border:1.5px solid var(--color-border-tertiary); border-radius:10px; padding:10px 12px; cursor:pointer;",
                        onclick = paste0("Shiny.setInputValue('", ns("familia_datos"), "', 'binomial', {priority:'event'})"),
                        tags$div(style = "font-size:18px; color:#185FA5;", bs_icon("toggles")),
                        tags$p(style = "font-size:13px; font-weight:500;", "Binomial"),
                        tags$p(style = "font-size:11px; color:#555; margin:0;", "Presencia/ausencia (0/1)"),
                        tags$span(style = "font-size:10px; background:#185FA5; color:#fff; padding:1px 6px; border-radius:4px;", "logit")
                      )
                    ),

                    shinyjs::hidden(
                      textInput(ns("familia_datos"), label = NULL, value = "poisson")
                    ),

                    tags$hr(),
                    uiOutput(ns("sel_fuente_datos")),
                    conditionalPanel(
                      condition = paste0("input['", ns("fuente_datos"), "'] === 'propio'"),
                      tags$hr(),
                      fileInput(ns("archivo"), label = "Seleccionar archivo:",
                                accept = c(".csv", ".xlsx", ".xls"),
                                buttonLabel = "Buscar\u2026",
                                placeholder = "CSV o Excel"),
                      selectInput(ns("separador"), label = "Separador (CSV):",
                                  choices = c("Coma (,)" = ",", "Punto y coma (;)" = ";",
                                              "Tabulador" = "\t"),
                                  selected = ","),
                      p(class = "small text-muted mb-0",
                        bs_icon("info-circle", class = "me-1"),
                        "La primera fila debe contener los nombres de las columnas.")
                    ),
                    tags$hr(),
                    uiOutput(ns("contexto_dataset")),
                    uiOutput(ns("resumen_datos"))
                  )
                ),

                div(
                  uiOutput(ns("cards_datos")),
                  br(),
                  card(
                    card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                    card_body(DTOutput(ns("tabla_preview")))
                  )
                )
              )
            ),

            # Sub-tab 2: Tipos de variables
            nav_panel(
              title = tagList(bs_icon("sliders2", class = "me-1"),
                              "Tipos de variables"),
              br(),
              p(class = "small text-muted mb-3",
                "Verifica que cada variable tenga el tipo correcto. ",
                "Las variables ", strong("categ\u00f3ricas"),
                " deben ser ", strong("Factor"), "."),
              layout_columns(
                col_widths = c(10, 2),
                uiOutput(ns("tabla_tipos")),
                div(
                  class = "pt-2",
                  actionButton(ns("aplicar_tipos"), "Aplicar tipos",
                               class = "btn-primary w-100", icon = icon("check")),
                  br(), br(),
                  actionButton(ns("resetear_tipos"), "Restaurar",
                               class = "btn-outline-secondary w-100 btn-sm",
                               icon = icon("rotate-left"))
                )
              ),
              uiOutput(ns("tipos_aplicados_msg"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Explorar
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("zoom-in", class = "me-1"), "Explorar"),
        card_body(
          p(class = "small text-muted mb-3",
            "Visualiza la distribuci\u00f3n de Y y su relaci\u00f3n con los predictores ",
            "antes de ajustar el modelo. El gr\u00e1fico muestra las tendencias ",
            "por grupo para anticipar la estructura aleatoria."),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_var_y_exp")),
                uiOutput(ns("sel_var_x_exp")),
                uiOutput(ns("sel_grupo_exp")),
                checkboxInput(ns("mostrar_lineas"),
                              "Mostrar l\u00ednea por grupo", value = TRUE),
                checkboxInput(ns("mostrar_global"),
                              "Mostrar tendencia global", value = TRUE),
                tags$hr(),
                uiOutput(ns("resumen_y"))
              )
            ),

            div(
              layout_columns(
                col_widths = c(6, 6),
                fill = FALSE,
                card(
                  card_header(bs_icon("bar-chart", class = "me-1"),
                              "Distribuci\u00f3n de Y"),
                  card_body(plotOutput(ns("plot_hist_y"), height = "220px"))
                ),
                card(
                  card_header(bs_icon("graph-up", class = "me-1"),
                              "Ceros y dispersi\u00f3n"),
                  card_body(uiOutput(ns("cards_ceros")))
                )
              ),
              br(),
              card(
                card_header(bs_icon("diagram-3", class = "me-1"),
                            "Spaghetti plot — variaci\u00f3n entre grupos"),
                card_body(plotOutput(ns("plot_spaghetti"), height = "320px"))
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 5: Ajustar modelo
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("gear", class = "me-1"), "Ajustar modelo"),
        card_body(
          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("toggles", class = "me-1"),
                          "Especificar el modelo"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Selecciona la variable respuesta, la familia, los efectos fijos ",
                  "y la estructura de efectos aleatorios."),

                uiOutput(ns("sel_var_y")),
                tags$hr(),

                # Familia
                selectInput(
                  ns("familia"),
                  label = "Familia de distribuci\u00f3n:",
                  choices = c(
                    "Poisson"             = "poisson",
                    "Binomial negativa"   = "nbinom2",
                    "Binomial"            = "binomial"
                  ),
                  selected = "poisson"
                ),

                # Offset opcional
                uiOutput(ns("sel_offset")),
                tags$hr(),

                # Efectos fijos
                p(class = "small fw-bold text-muted mb-1",
                  "Efectos fijos (predictores):"),
                uiOutput(ns("checks_numericos")),
                uiOutput(ns("checks_categoricos")),
                tags$hr(),

                # Estructura aleatoria
                p(class = "small fw-bold text-muted mb-1",
                  bs_icon("diagram-3", class = "me-1"),
                  "Estructura aleatoria:"),
                uiOutput(ns("sel_grupo")),
                selectInput(
                  ns("estructura_aleatoria"),
                  label = "Estructura:",
                  choices = c(
                    "Intercepto aleatorio: (1 | grupo)"           = "intercepto",
                    "Intercepto + pendiente: (1 + X | grupo)"     = "pendiente",
                    "Solo pendiente: (0 + X | grupo)"             = "solo_pendiente",
                    "Anidado: (1 | nivel_superior/nivel_inferior)" = "anidado"
                  ),
                  selected = "intercepto"
                ),
                conditionalPanel(
                  condition = paste0("input['", ns("estructura_aleatoria"),
                                     "'] == 'pendiente' || input['",
                                     ns("estructura_aleatoria"), "'] == 'solo_pendiente'"),
                  uiOutput(ns("sel_pendiente_var"))
                ),
                conditionalPanel(
                  condition = paste0("input['", ns("estructura_aleatoria"), "'] == 'anidado'"),
                  div(
                    class = "alert alert-info small py-2 px-3 mb-2",
                    bs_icon("info-circle", class = "me-1"),
                    "F\u00f3rmula: ", code("(1 | A/B)"), " donde ",
                    strong("A = nivel superior"), " y ",
                    strong("B = nivel inferior"), "."
                  ),
                  uiOutput(ns("sel_grupo_b"))
                ),
                tags$hr(),

                # Guardar
                actionButton(ns("ajustar"), "Ajustar modelo",
                             class = "btn-primary w-100", icon = icon("play")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  bs_icon("floppy", class = "me-1"), "Guardar para comparar"),
                textInput(ns("nombre_modelo"), label = NULL,
                          placeholder = "Ej: poisson_base, nb_full\u2026"),
                actionButton(ns("guardar_modelo"), "Guardar modelo",
                             class = "btn-outline-primary w-100 btn-sm",
                             icon = icon("floppy-disk"))
              )
            ),

            div(
              uiOutput(ns("aviso_singular")),
              uiOutput(ns("cards_metricas")),
              br(),
              layout_columns(
                col_widths = c(6, 6),
                card(
                  card_header(bs_icon("bullseye", class = "me-1"),
                              "Predichos vs. observados"),
                  card_body(
                    p(class = "small text-muted",
                      "Puntos cerca de la diagonal = buenas predicciones."),
                    plotOutput(ns("plot_predobs"), height = "240px")
                  )
                ),
                card(
                  card_header(bs_icon("lightbulb", class = "me-1"),
                              "F\u00f3rmula del modelo"),
                  card_body(uiOutput(ns("formula_modelo")))
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 6: Diagnóstico
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard-check", class = "me-1"),
                        "Diagn\u00f3stico"),
        div(
          class = "p-3",
          p(class = "small text-muted mb-3",
            "Verificaci\u00f3n de supuestos espec\u00edficos del GLMM. ",
            "Generado con ", strong("performance"), " de easystats."),

          # Fila 1: gráficos
          card(
            class = "mb-3",
            card_header(bs_icon("graph-up", class = "me-1"),
                        "Gr\u00e1ficos de diagn\u00f3stico",
                        span(class = "text-muted small ms-2",
                             "— performance::check_model()")),
            card_body(
              style = "overflow: visible; height: auto; min-height: 720px;",
              plotOutput(ns("plot_diagnostico"), height = "700px")
            )
          ),

          # Fila 2: tests específicos GLMM
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            card(
              class = "mb-3",
              card_header(bs_icon("arrows-expand", class = "me-1"),
                          "Sobredispersi\u00f3n",
                          span(class = "text-muted small ms-2",
                               "— check_overdispersion()")),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("test_sobredispersion"))
              )
            ),
            card(
              class = "mb-3",
              card_header(bs_icon("0-circle", class = "me-1"),
                          "Inflaci\u00f3n de ceros",
                          span(class = "text-muted small ms-2",
                               "— check_zeroinflation()")),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("test_zeroinflation"))
              )
            )
          ),

          # Fila 3: caterpillar + supuestos
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            card(
              class = "mb-0",
              card_header(bs_icon("diagram-3", class = "me-1"),
                          "Efectos aleatorios (caterpillar plot)"),
              card_body(
                style = "overflow: visible; height: auto;",
                plotOutput(ns("plot_ranef"), height = "280px")
              )
            ),
            card(
              class = "mb-0",
              card_header(bs_icon("info-circle", class = "me-1"),
                          "Resumen de supuestos"),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("tabla_supuestos"))
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 7: Performance
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("speedometer2", class = "me-1"),
                        "Performance"),
        card_body(
          p(class = "small text-muted mb-3",
            "M\u00e9tricas de ajuste del GLMM. Se usan AIC/BIC (no R\u00b2 directo), ",
            "R\u00b2 Nakagawa para la partici\u00f3n de varianza, e ICC para cuantificar ",
            "la importancia de los efectos aleatorios. ",
            "Generadas con ", strong("performance::model_performance()"), "."),

          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            card(
              card_header(bs_icon("speedometer2", class = "me-1"),
                          "M\u00e9tricas del modelo",
                          span(class = "text-muted small ms-2",
                               "— model_performance()")),
              card_body(uiOutput(ns("tabla_performance")))
            ),
            card(
              card_header(bs_icon("pie-chart", class = "me-1"),
                          "ICC \u2014 Correlaci\u00f3n intraclase",
                          span(class = "text-muted small ms-2",
                               "— performance::icc()")),
              card_body(
                uiOutput(ns("tabla_icc")),
                br(),
                plotOutput(ns("plot_icc"), height = "160px"),
                uiOutput(ns("interp_nakagawa"))
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 8: Parámetros
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("table", class = "me-1"),
                        "Par\u00e1metros"),
        div(
          class = "p-3",
          uiOutput(ns("params_intro")),
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            card(
              card_header(bs_icon("layout-text-sidebar", class = "me-1"),
                          "Efectos fijos",
                          span(class = "text-muted small ms-2",
                               "— parameters (easystats)")),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("tabla_efectos_fijos"))
              )
            ),
            card(
              card_header(bs_icon("diagram-3", class = "me-1"),
                          "Efectos aleatorios",
                          span(class = "text-muted small ms-2",
                               "— varianzas \u03c3\u00b2 \u00b7 glmmTMB")),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("tabla_efectos_aleatorios"))
              )
            )
          ),
          br(),
          card(
            card_header(bs_icon("arrow-left-right", class = "me-1"),
                        uiOutput(ns("transformed_header"))),
            card_body(
              style = "overflow: visible; height: auto;",
              uiOutput(ns("tabla_transformada"))
            )
          ),
          br(),
          card(
            card_header(bs_icon("bar-chart-steps", class = "me-1"),
                        "Importancia de variables",
                        span(class = "text-muted small ms-2",
                             "— \u03b2 estandarizados")),
            card_body(
              style = "height: auto;",
              p(class = "small text-muted mb-2",
                strong("Azul"), " = efecto positivo \u00b7 ",
                strong("rojo"), " = efecto negativo. ",
                "Barras en la escala del enlace."),
              plotOutput(ns("plot_importancia"), height = "280px")
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 9: Efectos marginales
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("graph-up-arrow", class = "me-1"),
                        "Efectos marginales"),
        card_body(
          p(class = "small text-muted mb-3",
            "Los ", strong("efectos marginales"),
            " muestran c\u00f3mo cambia Y al variar un predictor, ",
            "manteniendo el resto en sus valores t\u00edpicos, ",
            "en la ", strong("escala original de Y"),
            " (probabilidades para binomial, conteo esperado para Poisson). ",
            "Generados con ", strong("modelbased::estimate_relation()"), "."),

          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("sel_pred_marginal")),
                tags$hr(),
                checkboxInput(ns("marginal_ci"),
                              "Mostrar intervalo de confianza 95%",
                              value = TRUE),
                checkboxInput(ns("marginal_puntos"),
                              "Mostrar datos observados", value = TRUE),
                tags$hr(),
                uiOutput(ns("marginal_valores_tipicos"))
              )
            ),

            div(
              card(
                card_header(bs_icon("graph-up-arrow", class = "me-1"),
                            "Efecto marginal",
                            span(class = "text-muted small ms-2",
                                 "— estimate_relation() \u00b7 modelbased")),
                card_body(plotOutput(ns("plot_marginal"), height = "360px"))
              ),
              br(),
              uiOutput(ns("marginal_interpretacion"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 10: Contrastes
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("arrows-angle-expand", class = "me-1"),
                        "Contrastes"),
        card_body(
          p(class = "small text-muted mb-3",
            "Los ", strong("contrastes"), " comparan niveles de variables ",
            "categ\u00f3ricas. Se estiman en la escala del enlace y se pueden ",
            "transformar a la escala original (OR para binomial, IRR para conteos). ",
            "Generados con ", strong("modelbased::estimate_contrasts()"), "."),
          uiOutput(ns("contrasts_no_cat_msg")),
          layout_columns(
            col_widths = c(4, 8),
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_var_contraste")),
                tags$hr(),
                selectInput(ns("metodo_ajuste"),
                            label = "Ajuste de p-valores:",
                            choices = c("Sin ajuste" = "none",
                                        "Bonferroni" = "bonferroni",
                                        "Holm"       = "holm",
                                        "FDR (BH)"   = "fdr"),
                            selected = "none")
              )
            ),
            div(
              card(class = "mb-3",
                   card_header(bs_icon("table", class = "me-1"),
                               "Tabla de contrastes"),
                   card_body(uiOutput(ns("tabla_contrastes")))),
              card(class = "mb-0",
                   card_header(bs_icon("bar-chart-fill", class = "me-1"),
                               "Visualizaci\u00f3n"),
                   card_body(plotOutput(ns("plot_contrastes"), height = "280px")))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 11: Comparar modelos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("arrow-left-right", class = "me-1"),
                        "Comparar modelos"),
        card_body(
          p(class = "small text-muted mb-3",
            "Ajusta distintos modelos (ej. Poisson vs NB, con y sin offset), ",
            "gu\u00e1rdalos con nombres descriptivos y comp\u00e1ralos aqu\u00ed por AIC y BIC."),
          layout_columns(
            col_widths = c(4, 8),
            card(
              card_header(bs_icon("list-check", class = "me-1"),
                          "Modelos guardados"),
              card_body(
                uiOutput(ns("lista_modelos_guardados")),
                tags$hr(),
                actionButton(ns("limpiar_modelos"), "Limpiar todos",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon = icon("trash"))
              )
            ),
            div(
              card(class = "mb-3",
                   card_header(bs_icon("table", class = "me-1"),
                               "Tabla comparativa",
                               span(class = "text-muted small ms-2",
                                    "— compare_performance() \u00b7 easystats")),
                   card_body(uiOutput(ns("tabla_comparacion")))),
              card(class = "mb-0",
                   card_header(bs_icon("diagram-3", class = "me-1"),
                               "Gr\u00e1fico radar",
                               span(class = "text-muted small ms-2",
                                    "— compare_performance() \u00b7 see")),
                   card_body(
                     p(class = "small text-muted mb-2",
                       "Mayor \u00e1rea = mejor modelo en m\u00e1s dimensiones."),
                     plotOutput(ns("plot_comparacion"), height = "320px")
                   ))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 12: Código R
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"),
                        "C\u00f3digo R"),
        card_body(
          p(class = "text-muted small mb-3",
            "Script que reproduce este an\u00e1lisis usando ",
            strong("glmmTMB"), " y ", strong("easystats"),
            ". Se actualiza seg\u00fan las selecciones activas."),
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tagList(bs_icon("code-slash"), " Script reproducible"),
              downloadButton(ns("descargar_codigo"), "Descargar .R",
                             icon = bs_icon("download"),
                             class = "btn-sm btn-outline-primary")
            ),
            verbatimTextOutput(ns("codigo_r"))
          )
        )
      )

    ) # /navset_card_tab
  )   # /tagList
}

# ── Server ────────────────────────────────────────────────
mod_glmm_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ────────────────────────────────────────────────────
    # DATOS
    # ────────────────────────────────────────────────────

    # Datasets disponibles por familia
    datasets_por_familia <- list(
      poisson  = c("Aves en fragmentos \u2014 Poisson (ecolog\u00eda)"  = "aves_glmm",
                   "Cargar mis propios datos"                    = "propio"),
      nbinom2  = c("Aves en fragmentos \u2014 NB (ecolog\u00eda)"       = "aves_glmm",
                   "Cargar mis propios datos"                    = "propio"),
      binomial = c("Ranas en charcas \u2014 Binomial (ecolog\u00eda)"   = "ranas_glmm",
                   "Cargar mis propios datos"                    = "propio"),
    )

    # familia_activa: reactiveVal que garantiza valor desde el inicio
    familia_activa <- reactiveVal("poisson")
    observeEvent(input$familia_datos, {
      req(!is.null(input$familia_datos) && nchar(input$familia_datos) > 0)
      familia_activa(input$familia_datos)
    }, ignoreNULL = TRUE)

    # Tipos de usuario
    tipos_usuario <- reactiveVal(NULL)
    observeEvent(input$fuente_datos, { tipos_usuario(NULL) })
    observeEvent(input$resetear_tipos, {
      tipos_usuario(NULL)
      showNotification("Tipos restaurados.", type = "message", duration = 2)
    })
    observeEvent(input$aplicar_tipos, {
      df <- datos_base(); req(df)
      nuevos <- lapply(names(df), function(nm) input[[paste0("tipo_", nm)]])
      names(nuevos) <- names(df)
      tipos_usuario(nuevos)
      showNotification("Tipos aplicados.", type = "message", duration = 2)
    })

    # Cargar datos base (sin tipos aplicados)
    datos_base <- reactive({
      fuente <- input$fuente_datos
      req(fuente)

      if (fuente == "aves_glmm") {
        e <- new.env()
        load(system.file("app/data/aves_glmm.rda",
                         package = "StatModels"), envir = e)
        df <- e$aves_glmm
        df$fragmento <- factor(df$fragmento)
        df
      } else if (fuente == "ranas_glmm") {
        e <- new.env()
        load(system.file("app/data/ranas_glmm.rda",
                         package = "StatModels"), envir = e)
        df <- e$ranas_glmm
        df$charca <- factor(df$charca)
        df
      } else {
        req(input$archivo)
        ext <- tools::file_ext(input$archivo$name)
        tryCatch({
          df <- if (ext %in% c("xlsx", "xls")) {
            readxl::read_excel(input$archivo$datapath)
          } else {
            read.csv(input$archivo$datapath,
                     sep    = input$separador,
                     stringsAsFactors = FALSE)
          }
          for (nm in names(df))
            if (is.character(df[[nm]]) && length(unique(df[[nm]])) < 15)
              df[[nm]] <- factor(df[[nm]])
          as.data.frame(df)
        }, error = function(err) {
          showNotification("Error al leer el archivo.", type = "error")
          NULL
        })
      }
    })

    # datos_activos: aplica tipos de usuario sobre datos_base
    datos_activos <- reactive({
      df <- datos_base(); req(df)
      tu <- tipos_usuario()
      if (is.null(tu)) return(df)
      for (nm in names(tu)) {
        if (!nm %in% names(df) || is.null(tu[[nm]])) next
        if (tu[[nm]] == "factor"  && !is.factor(df[[nm]]))
          df[[nm]] <- factor(df[[nm]])
        else if (tu[[nm]] == "numeric" && !is.numeric(df[[nm]]))
          df[[nm]] <- suppressWarnings(as.numeric(as.character(df[[nm]])))
      }
      as.data.frame(df)
    })

    # Reactivos de tipos de variables
    vars_numericas <- reactive({
      df <- datos_activos(); req(df)
      names(df)[sapply(df, is.numeric)]
    })
    vars_categoricas <- reactive({
      df <- datos_activos(); req(df)
      names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    })

    # Selector dinámico de dataset según familia
    output$sel_fuente_datos <- renderUI({
      fam      <- familia_activa()
      opciones <- datasets_por_familia[[fam]]
      if (is.null(opciones)) opciones <- datasets_por_familia[["poisson"]]
      radioButtons(ns("fuente_datos"),
                   label   = tagList(bs_icon("database", class = "me-1"),
                                     "Dataset de ejemplo:"),
                   choices = opciones, selected = opciones[1])
    })

    # Sincronizar familia en Ajustar modelo con la elegida en Los datos
    observeEvent(input$familia_datos, {
      req(!is.null(input$familia_datos) && nchar(input$familia_datos) > 0)
      updateSelectInput(session, "familia", selected = input$familia_datos)
      tryCatch({
        fam <- familia_activa()
        colores_fam <- list(
          poisson  = "background:#E1F5EE; border:2px solid #0F6E56;",
          nbinom2  = "background:#FAEEDA; border:2px solid #853F0B;",
          binomial = "background:#E6F1FB; border:2px solid #185FA5;",
        )
        estilo_base  <- "background:var(--color-background-secondary); border:1.5px solid var(--color-border-tertiary);"
        estilo_comun <- "border-radius:10px; padding:10px 12px; cursor:pointer;"
        for (f in names(colores_fam)) {
          card_id <- paste0("#", ns(paste0("card_", f)))
          estilo  <- if (f == fam)
            paste0(colores_fam[[f]], estilo_comun)
          else
            paste0(estilo_base, estilo_comun)
          shinyjs::runjs(paste0(
            'document.querySelector("', card_id,
            '").setAttribute("style", "', estilo, '");'))
        }
      }, error = function(e) NULL)
    })

    # Contexto del dataset
    output$contexto_dataset <- renderUI({
      fuente <- input$fuente_datos
      if (is.null(fuente) || fuente == "propio") return(NULL)
      info <- list(
        aves_glmm = list(
          titulo = "Aves en fragmentos de bosque tropical seco (simulado)",
          texto  = tagList(
            "Conteo de aves en ", strong("72 puntos de conteo"),
            " (6 por fragmento) en 12 fragmentos de bosque tropical seco. ",
            "Predictores: cobertura de dosel (%), distancia al borde (m), NDVI. ",
            strong("Offset: log(area_ha)"), " \u2014 estandariza por \u00e1rea del fragmento. ",
            "Sobredispersi\u00f3n esperada \u2014 comparar Poisson vs NB. ",
            "Dataset simulado con estructura jer\u00e1rquica real."
          )
        ),
        ranas_glmm = list(
          titulo = "Ranas en charcas temporales (simulado)",
          texto  = tagList(
            "Presencia (1) / ausencia (0) de rana arb\u00f3rea en ",
            strong("120 visitas"), " (8 por charca) a 15 charcas temporales. ",
            "Predictores: hidroperiodo (d\u00edas/a\u00f1o), cobertura vegetal acuática (%), ",
            "distancia al bosque (m), pH del agua. ",
            "Dataset simulado para regresi\u00f3n log\u00edstica mixta."
          )
        )
      )
      ctx <- info[[fuente]]
      if (is.null(ctx)) return(NULL)
      div(
        class = "alert alert-info small py-2 px-3 mb-2",
        bs_icon("info-circle", class = "me-1"),
        strong(ctx$titulo), tags$br(), ctx$texto
      )
    })

    # Vista previa y resumen
    output$tabla_preview <- DT::renderDT({
      df <- datos_activos(); req(df)
      DT::datatable(head(df, 20), options = list(pageLength = 8,
                    scrollX = TRUE, dom = "tip"),
                    rownames = FALSE, class = "table-sm table-striped")
    })

    output$resumen_datos <- renderUI({
      df <- datos_activos(); req(df)
      div(class = "small text-muted mt-2",
          bs_icon("info-circle", class = "me-1"),
          strong(nrow(df)), " filas \u00b7 ",
          strong(ncol(df)), " columnas")
    })

    output$cards_datos <- renderUI({
      df <- datos_activos(); req(df)
      nums <- sum(sapply(df, is.numeric))
      cats <- sum(sapply(df, function(x) is.factor(x) || is.character(x)))
      layout_columns(
        col_widths = c(6, 6),
        fill = FALSE,
        div(class = "alert alert-primary small py-2 px-3 mb-0 text-center",
            bs_icon("123", class = "me-1"),
            strong(nums), " num\u00e9ricas"),
        div(class = "alert alert-secondary small py-2 px-3 mb-0 text-center",
            bs_icon("tag", class = "me-1"),
            strong(cats), " categ\u00f3ricas")
      )
    })

    # Tipos de variables
    output$tabla_tipos <- renderUI({
      df <- datos_activos(); req(df)
      filas <- lapply(names(df), function(nm) {
        tipo_actual <- if (is.factor(df[[nm]])) "factor"
                       else if (is.numeric(df[[nm]])) "numeric"
                       else "character"
        tags$tr(
          tags$td(code(nm)),
          tags$td(tags$span(
            class = if (tipo_actual == "factor") "badge bg-secondary"
                    else "badge bg-primary",
            tipo_actual)),
          tags$td(selectInput(ns(paste0("tipo_", nm)), label = NULL,
                              choices = c("Num\u00e9rico" = "numeric",
                                          "Factor" = "factor"),
                              selected = tipo_actual,
                              width = "120px"))
        )
      })
      tags$table(
        class = "table table-sm small",
        tags$thead(tags$tr(tags$th("Variable"), tags$th("Tipo actual"),
                           tags$th("Cambiar a"))),
        tags$tbody(filas)
      )
    })

    output$tipos_aplicados_msg <- renderUI({
      req(input$aplicar_tipos)
      div(class = "alert alert-success small py-2 px-3 mt-2",
          bs_icon("check-circle-fill", class = "me-1"),
          "Tipos aplicados correctamente.")
    })

    # ────────────────────────────────────────────────────
    # EXPLORAR
    # ────────────────────────────────────────────────────

    output$sel_var_y_exp <- renderUI({
      nums <- vars_numericas(); req(nums)
      selectInput(ns("var_y_exp"), "Variable respuesta (Y):",
                  choices = nums, selected = nums[1])
    })

    output$sel_var_x_exp <- renderUI({
      nums <- vars_numericas(); req(nums, input$var_y_exp)
      opts <- nums[nums != input$var_y_exp]
      if (length(opts) == 0) return(NULL)
      selectInput(ns("var_x_exp"), "Predictor (X):",
                  choices = opts, selected = opts[1])
    })

    output$sel_grupo_exp <- renderUI({
      cats <- vars_categoricas(); req(cats)
      selectInput(ns("grupo_exp"), "Variable de grupo (color):",
                  choices = cats, selected = cats[length(cats)])
    })

    output$resumen_y <- renderUI({
      df <- datos_activos(); req(df, input$var_y_exp)
      y  <- df[[input$var_y_exp]]
      req(is.numeric(y))
      p_ceros <- round(mean(y == 0) * 100, 1)
      disp    <- round(var(y) / mean(y), 2)
      tagList(
        div(class = "alert alert-info small py-2 px-3 mb-0",
            bs_icon("info-circle", class = "me-1"),
            strong("Ceros: "), paste0(p_ceros, "%"), tags$br(),
            strong("Dispersi\u00f3n (Var/Media): "), disp,
            if (disp > 2)
              tags$span(class = "text-danger ms-1", "\u26a0 sobredispersi\u00f3n")
        )
      )
    })

    output$plot_hist_y <- renderPlot({
      df <- datos_activos(); req(df, input$var_y_exp)
      y  <- df[[input$var_y_exp]]
      req(is.numeric(y))
      ggplot2::ggplot(data.frame(y = y), ggplot2::aes(x = y)) +
        ggplot2::geom_histogram(bins = 20, fill = colores$primario,
                                color = "white", alpha = 0.85) +
        ggplot2::labs(x = input$var_y_exp, y = "Frecuencia") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    }, res = 96)

    output$cards_ceros <- renderUI({
      df <- datos_activos(); req(df, input$var_y_exp)
      y  <- df[[input$var_y_exp]]; req(is.numeric(y))
      p_ceros <- round(mean(y == 0) * 100, 1)
      disp    <- round(var(y) / mean(y), 2)
      col_c   <- if (p_ceros > 50) colores$peligro else
                 if (p_ceros > 20) colores$acento else colores$exito
      col_d   <- if (disp > 3) colores$peligro else
                 if (disp > 1.5) colores$acento else colores$exito
      tagList(
        div(class = "alert small py-2 px-3 mb-2",
            style = paste0("border-left: 4px solid ", col_c, ";"),
            bs_icon("0-circle", class = "me-1"),
            strong(paste0(p_ceros, "% ceros")), tags$br(),
            tags$span(class = "text-muted",
                      if (p_ceros > 50) "Considerar ZIP o ZINB"
                      else if (p_ceros > 20) "Monitorear"
                      else "OK para Poisson/NB")),
        div(class = "alert small py-2 px-3 mb-0",
            style = paste0("border-left: 4px solid ", col_d, ";"),
            bs_icon("arrows-expand", class = "me-1"),
            strong(paste0("Var/Media = ", disp)), tags$br(),
            tags$span(class = "text-muted",
                      if (disp > 3) "Sobredispersi\u00f3n severa \u2192 NB"
                      else if (disp > 1.5) "Sobredispersi\u00f3n moderada"
                      else "OK para Poisson"))
      )
    })

    output$plot_spaghetti <- renderPlot({
      df <- datos_activos()
      req(df, input$var_y_exp, input$var_x_exp, input$grupo_exp)
      tryCatch({
        n_grps <- length(unique(df[[input$grupo_exp]]))
        pal    <- colorRampPalette(colores$tableau)(n_grps)
        p <- ggplot2::ggplot(df,
               ggplot2::aes(x = .data[[input$var_x_exp]],
                            y = .data[[input$var_y_exp]],
                            color = .data[[input$grupo_exp]],
                            group = .data[[input$grupo_exp]])) +
          ggplot2::geom_point(alpha = 0.5, size = 2)
        if (isTRUE(input$mostrar_lineas))
          p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                                        se = FALSE, linewidth = 0.8)
        if (isTRUE(input$mostrar_global))
          p <- p + ggplot2::geom_smooth(
            data = df,
            ggplot2::aes(x = .data[[input$var_x_exp]],
                         y = .data[[input$var_y_exp]]),
            method = "loess", formula = y ~ x, se = TRUE,
            color = "black", linewidth = 1.2, linetype = "dashed",
            inherit.aes = FALSE)
        p + ggplot2::scale_color_manual(values = pal) +
          ggplot2::labs(x = input$var_x_exp, y = input$var_y_exp,
                        color = input$grupo_exp) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         legend.position  = "bottom")
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label = "Selecciona variables para visualizar.",
                            color = colores$texto, size = 4) +
          ggplot2::theme_void()
      })
    }, res = 96)

    # ────────────────────────────────────────────────────
    # AJUSTAR MODELO
    # ────────────────────────────────────────────────────

    output$sel_var_y <- renderUI({
      nums <- vars_numericas(); req(nums)
      selectInput(ns("var_y"), "Variable respuesta (Y):",
                  choices = nums, selected = nums[1])
    })

    output$sel_offset <- renderUI({
      nums <- vars_numericas(); req(nums)
      opts <- c("Ninguno" = "", nums)
      selectInput(ns("offset_var"), "Offset (opcional):",
                  choices = opts, selected = "",
                  width = "100%")
    })

    output$checks_numericos <- renderUI({
      nums <- vars_numericas(); req(nums, input$var_y)
      opts <- nums[nums != input$var_y]
      if (!is.null(input$offset_var) && nchar(input$offset_var) > 0)
        opts <- opts[opts != input$offset_var]
      if (length(opts) == 0) return(p(class = "small text-muted", "No hay variables num\u00e9ricas."))
      checkboxGroupInput(ns("preds_num"), label = NULL, choices = opts)
    })

    output$checks_categoricos <- renderUI({
      cats <- vars_categoricas(); req(cats)
      if (length(cats) == 0) return(p(class = "small text-muted", "No hay variables categ\u00f3ricas."))
      checkboxGroupInput(ns("preds_cat"), label = NULL, choices = cats)
    })

    output$sel_grupo <- renderUI({
      cats <- vars_categoricas()
      if (length(cats) == 0) return(
        div(class = "alert alert-warning small py-2 px-3",
            "No hay variables categ\u00f3ricas. Convierte la variable de grupo en Tipos de variables."))
      selectInput(ns("var_grupo"), "Variable de agrupamiento (A):",
                  choices = cats, selected = cats[length(cats)])
    })

    output$sel_pendiente_var <- renderUI({
      nums <- vars_numericas(); req(nums, input$var_y)
      opts <- nums[nums != input$var_y]
      req(length(opts) > 0)
      selectInput(ns("pendiente_var"), "Predictor con pendiente aleatoria:",
                  choices = opts, selected = opts[1])
    })

    output$sel_grupo_b <- renderUI({
      cats <- vars_categoricas(); req(cats, input$var_grupo)
      opts <- cats[cats != input$var_grupo]
      if (length(opts) == 0) return(
        div(class = "alert alert-warning small py-2 px-3",
            "Solo hay una variable categ\u00f3rica. Se necesitan \u22652 para estructura anidada."))
      selectInput(ns("var_grupo_b"),
                  label = HTML("Nivel inferior (B):<small class='text-muted d-block'>Debe tener m&aacute;s grupos que A</small>"),
                  choices = opts, selected = opts[1])
    })

    # Construcción de la fórmula de efectos aleatorios
    formula_re <- reactive({
      req(input$var_grupo, input$estructura_aleatoria)
      switch(input$estructura_aleatoria,
        "intercepto"    = paste0("(1 | ", input$var_grupo, ")"),
        "pendiente"     = {
          req(input$pendiente_var)
          paste0("(1 + ", input$pendiente_var, " | ", input$var_grupo, ")")
        },
        "solo_pendiente" = {
          req(input$pendiente_var)
          paste0("(0 + ", input$pendiente_var, " | ", input$var_grupo, ")")
        },
        "anidado" = {
          req(input$var_grupo_b)
          paste0("(1 | ", input$var_grupo, "/", input$var_grupo_b, ")")
        }
      )
    })

    # Modelo GLMM
    modelo_glmm <- eventReactive(input$ajustar, {
      df  <- datos_activos(); req(df, input$var_y, input$familia, input$var_grupo)
      fam <- input$familia
      preds_num <- input$preds_num
      preds_cat <- input$preds_cat
      todos_preds <- c(preds_num, preds_cat)
      re  <- formula_re()

      # Construir parte de efectos fijos
      parte_fija <- if (length(todos_preds) > 0)
        paste(todos_preds, collapse = " + ")
      else "1"

      # Offset
      offset_str <- if (!is.null(input$offset_var) && nchar(input$offset_var) > 0)
        paste0(" + offset(log(", input$offset_var, "))")
      else ""

      fm_str <- paste0(input$var_y, " ~ ", parte_fija, offset_str, " + ", re)

      tryCatch({
        familia_glmm <- switch(fam,
          "poisson"  = glmmTMB::poisson(),
          "nbinom2"  = glmmTMB::nbinom2(),
          "binomial" = stats::binomial(),
          "zip"      = glmmTMB::poisson(),
          "zinb"     = glmmTMB::nbinom2()
        )
        zi_formula <- if (fam %in% c("zip", "zinb")) ~1 else ~0

        glmmTMB::glmmTMB(
          formula    = as.formula(fm_str),
          ziformula  = zi_formula,
          family     = familia_glmm,
          data       = df,
          REML       = FALSE
        )
      }, error = function(e) {
        showNotification(paste0("Error al ajustar: ", conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      })
    })

    # Aviso singular fit
    output$aviso_singular <- renderUI({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        if (glmmTMB::isSingular(fm)) {
          div(class = "alert alert-warning small py-2 px-3 mb-3",
              bs_icon("exclamation-triangle-fill", class = "me-1"),
              strong("Singular fit \u2014 "),
              "el modelo tiene m\u00e1s par\u00e1metros de los que los datos soportan. ",
              "Simplifica la estructura aleatoria.")
        }
      }, error = function(e) NULL)
    })

    # Fórmula del modelo
    output$formula_modelo <- renderUI({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        fam  <- input$familia
        re   <- formula_re()
        fml  <- deparse(formula(fm))
        tagList(
          p(class = "small fw-bold mb-1", "F\u00f3rmula:"),
          div(class = "alert alert-secondary small py-2 px-3 mb-2",
              style = "font-family: monospace;", fml),
          p(class = "small fw-bold mb-1", "Familia:"),
          div(class = "alert alert-info small py-2 px-3 mb-0",
              switch(fam,
                "poisson"  = tagList(code("Poisson"), " \u2014 conteos sin sobredispersi\u00f3n"),
                "nbinom2"  = tagList(code("Binomial negativa"), " \u2014 conteos sobredispersos"),
                "binomial" = tagList(code("Binomial"), " \u2014 presencia/ausencia"),
                "zip"      = tagList(code("ZIP"), " \u2014 Poisson zero-inflated"),
                "zinb"     = tagList(code("ZINB"), " \u2014 NB zero-inflated")
              ))
        )
      }, error = function(e) NULL)
    })

    # Métricas rápidas post-ajuste
    output$cards_metricas <- renderUI({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        aic  <- round(AIC(fm), 1)
        bic  <- round(BIC(fm), 1)
        ll   <- round(logLik(fm)[1], 1)
        r2   <- tryCatch(
          suppressWarnings(performance::r2_nakagawa(fm, verbose = FALSE)),
          error = function(e) list(R2_marginal = NA, R2_conditional = NA))
        r2m  <- if (!is.na(r2$R2_marginal))
          round(r2$R2_marginal, 3) else "\u2014"
        r2c  <- if (!is.na(r2$R2_conditional))
          round(r2$R2_conditional, 3) else "\u2014"
        layout_columns(
          col_widths = c(4, 4, 4),
          div(class = "alert alert-primary small py-2 px-3 mb-0 text-center",
              bs_icon("calculator", class = "me-1"),
              strong("AIC"), br(), strong(aic)),
          div(class = "alert alert-secondary small py-2 px-3 mb-0 text-center",
              title = "Varianza explicada solo por los efectos fijos",
              bs_icon("bar-chart-steps", class = "me-1"),
              strong("R\u00b2 marginal"), br(), strong(r2m)),
          div(class = "alert alert-info small py-2 px-3 mb-0 text-center",
              title = "Varianza explicada por efectos fijos + aleatorios",
              bs_icon("bar-chart-steps", class = "me-1"),
              strong("R\u00b2 condicional"), br(), strong(r2c))
        )
      }, error = function(e) NULL)
    })

    # Predichos vs observados
    output$plot_predobs <- renderPlot({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        obs  <- modelo_glmm()$frame[[input$var_y]]
        pred <- fitted(fm)
        df_p <- data.frame(obs = obs, pred = pred)
        ggplot2::ggplot(df_p, ggplot2::aes(x = pred, y = obs)) +
          ggplot2::geom_point(alpha = 0.5, color = colores$primario, size = 2) +
          ggplot2::geom_abline(slope = 1, intercept = 0,
                               linetype = "dashed", color = "gray50") +
          ggplot2::labs(x = "Predichos", y = "Observados") +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
      }, error = function(e) ggplot2::ggplot() + ggplot2::theme_void())
    }, res = 96)

    # Modelos guardados
    modelos_guardados <- reactiveVal(list())

    observeEvent(input$guardar_modelo, {
      fm   <- modelo_glmm(); req(fm)
      nombre <- trimws(input$nombre_modelo)
      if (nchar(nombre) == 0) {
        showNotification("Ingresa un nombre.", type = "warning"); return()
      }
      lista <- modelos_guardados()
      lista[[nombre]] <- fm
      modelos_guardados(lista)
      showNotification(paste0("Modelo '", nombre, "' guardado."),
                       type = "message", duration = 3)
    })

    observeEvent(input$limpiar_modelos, {
      modelos_guardados(list())
      showNotification("Modelos eliminados.", type = "message", duration = 2)
    })

    # ────────────────────────────────────────────────────
    # DIAGNÓSTICO
    # ────────────────────────────────────────────────────

    output$plot_diagnostico <- renderPlot({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        p <- performance::check_model(
          fm, verbose = FALSE,
          check = c("pp_check", "linearity", "homogeneity",
                    "outliers", "qq", "reqq"))
        print(p)
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label = "Diagn\u00f3stico no disponible.",
                            color = colores$texto, size = 4) +
          ggplot2::theme_void()
      })
    }, res = 96, height = 700, width = 950)

    output$test_sobredispersion <- renderUI({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        test <- performance::check_overdispersion(fm)
        disp <- round(test$dispersion_ratio, 3)
        pval <- round(test$p_value, 4)
        prob <- test$dispersion_ratio > 1.5
        col  <- if (prob) colores$peligro else colores$exito
        ico  <- if (prob) "\u26a0" else "\u2713"

        tagList(
          div(class = "text-center mb-3",
              h3(style = paste0("color:", col, "; font-weight:700;"), disp),
              p(class = "small text-muted mb-0", "Raz\u00f3n de dispersi\u00f3n")),
          tags$table(
            class = "table table-sm small mb-2",
            tags$tbody(
              tags$tr(tags$td("Raz\u00f3n de dispersi\u00f3n"),
                      tags$td(style = paste0("color:", col, "; font-weight:600;"),
                              paste0(ico, " ", disp))),
              tags$tr(tags$td("p-valor"),
                      tags$td(round(pval, 4)))
            )
          ),
          div(class = paste0("alert small py-2 px-3 mb-0 ",
                             if (prob) "alert-warning" else "alert-success"),
              if (prob)
                tagList(bs_icon("exclamation-triangle-fill", class = "me-1"),
                        "Sobredispersi\u00f3n detectada. Considera cambiar a Binomial Negativa.")
              else
                tagList(bs_icon("check-circle-fill", class = "me-1"),
                        "Sin sobredispersi\u00f3n significativa."))
        )
      }, error = function(e) {
        p(class = "small text-muted",
          "Test de sobredispersi\u00f3n no disponible para esta familia.")
      })
    })

    output$test_zeroinflation <- renderUI({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        test  <- performance::check_zeroinflation(fm)
        obs   <- test$count.observed
        pred  <- round(test$count.predicted, 1)
        ratio <- round(obs / pred, 3)
        prob  <- ratio > 1.2
        col   <- if (prob) colores$peligro else colores$exito
        ico   <- if (prob) "\u26a0" else "\u2713"

        tagList(
          div(class = "text-center mb-3",
              h3(style = paste0("color:", col, "; font-weight:700;"),
                 paste0(round((obs / sum(fitted(fm) >= 0)) * 100, 1), "%")),
              p(class = "small text-muted mb-0", "Ceros observados")),
          tags$table(
            class = "table table-sm small mb-2",
            tags$tbody(
              tags$tr(tags$td("Ceros observados"),
                      tags$td(obs)),
              tags$tr(tags$td("Ceros predichos"),
                      tags$td(pred)),
              tags$tr(tags$td("Raz\u00f3n obs/pred"),
                      tags$td(style = paste0("color:", col, "; font-weight:600;"),
                              paste0(ico, " ", ratio)))
            )
          ),
          div(class = paste0("alert small py-2 px-3 mb-0 ",
                             if (prob) "alert-warning" else "alert-success"),
              if (prob)
                tagList(bs_icon("exclamation-triangle-fill", class = "me-1"),
                        "Inflaci\u00f3n de ceros detectada. Considera ZIP o ZINB.")
              else
                tagList(bs_icon("check-circle-fill", class = "me-1"),
                        "N\u00famero de ceros consistente con la distribuci\u00f3n elegida."))
        )
      }, error = function(e) {
        p(class = "small text-muted",
          "Test de inflaci\u00f3n de ceros no disponible.")
      })
    })

    output$plot_ranef <- renderPlot({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        re <- glmmTMB::ranef(fm)$cond
        if (length(re) == 0) stop("sin_re")
        grp_nm <- names(re)[1]
        df_re  <- data.frame(
          grupo = rownames(re[[grp_nm]]),
          efecto = re[[grp_nm]][, 1]
        )
        df_re <- df_re[order(df_re$efecto), ]
        df_re$grupo <- factor(df_re$grupo, levels = df_re$grupo)
        ggplot2::ggplot(df_re, ggplot2::aes(x = efecto, y = grupo)) +
          ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                              color = "gray60") +
          ggplot2::geom_point(color = colores$primario, size = 3) +
          ggplot2::labs(x = "Efecto aleatorio (BLUP)", y = grp_nm,
                        subtitle = "Interceptos aleatorios por grupo") +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label = "Ajusta el modelo primero.",
                            color = colores$texto, size = 3.5) +
          ggplot2::theme_void()
      })
    }, res = 96)

    output$tabla_supuestos <- renderUI({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        sing   <- tryCatch(glmmTMB::isSingular(fm), error = function(e) FALSE)
        n_grps <- tryCatch({
          re <- glmmTMB::ranef(fm)$cond
          if (length(re) > 0) nrow(re[[1]]) else NA
        }, error = function(e) NA)
        pocos  <- !is.na(n_grps) && n_grps < 10

        tags$table(
          class = "table table-sm small mb-0",
          tags$tbody(
            tags$tr(
              tags$td(strong("Singular fit")),
              tags$td(style = paste0("color:", if (sing) colores$peligro else colores$exito,
                                     "; font-weight:600;"),
                      if (sing) "\u26a0 S\u00ed" else "\u2713 No"),
              tags$td(class = "text-muted small",
                      if (sing)
                        "El modelo tiene m\u00e1s par\u00e1metros de los que los datos soportan. Simplifica los efectos aleatorios."
                      else
                        "El modelo no est\u00e1 sobreparametrizado.")
            ),
            if (!is.na(n_grps))
              tags$tr(
                tags$td(strong("N\u00famero de grupos")),
                tags$td(style = paste0("color:", if (pocos) colores$acento else colores$exito,
                                       "; font-weight:600;"),
                        n_grps),
                tags$td(class = "text-muted small",
                        if (pocos)
                          paste0("Hay ", n_grps, " grupos \u2014 menos de los 10 recomendados. La estimaci\u00f3n de varianza puede ser imprecisa.")
                        else
                          paste0(n_grps, " grupos \u2014 suficiente para estimar bien la varianza entre grupos."))
              )
          )
        )
      }, error = function(e) NULL)
    })

    # ────────────────────────────────────────────────────
    # PERFORMANCE
    # ────────────────────────────────────────────────────

    output$tabla_performance <- renderUI({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        mp  <- performance::model_performance(fm, verbose = FALSE)
        df  <- as.data.frame(mp)
        metricas <- list(
          list(n = "AIC",       d = "Criterio de informaci\u00f3n de Akaike. Menor = mejor."),
          list(n = "BIC",       d = "Criterio bayesiano. Penaliza m\u00e1s la complejidad."),
          list(n = "R2_marginal",   d = "Varianza explicada por efectos fijos."),
          list(n = "R2_conditional", d = "Varianza explicada por fijos + aleatorios."),
          list(n = "ICC",       d = "Correlaci\u00f3n intraclase \u2014 varianza entre grupos / total."),
          list(n = "RMSE",      d = "Error cuadr\u00e1tico medio en escala de Y.")
        )
        filas <- lapply(metricas, function(m) {
          if (!(m$n %in% names(df))) return(NULL)
          val <- df[[m$n]]
          if (is.numeric(val)) val <- round(val, 4)
          tags$tr(
            tags$td(strong(m$n)),
            tags$td(style = paste0("text-align:right; font-family:monospace;",
                                   " color:", colores$primario, ";"),
                    val),
            tags$td(class = "small text-muted", m$d)
          )
        })
        tags$table(
          class = "table table-sm small mb-0",
          tags$thead(
            style = paste0("background:", colores$primario, "; color:#fff;"),
            tags$tr(tags$th("M\u00e9trica"), tags$th("Valor"), tags$th("Qu\u00e9 mide"))
          ),
          tags$tbody(Filter(Negate(is.null), filas))
        )
      }, error = function(e) {
        p(class = "small text-muted", "Ajusta el modelo primero.")
      })
    })

    output$tabla_icc <- renderUI({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        icc_  <- performance::icc(fm, verbose = FALSE)
        icc_v <- round(icc_$ICC_adjusted, 3)
        icc_p <- round(icc_v * 100, 1)
        res_p <- round((1 - icc_v) * 100, 1)
        label_mag <- if (icc_v < 0.1) list("Muy bajo",  colores$exito)  else
                     if (icc_v < 0.3) list("Bajo",      colores$acento) else
                     if (icc_v < 0.5) list("Moderado",  colores$primario) else
                                      list("Alto",       colores$peligro)
        tagList(
          div(class = "text-center mb-3",
              h2(style = paste0("color:", label_mag[[2]], "; font-weight:700; margin:0;"),
                 paste0(icc_p, "%")),
              p(class = "small mb-0",
                tags$span(class = "badge",
                          style = paste0("background:", label_mag[[2]]),
                          label_mag[[1]]),
                tags$span(class = "text-muted ms-1", "— ICC ajustado = ", icc_v))),
          p(class = "small fw-bold mb-1", "Partici\u00f3n de la varianza (ICC):"),
          div(style = "height:32px; border-radius:6px; overflow:hidden; display:flex; width:100%; margin-bottom:4px;",
              div(style = paste0("width:", icc_p, "%; background:", colores$primario,
                                 "; display:flex; align-items:center; justify-content:center;"),
                  if (icc_p >= 10)
                    tags$span(style = "color:#fff; font-size:0.75rem; font-weight:600;",
                               paste0(icc_p, "%"))),
              div(style = paste0("width:", res_p, "%; background:#CBD5E1;",
                                 " display:flex; align-items:center; justify-content:center;"),
                  if (res_p >= 10)
                    tags$span(style = "color:#334155; font-size:0.75rem; font-weight:600;",
                               paste0(res_p, "%")))),
          div(class = "d-flex gap-3 mb-3",
              div(class = "d-flex align-items-center gap-1",
                  div(style = paste0("width:12px; height:12px; border-radius:3px; background:", colores$primario, ";")),
                  tags$span(class = "small text-muted", paste0("Entre grupos (", icc_p, "%)"))),
              div(class = "d-flex align-items-center gap-1",
                  div(style = "width:12px; height:12px; border-radius:3px; background:#CBD5E1;"),
                  tags$span(class = "small text-muted", paste0("Dentro de grupos (", res_p, "%)")))))
      }, error = function(e) p(class = "small text-muted", "ICC no disponible."))
    })

    output$plot_icc <- renderPlot({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        r2  <- suppressWarnings(performance::r2_nakagawa(fm, verbose = FALSE))
        r2m <- r2$R2_marginal; r2c <- r2$R2_conditional
        ale <- r2c - r2m; res <- 1 - r2c
        df_bar <- data.frame(
          componente = factor(
            c("Efectos fijos\n(R\u00b2 marginal)",
              "Efectos aleatorios\n(ICC \u2248 R\u00b2c \u2212 R\u00b2m)",
              "Residual\n(no explicado)"),
            levels = c("Efectos fijos\n(R\u00b2 marginal)",
                       "Efectos aleatorios\n(ICC \u2248 R\u00b2c \u2212 R\u00b2m)",
                       "Residual\n(no explicado)")),
          valor = c(r2m, ale, res),
          pct   = round(c(r2m, ale, res) * 100, 1))
        ggplot2::ggplot(df_bar,
                        ggplot2::aes(x = 1, y = valor, fill = componente)) +
          ggplot2::geom_col(width = 0.5) +
          ggplot2::geom_text(ggplot2::aes(label = paste0(pct, "%")),
                             position = ggplot2::position_stack(vjust = 0.5),
                             color = "white", size = 3.5, fontface = "bold") +
          ggplot2::coord_flip() +
          ggplot2::scale_fill_manual(
            values = c(colores$primario, colores$acento, "#CBD5E1"), name = NULL) +
          ggplot2::scale_y_continuous(labels = scales::percent_format()) +
          ggplot2::labs(x = NULL, y = NULL,
                        subtitle = "Descomposici\u00f3n R\u00b2 Nakagawa") +
          ggplot2::theme_minimal(base_size = 10) +
          ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank(),
                         panel.grid = ggplot2::element_blank(),
                         legend.position = "bottom",
                         legend.text = ggplot2::element_text(size = 7))
      }, error = function(e) ggplot2::ggplot() + ggplot2::theme_void())
    }, res = 96)

    output$interp_nakagawa <- renderUI({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        r2   <- suppressWarnings(performance::r2_nakagawa(fm, verbose = FALSE))
        r2m  <- round(r2$R2_marginal,    3)
        r2c  <- round(r2$R2_conditional, 3)
        ale  <- round(r2c - r2m, 3)
        res  <- round(1 - r2c, 3)
        r2m_p <- round(r2m * 100, 1)
        ale_p <- round(ale * 100, 1)
        res_p <- round(res * 100, 1)
        justifica <- ale_p >= 10
        div(class = "card border-0 mt-2",
            style = paste0("background:", colores$fondo, ";"),
            div(class = "card-body p-2",
                p(class = "small fw-bold mb-2",
                  bs_icon("info-circle-fill", class = "me-1"),
                  "Interpretaci\u00f3n"),
                tags$ul(class = "small mb-2",
                  tags$li(strong(paste0(r2m_p, "% \u2014 efectos fijos: ")),
                          "varianza explicada por los predictores"),
                  tags$li(strong(paste0(ale_p, "% \u2014 efectos aleatorios: ")),
                          "varianza adicional por diferencias entre grupos"),
                  tags$li(strong(paste0(res_p, "% \u2014 residual: ")),
                          "varianza no explicada")),
                div(class = paste0("alert small py-2 px-3 mb-0 ",
                                   if (justifica) "alert-success" else "alert-warning"),
                    if (justifica)
                      tagList(bs_icon("check-circle-fill", class = "me-1"),
                              strong("\u00bfJustifica el GLMM? S\u00ed \u2014 "),
                              "los efectos aleatorios aportan ", strong(paste0(ale_p, "%")),
                              " de varianza adicional.")
                    else
                      tagList(bs_icon("exclamation-triangle-fill", class = "me-1"),
                              strong("\u00bfJustifica el GLMM? Marginal \u2014 "),
                              "los efectos aleatorios solo aportan ", strong(paste0(ale_p, "%")),
                              ". Compara con GLM simple."))))
      }, error = function(e) NULL)
    })

    # ────────────────────────────────────────────────────
    # PARÁMETROS
    # ────────────────────────────────────────────────────

    output$params_intro <- renderUI({
      fm <- modelo_glmm(); req(fm)
      fam <- input$familia
      tagList(
        div(class = "alert alert-info small py-2 px-3 mb-3",
            bs_icon("info-circle", class = "me-1"),
            "Los coeficientes est\u00e1n en la ", strong("escala del enlace"),
            switch(fam,
              "binomial" = tagList(" (log-odds). Exponencia para obtener ", strong("odds ratio (OR)")),
              tagList(" (log). Exponencia para obtener ", strong("raz\u00f3n de tasas (IRR)"))),
            ". Ver tabla de transformaci\u00f3n m\u00e1s abajo.")
      )
    })

    output$tabla_efectos_fijos <- renderUI({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        coefs <- summary(fm)$coefficients$cond
        df    <- as.data.frame(coefs)
        df$Parameter <- rownames(df)
        names(df)[1:4] <- c("Estimate", "Std_Error", "z_value", "p_value")

        filas <- lapply(seq_len(nrow(df)), function(i) {
          p    <- df$p_value[i]
          p_txt <- if (!is.na(p)) {
            if (p < 0.001) "< 0.001 ***" else
            if (p < 0.01)  paste0(round(p,3), " **")  else
            if (p < 0.05)  paste0(round(p,3), " *")   else
            if (p < 0.1)   paste0(round(p,3), " .")   else
                            as.character(round(p,3))
          } else "\u2014"
          col_p <- if (!is.na(p) && p < 0.05) colores$exito else colores$texto
          tags$tr(
            tags$td(strong(df$Parameter[i])),
            tags$td(style = "text-align:right; font-family:monospace;",
                    round(df$Estimate[i], 4)),
            tags$td(style = "text-align:right; font-family:monospace;",
                    round(df$Std_Error[i], 4)),
            tags$td(style = paste0("color:", col_p, "; font-weight:600; text-align:center;"),
                    p_txt)
          )
        })
        tagList(
          tags$table(
            class = "table table-sm table-hover small mb-2",
            tags$thead(
              style = paste0("background:", colores$primario, "; color:#fff;"),
              tags$tr(tags$th("Par\u00e1metro"),
                      tags$th(style="text-align:right;", "Estimado"),
                      tags$th(style="text-align:right;", "EE"),
                      tags$th(style="text-align:center;", "p-valor"))),
            tags$tbody(filas)),
          div(class = "small text-muted",
              style = "font-family:monospace; font-size:0.75rem;",
              "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"))
      }, error = function(e) p(class = "small text-muted", "Ajusta el modelo primero."))
    })

    output$tabla_efectos_aleatorios <- renderUI({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        vc   <- as.data.frame(glmmTMB::VarCorr(fm)$cond)
        filas <- lapply(seq_len(nrow(vc)), function(i) {
          grp   <- vc$grp[i]
          var1  <- if (!is.na(vc$var1[i])) vc$var1[i] else "\u2014"
          vcov  <- round(vc$vcov[i],  3)
          sdcor <- round(vc$sdcor[i], 3)
          label <- if (grp == "Residual")
            "Variaci\u00f3n dentro de grupos (no explicada)"
          else
            paste0("Variabilidad entre grupos (", grp, ")")
          tags$tr(
            tags$td(code(grp)),
            tags$td(style="text-align:center;", var1),
            tags$td(style="text-align:right; font-family:monospace;", vcov),
            tags$td(style=paste0("text-align:right; font-family:monospace;",
                                 " font-weight:600; color:", colores$primario, ";"),
                    sdcor),
            tags$td(class = "small text-muted", label))
        })
        tagList(
          tags$table(
            class = "table table-sm small mb-2",
            tags$thead(
              style = paste0("background:", colores$primario, "; color:#fff;"),
              tags$tr(tags$th("Grupo"), tags$th(style="text-align:center;","Variable"),
                      tags$th(style="text-align:right;","Varianza (\u03c3\u00b2)"),
                      tags$th(style="text-align:right;","SD"),
                      tags$th("Significado"))),
            tags$tbody(filas)),
          div(class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("info-circle", class = "me-1"),
              "ICC = \u03c3\u00b2(grupo) / [\u03c3\u00b2(grupo) + \u03c3\u00b2(residual)] \u2014 ver pesta\u00f1a ",
              strong("Performance"), "."))
      }, error = function(e) p(class = "small text-muted", "Ajusta el modelo primero."))
    })

    output$transformed_header <- renderUI({
      fam <- input$familia
      switch(fam,
        "binomial" = tagList(bs_icon("arrow-left-right", class = "me-1"),
                             "Odds Ratio (OR) \u2014 exp(\u03b2)"),
        tagList(bs_icon("arrow-left-right", class = "me-1"),
                "Raz\u00f3n de tasas (IRR) \u2014 exp(\u03b2)"))
    })

    output$tabla_transformada <- renderUI({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        coefs <- summary(fm)$coefficients$cond
        df    <- as.data.frame(coefs)
        df$Parameter <- rownames(df)
        names(df)[1:4] <- c("Estimate", "Std_Error", "z_value", "p_value")
        df <- df[!grepl("Intercept", df$Parameter), , drop = FALSE]
        if (nrow(df) == 0) return(p(class = "small text-muted",
                                    "No hay predictores para transformar."))
        fam   <- input$familia
        label <- if (fam == "binomial") "OR" else "IRR"
        filas <- lapply(seq_len(nrow(df)), function(i) {
          est   <- df$Estimate[i]
          ee    <- df$Std_Error[i]
          trans <- round(exp(est), 3)
          ci_l  <- round(exp(est - 1.96 * ee), 3)
          ci_h  <- round(exp(est + 1.96 * ee), 3)
          p     <- df$p_value[i]
          col_p <- if (!is.na(p) && p < 0.05) colores$exito else colores$texto
          tags$tr(
            tags$td(strong(df$Parameter[i])),
            tags$td(style = paste0("text-align:right; font-family:monospace;",
                                   " font-weight:700; color:", col_p, ";"),
                    trans),
            tags$td(style = "text-align:right; font-family:monospace;",
                    paste0("[", ci_l, ", ", ci_h, "]")),
            tags$td(class = "small text-muted",
                    if (fam == "binomial") {
                      if (trans > 1) paste0("Aumenta la probabilidad ", round((trans-1)*100,1), "%")
                      else paste0("Reduce la probabilidad ", round((1-trans)*100,1), "%")
                    } else {
                      if (trans > 1) paste0("El conteo esperado aumenta ", round((trans-1)*100,1), "%")
                      else paste0("El conteo esperado reduce ", round((1-trans)*100,1), "%")
                    })
          )
        })
        tagList(
          p(class = "small text-muted mb-2",
            "exp(\u03b2) transforma los coeficientes de la escala del enlace ",
            "a la escala original: ",
            strong(if (fam == "binomial") "OR > 1 aumenta la probabilidad del evento"
                   else "IRR > 1 aumenta el conteo esperado"), "."),
          tags$table(
            class = "table table-sm table-hover small mb-0",
            tags$thead(
              style = paste0("background:", colores$primario, "; color:#fff;"),
              tags$tr(tags$th("Predictor"),
                      tags$th(style="text-align:right;", label),
                      tags$th(style="text-align:right;", "IC 95%"),
                      tags$th("Interpretaci\u00f3n"))),
            tags$tbody(filas)))
      }, error = function(e) p(class = "small text-muted", "Ajusta el modelo primero."))
    })

    output$plot_importancia <- renderPlot({
      fm <- modelo_glmm(); req(fm)
      tryCatch({
        coefs <- summary(fm)$coefficients$cond
        df    <- as.data.frame(coefs)
        df$Parameter <- rownames(df)
        names(df)[1:4] <- c("Estimate", "Std_Error", "z_value", "p_value")
        df    <- df[!grepl("Intercept", df$Parameter), , drop = FALSE]
        if (nrow(df) == 0) stop("sin_preds")
        df_orig <- fm$frame
        coefs_std <- sapply(df$Parameter, function(nm) {
          if (nm %in% names(df_orig) && is.numeric(df_orig[[nm]])) {
            sd_x <- sd(df_orig[[nm]], na.rm = TRUE)
            if (sd_x > 0) df[df$Parameter == nm, "Estimate"] * sd_x
            else df[df$Parameter == nm, "Estimate"]
          } else df[df$Parameter == nm, "Estimate"]
        })
        df$Coef_std <- as.numeric(coefs_std)
        df$abs_est  <- abs(df$Coef_std)
        df$dir      <- ifelse(df$Coef_std >= 0, "Positivo", "Negativo")
        df$Parameter <- factor(df$Parameter,
                                levels = df$Parameter[order(df$abs_est)])
        ggplot2::ggplot(df, ggplot2::aes(x = abs_est, y = Parameter, fill = dir)) +
          ggplot2::geom_col(width = 0.65) +
          ggplot2::scale_fill_manual(
            values = c("Positivo" = colores$primario, "Negativo" = colores$peligro),
            name = "Direcci\u00f3n") +
          ggplot2::labs(x = "|\u03b2 estandarizado|", y = NULL,
                        subtitle = "Efectos fijos estandarizados (post-hoc)") +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         panel.grid.major.y = ggplot2::element_blank(),
                         legend.position = "bottom")
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label = "Agrega predictores y ajusta el modelo.",
                            color = colores$texto, size = 3.5, hjust = 0.5) +
          ggplot2::theme_void()
      })
    }, res = 96)

    # ────────────────────────────────────────────────────
    # EFECTOS MARGINALES
    # ────────────────────────────────────────────────────

    output$sel_pred_marginal <- renderUI({
      fm <- modelo_glmm(); req(fm)
      preds <- c(input$preds_num, input$preds_cat)
      req(length(preds) > 0)
      selectInput(ns("pred_marginal"), "Predictor focal:",
                  choices = preds, selected = preds[1])
    })

    output$marginal_valores_tipicos <- renderUI({
      fm <- modelo_glmm(); req(fm)
      df <- datos_activos(); req(df)
      preds <- c(input$preds_num, input$preds_cat)
      req(length(preds) > 1, input$pred_marginal)
      otros <- preds[preds != input$pred_marginal]
      div(
        p(class = "small fw-bold mb-1", "Resto en sus valores t\u00edpicos:"),
        lapply(otros, function(nm) {
          if (is.numeric(df[[nm]])) {
            p(class = "small text-muted mb-0",
              code(nm), " = ", round(mean(df[[nm]], na.rm = TRUE), 2),
              " (media)")
          } else {
            p(class = "small text-muted mb-0",
              code(nm), " = ", levels(df[[nm]])[1], " (nivel base)")
          }
        })
      )
    })

    output$plot_marginal <- renderPlot({
      fm <- modelo_glmm(); req(fm, input$pred_marginal)
      tryCatch({
        rel <- modelbased::estimate_relation(
          fm, at = input$pred_marginal, length = 100)
        df_r <- as.data.frame(rel)
        p <- ggplot2::ggplot(df_r,
               ggplot2::aes(x = .data[[input$pred_marginal]],
                            y = Predicted)) +
          ggplot2::geom_line(color = colores$primario, linewidth = 1.2)
        if (isTRUE(input$marginal_ci) && "CI_low" %in% names(df_r))
          p <- p + ggplot2::geom_ribbon(
            ggplot2::aes(ymin = CI_low, ymax = CI_high),
            fill = colores$primario, alpha = 0.15)
        if (isTRUE(input$marginal_puntos)) {
          df_pts <- datos_activos()
          req(df_pts, input$var_y)
          p <- p + ggplot2::geom_point(
            data = df_pts,
            ggplot2::aes(x = .data[[input$pred_marginal]],
                         y = .data[[input$var_y]]),
            alpha = 0.35, size = 1.8, color = colores$texto,
            inherit.aes = FALSE)
        }
        p + ggplot2::labs(x = input$pred_marginal,
                          y = paste0(input$var_y, " (escala original)")) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label = "Ajusta el modelo primero.",
                            color = colores$texto, size = 3.5) +
          ggplot2::theme_void()
      })
    }, res = 96)

    output$marginal_interpretacion <- renderUI({
      fm <- modelo_glmm(); req(fm, input$pred_marginal)
      tryCatch({
        fam <- input$familia
        div(class = "alert alert-info small py-2 px-3 mb-0",
            bs_icon("lightbulb", class = "me-1"),
            "El gr\u00e1fico muestra el ", strong("efecto marginal"),
            " de ", code(input$pred_marginal),
            " en la escala original de Y ",
            switch(fam,
              "binomial" = "(probabilidad de presencia)",
              "(conteo esperado)"),
            ", manteniendo el resto de predictores en sus valores t\u00edpicos.")
      }, error = function(e) NULL)
    })

    # ────────────────────────────────────────────────────
    # CONTRASTES
    # ────────────────────────────────────────────────────

    output$contrasts_no_cat_msg <- renderUI({
      cats <- vars_categoricas()
      preds_cat <- input$preds_cat
      if (is.null(preds_cat) || length(preds_cat) == 0)
        div(class = "alert alert-warning small py-2 px-3 mb-3",
            bs_icon("exclamation-triangle", class = "me-1"),
            "Agrega al menos una variable categ\u00f3rica como predictor en Ajustar modelo.")
    })

    output$sel_var_contraste <- renderUI({
      req(input$preds_cat, length(input$preds_cat) > 0)
      selectInput(ns("var_contraste"), "Variable categ\u00f3rica:",
                  choices = input$preds_cat, selected = input$preds_cat[1])
    })

    output$tabla_contrastes <- renderUI({
      fm <- modelo_glmm()
      req(fm, input$var_contraste)
      tryCatch({
        cont <- modelbased::estimate_contrasts(
          fm, contrast = input$var_contraste,
          p_adjust = input$metodo_ajuste)
        df <- as.data.frame(cont)
        filas <- lapply(seq_len(nrow(df)), function(i) {
          p     <- df$p[i]
          sig   <- if (!is.na(p)) {
            if (p < 0.001) "***" else if (p < 0.01) "**" else
            if (p < 0.05)  "*"   else if (p < 0.1)  "."  else " "
          } else ""
          col_p <- if (!is.na(p) && p < 0.05) colores$exito else colores$texto
          tags$tr(
            tags$td(paste(df$Level1[i], "vs", df$Level2[i])),
            tags$td(style="text-align:right; font-family:monospace;",
                    round(df$Difference[i], 4)),
            tags$td(style="text-align:right; font-family:monospace;",
                    paste0("[", round(df$CI_low[i],3), ", ",
                           round(df$CI_high[i],3), "]")),
            tags$td(style=paste0("color:",col_p,"; font-weight:600; text-align:center;"),
                    paste0(round(p,4), " ", sig)))
        })
        tags$table(
          class = "table table-sm small mb-0",
          tags$thead(
            style = paste0("background:", colores$primario, "; color:#fff;"),
            tags$tr(tags$th("Contraste"), tags$th("Diferencia"),
                    tags$th("IC 95%"), tags$th("p-valor"))),
          tags$tbody(filas))
      }, error = function(e) p(class = "small text-muted", "Ajusta el modelo primero."))
    })

    output$plot_contrastes <- renderPlot({
      fm <- modelo_glmm()
      req(fm, input$var_contraste)
      tryCatch({
        cont <- modelbased::estimate_contrasts(
          fm, contrast = input$var_contraste,
          p_adjust = input$metodo_ajuste)
        df <- as.data.frame(cont)
        df$contraste <- paste(df$Level1, "vs", df$Level2)
        df$sig       <- df$p < 0.05
        ggplot2::ggplot(df, ggplot2::aes(x = Difference, y = contraste,
                                         color = sig)) +
          ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                              color = "gray60") +
          ggplot2::geom_errorbarh(ggplot2::aes(xmin = CI_low, xmax = CI_high),
                                  height = 0.25, linewidth = 0.8) +
          ggplot2::geom_point(size = 3) +
          ggplot2::scale_color_manual(values = c("FALSE" = "gray60",
                                                  "TRUE"  = colores$primario),
                                       guide = "none") +
          ggplot2::labs(x = "Diferencia", y = NULL) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
      }, error = function(e) ggplot2::ggplot() + ggplot2::theme_void())
    }, res = 96)

    # ────────────────────────────────────────────────────
    # COMPARAR MODELOS
    # ────────────────────────────────────────────────────

    output$lista_modelos_guardados <- renderUI({
      lista <- modelos_guardados()
      if (length(lista) == 0)
        return(p(class = "small text-muted",
                 "No hay modelos guardados. Ajusta y guarda modelos en la pesta\u00f1a ",
                 strong("Ajustar modelo"), "."))
      tagList(lapply(names(lista), function(nm) {
        fm   <- lista[[nm]]
        fam  <- tryCatch(family(fm)$family, error = function(e) "?")
        div(class = "d-flex align-items-center justify-content-between mb-1",
            div(bs_icon("check-circle-fill",
                        style = paste0("color:", colores$exito, "; margin-right:6px;")),
                strong(nm),
                tags$span(class = "text-muted small ms-1",
                          paste0("(", fam, ")"))))
      }))
    })

    output$tabla_comparacion <- renderUI({
      lista <- modelos_guardados()
      if (length(lista) < 2)
        return(p(class = "small text-muted", "Guarda al menos 2 modelos para comparar."))
      tryCatch({
        comp <- do.call(performance::compare_performance,
                        c(unname(lista), list(verbose = FALSE)))
        df   <- as.data.frame(comp)
        cols_show <- intersect(c("Name","AIC","AICc","BIC","R2_marginal",
                                  "R2_conditional","ICC","RMSE"), names(df))
        df_show <- df[, cols_show, drop = FALSE]
        mejor_aic <- df_show$Name[which.min(df_show$AIC)]
        filas <- lapply(seq_len(nrow(df_show)), function(i) {
          es_mejor <- df_show$Name[i] == mejor_aic
          tags$tr(
            style = if (es_mejor)
              paste0("background:", colores$fondo, "; font-weight:600;") else "",
            lapply(cols_show, function(col) {
              val <- df_show[i, col]
              if (is.numeric(val)) val <- round(val, 3)
              tags$td(style = "text-align:right;",
                      if (col == "Name") strong(val) else val)
            })
          )
        })
        tags$table(
          class = "table table-sm small mb-0",
          tags$thead(
            style = paste0("background:", colores$primario, "; color:#fff;"),
            tags$tr(lapply(cols_show, tags$th))),
          tags$tbody(filas))
      }, error = function(e) p(class = "small text-muted", conditionMessage(e)))
    })

    output$plot_comparacion <- renderPlot({
      lista <- modelos_guardados()
      if (length(lista) < 2) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="Guarda al menos 2 modelos para comparar.",
                            color = colores$texto, size = 3.5) +
          ggplot2::theme_void()
        return()
      }
      tryCatch({
        comp <- do.call(performance::compare_performance,
                        c(unname(lista), list(verbose = FALSE)))
        plot(comp)
      }, error = function(e) ggplot2::ggplot() + ggplot2::theme_void())
    }, res = 96)

    # ────────────────────────────────────────────────────
    # CÓDIGO R
    # ────────────────────────────────────────────────────

    codigo_generado <- reactive({
      fm <- modelo_glmm()
      if (is.null(fm)) return(
        "# Ajusta el modelo en la pestaña 'Ajustar modelo' antes de generar el código.\n")
      fam      <- input$familia
      re       <- tryCatch(formula_re(), error = function(e) "(1 | grupo)")
      preds    <- c(input$preds_num, input$preds_cat)
      parte_fija <- if (length(preds) > 0) paste(preds, collapse = " + ") else "1"
      offset_str <- if (!is.null(input$offset_var) && nchar(input$offset_var) > 0)
        paste0(" + offset(log(", input$offset_var, "))") else ""
      fml_str  <- paste0(input$var_y, " ~ ", parte_fija, offset_str, " + ", re)
      zi_str   <- if (fam %in% c("zip", "zinb")) "ziformula = ~1," else ""
      fam_str  <- switch(fam,
        "poisson"  = "poisson()",
        "nbinom2"  = "nbinom2()",
        "binomial" = "binomial()",
        "zip"      = "poisson()",
        "zinb"     = "nbinom2()")
      datos_str <- switch(fuente,
        "propio"     = "datos <- read.csv(\"mi_archivo.csv\")",
        "aves_glmm"  = paste0(
          "# Datos: aves_glmm (simulado, StatModels)\n",
          "# Variables: conteo, area_ha, cobertura_dosel, dist_borde, ndvi, fragmento\n",
          "data(aves_glmm, package = \"StatModels\")"),
        "ranas_glmm" = paste0(
          "# Datos: ranas_glmm (simulado, StatModels)\n",
          "# Variables: presencia, hidroperiodo, cobertura_vegetal, dist_bosque, ph, charca\n",
          "data(ranas_glmm, package = \"StatModels\")"),
        paste0("datos <- datos_activos  # dataset activo")
      )
      paste0(
        "# ── GLMM con glmmTMB ──────────────────────────────────────\n",
        "# Generado con StatModels · StatSuite\n\n",
        "library(glmmTMB)\n",
        "library(parameters)   # model_parameters()\n",
        "library(performance)   # check_model(), icc(), r2_nakagawa()\n",
        "library(modelbased)    # estimate_relation(), estimate_contrasts()\n\n",
        "# Datos\n",
        datos_str, "\n\n",
        "# Ajustar modelo\n",
        "modelo <- glmmTMB(\n",
        "  formula   = ", fml_str, ",\n",
        if (nchar(zi_str) > 0) paste0("  ", zi_str, "\n") else "",
        "  family    = ", fam_str, ",\n",
        "  data      = datos\n",
        ")\n\n",
        "# Resumen\n",
        "summary(modelo)\n\n",
        "# Parámetros con easystats\n",
        "model_parameters(modelo)\n\n",
        "# Performance\n",
        "model_performance(modelo)\n",
        "icc(modelo)\n",
        "r2_nakagawa(modelo)\n\n",
        "# Diagnóstico\n",
        "check_model(modelo)\n",
        "check_overdispersion(modelo)\n",
        "check_zeroinflation(modelo)\n\n",
        "# Efectos marginales\n",
        "estimate_relation(modelo)\n\n",
        "# Contrastes (si hay variables categóricas)\n",
        "# estimate_contrasts(modelo, contrast = 'mi_variable_cat')\n"
      )
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descargar_codigo <- downloadHandler(
      filename = function() paste0("glmm_", Sys.Date(), ".R"),
      content  = function(file) {
        texto <- tryCatch(codigo_generado(), error = function(e) {
          paste0("# Error al generar el código: ", conditionMessage(e), "\n")
        })
        writeLines(texto, con = file, useBytes = FALSE)
      }
    )

  }) # /moduleServer
} # /mod_glmm_server
