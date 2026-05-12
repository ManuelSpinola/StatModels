# ============================================================
# mod_glm.R — Modelo lineal generalizado (GLM)
# StatModels · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Familias: binomial (logística), Poisson, binomial negativa
# Motor: glmmTMB vía tidymodels
# Ecosistema: easystats (parameters, performance, modelbased)
#
# Filosofía: didáctico, sin conocimiento previo de programación
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_glm_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("toggles", class = "me-2"),
        "Modelo lineal generalizado (GLM)",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Extiende el modelo lineal a variables respuesta que no siguen ",
        "una distribución normal: proporciones, presencia/ausencia, ",
        "conteos, tiempos. Misma lógica que el LM pero con una ",
        strong("función de enlace"), " y una ", strong("familia"),
        " de distribución apropiada para cada tipo de datos."
      )
    ),

    navset_card_tab(

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "¿Qué es?"),
        card_body(

          h5(style = paste0("color:", colores$primario,
                            "; font-weight:700;"),
             "Del modelo lineal al modelo lineal generalizado"),
          p(class = "small text-muted mb-3",
            "El modelo lineal general (LM) asume que Y es numérica y ",
            "continua con errores normales. El GLM relaja esa restricción ",
            "permitiendo que Y siga otras distribuciones. El cambio clave ",
            "es la ", strong("función de enlace (link function)"),
            " — una transformación matemática que conecta los predictores ",
            "con la escala de Y."
          ),

          # ── Tabla de familias ─────────────────────────
          h5(style = paste0("color:", colores$primario,
                            "; font-weight:700;"),
             "Familias del GLM"),

          div(
            style = "overflow-x: auto;",
            tags$table(
              class = "table table-sm table-bordered small mb-0",
              style = "background: #ffffff;",
              tags$thead(
                tags$tr(
                  tags$th(style = paste0(
                    "background:", colores$primario,
                    " !important; color:#fff !important; padding:8px;"
                  ), "Familia"),
                  tags$th(style = paste0(
                    "background:", colores$primario,
                    " !important; color:#fff !important; padding:8px;"
                  ), "Variable Y"),
                  tags$th(style = paste0(
                    "background:", colores$primario,
                    " !important; color:#fff !important; padding:8px;"
                  ), "Enlace"),
                  tags$th(style = paste0(
                    "background:", colores$primario,
                    " !important; color:#fff !important; padding:8px;"
                  ), "Parámetro"),
                  tags$th(style = paste0(
                    "background:", colores$primario,
                    " !important; color:#fff !important; padding:8px;"
                  ), "Ejemplo")
                )
              ),
              tags$tbody(
                # Binomial
                tags$tr(
                  tags$td(tags$span(class = "badge",
                                    style = paste0("background:", colores$primario),
                                    "Binomial (logística)")),
                  tags$td("Binaria (0/1, sí/no)"),
                  tags$td(tags$code("logit")),
                  tags$td("Odds ratio (OR)"),
                  tags$td("¿Presencia de especie? ¿Tiene diabetes?")
                ),
                # Poisson
                tags$tr(style = paste0("background:", colores$fondo),
                        tags$td(tags$span(class = "badge",
                                          style = paste0("background:", colores$acento),
                                          "Poisson")),
                        tags$td("Conteos (0, 1, 2, …)"),
                        tags$td(tags$code("log")),
                        tags$td("Tasa de incidencia (IRR)"),
                        tags$td("¿Cuántas especies? ¿Cuántos casos?")
                ),
                # Binomial negativa
                tags$tr(
                  tags$td(tags$span(class = "badge",
                                    style = paste0("background:", colores$peligro),
                                    "Binomial negativa")),
                  tags$td("Conteos con sobredispersión"),
                  tags$td(tags$code("log")),
                  tags$td("Tasa de incidencia (IRR)"),
                  tags$td("Conteos con alta variabilidad")
                ),

              )
            )
          ),

          tags$hr(),

          # ── Función de enlace ─────────────────────────
          h5(style = paste0("color:", colores$primario,
                            "; font-weight:700;"),
             "La función de enlace"),
          p(class = "small text-muted mb-3",
            "La función de enlace transforma la escala de Y para que la ",
            "relación con los predictores sea lineal. Por ejemplo, en ",
            "regresión logística el enlace ", tags$code("logit"),
            " transforma probabilidades (0-1) a log-odds (-∞, +∞). ",
            "Esto significa que los coeficientes se interpretan en la ",
            "escala del enlace, no en la escala original de Y."
          ),

          layout_columns(
            col_widths = c(6, 6),

            div(
              class = "card-muestreo",
              style = paste0("border-left: 4px solid ",
                             colores$primario, ";"),
              p(class = "small mb-1",
                strong("Regresión logística — enlace logit")),
              p(class = "small text-muted mb-1",
                "El modelo predice el ", strong("log-odds"),
                " de que Y = 1. Para interpretar en escala original, ",
                "exponenciamos: exp(β) = ",
                strong("odds ratio (OR)"), "."),
              p(class = "small mb-0",
                "OR > 1 → aumenta la probabilidad del evento.", br(),
                "OR < 1 → disminuye la probabilidad del evento.", br(),
                "OR = 1 → sin efecto.")
            ),

            div(
              class = "card-muestreo",
              style = paste0("border-left: 4px solid ",
                             colores$acento, ";"),
              p(class = "small mb-1",
                strong("Poisson / BN — enlace log")),
              p(class = "small text-muted mb-1",
                "El modelo predice el ", strong("logaritmo del conteo"),
                " esperado. Para interpretar en escala original, ",
                "exponenciamos: exp(β) = ",
                strong("razón de tasas de incidencia (IRR)"), "."),
              p(class = "small mb-0",
                "IRR > 1 → aumenta el conteo esperado.", br(),
                "IRR < 1 → disminuye el conteo esperado.", br(),
                "IRR = 1 → sin efecto.")
            )
          ),

          tags$hr(),

          # ── Cuándo NO usar GLM ────────────────────────
          h5(style = paste0("color:", colores$primario,
                            "; font-weight:700;"),
             "¿Cuándo ir más allá del GLM?"),

          layout_columns(
            col_widths = c(4, 4, 4),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("x-circle-fill", class = "me-2",
                      style = paste0("color:", colores$peligro)),
              strong("Datos agrupados o repetidos"), br(),
              "Usa ", strong("modelos mixtos (GLMM)"),
              " para manejar la estructura jerárquica."
            ),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("x-circle-fill", class = "me-2",
                      style = paste0("color:", colores$peligro)),
              strong("Relación no lineal"), br(),
              "Usa ", strong("GAM"),
              " si la relación entre X e Y no puede linealizarse."
            ),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("x-circle-fill", class = "me-2",
                      style = paste0("color:", colores$peligro)),
              strong("Exceso de ceros"), br(),
              "Considera modelos ", strong("zero-inflated"),
              " o hurdle si hay demasiados ceros en los conteos."
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 2: Fundamentos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("journal-bookmark", class = "me-1"),
                        "Fundamentos"),
        card_body(

          h5(style = paste0("color:", colores$primario,
                            "; font-weight:700;"),
             "Supuestos del GLM"),
          p(class = "small text-muted mb-3",
            "El GLM relaja los supuestos del LM. Ya no necesitamos ",
            "normalidad ni homocedasticidad de los residuos — pero ",
            "hay nuevos supuestos que verificar."
          ),

          # Supuesto 1: Distribución correcta
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ",
                           colores$primario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("toggles",
                        style = paste0("color:", colores$primario,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$primario,
                                  "; font-weight:700;"),
                   "1. Distribución correcta de la familia")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("¿Qué significa?")),
                p(class = "small text-muted mb-0",
                  "La familia elegida debe corresponder al tipo de Y. ",
                  "Usar Poisson para datos binarios o binomial para ",
                  "conteos produce estimaciones incorrectas.")
              ),
              div(
                p(class = "small mb-1", strong("¿Cómo verificarlo?")),
                p(class = "small text-muted mb-1",
                  strong("check_distribution()"),
                  " de performance sugiere la familia más apropiada ",
                  "basándose en la distribución de Y."),
                div(class = "alert alert-warning small py-1 px-2 mb-0",
                    bs_icon("exclamation-triangle", class = "me-1"),
                    strong("Si falla:"),
                    " cambia la familia en 'Construir el modelo'.")
              )
            )
          ),

          # Supuesto 2: Linealidad en escala del enlace
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ",
                           colores$secundario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("graph-up",
                        style = paste0("color:", colores$secundario,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$secundario,
                                  "; font-weight:700;"),
                   "2. Linealidad en la escala del enlace")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("¿Qué significa?")),
                p(class = "small text-muted mb-0",
                  "La relación entre los predictores y ",
                  strong("g(Y)"), " (Y transformada por el enlace) ",
                  "debe ser lineal. Por ejemplo, en logística la ",
                  "relación entre X y el log-odds debe ser lineal.")
              ),
              div(
                p(class = "small mb-1", strong("¿Cómo verificarlo?")),
                p(class = "small text-muted mb-1",
                  "Gráfico de residuos de Pearson vs. valores ajustados. ",
                  "Para logística: gráfico de residuos agrupados ",
                  strong("(binned residuals)"), "."),
                div(class = "alert alert-warning small py-1 px-2 mb-0",
                    bs_icon("exclamation-triangle", class = "me-1"),
                    strong("Si falla:"),
                    " transforma el predictor o usa GAM.")
              )
            )
          ),

          # Supuesto 3: Independencia
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ",
                           colores$peligro, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("link-45deg",
                        style = paste0("color:", colores$peligro,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$peligro,
                                  "; font-weight:700;"),
                   "3. Independencia de las observaciones")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("¿Qué significa?")),
                p(class = "small text-muted mb-0",
                  "Igual que en el LM: las observaciones no deben ",
                  "estar correlacionadas. Datos de múltiples ",
                  "individuos del mismo sitio, o medidas repetidas, ",
                  "violan este supuesto.")
              ),
              div(
                p(class = "small mb-1", strong("¿Cómo verificarlo?")),
                p(class = "small text-muted mb-1",
                  "Se garantiza con un buen diseño de muestreo. ",
                  "Si hay estructura jerárquica, usa GLMM."),
                div(class = "alert alert-warning small py-1 px-2 mb-0",
                    bs_icon("exclamation-triangle", class = "me-1"),
                    strong("Si falla:"),
                    " usa modelos mixtos (GLMM) con lme4 o glmmTMB.")
              )
            )
          ),

          # Supuesto 4: Sobredispersión (Poisson/BN)
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ",
                           colores$acento, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("arrows-expand",
                        style = paste0("color:", colores$acento,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$acento,
                                  "; font-weight:700;"),
                   "4. Sobredispersión (solo Poisson)")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("¿Qué significa?")),
                p(class = "small text-muted mb-0",
                  "En Poisson la varianza debe ser igual a la media. ",
                  "Si la varianza es mucho mayor (sobredispersión), ",
                  "los errores estándar son incorrectos y hay falsos ",
                  "positivos. La binomial negativa no tiene este problema.")
              ),
              div(
                p(class = "small mb-1", strong("¿Cómo verificarlo?")),
                p(class = "small text-muted mb-1",
                  strong("check_overdispersion()"),
                  " de performance. ",
                  "Estadístico de dispersión: suma de residuos de Pearson² ",
                  "/ grados de libertad. ",
                  "Valor > 1.5 indica sobredispersión."),
                div(class = "alert alert-warning small py-1 px-2 mb-0",
                    bs_icon("exclamation-triangle", class = "me-1"),
                    strong("Si falla:"),
                    " cambia a binomial negativa.")
              )
            )
          ),

          # Supuesto 5: Inflación de ceros
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ",
                           colores$texto, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("0-circle",
                        style = paste0("color:", colores$texto,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$texto,
                                  "; font-weight:700;"),
                   "5. Inflación de ceros (conteos)")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("¿Qué significa?")),
                p(class = "small text-muted mb-0",
                  "Si hay muchos más ceros de los esperados por la ",
                  "distribución elegida, el modelo subestima los ceros. ",
                  "Común en datos de presencia de especies, ",
                  "enfermedades raras, o comportamientos infrecuentes.")
              ),
              div(
                p(class = "small mb-1", strong("¿Cómo verificarlo?")),
                p(class = "small text-muted mb-1",
                  strong("check_zeroinflation()"),
                  " de performance compara los ceros observados vs. ",
                  "los predichos por el modelo."),
                div(class = "alert alert-warning small py-1 px-2 mb-0",
                    bs_icon("exclamation-triangle", class = "me-1"),
                    strong("Si falla:"),
                    " usa modelos zero-inflated (ZIP, ZINB) con glmmTMB.")
              )
            )
          ),

          div(
            class = "alert alert-warning small py-2 px-3 mb-0",
            bs_icon("lightbulb-fill", class = "me-2"),
            strong("Perspectiva práctica:"),
            " el supuesto más importante en GLM es elegir la familia ",
            "correcta. La independencia es crítica en todos los modelos. ",
            "La sobredispersión y los ceros excesivos son comunes en ",
            "datos ecológicos y de salud — la pestaña ",
            strong("Diagnóstico"), " los detecta automáticamente."
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

                card(
                  card_header(bs_icon("database", class = "me-1"),
                              "Fuente de datos"),
                  card_body(
                    # Paso 1: Tarjetas de familia
                    p(class = "small fw-bold text-muted mb-2",
                      bs_icon("toggles", class = "me-1"),
                      "¿Qué tipo de variable respuesta tienes?"),
                    tags$div(
                      style = paste0(
                        "display:grid; grid-template-columns:1fr 1fr 1fr;",
                        "gap:8px; margin-bottom:12px;"
                      ),

                      # Binomial
                      tags$div(
                        id = ns("card_binomial"),
                        style = paste0(
                          "background:#E6F1FB; border:2px solid #185FA5;",
                          "border-radius:10px; padding:10px 12px;",
                          "cursor:pointer;"
                        ),
                        onclick = paste0(
                          "Shiny.setInputValue('", ns("familia_datos"),
                          "', 'binomial', {priority:'event'})"
                        ),
                        tags$div(style="font-size:18px; color:#185FA5;",
                                 bs_icon("toggles")),
                        tags$p(style="font-size:13px; font-weight:500;",
                               "Binomial"),
                        tags$p(style="font-size:11px; color:#555; margin:0;",
                               "Y binaria (0/1, sí/no)"),
                        tags$span(
                          style=paste0(
                            "font-size:10px; background:#185FA5;",
                            "color:#fff; padding:1px 6px;",
                            "border-radius:4px;"
                          ), "logit")
                      ),

                      # Poisson
                      tags$div(
                        id = ns("card_poisson"),
                        style = paste0(
                          "background:var(--color-background-secondary);",
                          "border:1.5px solid var(--color-border-tertiary);",
                          "border-radius:10px; padding:10px 12px;",
                          "cursor:pointer;"
                        ),
                        onclick = paste0(
                          "Shiny.setInputValue('", ns("familia_datos"),
                          "', 'poisson', {priority:'event'})"
                        ),
                        tags$div(style="font-size:18px; color:#0F6E56;",
                                 bs_icon("bar-chart-steps")),
                        tags$p(style="font-size:13px; font-weight:500;",
                               "Poisson"),
                        tags$p(style="font-size:11px; color:#555; margin:0;",
                               "Y conteos (0, 1, 2, …)"),
                        tags$span(
                          style=paste0(
                            "font-size:10px; background:#0F6E56;",
                            "color:#fff; padding:1px 6px;",
                            "border-radius:4px;"
                          ), "log")
                      ),

                      # Binomial negativa
                      tags$div(
                        id = ns("card_nbinom2"),
                        style = paste0(
                          "background:var(--color-background-secondary);",
                          "border:1.5px solid var(--color-border-tertiary);",
                          "border-radius:10px; padding:10px 12px;",
                          "cursor:pointer;"
                        ),
                        onclick = paste0(
                          "Shiny.setInputValue('", ns("familia_datos"),
                          "', 'nbinom2', {priority:'event'})"
                        ),
                        tags$div(style="font-size:18px; color:#853F0B;",
                                 bs_icon("graph-up")),
                        tags$p(style="font-size:13px; font-weight:500;",
                               "Binomial negativa"),
                        tags$p(style="font-size:11px; color:#555; margin:0;",
                               "Conteos sobredispersados"),
                        tags$span(
                          style=paste0(
                            "font-size:10px; background:#853F0B;",
                            "color:#fff; padding:1px 6px;",
                            "border-radius:4px;"
                          ), "log")
                      ),

                    ),

                    # Input oculto para familia seleccionada
                    shinyjs::hidden(
                      textInput(ns("familia_datos"), label=NULL,
                                value="binomial")
                    ),

                    tags$hr(),
                    # Paso 2: Dataset según familia
                    uiOutput(ns("sel_fuente_datos")),
                    conditionalPanel(
                      condition = paste0("input['", ns("fuente_datos"),
                                         "'] === 'propio'"),
                      tags$hr(),
                      fileInput(
                        ns("archivo"),
                        label       = "Seleccionar archivo:",
                        accept      = c(".csv", ".xlsx", ".xls"),
                        buttonLabel = "Buscar\u2026",
                        placeholder = "CSV o Excel"
                      ),
                      selectInput(
                        ns("separador"),
                        label    = "Separador (CSV):",
                        choices  = c(
                          "Coma (,)"         = ",",
                          "Punto y coma (;)" = ";",
                          "Tabulador"        = "\\t"
                        ),
                        selected = ","
                      ),
                      p(class = "small text-muted mb-0",
                        bs_icon("info-circle", class = "me-1"),
                        "La primera fila debe contener los nombres ",
                        "de las columnas.")
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
                    card_header(bs_icon("eye", class = "me-1"),
                                "Vista previa"),
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
                "Las variables ", strong("cat\u00e9goricas"),
                " deben ser ", strong("Factor"), ". ",
                "Las variables codificadas como n\u00fameros pero que ",
                "representan grupos deben cambiarse a Factor antes de modelar."
              ),
              layout_columns(
                col_widths = c(10, 2),
                uiOutput(ns("tabla_tipos")),
                div(
                  class = "pt-2",
                  actionButton(
                    ns("aplicar_tipos"),
                    "Aplicar tipos",
                    class = "btn-primary w-100",
                    icon  = icon("check")
                  ),
                  br(), br(),
                  actionButton(
                    ns("resetear_tipos"),
                    "Restaurar",
                    class = "btn-outline-secondary w-100 btn-sm",
                    icon  = icon("rotate-left")
                  )
                )
              ),
              uiOutput(ns("tipos_aplicados_msg"))
            ),

            # Sub-tab 3: Explorar
            nav_panel(
              title = tagList(bs_icon("zoom-in", class = "me-1"),
                              "Explorar"),
              br(),
              layout_columns(
                col_widths = c(4, 8),
                card(
                  card_header(bs_icon("sliders", class = "me-1"),
                              "Controles"),
                  card_body(
                    uiOutput(ns("sel_var_x")),
                    uiOutput(ns("sel_color")),
                    checkboxInput(ns("mostrar_suavizado"),
                                  "Mostrar curva suavizada",
                                  value = TRUE),
                    tags$hr(),
                    uiOutput(ns("cards_correlacion"))
                  )
                ),
                div(
                  plotOutput(ns("plot_scatter"), height = "380px"),
                  uiOutput(ns("insight_scatter"))
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Construir el modelo
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("gear", class = "me-1"),
                        "Construir el modelo"),
        card_body(
          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("toggles", class = "me-1"),
                          "Especificar el modelo"),
              card_body(
                p(class = "small text-muted",
                  "Selecciona la variable respuesta, la familia de ",
                  "distribución y los predictores."),
                uiOutput(ns("sel_var_y")),
                tags$hr(),
                selectInput(
                  ns("familia"),
                  label = "Familia de distribución:",
                  choices = c(
                    "Binomial (logística)"    = "binomial",
                    "Poisson"                 = "poisson",
                    "Binomial negativa"       = "nbinom2"
                  ),
                  selected = "binomial"
                ),
                uiOutput(ns("sel_enlace")),
                uiOutput(ns("sel_offset")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  "Predictores numéricos"),
                uiOutput(ns("checks_numericos")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  "Predictores categóricos"),
                uiOutput(ns("checks_categoricos")),
                tags$hr(),
                # Interacciones
                conditionalPanel(
                  condition = paste0(
                    "(input['", ns("preds_num"), "'] !== null && ",
                    "input['", ns("preds_num"), "'].length + ",
                    "(input['", ns("preds_cat"), "'] !== null ? ",
                    "input['", ns("preds_cat"), "'].length : 0)) >= 2"
                  ),
                  div(
                    p(class = "small fw-bold text-muted mb-1",
                      bs_icon("diagram-2", class = "me-1"),
                      "Interacciones (opcional)"),
                    uiOutput(ns("checks_interacciones")),
                    tags$hr()
                  )
                ),
                actionButton(
                  ns("ajustar"),
                  "Ajustar modelo",
                  class = "btn-primary w-100",
                  icon  = icon("play")
                )
              )
            ),

            div(
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
                              "Interpretación"),
                  card_body(uiOutput(ns("texto_modelo")))
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 5: Parámetros
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("table", class = "me-1"), "Parámetros"),
        card_body(

          uiOutput(ns("params_intro")),

          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header(
                bs_icon("layout-text-sidebar", class = "me-1"),
                "Tabla de coeficientes",
                span(class = "text-muted small ms-2",
                     "— parameters (easystats)")
              ),
              card_body(uiOutput(ns("tabla_params_ui")))
            ),
            card(
              card_header(
                bs_icon("bar-chart-fill", class = "me-1"),
                "Forest plot",
                span(class = "text-muted small ms-2",
                     "— coeficiente ± IC 95%")
              ),
              card_body(
                plotOutput(ns("plot_forest"), height = "260px")
              )
            )
          ),

          # Escala transformada (OR o IRR)
          card(
            class = "mt-3",
            card_header(
              bs_icon("arrow-left-right", class = "me-1"),
              uiOutput(ns("transformed_header"))
            ),
            card_body(uiOutput(ns("tabla_transformada")))
          ),

          card(
            class = "mt-3",
            card_header(bs_icon("chat-text", class = "me-1"),
                        "Interpretación — haz clic en una fila"),
            card_body(uiOutput(ns("interp_coef")))
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 6: Efectos marginales
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("graph-up-arrow", class = "me-1"),
                        "Efectos marginales"),
        card_body(
          p(class = "small text-muted mb-3",
            "Los ", strong("efectos marginales"),
            " muestran cómo cambia la variable respuesta al variar ",
            "un predictor, manteniendo el resto en sus valores típicos. ",
            "En GLM se muestran en la ", strong("escala original de Y"),
            " (probabilidades para logística, conteo esperado para Poisson), ",
            "no en la escala del enlace. Generados con ",
            strong("modelbased::estimate_relation()"), " de easystats."
          ),

          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("sliders", class = "me-1"),
                          "Controles"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Selecciona el predictor focal. ",
                  "El resto se mantiene en sus valores típicos."),
                uiOutput(ns("sel_pred_marginal")),
                tags$hr(),
                checkboxInput(ns("marginal_ci"),
                              "Mostrar intervalo de confianza 95%",
                              value = TRUE),
                checkboxInput(ns("marginal_puntos"),
                              "Mostrar datos observados",
                              value = TRUE),
                tags$hr(),
                uiOutput(ns("marginal_valores_tipicos"))
              )
            ),

            div(
              card(
                card_header(
                  bs_icon("graph-up-arrow", class = "me-1"),
                  "Efecto marginal",
                  span(class = "text-muted small ms-2",
                       "— estimate_relation() · modelbased")
                ),
                card_body(
                  plotOutput(ns("plot_marginal"), height = "380px")
                )
              ),
              br(),
              uiOutput(ns("marginal_interpretacion"))
            )
          ),

          tags$hr(),

          # ── Predicción puntual ────────────────────────
          h5(style = paste0("color:", colores$primario,
                            "; font-weight:700;"),
             "Predicción puntual"),
          p(class = "small text-muted mb-3",
            "Ingresa valores específicos para cada predictor y obtén ",
            "la probabilidad (o conteo esperado) predicha por el modelo ",
            "con su intervalo de confianza 95%. Usa ",
            strong("modelbased::estimate_expectation()"),
            " de easystats."
          ),

          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("sliders", class = "me-1"),
                          "Valores de los predictores"),
              card_body(
                uiOutput(ns("inputs_prediccion")),
                br(),
                actionButton(
                  ns("calcular_prediccion"),
                  "Calcular probabilidad",
                  class = "btn-primary w-100",
                  icon  = icon("calculator")
                )
              )
            ),

            card(
              card_header(bs_icon("bullseye", class = "me-1"),
                          "Resultado"),
              card_body(uiOutput(ns("resultado_prediccion")))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 7: Contrastes
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("arrows-angle-expand", class = "me-1"),
                        "Contrastes"),
        card_body(
          p(class = "small text-muted mb-3",
            "Los ", strong("contrastes"), " comparan grupos entre sí. ",
            "Las diferencias se estiman en la escala del enlace y se ",
            "pueden transformar a la escala original. Generados con ",
            strong("modelbased::estimate_contrasts()"), " de easystats."
          ),

          uiOutput(ns("contrasts_no_cat_msg")),

          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("sliders", class = "me-1"),
                          "Controles"),
              card_body(
                uiOutput(ns("sel_var_contraste")),
                tags$hr(),
                selectInput(
                  ns("metodo_ajuste"),
                  label   = "Ajuste de p-valores:",
                  choices = c(
                    "Sin ajuste"  = "none",
                    "Bonferroni"  = "bonferroni",
                    "Holm"        = "holm",
                    "FDR (BH)"    = "fdr"
                  ),
                  selected = "none"
                )
              )
            ),

            div(
              card(
                class = "mb-3",
                card_header(
                  bs_icon("table", class = "me-1"),
                  "Tabla de contrastes"
                ),
                card_body(uiOutput(ns("tabla_contrastes")))
              ),
              card(
                class = "mb-0",
                card_header(
                  bs_icon("bar-chart-fill", class = "me-1"),
                  "Visualización de contrastes"
                ),
                card_body(
                  plotOutput(ns("plot_contrastes"), height = "300px")
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 8: Performance
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("speedometer2", class = "me-1"),
                        "Performance"),
        card_body(

          p(class = "small text-muted mb-3",
            "Las métricas de performance del GLM difieren del LM. ",
            "No hay R² directo — se usan pseudo-R², criterios de ",
            "información y métricas específicas por familia. ",
            "Generadas con ", strong("performance::model_performance()"),
            " de easystats."
          ),

          layout_columns(
            col_widths = c(6, 6),

            card(
              card_header(
                bs_icon("speedometer2", class = "me-1"),
                "Métricas del modelo",
                span(class = "text-muted small ms-2",
                     "— model_performance() · easystats")
              ),
              card_body(uiOutput(ns("tabla_performance")))
            ),

            div(
              # Curva ROC — solo para logística
              uiOutput(ns("card_roc")),

              card(
                card_header(
                  bs_icon("arrow-repeat", class = "me-1"),
                  "Validación cruzada",
                  span(class = "text-muted small ms-2",
                       "— vfold_cv() · tidymodels")
                ),
                card_body(
                  p(class = "small text-muted mb-2",
                    "¿Cuánto error cometo al predecir ",
                    strong("datos nuevos"), "?"
                  ),
                  layout_columns(
                    col_widths = c(4, 4, 4),
                    numericInput(
                      ns("cv_folds"),
                      label = "Folds:",
                      value = 10, min = 3, max = 20
                    ),
                    div(class = "pt-4",
                        checkboxInput(ns("cv_estratificado"),
                                      "Estratificar",
                                      value = TRUE)),
                    div(class = "pt-4",
                        actionButton(ns("correr_cv"), "Correr CV",
                                     class = "btn-primary w-100",
                                     icon  = icon("rotate")))
                  ),
                  tags$hr(),
                  uiOutput(ns("resultado_cv"))
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 9: Comparar modelos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("arrow-left-right", class = "me-1"),
                        "Comparar modelos"),
        card_body(
          p(class = "small text-muted mb-3",
            "Construye distintos modelos en la pestaña ",
            strong("Construir el modelo"),
            " y guárdalos aquí para compararlos por AIC, AICc y BIC."
          ),
          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("floppy", class = "me-1"),
                          "Guardar modelo actual"),
              card_body(
                textInput(ns("nombre_modelo"),
                          label       = "Nombre del modelo:",
                          placeholder = "Ej: solo habitat, full…"),
                actionButton(ns("guardar_modelo"),
                             "Guardar modelo",
                             class = "btn-primary w-100 mb-2",
                             icon  = icon("floppy-disk")),
                actionButton(ns("limpiar_modelos"),
                             "Limpiar todos",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon  = icon("trash")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  "Modelos guardados:"),
                uiOutput(ns("lista_modelos_guardados"))
              )
            ),

            div(
              card(
                class = "mb-3",
                card_header(
                  bs_icon("table", class = "me-1"),
                  "Tabla comparativa",
                  span(class = "text-muted small ms-2",
                       "— compare_performance() · easystats")
                ),
                card_body(uiOutput(ns("tabla_comparacion")))
              ),
              card(
                class = "mb-0",
                card_header(
                  bs_icon("diagram-3", class = "me-1"),
                  "Gráfico radar",
                  span(class = "text-muted small ms-2",
                       "— compare_performance() · see")
                ),
                card_body(
                  p(class = "small text-muted mb-2",
                    "Mayor área = mejor modelo en más dimensiones. ",
                    "Requiere al menos 2 modelos guardados."),
                  plotOutput(ns("plot_comparacion_aic"),
                             height = "340px")
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 10: Diagnóstico
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard-check", class = "me-1"),
                        "Diagnóstico"),
        card_body(
          p(class = "small text-muted mb-3",
            "Verificamos los supuestos descritos en la pestaña ",
            strong("Fundamentos"), ". Las pruebas se adaptan ",
            "automáticamente según la familia del modelo. ",
            "Generado con ", strong("performance"), " de easystats."
          ),

          layout_columns(
            col_widths = c(3, 9),

            # Semáforo
            div(
              uiOutput(ns("semaforo_col1")),
              uiOutput(ns("semaforo_col2"))
            ),

            # Gráficos
            card(
              card_header(
                bs_icon("clipboard-check", class = "me-1"),
                "Gráficos de diagnóstico",
                span(class = "text-muted small ms-2",
                     "— performance::check_model() · easystats")
              ),
              card_body(
                class = "p-1",
                plotOutput(ns("plot_check_model"), height = "650px")
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 11: Código R
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"),
                        "Código R"),
        card_body(
          p(class = "text-muted small mb-3",
            "Script que reproduce este análisis usando ",
            strong("glmmTMB"), ", ", strong("tidymodels"),
            " y ", strong("easystats"),
            ". Se actualiza según las selecciones activas."
          ),
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tagList(bs_icon("code-slash"), " Script reproducible"),
              downloadButton(
                ns("descargar_script"),
                label = "Descargar .R",
                icon  = bs_icon("download"),
                class = "btn-sm btn-outline-primary"
              )
            ),
            verbatimTextOutput(ns("codigo_r"))
          )
        )
      )

    ) # fin navset_card_tab
  )   # fin tagList
}

# ── Server ───────────────────────────────────────────────
mod_glm_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ────────────────────────────────────────────────────
    # DATOS
    # ────────────────────────────────────────────────────

    # Datasets disponibles por familia
    datasets_por_familia <- list(
      binomial = c(
        "Presencia NPRA — ácaros (ecología)"   = "mite_log",
        "Diabetes — mujeres Pima (salud)"       = "pima",
        "Voluntariado (psicología social)"       = "cowles",
        "Cargar mis propios datos"               = "propio"
      ),
      poisson = c(
        "Abundancia Brachy — ácaros (ecología)" = "mite_poi",
        "Riqueza de hormigas (ecología)"         = "ants",
        "Reclamaciones — con offset (seguros)"   = "insurance",
        "Cáncer de pulmón — con offset (salud)"  = "danish",
        "Cargar mis propios datos"               = "propio"
      ),
      nbinom2 = c(
        "Cangrejos herradura (biología)"         = "hcrabs",
        "Cargar mis propios datos"               = "propio"
      )
    )

    # Selector dinámico de dataset según familia elegida
    output$sel_fuente_datos <- renderUI({
      fam_sel <- input$familia_datos
      # Default a binomial si no hay selección
      if (is.null(fam_sel) || nchar(fam_sel) == 0)
        fam_sel <- "binomial"
      opciones <- datasets_por_familia[[fam_sel]]
      if (is.null(opciones)) opciones <- datasets_por_familia[["binomial"]]
      radioButtons(
        ns("fuente_datos"),
        label    = tagList(bs_icon("database", class = "me-1"),
                           "Dataset de ejemplo:"),
        choices  = opciones,
        selected = opciones[1]
      )
    })

    # Sincronizar familia en Construir el modelo con la elegida en Los datos
    observeEvent(input$familia_datos, {
      req(nchar(input$familia_datos) > 0)
      updateSelectInput(session, "familia",
                        selected = input$familia_datos)

      # Actualizar estilo visual de las tarjetas con shinyjs
      tryCatch({
        fam <- input$familia_datos
        colores_sel <- list(
          binomial = "background:#E6F1FB; border:2px solid #185FA5;",
          poisson  = "background:#E1F5EE; border:2px solid #0F6E56;",
          nbinom2  = "background:#FAEEDA; border:2px solid #853F0B;"
        )
        estilo_base <- paste0(
          "background:var(--color-background-secondary);",
          "border:1.5px solid var(--color-border-tertiary);"
        )
        estilo_comun <- "border-radius:10px; padding:10px 12px; cursor:pointer;"

        for (f in c("binomial", "poisson", "nbinom2")) {
          card_id <- paste0("#", ns(paste0("card_", f)))
          estilo <- if (f == fam)
            paste0(colores_sel[[f]], estilo_comun)
          else
            paste0(estilo_base, estilo_comun)
          shinyjs::runjs(paste0(
            'document.querySelector("', card_id,
            '").setAttribute("style", "', estilo, '");'
          ))
        }
      }, error = function(e) NULL)
    })

    # Contexto dinámico del dataset
    output$contexto_dataset <- renderUI({
      fuente <- input$fuente_datos
      if (is.null(fuente) || fuente == "propio") return(NULL)

      info <- list(
        mite_log  = list(
          titulo = "Presencia/ausencia de ácaros (mite, vegan)",
          texto  = paste0(
            "Presencia (1) o ausencia (0) del ácaro ", em("NPRA"),
            " en 70 muestras de musgo en Quebec, Canadá. ",
            "Predictores: densidad del sustrato, contenido de agua, ",
            "cobertura de arbustos y topografía. ",
            "Fuente: Borcard & Legendre (1994)."
          )
        ),
        mite_poi  = list(
          titulo = "Abundancia de ácaros (mite, vegan)",
          texto  = paste0(
            "Abundancia del ácaro ", em("Brachy"),
            " en 70 muestras de musgo en Quebec, Canadá. ",
            "Predictores: densidad del sustrato, contenido de agua, ",
            "cobertura de arbustos y topografía."
          )
        ),
        pima      = list(
          titulo = "Diabetes en mujeres Pima (PimaIndiansDiabetes, mlbench)",
          texto  = paste0(
            "Diagnóstico de diabetes (positivo/negativo) en ",
            strong("768 mujeres"), " de la tribu Pima, Arizona, EE.UU. ",
            "Predictores: glucosa, presión arterial, IMC, edad y otros. ",
            "Fuente: Smith et al. (1988)."
          )
        ),
        cowles    = list(
          titulo = "Voluntariado (Cowles, carData)",
          texto  = paste0(
            "¿Participa la persona como voluntaria en investigación? ",
            "(sí/no) en función de rasgos de personalidad. ",
            strong("1421 participantes"), ". ",
            "Predictores: neuroticismo, extraversión y sexo. ",
            "Fuente: Cowles & Davis (1987)."
          )
        ),
        ants      = list(
          titulo = "Riqueza de hormigas (ants, GLMsData)",
          texto  = paste0(
            "Número de especies de hormigas en ",
            strong("44 sitios"), " en el noreste de EE.UU. ",
            "Predictores: tipo de hábitat (turbera/bosque), ",
            "latitud y elevación. ",
            "Fuente: GLMsData (Dunn & Smyth, 2018)."
          )
        ),
        insurance = list(
          titulo = "Reclamaciones de seguro (Insurance, MASS)",
          texto  = paste0(
            "Número de reclamaciones de seguro de automóvil en ",
            strong("64 grupos"), " clasificados por distrito, ",
            "grupo de motor y edad del conductor. ",
            "Offset: número de asegurados. ",
            "Fuente: Baxter et al. (1980)."
          )
        ),
        danish    = list(
          titulo = "Cáncer de pulmón en Dinamarca (danishlc, GLMsData)",
          texto  = paste0(
            "Casos de cáncer de pulmón en ",
            strong("24 grupos"), " por ciudad y grupo de edad ",
            "en Dinamarca (1968-1971). ",
            "Offset: tamaño de la población expuesta. ",
            "Fuente: Breslow & Day (1987)."
          )
        ),
        hcrabs    = list(
          titulo = "Cangrejos herradura (hcrabs, GLMsData)",
          texto  = paste0(
            "Número de machos satélite adheridos a hembras de ",
            em("Limulus polyphemus"), " en ",
            strong("173 hembras"), ". ",
            "Predictores: color, estado de la espina, ancho y peso. ",
            "Fuente: Brockmann (1996)."
          )
        )
      )

      datos_info <- info[[fuente]]
      if (is.null(datos_info)) return(NULL)

      div(
        class = "alert alert-info small py-2 px-3 mb-2",
        bs_icon("info-circle-fill", class = "me-1"),
        strong(datos_info$titulo), br(),
        datos_info$texto
      )
    })

    # Dataset base
    datos_base <- reactive({
      fuente <- input$fuente_datos
      req(fuente)

      if (fuente == "mite_log") {
        load("data/mite_logistic.rda"); mite_logistic
      } else if (fuente == "mite_poi") {
        load("data/mite_counts.rda"); mite_counts
      } else if (fuente == "pima") {
        load("data/pima_glm.rda"); pima_glm
      } else if (fuente == "cowles") {
        load("data/cowles_glm.rda"); cowles_glm
      } else if (fuente == "ants") {
        load("data/ants_glm.rda"); ants_glm
      } else if (fuente == "insurance") {
        load("data/insurance_glm.rda"); insurance_glm
      } else if (fuente == "danish") {
        load("data/danish_glm.rda"); danish_glm
      } else if (fuente == "hcrabs") {
        load("data/hcrabs_glm.rda"); hcrabs_glm
      } else {
        req(input$archivo)
        ext <- tools::file_ext(input$archivo$name)
        tryCatch({
          df <- if (ext %in% c("xlsx", "xls")) {
            readxl::read_excel(input$archivo$datapath)
          } else {
            readr::read_delim(input$archivo$datapath,
                              delim = input$separador,
                              show_col_types = FALSE)
          }
          df |> dplyr::mutate(dplyr::across(where(is.character), factor))
        }, error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error", duration = 6)
          NULL
        })
      }
    })

    # Tipos de variables
    tipos_usuario <- reactiveVal(NULL)
    observeEvent(datos_base(), { tipos_usuario(NULL) })
    observeEvent(input$resetear_tipos, {
      tipos_usuario(NULL)
      showNotification("Tipos restaurados.", type = "message",
                       duration = 2)
    })
    observeEvent(input$aplicar_tipos, {
      df  <- datos_base(); req(df)
      nms <- names(df)
      nuevos <- lapply(nms, function(nm) input[[paste0("tipo_", nm)]])
      names(nuevos) <- nms
      tipos_usuario(nuevos)
      showNotification("Tipos aplicados.", type = "message",
                       duration = 2)
    })

    datos_activos <- reactive({
      df <- datos_base(); req(df)
      tu <- tipos_usuario()
      if (is.null(tu)) return(df)
      for (nm in names(tu)) {
        if (!nm %in% names(df)) next
        tipo <- tu[[nm]]
        if (is.null(tipo)) next
        if (tipo == "factor" && !is.factor(df[[nm]]))
          df[[nm]] <- factor(df[[nm]])
        else if (tipo == "numeric" && !is.numeric(df[[nm]]))
          df[[nm]] <- suppressWarnings(as.numeric(as.character(df[[nm]])))
        else if (tipo == "excluir")
          df[[nm]] <- NULL
      }
      df
    })

    vars_numericas <- reactive({
      df <- datos_activos(); req(df)
      names(df)[sapply(df, is.numeric)]
    })

    vars_categoricas <- reactive({
      df <- datos_activos(); req(df)
      names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    })

    # Resumen, cards y preview
    output$resumen_datos <- renderUI({
      df <- datos_activos()
      if (is.null(df)) return(NULL)
      div(class = "small text-muted mt-1",
          bs_icon("check-circle-fill",
                  style = paste0("color:", colores$exito),
                  class = "me-1"),
          paste0(nrow(df), " filas · ", ncol(df), " columnas."))
    })

    output$cards_datos <- renderUI({
      df <- datos_activos(); req(df)
      nnum <- length(vars_numericas())
      ncat <- length(vars_categoricas())
      layout_columns(
        col_widths = c(4, 4, 4),
        card(class = "text-center", card_body(class = "p-2",
                                              h3(style = paste0("color:", colores$primario,
                                                                "; font-weight:700;"), nrow(df)),
                                              p(class = "small text-muted mb-0", "Observaciones")
        )),
        card(class = "text-center", card_body(class = "p-2",
                                              h3(style = paste0("color:", colores$acento,
                                                                "; font-weight:700;"), nnum),
                                              p(class = "small text-muted mb-0", "Numéricas")
        )),
        card(class = "text-center", card_body(class = "p-2",
                                              h3(style = paste0("color:", colores$secundario,
                                                                "; font-weight:700;"), ncat),
                                              p(class = "small text-muted mb-0", "Categóricas")
        ))
      )
    })

    output$tabla_preview <- renderDT({
      df <- datos_activos(); req(df)
      datatable(head(df, 8), rownames = FALSE,
                options = list(dom = "t", scrollX = TRUE),
                class = "table-sm table-striped")
    })

    # Tabla de tipos
    output$tabla_tipos <- renderUI({
      df <- datos_base(); req(df)
      tu <- tipos_usuario()
      filas <- lapply(names(df), function(nm) {
        col    <- df[[nm]]
        actual <- if (is.factor(col) || is.character(col)) "factor"
        else if (is.numeric(col)) "numeric" else "otro"
        icono  <- if (actual == "factor")
          bs_icon("tag-fill", style = paste0("color:", colores$acento))
        else
          bs_icon("123", style = paste0("color:", colores$primario))
        sel <- if (!is.null(tu) && !is.null(tu[[nm]])) tu[[nm]] else actual
        tags$tr(
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  div(class = "d-flex align-items-center gap-2",
                      icono, strong(nm))),
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  tags$span(class = "badge",
                            style = paste0("background:",
                                           if (actual == "factor") colores$acento
                                           else colores$primario, "; font-size:0.75rem;"),
                            if (actual == "factor") "Factor" else "Numérico")),
          tags$td(style = "padding:5px 8px;",
                  selectInput(
                    inputId  = paste0(ns("tipo_"), nm),
                    label    = NULL,
                    choices  = c("Numérico" = "numeric",
                                 "Factor (categórico)" = "factor",
                                 "Excluir" = "excluir"),
                    selected = sel, width = "180px")),
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  if (!is.null(tu) && !is.null(tu[[nm]]) &&
                      tu[[nm]] != actual)
                    tags$span(class = "badge",
                              style = paste0("background:", colores$exito),
                              "Modificado")
                  else
                    tags$span(class = "text-muted small", "Sin cambios"))
        )
      })
      tags$table(
        class = "table table-sm table-hover small mb-0",
        tags$thead(tags$tr(
          tags$th(style = paste0("background:", colores$primario,
                                 " !important; color:#fff !important; padding:7px 8px;"),
                  "Variable"),
          tags$th(style = paste0("background:", colores$primario,
                                 " !important; color:#fff !important; padding:7px 8px;"),
                  "Tipo detectado"),
          tags$th(style = paste0("background:", colores$primario,
                                 " !important; color:#fff !important; padding:7px 8px;"),
                  "Tipo a usar"),
          tags$th(style = paste0("background:", colores$primario,
                                 " !important; color:#fff !important; padding:7px 8px;"),
                  "Estado")
        )),
        tags$tbody(filas)
      )
    })

    output$tipos_aplicados_msg <- renderUI({
      tu <- tipos_usuario(); if (is.null(tu)) return(NULL)
      df <- datos_base(); req(df)
      n_cambios <- sum(sapply(names(tu), function(nm) {
        if (!nm %in% names(df)) return(FALSE)
        actual <- if (is.factor(df[[nm]]) || is.character(df[[nm]]))
          "factor" else "numeric"
        !is.null(tu[[nm]]) && tu[[nm]] != actual && tu[[nm]] != "excluir"
      }))
      n_excl <- sum(sapply(tu, function(t) !is.null(t) && t == "excluir"))
      if (n_cambios == 0 && n_excl == 0) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("check-circle-fill", class = "me-1",
                  style = paste0("color:", colores$exito)),
          if (n_cambios > 0) paste0(n_cambios, " variable(s) convertida(s). "),
          if (n_excl > 0) paste0(n_excl, " variable(s) excluida(s). "),
          "El modelo usará estos tipos.")
    })

    # Explorar
    output$sel_var_x <- renderUI({
      req(vars_numericas())
      selectInput(ns("var_x"), label = "Variable X:",
                  choices = vars_numericas(),
                  selected = vars_numericas()[1])
    })

    output$sel_color <- renderUI({
      cats <- vars_categoricas()
      if (length(cats) == 0) return(NULL)
      selectInput(ns("var_color"), label = "Colorear por:",
                  choices = c("Ninguna" = "ninguna", cats),
                  selected = "ninguna")
    })

    output$cards_correlacion <- renderUI({
      df <- datos_activos(); req(df, input$var_x)
      yv <- vars_numericas(); req(length(yv) >= 2)
      yvar <- yv[yv != input$var_x][1]; req(yvar)
      cor_val <- cor(df[[yvar]], df[[input$var_x]], use = "complete.obs")
      layout_columns(
        col_widths = c(6, 6),
        card(class = "text-center border-0",
             style = paste0("background:", colores$fondo),
             card_body(class = "p-2",
                       h4(style = paste0("color:", colores$primario,
                                         "; font-weight:700;"),
                          round(cor_val, 2)),
                       p(class = "small text-muted mb-0", "Correlación (r)")
             )),
        card(class = "text-center border-0",
             style = paste0("background:", colores$fondo),
             card_body(class = "p-2",
                       h4(style = paste0("color:", colores$acento,
                                         "; font-weight:700;"),
                          paste0(round(cor_val^2 * 100, 0), "%")),
                       p(class = "small text-muted mb-0", "R² simple")
             ))
      )
    })

    output$plot_scatter <- renderPlot({
      df <- datos_activos(); req(df, input$var_x)
      yv <- vars_numericas(); req(length(yv) >= 2)
      yvar <- yv[yv != input$var_x][1]; req(yvar)
      p <- ggplot(df, aes(x = .data[[input$var_x]],
                          y = .data[[yvar]]))
      usar_color <- !is.null(input$var_color) &&
        input$var_color != "ninguna" &&
        input$var_color %in% names(df)
      if (usar_color)
        p <- p + aes(color = .data[[input$var_color]]) +
        scale_color_manual(values = colores$tableau,
                           name = input$var_color)
      p <- p + geom_point(alpha = 0.5, size = 2)
      if (isTRUE(input$mostrar_suavizado))
        p <- p + geom_smooth(method = "loess", se = TRUE,
                             color = colores$primario,
                             fill  = colores$secundario,
                             alpha = 0.15, linewidth = 1.2)
      p + labs(x = input$var_x, y = yvar,
               subtitle = paste0("n = ", nrow(df), " observaciones")) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              legend.position  = "bottom")
    }, res = 96)

    output$insight_scatter <- renderUI({
      df <- datos_activos(); req(df, input$var_x)
      yv <- vars_numericas(); req(length(yv) >= 2)
      yvar <- yv[yv != input$var_x][1]; req(yvar)
      cor_val <- cor(df[[yvar]], df[[input$var_x]], use = "complete.obs")
      dir <- if (cor_val > 0.5) "positiva y fuerte" else
        if (cor_val > 0.2) "positiva y moderada" else
          if (cor_val < -0.5) "negativa y fuerte" else "débil"
      div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("lightbulb-fill", class = "me-1"),
          paste0("La relación entre ", input$var_x, " y ", yvar,
                 " es ", dir, " (r = ", round(cor_val, 2), ")."))
    })

    # ────────────────────────────────────────────────────
    # CONSTRUIR EL MODELO
    # ────────────────────────────────────────────────────

    # Enlace según familia
    output$sel_enlace <- renderUI({
      enlaces <- switch(input$familia,
                        "binomial" = c("logit", "probit", "cloglog"),
                        "poisson"  = c("log", "sqrt", "identity"),
                        "nbinom2"  = c("log"),
                        c("log")
      )
      selectInput(ns("enlace"), label = "Función de enlace:",
                  choices = enlaces, selected = enlaces[1])
    })

    # Offset opcional
    output$sel_offset <- renderUI({
      nums <- vars_numericas()
      if (length(nums) == 0) return(NULL)
      familia <- input$familia
      if (!familia %in% c("poisson", "nbinom2")) return(NULL)
      selectInput(ns("offset_var"),
                  label = "Variable offset (opcional):",
                  choices = c("Sin offset" = "ninguno", nums),
                  selected = "ninguno")
    })

    output$sel_var_y <- renderUI({
      req(vars_numericas(), vars_categoricas())
      todas <- c(vars_numericas(), vars_categoricas())
      req(length(todas) > 0)
      selectInput(ns("var_y"), label = "Variable respuesta (Y):",
                  choices = todas, selected = todas[1])
    })

    output$checks_numericos <- renderUI({
      req(vars_numericas(), input$var_y)
      opts <- vars_numericas()
      # Excluir Y y offset
      excluir <- c(input$var_y)
      if (!is.null(input$offset_var) && input$offset_var != "ninguno")
        excluir <- c(excluir, input$offset_var)
      opts <- opts[!opts %in% excluir]
      if (length(opts) == 0)
        return(p(class = "small text-muted",
                 "No hay más variables numéricas."))
      checkboxGroupInput(ns("preds_num"), label = NULL,
                         choices = opts, selected = opts[1])
    })

    output$checks_categoricos <- renderUI({
      cats <- vars_categoricas()
      cats <- cats[cats != input$var_y]
      if (length(cats) == 0)
        return(p(class = "small text-muted",
                 "No hay variables categóricas."))
      checkboxGroupInput(ns("preds_cat"), label = NULL,
                         choices = cats, selected = NULL)
    })

    output$checks_interacciones <- renderUI({
      preds <- c(input$preds_num, input$preds_cat)
      if (length(preds) < 2) return(NULL)
      pares     <- combn(preds, 2, simplify = FALSE)
      etiquetas <- sapply(pares, function(p)
        paste0(p[1], " \u00d7 ", p[2]))
      valores   <- sapply(pares, function(p)
        paste0(p[1], "*", p[2]))
      checkboxGroupInput(ns("interacciones"), label = NULL,
                         choices = setNames(valores, etiquetas),
                         selected = NULL)
    })

    # Ajuste del modelo con glmmTMB
    modelo_glm <- eventReactive(input$ajustar, {
      df    <- datos_activos(); req(df, input$var_y)
      preds <- c(input$preds_num, input$preds_cat)
      if (length(preds) == 0) {
        showNotification("Selecciona al menos un predictor.",
                         type = "warning", duration = 4)
        return(NULL)
      }

      ints     <- input$interacciones
      terminos <- if (!is.null(ints) && length(ints) > 0)
        c(preds, ints) else preds

      # Offset
      offset_txt <- ""
      if (!is.null(input$offset_var) &&
          input$offset_var != "ninguno") {
        offset_txt <- paste0(" + offset(log(", input$offset_var, "))")
      }

      fm_txt <- paste0(
        input$var_y, " ~ ",
        paste(terminos, collapse = " + "),
        offset_txt
      )
      fm <- as.formula(fm_txt)

      withProgress(message = "Ajustando modelo GLM...", value = 0.5, {
        fit <- tryCatch({
          if (input$familia == "nbinom2") {
            glmmTMB::glmmTMB(fm,
                             family = glmmTMB::nbinom2(link = "log"),
                             data   = df)
          } else {
            fam_glm <- switch(input$familia,
                              "binomial" = stats::binomial(link = input$enlace),
                              "poisson"  = stats::poisson(link  = input$enlace))
            glm(fm, family = fam_glm, data = df)
          }
        }, error = function(e) {
          showNotification(
            paste("Error al ajustar:", conditionMessage(e)),
            type = "error", duration = 6)
          NULL
        })
        incProgress(0.5)
        fit
      })
    }, ignoreNULL = FALSE)

    # Métricas básicas
    output$cards_metricas <- renderUI({
      fit <- modelo_glm()
      if (is.null(fit)) return(
        div(class = "text-muted small py-3",
            bs_icon("arrow-left-circle", class = "me-1"),
            "Selecciona predictores y ajusta el modelo.")
      )
      tryCatch({
        pm  <- performance::model_performance(fit, verbose = FALSE)
        aic <- round(pm$AIC, 1)
        bic <- round(pm$BIC, 1)

        # R² según familia
        r2_val <- tryCatch({
          r2 <- performance::r2(fit, verbose = FALSE)
          if (!is.null(r2$R2_Tjur))
            paste0("Tjur: ", round(r2$R2_Tjur, 3))
          else if (!is.null(r2$R2_McFadden))
            paste0("McF: ", round(r2$R2_McFadden, 3))
          else if (!is.null(r2$R2_Nagelkerke))
            paste0("Nagel: ", round(r2$R2_Nagelkerke, 3))
          else "—"
        }, error = function(e) "—")

        n_p <- if (inherits(fit, "glmmTMB"))
          length(glmmTMB::fixef(fit)$cond) - 1L
        else
          length(coef(fit)) - 1L

        layout_columns(
          col_widths = c(3, 3, 3, 3),
          card(class = "text-center", card_body(class = "p-2",
                                                h3(style = paste0("color:", colores$primario,
                                                                  "; font-weight:700;"), aic),
                                                p(class = "small text-muted mb-0", strong("AIC"))
          )),
          card(class = "text-center", card_body(class = "p-2",
                                                h3(style = paste0("color:", colores$secundario,
                                                                  "; font-weight:700;"), bic),
                                                p(class = "small text-muted mb-0", strong("BIC"))
          )),
          card(class = "text-center", card_body(class = "p-2",
                                                h3(style = paste0("color:", colores$acento,
                                                                  "; font-weight:700;"), r2_val),
                                                p(class = "small text-muted mb-0", strong("Pseudo-R²"))
          )),
          card(class = "text-center", card_body(class = "p-2",
                                                h3(style = paste0("color:", colores$texto,
                                                                  "; font-weight:700;"), n_p),
                                                p(class = "small text-muted mb-0", strong("Predictores"))
          ))
        )
      }, error = function(e) {
        div(class = "text-muted small", "Error calculando métricas.")
      })
    })

    output$plot_predobs <- renderPlot({
      fit <- modelo_glm(); req(fit)
      tryCatch({
        df  <- datos_activos()
        obs <- df[[input$var_y]]
        pred <- fitted(fit)
        tibble::tibble(obs = as.numeric(obs), pred = pred) |>
          ggplot(aes(x = obs, y = pred)) +
          geom_abline(slope = 1, intercept = 0,
                      linetype = "dashed",
                      color = colores$texto, linewidth = 0.8) +
          geom_point(color = colores$primario, alpha = 0.5,
                     size = 1.8) +
          labs(x = "Observado", y = "Predicho") +
          theme_minimal(base_size = 12) +
          theme(panel.grid.minor = element_blank())
      }, error = function(e) {
        ggplot() + annotate("text", x = 0.5, y = 0.5,
                            label = "Ajusta el modelo primero.",
                            color = colores$texto, size = 4) + theme_void()
      })
    }, res = 96)

    output$texto_modelo <- renderUI({
      fit <- modelo_glm()
      if (is.null(fit)) return(
        p(class = "small text-muted",
          "Ajusta el modelo para ver la interpretación.")
      )
      tryCatch({
        pm    <- performance::model_performance(fit, verbose = FALSE)
        fam   <- input$familia
        enl   <- input$enlace
        n_p   <- if (inherits(fit, "glmmTMB"))
          length(glmmTMB::fixef(fit)$cond) - 1L
        else
          length(coef(fit)) - 1L
        fam_txt <- switch(fam,
                          "binomial" = "binomial (logística)",
                          "poisson"  = "Poisson",
                          "nbinom2"  = "binomial negativa",
        )
        tagList(
          p(class = "small",
            "Modelo ", strong(fam_txt),
            " con enlace ", code(enl),
            " y ", strong(n_p), " predictor(es)."),
          p(class = "small",
            "AIC = ", strong(round(pm$AIC, 1)),
            " · BIC = ", strong(round(pm$BIC, 1))),
          if (fam == "binomial")
            p(class = "small text-muted",
              "Los coeficientes están en escala ",
              strong("log-odds"),
              ". Ver pestaña Parámetros para odds ratios.")
          else if (fam %in% c("poisson", "nbinom2"))
            p(class = "small text-muted",
              "Los coeficientes están en escala ",
              strong("log"),
              ". Ver pestaña Parámetros para razones de tasas (IRR).")
        )
      }, error = function(e) NULL)
    })

    # ────────────────────────────────────────────────────
    # PARÁMETROS
    # ────────────────────────────────────────────────────

    output$params_intro <- renderUI({
      fam <- input$familia
      if (is.null(fam)) return(NULL)

      # Detectar separación perfecta
      sep_warning <- tryCatch({
        fit <- modelo_glm()
        if (!is.null(fit) && fam == "binomial") {
          se_vals <- sqrt(diag(vcov(fit)$cond))
          if (any(se_vals > 100, na.rm = TRUE)) {
            # Identificar qué predictor tiene el problema
            nms_prob <- names(se_vals)[se_vals > 100 &
                                         !is.na(se_vals)]
            div(
              class = "alert alert-danger small py-2 px-3 mb-3",
              bs_icon("exclamation-triangle-fill", class = "me-2"),
              strong("Separación perfecta detectada."), br(),
              "Los coeficientes y OR de: ",
              strong(paste(nms_prob, collapse = ", ")),
              " son inválidos (errores estándar > 100, IC [0, Inf]). ",
              "Esto ocurre cuando una categoría predice perfectamente ",
              "Y = 0 o Y = 1. Verifica con ",
              code("table(predictor, Y)"),
              " en la consola de R. ",
              "Los demás coeficientes del modelo pueden ser válidos."
            )
          }
        }
      }, error = function(e) NULL)

      tagList(
        sep_warning,
        if (fam == "binomial") {
          p(class = "small text-muted mb-3",
            "Los coeficientes están en escala ", strong("log-odds"),
            ". Exponenciando (exp(β)) se obtienen los ",
            strong("odds ratios (OR)"),
            " — la forma más común de reportar regresión logística. ",
            "OR > 1 aumenta las chances, OR < 1 las disminuye, OR = 1 sin efecto.")
        } else if (fam %in% c("poisson", "nbinom2")) {
          p(class = "small text-muted mb-3",
            "Los coeficientes están en escala ", strong("log"),
            ". Exponenciando (exp(β)) se obtienen las ",
            strong("razones de tasas de incidencia (IRR)"),
            " — cuántas veces cambia el conteo esperado por unidad de X. ",
            "IRR > 1 aumenta el conteo, IRR < 1 lo disminuye, IRR = 1 sin efecto.")
        }
      )
    })


    output$tabla_params_ui <- renderUI({
      fit <- modelo_glm()
      if (is.null(fit)) return(
        div(class = "text-muted small py-3", "Ajusta el modelo primero.")
      )
      tryCatch({
        mp  <- parameters::model_parameters(fit, ci = 0.95,
                                            verbose = FALSE,
                                            component = "conditional")
        df  <- as.data.frame(mp)

        # Filtrar solo parámetros condicionales (excluir dispersión)
        # glmmTMB con nbinom2 devuelve dos (Intercept) — el segundo es dispersión
        if ("Component" %in% names(df)) {
          df <- df[df$Component == "conditional", ]
        } else {
          # Eliminar duplicados de (Intercept) — quedarse con el primero
          df <- df[!duplicated(df$Parameter) |
                     df$Parameter != "(Intercept)", ]
        }

        filas <- lapply(seq_len(nrow(df)), function(i) {
          nm   <- df$Parameter[i]
          est  <- round(df$Coefficient[i], 3)
          se   <- round(df$SE[i], 3)
          lo   <- round(df$CI_low[i], 3)
          hi   <- round(df$CI_high[i], 3)
          pval <- df$p[i]
          if (is.na(pval)) pval <- 1
          p_txt <- if (pval < 0.001) "< 0.001 ***" else
            if (pval < 0.01)  paste0(round(pval,3), " **") else
              if (pval < 0.05)  paste0(round(pval,3), " *")  else
                round(pval, 3)
          col_p <- if (pval < 0.001) colores$exito else
            if (pval < 0.05)  colores$acento else colores$texto

          tags$tr(
            style = "cursor:pointer;",
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority:'event'})",
              ns("param_seleccionado"), nm),
            tags$td(strong(nm)),
            tags$td(style = "text-align:center;", est),
            tags$td(style = "text-align:center;", se),
            tags$td(style = "text-align:center;",
                    paste0("[", lo, ", ", hi, "]")),
            tags$td(style = paste0("color:", col_p,
                                   "; font-weight:600; text-align:center;"),
                    p_txt)
          )
        })

        tags$table(
          class = "table table-sm table-hover small mb-0",
          tags$thead(tags$tr(
            tags$th(style = paste0("background:", colores$primario,
                                   " !important; color:#fff !important;"), "Parámetro"),
            tags$th(style = paste0("background:", colores$primario,
                                   " !important; color:#fff !important; text-align:center;"),
                    "Estimado"),
            tags$th(style = paste0("background:", colores$primario,
                                   " !important; color:#fff !important; text-align:center;"),
                    "EE"),
            tags$th(style = paste0("background:", colores$primario,
                                   " !important; color:#fff !important; text-align:center;"),
                    "IC 95%"),
            tags$th(style = paste0("background:", colores$primario,
                                   " !important; color:#fff !important; text-align:center;"),
                    "p-valor")
          )),
          tags$tbody(filas)
        )
      }, error = function(e) {
        div(class = "text-muted small", "Error al obtener parámetros.")
      })
    })

    output$plot_forest <- renderPlot({
      fit <- modelo_glm(); req(fit)
      tryCatch({
        mp   <- parameters::model_parameters(fit, ci = 0.95,
                                             verbose = FALSE,
                                             component = "conditional")
        df_f <- as.data.frame(mp) |>
          dplyr::filter(Parameter != "(Intercept)") |>
          dplyr::mutate(
            Parameter = factor(Parameter,
                               levels = rev(unique(Parameter))),
            sig = p < 0.05
          )
        if (nrow(df_f) == 0) return(invisible(NULL))

        ggplot(df_f, aes(x = Coefficient, y = Parameter,
                         xmin = CI_low, xmax = CI_high,
                         color = sig)) +
          geom_vline(xintercept = 0, linetype = "dashed",
                     color = colores$texto, linewidth = 0.7) +
          geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=0.25, linewidth=1.1, orientation="y") +
          geom_point(size = 3.5) +
          scale_color_manual(
            values = c(`TRUE`  = colores$acento,
                       `FALSE` = colores$primario),
            labels = c(`TRUE`  = "Significativo (p < 0.05)",
                       `FALSE` = "No significativo"),
            name = NULL
          ) +
          labs(x = paste0("Coeficiente (escala ", input$enlace, ")"),
               y = NULL,
               subtitle = "IC 95% — si cruza el cero, no es significativo") +
          theme_minimal(base_size = 13) +
          theme(panel.grid.minor   = element_blank(),
                panel.grid.major.y = element_blank(),
                legend.position    = "bottom",
                plot.subtitle      = element_text(color = colores$texto,
                                                  size  = 9),
                plot.margin        = margin(10, 15, 5, 10))
      }, error = function(e) {
        ggplot() + annotate("text", x=0.5, y=0.5,
                            label="Ajusta el modelo primero.",
                            color=colores$texto, size=4) + theme_void()
      })
    }, res = 96)

    # Tabla transformada (OR o IRR)
    output$transformed_header <- renderUI({
      fam <- input$familia
      if (is.null(fam)) return(NULL)
      if (fam == "binomial")
        tagList(bs_icon("arrow-left-right", class = "me-1"),
                "Odds Ratios (OR) — exp(β)")
      else if (fam %in% c("poisson", "nbinom2"))
        tagList(bs_icon("arrow-left-right", class = "me-1"),
                "Razones de tasas de incidencia (IRR) — exp(β)")
      else
        tagList(bs_icon("arrow-left-right", class = "me-1"),
                "Coeficientes transformados — exp(β)")
    })

    output$tabla_transformada <- renderUI({
      fit <- modelo_glm()
      if (is.null(fit)) return(
        p(class = "small text-muted", "Ajusta el modelo primero.")
      )
      fam <- input$familia
      if (!fam %in% c("binomial", "poisson", "nbinom2")) return(
        p(class = "small text-muted",
          "Transformación exp(β) no aplica para esta familia.")
      )
      tryCatch({
        mp  <- parameters::model_parameters(
          fit, exponentiate = TRUE, ci = 0.95,
          verbose = FALSE, component = "conditional"
        )
        df  <- as.data.frame(mp)

        # Filtrar solo parámetros condicionales
        if ("Component" %in% names(df)) {
          df <- df[df$Component == "conditional", ]
        } else {
          df <- df[!duplicated(df$Parameter) |
                     df$Parameter != "(Intercept)", ]
        }
        lbl <- if (fam == "binomial") "OR" else "IRR"

        filas <- lapply(seq_len(nrow(df)), function(i) {
          nm   <- df$Parameter[i]
          est  <- round(df$Coefficient[i], 3)
          lo   <- round(df$CI_low[i], 3)
          hi   <- round(df$CI_high[i], 3)
          pval <- df$p[i]
          sig  <- pval < 0.05
          col  <- if (sig) colores$acento else colores$texto

          interp <- if (nm == "(Intercept)") "—" else {
            if (fam == "binomial") {
              if (est > 1)
                if (est > 1)
                  paste0("Las chances del evento son ",
                         round(est, 2), " veces mayores")
              else if (est == 1)
                paste0("Sin asociaci\u00f3n (OR = 1)")
              else
                paste0("Las chances del evento son ",
                       round((1 - est) * 100, 1),
                       "% menores")
              if (est > 1)
                paste0("El conteo esperado es ",
                       round(est, 2), " veces mayor")
              else if (est == 1)
                paste0("Sin efecto (IRR = 1)")
              else
                paste0("El conteo esperado se reduce un ",
                       round((1 - est) * 100, 1), "%")
            }
          }

          tags$tr(
            tags$td(strong(nm)),
            tags$td(style = paste0("text-align:center; color:", col,
                                   "; font-weight:700;"),
                    paste0(lbl, " = ", est)),
            tags$td(style = "text-align:center;",
                    paste0("[", lo, ", ", hi, "]")),
            tags$td(class = "small text-muted",
                    if (!is.null(interp) && nchar(interp) > 0)
                      interp else "—")
          )
        })

        tags$table(
          class = "table table-sm table-hover small mb-0",
          tags$thead(tags$tr(
            tags$th(style = paste0("background:", colores$primario,
                                   " !important; color:#fff !important;"), "Parámetro"),
            tags$th(style = paste0("background:", colores$primario,
                                   " !important; color:#fff !important; text-align:center;"),
                    lbl),
            tags$th(style = paste0("background:", colores$primario,
                                   " !important; color:#fff !important; text-align:center;"),
                    "IC 95%"),
            tags$th(style = paste0("background:", colores$primario,
                                   " !important; color:#fff !important;"),
                    "Interpretación")
          )),
          tags$tbody(filas)
        )
      }, error = function(e) {
        div(class = "text-muted small",
            "Error al transformar parámetros.")
      })
    })

    output$interp_coef <- renderUI({
      fit <- modelo_glm(); req(fit)
      sel <- input$param_seleccionado
      if (is.null(sel) || sel == "") return(
        p(class = "small text-muted",
          "Haz clic en una fila para ver la interpretación.")
      )
      tryCatch({
        mp   <- parameters::model_parameters(fit, ci = 0.95,
                                             verbose = FALSE,
                                             component = "conditional")
        df   <- as.data.frame(mp)
        fila <- df[df$Parameter == sel, ]
        req(nrow(fila) > 0)

        est   <- round(fila$Coefficient[1], 3)
        lo    <- round(fila$CI_low[1], 3)
        hi    <- round(fila$CI_high[1], 3)
        pval  <- fila$p[1]
        sig   <- pval < 0.05
        p_txt <- if (pval < 0.001) "< 0.001" else round(pval, 3)
        col   <- if (sig) colores$exito else colores$advertencia
        fam   <- input$familia
        or_irr <- round(exp(est), 3)

        interp <- if (sel == "(Intercept)") {
          paste0("El intercepto (β₀ = ", est,
                 ") es el valor predicho en escala ", input$enlace,
                 " cuando todos los predictores son cero.")
        } else if (fam == "binomial") {
          paste0(
            "Por cada unidad adicional de '", sel,
            "', el log-odds de Y = 1 cambia en ",
            ifelse(est >= 0, "+", ""), est,
            " (IC 95%: [", lo, ", ", hi, "]). ",
            "Exponenciando: OR = ", or_irr, ". ",
            if (or_irr > 1)
              paste0(
                "Las chances del evento son ", or_irr,
                " veces mayores por cada unidad adicional de '",
                sel, "'.")
            else if (or_irr == 1)
              "No hay asociación (OR = 1)."
            else
              paste0(
                "Las chances del evento son ",
                round((1 - or_irr) * 100, 1),
                "% menores por cada unidad adicional de '", sel, "'."),
            " ",
            if (sig) paste0("Efecto significativo (p = ", p_txt, ").")
            else paste0("Efecto NO significativo (p = ", p_txt, ").")
          )
        } else if (fam %in% c("poisson", "nbinom2")) {
          paste0(
            "Por cada unidad adicional de '", sel,
            "', el log del conteo esperado cambia en ",
            ifelse(est >= 0, "+", ""), est,
            " (IC 95%: [", lo, ", ", hi, "]). ",
            "Exponenciando: IRR = ", or_irr, ". ",
            if (or_irr > 1)
              paste0(
                "El conteo esperado es ", or_irr,
                " veces mayor por cada unidad adicional de '", sel, "'.")
            else
              paste0(
                "El conteo esperado se reduce un ",
                round((1 - or_irr) * 100, 1), "%."),
            " ",
            if (sig) paste0("Efecto significativo (p = ", p_txt, ").")
            else paste0("Efecto NO significativo (p = ", p_txt, ").")
          )
        } else {
          paste0("β = ", est, " (IC 95%: [", lo, ", ", hi, "]). ",
                 "p = ", p_txt, ".")
        }

        div(
          class = "alert py-2 px-3 small mb-0",
          style = paste0("border-left: 4px solid ", col,
                         "; background: ",
                         if (sig) "#f0f9f5" else "#fffbf0", ";"),
          bs_icon(if (sig) "check-circle-fill" else "circle",
                  class = "me-1",
                  style = paste0("color:", col)),
          strong(sel), " \u2014 ", interp
        )
      }, error = function(e) NULL)
    })

    # ────────────────────────────────────────────────────
    # EFECTOS MARGINALES
    # ────────────────────────────────────────────────────

    output$sel_pred_marginal <- renderUI({
      fit <- modelo_glm(); req(fit)
      preds <- c(input$preds_num, input$preds_cat)
      req(length(preds) > 0)
      selectInput(ns("pred_marginal"),
                  label    = "Predictor a explorar:",
                  choices  = preds,
                  selected = preds[1])
    })

    output$marginal_valores_tipicos <- renderUI({
      fit  <- modelo_glm(); req(fit, input$pred_marginal)
      df   <- datos_activos()
      preds <- c(input$preds_num, input$preds_cat)
      otros <- preds[preds != input$pred_marginal]
      if (length(otros) == 0) return(NULL)
      vals <- lapply(otros, function(nm) {
        col <- df[[nm]]
        if (is.numeric(col)) paste0(nm, " = ", round(mean(col, na.rm=TRUE), 2))
        else paste0(nm, " = ", names(sort(table(col), decreasing=TRUE))[1])
      })
      div(class = "alert alert-info small py-2 px-2 mb-0",
          bs_icon("info-circle", class = "me-1"),
          strong("Valores fijos: "), br(),
          paste(unlist(vals), collapse = " · "))
    })

    output$plot_marginal <- renderPlot({
      fit  <- modelo_glm(); req(fit, input$pred_marginal)
      df   <- datos_activos()
      pred <- input$pred_marginal
      es_cat <- pred %in% vars_categoricas()

      tryCatch({
        # estimate_relation funciona mejor con glm que con glmmTMB
        # Reajustamos con glm equivalente para los efectos marginales
        fam_glm <- switch(input$familia,
                          "binomial" = stats::binomial(link = input$enlace),
                          "poisson"  = stats::poisson(link = input$enlace),
                          stats::poisson()   # fallback
        )

        preds_fm <- c(input$preds_num, input$preds_cat)
        fm_marg  <- as.formula(
          paste(input$var_y, "~", paste(preds_fm, collapse = " + "))
        )

        # Para binomial negativa no hay glm equivalente simple
        # usamos glmmTMB directamente con marginaleffects
        if (input$familia == "nbinom2") {
          grilla_args        <- list(model = fit)
          grilla_args[[pred]] <- if (es_cat) unique(df[[pred]]) else
            seq(min(df[[pred]], na.rm=TRUE),
                max(df[[pred]], na.rm=TRUE), length.out=80)
          otros <- preds_fm[preds_fm != pred]
          for (nm in otros) {
            if (nm %in% vars_categoricas())
              grilla_args[[nm]] <- names(sort(table(df[[nm]]),
                                              decreasing=TRUE))[1]
          }
          grilla    <- do.call(marginaleffects::datagrid, grilla_args)
          preds_out <- marginaleffects::predictions(fit,
                                                    newdata=grilla, conf_level=0.95)
          df_rel <- as.data.frame(preds_out) |>
            dplyr::rename(Predicted=estimate,
                          CI_low=conf.low, CI_high=conf.high)
        } else {
          fit_glm <- glm(fm_marg, family = fam_glm, data = df)
          rel     <- suppressWarnings(
            modelbased::estimate_relation(fit_glm, by = pred,
                                          verbose = FALSE)
          )
          df_rel <- as.data.frame(rel)
        }

        p <- ggplot(df_rel, aes(x = .data[[pred]], y = Predicted)) +
          theme_minimal(base_size = 13)

        if (es_cat) {
          if (isTRUE(input$marginal_ci))
            p <- p + geom_errorbar(
              aes(ymin = CI_low, ymax = CI_high),
              width = 0.2, linewidth = 0.8,
              color = colores$primario)
          p <- p + geom_point(color = colores$primario, size = 3.5)
        } else {
          if (isTRUE(input$marginal_ci))
            p <- p + geom_ribbon(aes(ymin = CI_low, ymax = CI_high),
                                 fill = colores$primario, alpha = 0.15)
          if (isTRUE(input$marginal_puntos)) {
            y_obs <- if (input$familia == "binomial")
              as.integer(df[[input$var_y]])
            else as.numeric(df[[input$var_y]])
            p <- p + geom_point(
              data = data.frame(x_obs = df[[pred]], y_obs = y_obs),
              aes(x = x_obs, y = y_obs),
              color = colores$primario, alpha = 0.3, size = 1.5,
              inherit.aes = FALSE)
          }
          p <- p + geom_line(color = colores$primario, linewidth = 1.2)
        }

        y_lab <- if (input$familia == "binomial")
          paste0("P(", input$var_y, " = 1)")
        else paste0(input$var_y, " (esperado)")

        if (input$familia == "binomial")
          p <- p + scale_y_continuous(
            limits = c(0, 1),
            labels = scales::percent_format(accuracy = 1)
          )

        p + labs(x = pred, y = y_lab,
                 subtitle = paste0(
                   "Efecto marginal de '", pred,
                   "' — resto en valores típicos")) +
          theme(panel.grid.minor = element_blank(),
                legend.position  = "none",
                plot.subtitle    = element_text(color = colores$texto,
                                                size  = 9),
                plot.margin      = margin(10, 15, 5, 10))
      }, error = function(e) {
        ggplot() + annotate("text", x = 0.5, y = 0.5,
                            label = paste0("Error al generar el gráfico:
",
                                           conditionMessage(e)),
                            color = colores$texto, size = 3.5, hjust = 0.5) +
          theme_void()
      })
    }, res = 96)

    output$marginal_interpretacion <- renderUI({
      fit  <- modelo_glm(); req(fit, input$pred_marginal)
      pred <- input$pred_marginal
      es_cat <- pred %in% vars_categoricas()
      tryCatch({
        mp    <- parameters::model_parameters(fit, verbose = FALSE,
                                              component = "conditional")
        df_mp <- as.data.frame(mp)
        fam   <- input$familia

        if (es_cat) {
          filas_cat <- df_mp[grepl(pred, df_mp$Parameter, fixed=TRUE), ]
          texto <- if (nrow(filas_cat) > 0) {
            or_vals <- round(exp(filas_cat$Coefficient), 2)
            paste0("La variable '", pred, "' genera diferencias en Y. ",
                   "Los OR/IRR respecto a la referencia: ",
                   paste(paste0(filas_cat$Parameter, " = ", or_vals),
                         collapse = ", "), ".")
          } else "Ver tabla de parámetros para la interpretación."
        } else {
          fila <- df_mp[df_mp$Parameter == pred, ]
          if (nrow(fila) > 0) {
            est <- round(fila$Coefficient[1], 3)
            or  <- round(exp(est), 3)
            texto <- if (fam == "binomial")
              paste0("Por cada unidad adicional de '", pred,
                     "', el log-odds cambia en ", est,
                     " (OR = ", or, ").")
            else
              paste0("Por cada unidad adicional de '", pred,
                     "', el log del conteo esperado cambia en ", est,
                     " (IRR = ", or, ").")
          } else texto <- "Ver tabla de parámetros."
        }
        div(class = "alert alert-info small py-2 px-3 mb-0",
            bs_icon("lightbulb-fill", class = "me-1"), texto)
      }, error = function(e) NULL)
    })

    # ────────────────────────────────────────────────────
    # PREDICCIÓN PUNTUAL
    # ────────────────────────────────────────────────────

    output$inputs_prediccion <- renderUI({
      fit   <- modelo_glm(); req(fit)
      df    <- datos_activos()
      preds <- c(input$preds_num, input$preds_cat)
      req(length(preds) > 0)

      inputs <- lapply(preds, function(nm) {
        col <- df[[nm]]
        if (is.numeric(col)) {
          numericInput(
            inputId = ns(paste0("pred_val_", nm)),
            label   = paste0(nm, " (media = ",
                             round(mean(col, na.rm=TRUE), 1), "):"),
            value   = round(mean(col, na.rm=TRUE), 1),
            step    = round(sd(col, na.rm=TRUE) / 10, 2)
          )
        } else {
          moda <- names(sort(table(col), decreasing=TRUE))[1]
          selectInput(
            inputId  = ns(paste0("pred_val_", nm)),
            label    = nm,
            choices  = levels(col),
            selected = moda
          )
        }
      })
      do.call(tagList, inputs)
    })

    resultado_prediccion_data <- eventReactive(
      input$calcular_prediccion, {
        fit   <- modelo_glm(); req(fit)
        df    <- datos_activos()
        preds <- c(input$preds_num, input$preds_cat)
        req(length(preds) > 0)

        nueva_obs <- tryCatch({
          vals <- lapply(preds, function(nm) {
            col <- df[[nm]]
            val <- input[[paste0("pred_val_", nm)]]
            req(!is.null(val))
            if (is.numeric(col)) as.numeric(val)
            else factor(val, levels = levels(col))
          })
          names(vals) <- preds
          as.data.frame(vals)
        }, error = function(e) NULL)
        req(nueva_obs)

        fam_glm <- switch(input$familia,
                          "binomial" = stats::binomial(link = input$enlace),
                          "poisson"  = stats::poisson(link = input$enlace),
                          stats::poisson()
        )
        fm <- as.formula(paste(input$var_y, "~",
                               paste(preds, collapse = " + ")))
        fit_base <- if (input$familia == "nbinom2") fit else
          glm(fm, family = fam_glm, data = df)

        tryCatch(
          modelbased::estimate_expectation(
            fit_base, data = nueva_obs, verbose = FALSE
          ),
          error = function(e) NULL
        )
      }, ignoreNULL = TRUE)

    output$resultado_prediccion <- renderUI({
      res <- resultado_prediccion_data()
      if (is.null(res)) return(
        div(class = "text-muted small py-3",
            bs_icon("calculator", class = "me-2"),
            "Define los valores y haz clic en ",
            strong("Calcular probabilidad"), ".")
      )
      df_res <- as.data.frame(res)
      pred   <- round(df_res$Predicted[1], 3)
      lo     <- round(df_res$CI_low[1], 3)
      hi     <- round(df_res$CI_high[1], 3)
      fam    <- input$familia

      if (fam == "binomial") {
        valor_txt <- paste0(round(pred * 100, 1), "%")
        ic_txt    <- paste0("[", round(lo*100,1), "%, ",
                            round(hi*100,1), "%]")
        etiqueta  <- paste0("P(", input$var_y, " = 1)")
        color     <- if (pred > 0.7) colores$peligro else
          if (pred > 0.4) colores$acento  else
            colores$primario
      } else {
        valor_txt <- as.character(round(pred, 3))
        ic_txt    <- paste0("[", round(lo,3), ", ", round(hi,3), "]")
        etiqueta  <- paste0(input$var_y, " esperado")
        color     <- colores$primario
      }

      tagList(
        div(class = "text-center py-3",
            h2(style = paste0("color:", color,
                              "; font-weight:700; font-size:2.5rem;"),
               valor_txt),
            p(class = "text-muted mb-1", strong(etiqueta)),
            p(class = "small text-muted", "IC 95%: ", strong(ic_txt))
        ),
        tags$hr(),
        div(class = "small text-muted",
            bs_icon("info-circle", class = "me-1"),
            "Valores usados: ",
            paste(sapply(c(input$preds_num, input$preds_cat), function(nm) {
              val <- input[[paste0("pred_val_", nm)]]
              paste0(nm, " = ", val)
            }), collapse = " · ")
        )
      )
    })

    # ────────────────────────────────────────────────────
    # CONTRASTES
    # ────────────────────────────────────────────────────

    # ────────────────────────────────────────────────────
    # CONTRASTES
    # ────────────────────────────────────────────────────

    output$contrasts_no_cat_msg <- renderUI({
      if (length(input$preds_cat) == 0) {
        div(class = "alert alert-warning small py-2 px-3 mb-3",
            bs_icon("exclamation-triangle-fill", class = "me-1"),
            "El modelo no tiene predictores categóricos. ",
            "Ve a ", strong("Construir el modelo"),
            " y agrega al menos una variable categórica.")
      }
    })

    output$sel_var_contraste <- renderUI({
      fit  <- modelo_glm(); req(fit)
      cats <- input$preds_cat; req(length(cats) > 0)
      selectInput(ns("var_contraste"),
                  label    = "Variable para contrastar:",
                  choices  = cats, selected = cats[1])
    })

    output$tabla_contrastes <- renderUI({
      fit <- modelo_glm(); req(fit, input$var_contraste)
      tryCatch({
        ct    <- modelbased::estimate_contrasts(
          fit, contrast = input$var_contraste,
          p_adjust = input$metodo_ajuste, verbose = FALSE)
        df_ct <- as.data.frame(ct)
        char_cols <- names(df_ct)[sapply(df_ct, function(x)
          is.character(x) || is.factor(x))]
        if (length(char_cols) >= 2) {
          c1 <- char_cols[1]; c2 <- char_cols[2]
          etiqueta <- paste0(df_ct[[c1]], " vs. ", df_ct[[c2]])
        } else etiqueta <- paste0("Contraste ", seq_len(nrow(df_ct)))

        diff_col <- if ("Difference" %in% names(df_ct)) "Difference"
        else names(df_ct)[sapply(df_ct, is.numeric)][1]
        ci_lo <- if ("CI_low"  %in% names(df_ct)) "CI_low" else NA
        ci_hi <- if ("CI_high" %in% names(df_ct)) "CI_high" else NA
        p_col <- if ("p" %in% names(df_ct)) "p"
        else if ("p.value" %in% names(df_ct)) "p.value" else NA

        filas <- lapply(seq_len(nrow(df_ct)), function(i) {
          sig   <- if (!is.na(p_col)) !is.na(df_ct[[p_col]][i]) &&
            df_ct[[p_col]][i] < 0.05 else FALSE
          p_txt <- if (!is.na(p_col) && !is.na(df_ct[[p_col]][i])) {
            pv <- df_ct[[p_col]][i]
            if (pv < 0.001) "< 0.001 ***"
            else if (pv < 0.01) paste0(round(pv,3), " **")
            else if (pv < 0.05) paste0(round(pv,3), " *")
            else round(pv, 3)
          } else "—"
          col_p <- if (sig) colores$exito else colores$texto
          tags$tr(
            tags$td(strong(etiqueta[i])),
            tags$td(style="text-align:center;",
                    round(df_ct[[diff_col]][i], 3)),
            tags$td(style="text-align:center;",
                    if (!is.na(ci_lo))
                      paste0("[", round(df_ct[[ci_lo]][i],3), ", ",
                             round(df_ct[[ci_hi]][i],3), "]")
                    else "—"),
            tags$td(style=paste0("text-align:center; color:", col_p,
                                 "; font-weight:600;"), p_txt)
          )
        })

        tags$table(
          class = "table table-sm table-hover small mb-0",
          tags$thead(tags$tr(
            tags$th(style=paste0("background:", colores$primario,
                                 " !important; color:#fff !important;"), "Contraste"),
            tags$th(style=paste0("background:", colores$primario,
                                 " !important; color:#fff !important; text-align:center;"),
                    "Diferencia"),
            tags$th(style=paste0("background:", colores$primario,
                                 " !important; color:#fff !important; text-align:center;"),
                    "IC 95%"),
            tags$th(style=paste0("background:", colores$primario,
                                 " !important; color:#fff !important; text-align:center;"),
                    "p-valor")
          )),
          tags$tbody(filas)
        )
      }, error = function(e) {
        div(class="text-muted small py-3",
            "Ajusta el modelo con predictores categóricos.")
      })
    })

    output$plot_contrastes <- renderPlot({
      fit <- modelo_glm(); req(fit, input$var_contraste)
      tryCatch({
        ct    <- modelbased::estimate_contrasts(
          fit, contrast = input$var_contraste,
          p_adjust = input$metodo_ajuste, verbose = FALSE)
        df_ct <- as.data.frame(ct)
        char_cols <- names(df_ct)[sapply(df_ct, function(x)
          is.character(x) || is.factor(x))]
        if (length(char_cols) >= 2)
          etiqueta <- paste0(df_ct[[char_cols[1]]], " vs. ",
                             df_ct[[char_cols[2]]])
        else etiqueta <- paste0("Contraste ", seq_len(nrow(df_ct)))

        diff_col <- if ("Difference" %in% names(df_ct)) "Difference"
        else names(df_ct)[sapply(df_ct, is.numeric)][1]
        ci_lo <- if ("CI_low"  %in% names(df_ct)) df_ct$CI_low  else
          df_ct[[diff_col]] - 1
        ci_hi <- if ("CI_high" %in% names(df_ct)) df_ct$CI_high else
          df_ct[[diff_col]] + 1
        p_col <- if ("p" %in% names(df_ct)) df_ct$p else
          if ("p.value" %in% names(df_ct)) df_ct$p.value else
            rep(0.5, nrow(df_ct))

        df_plot <- data.frame(
          contraste = factor(etiqueta, levels = rev(unique(etiqueta))),
          diff      = df_ct[[diff_col]],
          lo        = ci_lo, hi = ci_hi,
          sig       = !is.na(p_col) & p_col < 0.05
        )

        ggplot(df_plot, aes(x=diff, y=contraste, xmin=lo, xmax=hi,
                            color=sig)) +
          geom_vline(xintercept=0, linetype="dashed",
                     color=colores$texto, linewidth=0.7) +
          geom_errorbar(aes(ymin=lo, ymax=hi), width=0.25, linewidth=1.1, orientation="y") +
          geom_point(size=3.5) +
          scale_color_manual(
            values=c(`TRUE`=colores$acento, `FALSE`=colores$primario),
            labels=c(`TRUE`="Significativo", `FALSE`="No significativo"),
            name=NULL) +
          labs(x=paste0("Diferencia en ", input$var_y), y=NULL,
               subtitle=paste0("Ajuste: ", input$metodo_ajuste,
                               " · IC 95%")) +
          theme_minimal(base_size=12) +
          theme(panel.grid.minor=element_blank(),
                panel.grid.major.y=element_blank(),
                legend.position="bottom",
                plot.subtitle=element_text(color=colores$texto, size=9),
                plot.margin=margin(10,15,5,10))
      }, error = function(e) {
        ggplot() + annotate("text", x=0.5, y=0.5,
                            label="Sin contrastes disponibles.",
                            color=colores$texto, size=4) + theme_void()
      })
    }, res = 96)

    # ────────────────────────────────────────────────────
    # PERFORMANCE
    # ────────────────────────────────────────────────────

    output$tabla_performance <- renderUI({
      fit <- modelo_glm()
      if (is.null(fit)) return(
        div(class="text-muted small py-3", "Ajusta el modelo primero.")
      )
      tryCatch({
        fam <- input$familia

        # Cada métrica en su propio tryCatch para robustez
        aic_val  <- tryCatch(round(AIC(fit), 2),  error=function(e) NA)
        bic_val  <- tryCatch(round(BIC(fit), 2),  error=function(e) NA)
        aicc_val <- tryCatch(
          round(performance::performance_aicc(fit), 2),
          error=function(e) NA)
        ll_val   <- tryCatch(
          round(as.numeric(logLik(fit)), 2),
          error=function(e) NA)
        rmse_val <- tryCatch(
          round(performance::performance_rmse(fit, verbose=FALSE), 3),
          error=function(e) NA)
        r2_list  <- tryCatch(
          performance::r2(fit, verbose=FALSE),
          error=function(e) list())

        filas <- list(
          list(g="MUESTRA", m="n (observaciones)",
               v=nrow(datos_activos()),
               i="Tamaño de la muestra."),
          list(g=NULL, m="k (predictores)",
               v=tryCatch(
                 if (inherits(fit, "glmmTMB"))
                   length(glmmTMB::fixef(fit)$cond) - 1L
                 else
                   length(coef(fit)) - 1L,
                 error=function(e) "—"),
               i="Número de predictores sin intercepto."),
          list(g="AJUSTE DEL MODELO", m="AIC",
               v=aic_val,
               i="Criterio de Akaike. Menor = mejor."),
          list(g=NULL, m="AICc",
               v=aicc_val,
               i="AIC corregido para muestras pequeñas."),
          list(g=NULL, m="BIC",
               v=bic_val,
               i="Criterio Bayesiano de Schwarz. Menor = mejor.")
        )

        # R² según familia
        if (fam == "binomial") {
          if (!is.null(r2_list$R2_Tjur))
            filas <- c(filas, list(list(
              g=NULL, m="R² Tjur",
              v=round(r2_list$R2_Tjur,4),
              i="R² para regresión logística. Mayor = mejor.")))
          if (!is.null(r2_list$R2_McFadden))
            filas <- c(filas, list(list(
              g=NULL, m="R² McFadden",
              v=round(r2_list$R2_McFadden,4),
              i="Pseudo-R² basado en log-verosimilitud.")))
        } else {
          if (!is.null(r2_list$R2_Nagelkerke))
            filas <- c(filas, list(list(
              g=NULL, m="R² Nagelkerke",
              v=round(r2_list$R2_Nagelkerke,4),
              i="Pseudo-R² para GLM. Mayor = mejor.")))
        }

        if (!is.na(rmse_val))
          filas <- c(filas, list(list(
            g=NULL, m="RMSE",
            v=rmse_val,
            i="Error cuadrático medio. Menor = mejor.")))

        if (!is.na(ll_val))
          filas <- c(filas, list(list(
            g="AJUSTE RELATIVO", m="Log-verosimilitud",
            v=ll_val,
            i="Mayor = mejor ajuste. Base del AIC y BIC.")))


        # Métricas específicas para logística
        if (fam == "binomial") {

          # Reajustar con glm para métricas de clasificación
          preds_cl <- c(input$preds_num, input$preds_cat)
          fm_cl    <- as.formula(paste(input$var_y, "~",
                                       paste(preds_cl, collapse=" + ")))
          fit_cl   <- tryCatch(
            glm(fm_cl, family = stats::binomial(link = input$enlace),
                data = datos_activos()),
            error = function(e) NULL
          )

          if (!is.null(fit_cl)) {

            # 1. AUC
            auc_cl <- tryCatch({
              roc_cl  <- performance::performance_roc(fit_cl)
              df_roc  <- as.data.frame(roc_cl)
              df_roc  <- df_roc[order(1 - df_roc$Specificity,
                                      df_roc$Sensitivity), ]
              round(abs(sum(diff(1 - df_roc$Specificity) *
                              (df_roc$Sensitivity[-1] +
                                 df_roc$Sensitivity[-nrow(df_roc)]) / 2)), 3)
            }, error=function(e) NA)

            if (!is.na(auc_cl))
              filas <- c(filas, list(list(
                g = "CLASIFICACIÓN (logística)",
                m = "AUC",
                v = auc_cl,
                i = paste0(
                  "Área bajo la curva ROC. Mide discriminación: ",
                  "0.5 = azar, 1.0 = perfecta. ",
                  if (auc_cl >= 0.8) "Discriminación buena."
                  else if (auc_cl >= 0.7) "Discriminación aceptable."
                  else if (auc_cl >= 0.6) "Discriminación moderada."
                  else "Discriminación débil."
                )
              )))

            # 2. R² Tjur
            if (!is.null(r2_list$R2_Tjur))
              filas <- c(filas, list(list(
                g = NULL, m = "R² Tjur",
                v = round(r2_list$R2_Tjur, 4),
                i = paste0(
                  "Diferencia entre la probabilidad media predicha ",
                  "para presencias y ausencias. ",
                  "Rango 0-1. Mayor = mejor discriminación."
                )
              )))

            # 3. Sensibilidad, Especificidad, Accuracy
            sens_spec <- tryCatch({
              y_obs  <- as.integer(datos_activos()[[input$var_y]])
              y_pred <- as.integer(fitted(fit_cl) >= 0.5)
              tp <- sum(y_obs==1 & y_pred==1)
              tn <- sum(y_obs==0 & y_pred==0)
              fp <- sum(y_obs==0 & y_pred==1)
              fn <- sum(y_obs==1 & y_pred==0)
              list(
                sens = round(tp/(tp+fn), 3),
                spec = round(tn/(tn+fp), 3),
                acc  = round((tp+tn)/length(y_obs), 3)
              )
            }, error=function(e) NULL)

            if (!is.null(sens_spec)) {
              filas <- c(filas,
                         list(list(g=NULL, m="Sensibilidad",
                                   v=sens_spec$sens,
                                   i=paste0("Proporción de presencias correctamente ",
                                            "clasificadas (umbral = 0.5). ",
                                            "Alta = pocos falsos negativos."))),
                         list(list(g=NULL, m="Especificidad",
                                   v=sens_spec$spec,
                                   i=paste0("Proporción de ausencias correctamente ",
                                            "clasificadas (umbral = 0.5). ",
                                            "Alta = pocos falsos positivos."))),
                         list(list(g=NULL, m="Accuracy",
                                   v=sens_spec$acc,
                                   i=paste0("Proporción total correctamente clasificada ",
                                            "(umbral = 0.5). Puede ser engañosa con ",
                                            "datos desbalanceados.")))
              )
            }

            # 4. Hosmer-Lemeshow (calibración)
            hl <- tryCatch({
              h <- performance::performance_hosmer(fit_cl)
              list(chi2=round(h$chisq,2), p=round(h$p.value,3))
            }, error=function(e) NULL)

            if (!is.null(hl)) {
              filas <- c(filas, list(list(
                g = "CALIBRACIÓN",
                m = "Hosmer-Lemeshow",
                v = paste0("χ² = ", hl$chi2),
                i = if (hl$p > 0.05)
                  paste0("p = ", hl$p,
                         " — buen ajuste (predichos ≈ observados).")
                else
                  paste0("p = ", hl$p,
                         " — ajuste deficiente (predichos ≠ observados). ",
                         "El modelo puede estar mal calibrado.")
              )))
            }
          }
        }

        # Métricas específicas para Poisson y BN
        if (fam %in% c("poisson", "nbinom2")) {

          # D² — devianza explicada
          # Usando devianza del modelo vs modelo nulo de Poisson
          # El modelo nulo Poisson tiene devianza conocida:
          # D_nula = 2 * sum(y * log(y/mean(y)) - (y - mean(y)))
          d2 <- tryCatch({
            df_y <- datos_activos()
            y    <- as.numeric(df_y[[input$var_y]])
            mu_0 <- mean(y, na.rm = TRUE)
            # Devianza nula Poisson
            nd <- 2 * sum(
              ifelse(y > 0, y * log(y / mu_0), 0) - (y - mu_0),
              na.rm = TRUE
            )
            rd <- deviance(fit)
            round((nd - rd) / nd, 4)
          }, error = function(e) NA)

          if (!is.na(d2))
            filas <- c(filas, list(list(
              g = "CONTEOS",
              m = "D² (devianza explicada)",
              v = d2,
              i = paste0(
                "Proporción de la devianza nula explicada por el modelo. ",
                "Equivalente al R² para GLMs. ",
                if (d2 >= 0.5) "Ajuste bueno."
                else if (d2 >= 0.3) "Ajuste moderado."
                else "Ajuste débil."
              )
            )))

          # Sobredispersión
          disp <- tryCatch({
            od <- performance::check_overdispersion(fit)
            list(ratio = round(od$dispersion_ratio, 3),
                 p     = round(od$p_value, 3))
          }, error = function(e) NULL)

          if (!is.null(disp)) {
            disp_interp <- if (fam == "nbinom2")
              "La binomial negativa modela la sobredispersión explícitamente."
            else if (disp$ratio < 1.5)
              paste0("Ratio = ", disp$ratio,
                     " — sin sobredispersión. Poisson apropiado.")
            else if (disp$ratio < 3)
              paste0("Ratio = ", disp$ratio,
                     " — sobredispersión moderada. ",
                     "Considera binomial negativa.")
            else
              paste0("Ratio = ", disp$ratio,
                     " — sobredispersión severa. ",
                     "Usa binomial negativa.")

            filas <- c(filas, list(list(
              g = NULL,
              m = "Ratio de dispersión",
              v = if (fam == "nbinom2") "N/A" else disp$ratio,
              i = disp_interp
            )))
          }

          # Inflación de ceros
          zi <- tryCatch({
            cz <- performance::check_zeroinflation(fit)
            list(obs  = cz$observed.zeros,
                 pred = round(cz$predicted.zeros, 1),
                 ratio = round(cz$ratio, 3))
          }, error = function(e) NULL)

          if (!is.null(zi)) {
            zi_interp <- if (zi$ratio < 1.2)
              paste0("Ceros obs. = ", zi$obs,
                     " · predichos = ", zi$pred,
                     " — sin inflación de ceros.")
            else if (zi$ratio < 1.5)
              paste0("Ceros obs. = ", zi$obs,
                     " · predichos = ", zi$pred,
                     " — posible inflación. Verifica con gráfico de diagnóstico.")
            else
              paste0("Ceros obs. = ", zi$obs,
                     " · predichos = ", zi$pred,
                     " — inflación severa. Considera modelo ZIP o ZINB.")

            filas <- c(filas, list(list(
              g = NULL,
              m = "Ceros observados / predichos",
              v = paste0(zi$obs, " / ", zi$pred),
              i = zi_interp
            )))
          }
        }

        bg_map <- list(
          "MUESTRA"                    = "#F4F7FB",
          "AJUSTE DEL MODELO"           = "#EEF3FA",
          "AJUSTE RELATIVO"             = "#FFF5EC",
          "CLASIFICACIÓN (logística)" = "#FFF0F5",
          "CALIBRACIÓN"                = "#F0FFF4",
          "CONTEOS"                     = "#F0F8FF"
        )
        grupo_actual <- ""
        bg_actual    <- "#ffffff"

        filas_html <- lapply(filas, function(f) {
          if (!is.null(f$g) && nchar(f$g) > 0) {
            grupo_actual <<- f$g
            bg_actual    <<- bg_map[[f$g]] %||% "#ffffff"
          }
          tagList(
            if (!is.null(f$g) && nchar(f$g) > 0)
              tags$tr(
                style = paste0(
                  "background:", colores$secundario,
                  " !important; border-top: 2px solid ",
                  colores$primario, ";"),
                tags$td(colspan="3",
                        style=paste0(
                          "background:", colores$secundario,
                          " !important; padding:4px 10px; ",
                          "font-size:0.75rem; font-weight:700; ",
                          "letter-spacing:0.8px; color:#ffffff !important; ",
                          "text-transform:uppercase;"),
                        f$g)),
            tags$tr(
              style=paste0("background:", bg_actual, ";"),
              tags$td(style="padding:6px 10px;",
                      tags$span(style=paste0("color:",colores$primario,
                                             "; font-weight:700;"),
                                f$m)),
              tags$td(style=paste0("text-align:center; padding:6px 10px;",
                                   "font-weight:700; font-size:0.95rem;",
                                   "color:", colores$texto, ";"),
                      f$v),
              tags$td(style=paste0("padding:6px 10px; font-size:0.82rem;",
                                   "line-height:1.5; color:", colores$texto,";"),
                      f$i)
            )
          )
        })

        tags$table(
          class="table table-sm table-hover small mb-0",
          style="background:#ffffff;",
          tags$thead(tags$tr(
            tags$th(style=paste0("background:",colores$primario,
                                 " !important; color:#fff !important; padding:8px 10px;",
                                 " width:22%;"), "Métrica"),
            tags$th(style=paste0("background:",colores$primario,
                                 " !important; color:#fff !important; padding:8px 10px;",
                                 " text-align:center; width:13%;"), "Valor"),
            tags$th(style=paste0("background:",colores$primario,
                                 " !important; color:#fff !important; padding:8px 10px;"),
                    "Interpretación")
          )),
          tags$tbody(filas_html)
        )
      }, error=function(e) {
        div(class="text-muted small", "Error calculando métricas.")
      })
    })

    # Validación cruzada
    # Curva ROC — solo para logística
    output$card_roc <- renderUI({
      fit <- modelo_glm(); req(fit)
      if (input$familia != "binomial") return(NULL)
      card(
        class = "mb-3",
        card_header(
          bs_icon("graph-up", class = "me-1"),
          "Curva ROC",
          span(class = "text-muted small ms-2",
               "— performance_roc() · easystats")
        ),
        card_body(
          p(class = "small text-muted mb-2",
            "La curva ROC muestra el balance entre sensibilidad y ",
            "especificidad a distintos umbrales. El área bajo la curva ",
            strong("(AUC)"), " mide la discriminación: 0.5 = azar, ",
            "1.0 = discriminación perfecta."
          ),
          plotOutput(ns("plot_roc"), height = "240px")
        )
      )
    })

    output$plot_roc <- renderPlot({
      fit <- modelo_glm(); req(fit)
      req(input$familia == "binomial")
      tryCatch({
        # Reajustar con glm para performance_roc
        preds_fm <- c(input$preds_num, input$preds_cat)
        fm_roc   <- as.formula(paste(input$var_y, "~",
                                     paste(preds_fm, collapse=" + ")))
        fit_glm  <- glm(fm_roc,
                        family = stats::binomial(link = input$enlace),
                        data   = datos_activos())

        roc_data <- performance::performance_roc(fit_glm)
        df_roc   <- as.data.frame(roc_data)

        # Calcular AUC con regla del trapecio
        df_roc   <- df_roc[order(df_roc$Specificity), ]
        auc_val  <- round(
          abs(sum(diff(1 - df_roc$Specificity) *
                    (df_roc$Sensitivity[-1] +
                       df_roc$Sensitivity[-nrow(df_roc)]) / 2)),
          3
        )

        # Usar plot() de performance directamente
        p <- plot(roc_data) +
          ggplot2::scale_color_manual(
            values = colores$primario
          ) +
          ggplot2::labs(
            subtitle = paste0("AUC = ", auc_val,
                              " — datos de entrenamiento")
          ) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(
            panel.grid.minor = element_blank(),
            legend.position  = "none",
            plot.subtitle    = ggplot2::element_text(
              color = colores$texto, size = 9),
            plot.margin      = ggplot2::margin(5, 15, 5, 5)
          )
        print(p)
      }, error = function(e) {
        ggplot() + annotate("text", x=0.5, y=0.5,
                            label="Ajusta el modelo primero.",
                            color=colores$texto, size=4) + theme_void()
      })
    }, res = 96)

    cv_resultados <- reactiveVal(NULL)

    observeEvent(input$correr_cv, {
      fit <- modelo_glm()
      if (is.null(fit)) {
        showNotification("Ajusta un modelo primero.",
                         type="warning", duration=3)
        return()
      }
      withProgress(message="Corriendo validación cruzada...",
                   value=0.2, {
                     tryCatch({
                       df_cv <- datos_activos()
                       preds <- c(input$preds_num, input$preds_cat)
                       req(length(preds) > 0, input$var_y)
                       fam   <- input$familia

                       # Para binomial: convertir Y a factor (requerido por parsnip)
                       if (fam == "binomial") {
                         df_cv[[input$var_y]] <- factor(
                           df_cv[[input$var_y]],
                           levels = c(0, 1),
                           labels = c("ausente", "presente")
                         )
                       }

                       folds <- rsample::vfold_cv(
                         df_cv, v = input$cv_folds,
                         strata = if (isTRUE(input$cv_estratificado))
                           input$var_y else NULL
                       )

                       fm <- as.formula(paste(input$var_y, "~",
                                              paste(preds, collapse=" + ")))

                       # Modelo parsnip según familia
                       modelo_parsnip <- switch(fam,
                                                "binomial" = parsnip::logistic_reg() |>
                                                  parsnip::set_engine("glm") |>
                                                  parsnip::set_mode("classification"),
                                                "poisson" = parsnip::poisson_reg() |>
                                                  parsnip::set_engine("glm") |>
                                                  parsnip::set_mode("regression"),
                                                # nbinom2: aproximar con linear_reg
                                                parsnip::linear_reg() |>
                                                  parsnip::set_engine("lm") |>
                                                  parsnip::set_mode("regression")
                       )

                       tiene_cat <- any(sapply(df_cv[, preds, drop=FALSE],
                                               function(x) is.factor(x) || is.character(x)))
                       rec <- recipes::recipe(fm, data=df_cv)
                       if (tiene_cat)
                         rec <- recipes::step_dummy(
                           rec, recipes::all_nominal_predictors())
                       rec <- rec |>
                         recipes::step_impute_median(
                           recipes::all_numeric_predictors()) |>
                         recipes::step_zv(recipes::all_predictors())

                       wf <- workflows::workflow() |>
                         workflows::add_recipe(rec) |>
                         workflows::add_model(modelo_parsnip)

                       incProgress(0.5, detail="Evaluando folds...")

                       # Métricas según familia
                       metricas <- if (fam == "binomial") {
                         yardstick::metric_set(
                           yardstick::roc_auc,
                           yardstick::accuracy
                         )
                       } else {
                         yardstick::metric_set(
                           yardstick::rmse,
                           yardstick::rsq,
                           yardstick::mae
                         )
                       }

                       # Para clasificación necesitamos probabilidades
                       ctrl <- if (fam == "binomial")
                         tune::control_resamples(save_pred = TRUE)
                       else
                         tune::control_resamples()

                       res_cv <- tune::fit_resamples(
                         wf, resamples = folds,
                         metrics = metricas,
                         control = ctrl
                       )
                       cm <- tune::collect_metrics(res_cv)

                       cv_resultados(list(
                         formula  = deparse(fm),
                         folds    = input$cv_folds,
                         familia  = fam,
                         metricas = cm
                       ))

                     }, error=function(e) {
                       showNotification(paste("Error en CV:", conditionMessage(e)),
                                        type="error", duration=6)
                     })
                   })
    })


    output$resultado_cv <- renderUI({
      res <- cv_resultados()
      if (is.null(res)) return(
        div(class="text-muted small py-3",
            bs_icon("arrow-repeat", class="me-2"),
            "Haz clic en ", strong("Correr CV"),
            " para evaluar la capacidad predictiva.")
      )
      cm  <- res$metricas
      fam <- res$familia

      tarjetas <- lapply(seq_len(nrow(cm)), function(i) {
        card(class="text-center", card_body(class="p-2",
                                            h4(style=paste0("color:", colores$primario,
                                                            "; font-weight:700;"),
                                               round(cm$mean[i], 3)),
                                            p(class="small text-muted mb-0",
                                              strong(toupper(cm$.metric[i]))),
                                            p(class="small text-muted mb-0",
                                              paste0("\u00b1", round(cm$std_err[i], 3), " EE"))
        ))
      })

      tagList(
        do.call(layout_columns,
                c(list(col_widths=rep(12/nrow(cm), nrow(cm))),
                  tarjetas)),
        div(class="alert alert-info small py-2 px-3 mt-2 mb-0",
            bs_icon("info-circle", class="me-1"),
            strong(paste0(res$folds, "-fold CV · ")),
            "Fórmula: ", code(res$formula))
      )
    })

    # ────────────────────────────────────────────────────
    # COMPARAR MODELOS
    # ────────────────────────────────────────────────────

    modelos_guardados <- reactiveVal(list())

    observeEvent(input$guardar_modelo, {
      fit    <- modelo_glm()
      nombre <- trimws(input$nombre_modelo)
      if (is.null(fit)) {
        showNotification("Ajusta un modelo primero.",
                         type="warning", duration=3)
        return()
      }
      if (nchar(nombre) == 0) {
        showNotification("Escribe un nombre.",
                         type="warning", duration=3)
        return()
      }
      actual <- modelos_guardados()
      actual[[nombre]] <- list(
        fit     = fit,
        formula = deparse(formula(fit)),
        preds   = c(input$preds_num, input$preds_cat),
        var_y   = input$var_y,
        familia = input$familia,
        datos   = datos_activos()
      )
      modelos_guardados(actual)
      showNotification(paste0("Modelo '", nombre, "' guardado."),
                       type="message", duration=3)
      updateTextInput(session, "nombre_modelo", value="")
    })

    observeEvent(input$limpiar_modelos, {
      modelos_guardados(list())
      showNotification("Modelos eliminados.", type="message",
                       duration=2)
    })

    output$lista_modelos_guardados <- renderUI({
      mg <- modelos_guardados()
      if (length(mg) == 0) return(
        p(class="small text-muted mb-0", "Aún no hay modelos guardados.")
      )
      tagList(lapply(names(mg), function(nm) {
        m <- mg[[nm]]
        div(class="d-flex align-items-center gap-2 mb-1",
            bs_icon("check-circle-fill",
                    style=paste0("color:", colores$exito)),
            div(p(class="small mb-0", strong(nm)),
                p(class="small text-muted mb-0",
                  style="font-size:0.75rem;", m$formula)))
      }))
    })

    output$tabla_comparacion <- renderUI({
      mg <- modelos_guardados()
      if (length(mg) < 1) return(
        div(class="text-muted small py-3",
            bs_icon("info-circle", class="me-1"),
            "Guarda al menos un modelo para ver la comparación.")
      )
      rows <- lapply(names(mg), function(nm) {
        fit <- mg[[nm]]$fit
        pm  <- tryCatch(
          performance::model_performance(fit, verbose=FALSE),
          error=function(e) NULL)
        if (is.null(pm)) return(NULL)
        list(nm=nm, aic=round(pm$AIC,1),
             aicc=round(performance::performance_aicc(fit),1),
             bic=round(pm$BIC,1),
             fam=mg[[nm]]$familia)
      })
      rows <- rows[!sapply(rows, is.null)]
      if (length(rows) == 0) return(NULL)

      best_aicc <- which.min(sapply(rows, function(r) r$aicc))

      tags$table(
        class="table table-sm table-hover small mb-0",
        tags$thead(style=paste0("background:",colores$primario,
                                "; color:#fff;"),
                   tags$tr(
                     tags$th("Modelo"), tags$th("Familia"),
                     tags$th("AIC"), tags$th("AICc"), tags$th("BIC")
                   )),
        tags$tbody(lapply(seq_along(rows), function(i) {
          r  <- rows[[i]]
          bg <- if (i==best_aicc)
            "background:#f0f9f5; font-weight:600;" else ""
          tags$tr(style=bg,
                  tags$td(if(i==best_aicc)
                    tagList(bs_icon("trophy-fill",
                                    style=paste0("color:",colores$acento,
                                                 "; margin-right:4px")), r$nm)
                    else r$nm),
                  tags$td(r$fam),
                  tags$td(r$aic), tags$td(r$aicc), tags$td(r$bic))
        }))
      )
    })

    output$plot_comparacion_aic <- renderPlot({
      mg <- modelos_guardados(); req(length(mg) >= 2)
      fits <- lapply(mg, function(m) m$fit)
      tryCatch({
        comp <- do.call(performance::compare_performance,
                        c(fits, list(rank=TRUE, verbose=FALSE)))
        p <- plot(comp) +
          ggplot2::scale_color_manual(
            values=colores$tableau[seq_along(mg)]) +
          ggplot2::scale_fill_manual(
            values=paste0(colores$tableau[seq_along(mg)], "33")) +
          ggplot2::labs(title=NULL,
                        subtitle="Métricas normalizadas 0–1 · mayor área = mejor") +
          see::theme_radar() +
          ggplot2::theme(legend.position="bottom",
                         plot.subtitle=ggplot2::element_text(
                           color=colores$texto, size=9),
                         plot.margin=ggplot2::margin(10,10,5,10))
        print(p)
      }, error=function(e) {
        ggplot() + annotate("text", x=0.5, y=0.5,
                            label=paste0("Guarda al menos 2 modelos\n",
                                         "para ver el gráfico radar."),
                            color=colores$texto, size=5, hjust=0.5) + theme_void()
      })
    }, res=96)

    # ────────────────────────────────────────────────────
    # DIAGNÓSTICO
    # ────────────────────────────────────────────────────

    supuestos_data_glm <- reactive({
      fit <- modelo_glm(); req(fit)
      fam <- input$familia

      # Sobredispersión
      disp <- tryCatch({
        od <- performance::check_overdispersion(fit)
        list(stat=round(od$dispersion_ratio,2),
             p=od$p_value)
      }, error=function(e) list(stat=NA, p=NA))

      # Ceros
      zi <- tryCatch({
        cz <- performance::check_zeroinflation(fit)
        list(obs=cz$observed.zeros,
             pred=round(cz$predicted.zeros,1),
             ratio=round(cz$ratio,2))
      }, error=function(e) list(obs=NA, pred=NA, ratio=NA))

      list(
        list(
          nombre = "Distribución de la familia",
          def    = paste0("La familia elegida (", fam,
                          ") debe corresponder al tipo de Y."),
          st     = "ok",
          ok     = paste0("Familia seleccionada: ", fam, "."),
          warn   = "Verifica que la familia sea apropiada para tu Y.",
          bad    = "Familia incorrecta — el modelo no es válido."
        ),
        list(
          nombre = "Independencia",
          def    = "Las observaciones no deben estar correlacionadas.",
          st     = "ok",
          ok     = "Asumida según el diseño. Verifica si hay estructura jerárquica.",
          warn   = "Posible dependencia. Considera GLMM.",
          bad    = "Dependencia clara. Usa modelos mixtos."
        ),
        list(
          nombre = "Sobredispersión",
          def    = if (fam=="binomial") "No aplica para binomial."
          else "Varianza/media debe ser ≈ 1 en Poisson.",
          st     = if (fam=="binomial") "ok" else
            if (is.na(disp$stat)) "ok" else
              if (disp$stat < 1.5) "ok" else
                if (disp$stat < 3) "warn" else "bad",
          ok     = if (fam=="binomial") "No aplica para binomial."
          else if (is.na(disp$stat)) "No calculado."
          else paste0("Ratio = ", disp$stat, " — sin sobredispersión."),
          warn   = paste0("Ratio = ", disp$stat,
                          " — sobredispersión moderada. Considera binomial negativa."),
          bad    = paste0("Ratio = ", disp$stat,
                          " — sobredispersión severa. Usa binomial negativa.")
        ),
        list(
          nombre = "Inflación de ceros",
          def    = "No deben haber más ceros de los esperados.",
          st     = if (is.na(zi$ratio)) "ok" else
            if (zi$ratio < 1.2) "ok" else
              if (zi$ratio < 1.5) "warn" else "bad",
          ok     = if (is.na(zi$ratio)) "No calculado."
          else paste0("Ceros observados = ", zi$obs,
                      " · predichos = ", zi$pred, " — sin inflación."),
          warn   = paste0("Ceros observados = ", zi$obs,
                          " · predichos = ", zi$pred,
                          " — posible inflación. Considera modelo ZIP/ZINB."),
          bad    = paste0("Ceros observados = ", zi$obs,
                          " · predichos = ", zi$pred,
                          " — inflación severa. Usa modelo zero-inflated.")
        ),
        list(
          nombre = "Multicolinealidad (VIF)",
          def    = "Los predictores no deben estar muy correlacionados (VIF < 5).",
          st     = tryCatch({
            if (length(c(input$preds_num, input$preds_cat)) < 2) "ok" else {
              vif_max <- max(performance::check_collinearity(fit)$VIF,
                             na.rm=TRUE)
              if (vif_max < 3) "ok" else if (vif_max < 5) "warn" else "bad"
            }
          }, error=function(e) "ok"),
          ok     = "VIF < 3 — sin problema.",
          warn   = "VIF 3–5 — moderado, aceptable.",
          bad    = "VIF > 5 — problemático. Elimina predictores redundantes."
        ),

        # Separación perfecta (solo logística)
        list(
          nombre = "Separación perfecta",
          def    = paste0(
            if (fam == "binomial")
              "Una variable predice perfectamente el outcome en alguna categoría."
            else
              "No aplica para esta familia."
          ),
          st     = tryCatch({
            if (fam != "binomial") "ok" else {
              se_vals <- sqrt(diag(vcov(fit)$cond))
              if (any(se_vals > 100, na.rm = TRUE)) "bad"
              else if (any(se_vals > 10, na.rm = TRUE)) "warn"
              else "ok"
            }
          }, error = function(e) "ok"),
          ok   = if (fam == "binomial")
            "No se detectó separación perfecta — coeficientes confiables."
          else
            "No aplica para esta familia.",
          warn = paste0(
            "Posible cuasi-separación: errores estándar grandes. ",
            "Verifica con table(predictor, Y). Considera eliminar ",
            "el predictor problemático o usar regresión de Firth."
          ),
          bad  = paste0(
            "Separación perfecta detectada: una categoría predice ",
            "perfectamente Y = 0 o Y = 1. Los coeficientes y OR de ",
            "ese predictor son inválidos (EE enormes, IC [0, Inf]). ",
            "Verifica con table(predictor, Y). Opciones: combinar ",
            "categorías, excluir el predictor, o usar regresión ",
            "logística penalizada (método de Firth)."
          )
        )
      )
    })

    col_map_sem   <- list(ok=colores$exito, warn=colores$acento,
                          bad=colores$peligro)
    icon_map_sem  <- list(ok="check-circle-fill",
                          warn="exclamation-triangle-fill",
                          bad="x-circle-fill")
    label_map_sem <- list(ok="Cumplido", warn="Atención", bad="Problema")
    bg_map_sem    <- list(ok="#f0f9f5", warn="#fffbf0", bad="#fff0f2")

    sem_item_glm <- function(s) {
      col   <- col_map_sem[[s$st]]
      icono <- icon_map_sem[[s$st]]
      lbl   <- label_map_sem[[s$st]]
      bg    <- bg_map_sem[[s$st]]
      div(class="d-flex align-items-start gap-2 p-2 rounded mb-2",
          style=paste0("background:", bg,
                       "; border-left: 4px solid ", col, ";"),
          bs_icon(icono, size="1.1em",
                  style=paste0("color:", col,
                               "; flex-shrink:0; margin-top:2px")),
          div(p(class="small mb-0",
                strong(s$nombre), " ",
                tags$span(class="badge",
                          style=paste0("background:", col,
                                       "; font-size:0.7rem;"), lbl), br(),
                tags$span(class="text-muted",
                          style="font-size:0.78rem;", s$def), br(),
                tags$span(style="font-size:0.82rem;", s[[s$st]]))))
    }

    output$semaforo_col1 <- renderUI({
      sp <- supuestos_data_glm(); req(sp)
      do.call(tagList, lapply(sp[1:3], sem_item_glm))
    })

    output$semaforo_col2 <- renderUI({
      sp <- supuestos_data_glm(); req(sp)
      # sp[4:6]: independencia, VIF, separación perfecta
      idx <- seq(4, length(sp))
      do.call(tagList, lapply(sp[idx], sem_item_glm))
    })

    output$plot_check_model <- renderPlot({
      fit <- modelo_glm(); req(fit)
      tryCatch({
        checks <- if (inherits(fit, "glmmTMB"))
          c("pp_check", "linearity", "homogeneity", "vif",
            "overdispersion", "zero_inflation", "reqq")
        else
          NULL  # NULL = todos los chequeos por defecto (incluye outliers)
        cm <- suppressMessages(suppressWarnings(
          performance::check_model(fit, verbose=FALSE, check=checks)
        ))
        suppressMessages(suppressWarnings(
          plot(cm, panel=TRUE, base_size=11, dot_size=1.5,
               line_size=0.8,
               colors=c(colores$primario, colores$acento,
                        colores$secundario))
        ))
      }, error=function(e) {
        ggplot() + annotate("text", x=0.5, y=0.5,
                            label="Ajusta el modelo primero.",
                            color=colores$texto, size=5, hjust=0.5) + theme_void()
      })
    }, res=72, width="auto", height=650)

    # ────────────────────────────────────────────────────
    # CÓDIGO R
    # ────────────────────────────────────────────────────

    codigo_generado <- reactive({
      req(input$var_y)
      preds    <- c(input$preds_num, input$preds_cat)
      ints     <- input$interacciones
      terminos <- if (!is.null(ints) && length(ints) > 0)
        c(preds, ints) else preds
      formula_txt <- if (length(terminos) > 0)
        paste(input$var_y, "~", paste(terminos, collapse=" + "))
      else paste(input$var_y, "~ 1")

      # Offset
      offset_txt <- ""
      if (!is.null(input$offset_var) &&
          input$offset_var != "ninguno")
        offset_txt <- paste0(' + offset(log(', input$offset_var, '))')

      fam_txt <- switch(input$familia,
                        "binomial" = 'binomial(link = "logit")',
                        "poisson"  = 'poisson(link = "log")',
                        "nbinom2"  = 'glmmTMB::nbinom2(link = "log")',
      )

      fuente <- input$fuente_datos
      carga <- if (fuente == "mite_log")
        'load("data/mite_logistic.rda")\ndatos <- mite_logistic\n'
      else if (fuente == "mite_poi")
        'load("data/mite_counts.rda")\ndatos <- mite_counts\n'
      else if (fuente == "pima")
        'load("data/pima_glm.rda")\ndatos <- pima_glm\n'
      else if (fuente == "cowles")
        'load("data/cowles_glm.rda")\ndatos <- cowles_glm\n'
      else if (fuente == "ants")
        'load("data/ants_glm.rda")\ndatos <- ants_glm\n'
      else if (fuente == "insurance")
        'load("data/insurance_glm.rda")\ndatos <- insurance_glm\n'
      else if (fuente == "danish")
        'load("data/danish_glm.rda")\ndatos <- danish_glm\n'
      else if (fuente == "hcrabs")
        'load("data/hcrabs_glm.rda")\ndatos <- hcrabs_glm\n'
      else
        'datos <- read.csv("tu_archivo.csv")\n'

      encabezado <- encabezado_script("StatModels",
                                      "Modelo lineal generalizado (GLM)")

      paste0(
        encabezado,
        "# \u2500\u2500 Paquetes \u2500\u2500\n",
        "library(glmmTMB)\n",
        "library(parameters)   # easystats\n",
        "library(performance)  # easystats\n",
        "library(modelbased)   # easystats\n\n",
        "# \u2500\u2500 Datos \u2500\u2500\n",
        carga, "\n",
        "# \u2500\u2500 Ajuste del modelo \u2500\u2500\n",
        "fit <- glmmTMB(\n",
        "  ", formula_txt, offset_txt, ",\n",
        "  family = ", fam_txt, ",\n",
        "  data   = datos\n",
        ")\n\n",
        "# \u2500\u2500 Parámetros \u2500\u2500\n",
        "model_parameters(fit)                    # coeficientes\n",
        "model_parameters(fit, exponentiate=TRUE)  # OR o IRR\n\n",
        "# \u2500\u2500 Performance \u2500\u2500\n",
        "model_performance(fit)\n",
        "r2(fit)\n\n",
        "# \u2500\u2500 Diagnóstico \u2500\u2500\n",
        "check_model(fit)\n",
        if (input$familia %in% c("poisson","nbinom2"))
          paste0("check_overdispersion(fit)\n",
                 "check_zeroinflation(fit)\n")
        else if (input$familia == "binomial")
          paste0("binned_residuals(fit)\n",
                 "performance_hosmer(fit)\n",
                 "performance_roc(fit)\n"),
        "\n",
        "# \u2500\u2500 Efectos marginales \u2500\u2500\n",
        "estimate_relation(fit, by = 'predictor')\n\n",
        "# \u2500\u2500 Contrastes \u2500\u2500\n",
        "estimate_contrasts(fit, contrast = 'variable_categorica')\n"
      )
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descargar_script <- downloadHandler(
      filename = function()
        paste0("statmodels_glm_", format(Sys.Date(),"%Y%m%d"), ".R"),
      content = function(file)
        writeLines(codigo_generado(), file)
    )

  }) # fin moduleServer
}
