# ============================================================
# mod_lm.R — Modelo lineal general (LM)
# StatModels · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Familia: regresión simple, múltiple, ANOVA, ANCOVA
# Datos: palmerpenguins (ejemplo) o CSV/XLSX propio
# Ecosistema: tidymodels + easystats
#
# Filosofía: didáctico, sin conocimiento previo de programación
# El código R es secundario (pestaña final, descargable)
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_lm_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("graph-up", class = "me-2"),
        "Modelo lineal general (LM)",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Una familia de modelos que relaciona una variable respuesta ",
        "numérica continua con uno o más predictores. Incluye la regresión ",
        "lineal simple y múltiple, el ANOVA y el ANCOVA — todos bajo el ",
        "mismo marco matemático: ", strong("Y = β₀ + β₁X₁ + β₂X₂ + … + ε"), "."
      )
    ),

    navset_card_tab(

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "¿Qué es?"),
        card_body(

          # ── Contexto del dataset ────────────────────
          div(
            class = "alert alert-info small py-2 px-3 mb-3",
            bs_icon("info-circle-fill", class = "me-1"),
            strong("Dataset de ejemplo: palmerpenguins."),
            " Medidas morfológicas de ",
            strong("333 pingüinos antárticos"),
            " de 3 especies (Adelie, Chinstrap y Gentoo) recolectadas ",
            "en las Islas Palmer, Antártida (Horst, Hill & Gorman, 2020). ",
            "Variables disponibles: peso corporal (g), longitud de aleta (mm), ",
            "largo y profundidad del pico (mm), especie, isla y sexo. ",
            "Usaremos este dataset para ilustrar todos los modelos."
          ),

          # ── Variable respuesta ─────────────────────
          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Variable respuesta en el modelo lineal general"),
          p(class = "small text-muted mb-3",
            "En todos los modelos de esta familia, la variable que queremos ",
            "predecir o explicar (Y) debe ser ", strong("numérica y continua"),
            " — como el peso en gramos, la temperatura, o la concentración ",
            "de una sustancia. Lo que varía entre modelos es el tipo ",
            "de predictores (X) que incluimos."
          ),

          # ── Tabla comparativa ──────────────────────
          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "La familia del modelo lineal general"),
          p(class = "small text-muted mb-2",
            "Todos comparten la misma ecuación base. Lo que cambia es ",
            "el tipo y número de predictores incluidos."
          ),

          div(
            style = "overflow-x: auto;",
            tags$table(
              class = "table table-sm table-bordered small mb-0",
              style = "background: #ffffff;",

              # Encabezado
              tags$thead(
                style = paste0("background:", colores$primario,
                               "; color: #ffffff;"),
                tags$tr(
                  tags$th("Modelo"),
                  tags$th("Variable Y"),
                  tags$th("Predictores X"),
                  tags$th("Pregunta que responde"),
                  tags$th(paste0("Ejemplo con ",
                                 "palmerpenguins"))
                )
              ),

              tags$tbody(
                # Regresión simple
                tags$tr(
                  tags$td(
                    tags$span(
                      class = "badge",
                      style = paste0("background:", colores$primario),
                      "Regresión simple"
                    )
                  ),
                  tags$td(
                    tags$span(
                      style = paste0("color:", colores$exito,
                                     "; font-weight:600;"),
                      "Numérica continua"
                    )
                  ),
                  tags$td("1 numérico continuo"),
                  tags$td("¿Cómo cambia Y al aumentar X en una unidad?"),
                  tags$td(
                    style = paste0("color:", colores$texto),
                    "¿El peso (g) aumenta con la longitud de aleta (mm)?"
                  )
                ),

                # Regresión múltiple
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(
                    tags$span(
                      class = "badge",
                      style = paste0("background:", colores$secundario),
                      "Regresión múltiple"
                    )
                  ),
                  tags$td(
                    tags$span(
                      style = paste0("color:", colores$exito,
                                     "; font-weight:600;"),
                      "Numérica continua"
                    )
                  ),
                  tags$td("2 o más numéricos continuos"),
                  tags$td("¿Cuál es el efecto de cada X controlando las demás?"),
                  tags$td(
                    style = paste0("color:", colores$texto),
                    "¿El peso depende de la aleta y el largo del pico al mismo tiempo?"
                  )
                ),

                # ANOVA
                tags$tr(
                  tags$td(
                    tags$span(
                      class = "badge",
                      style = paste0("background:", colores$acento),
                      "ANOVA"
                    )
                  ),
                  tags$td(
                    tags$span(
                      style = paste0("color:", colores$exito,
                                     "; font-weight:600;"),
                      "Numérica continua"
                    )
                  ),
                  tags$td("1 o más categóricos (grupos)"),
                  tags$td("¿Difiere el promedio de Y entre los grupos?"),
                  tags$td(
                    style = paste0("color:", colores$texto),
                    "¿Difiere el peso promedio entre las 3 especies?"
                  )
                ),

                # ANCOVA
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(
                    tags$span(
                      class = "badge",
                      style = paste0("background:", colores$peligro),
                      "ANCOVA"
                    )
                  ),
                  tags$td(
                    tags$span(
                      style = paste0("color:", colores$exito,
                                     "; font-weight:600;"),
                      "Numérica continua"
                    )
                  ),
                  tags$td("Numérico(s) continuo(s) + categórico(s)"),
                  tags$td("¿Cuál es el efecto de X controlando por grupo?"),
                  tags$td(
                    style = paste0("color:", colores$texto),
                    "¿Depende el peso de la aleta, controlando por especie?"
                  )
                )
              )
            )
          ),

          tags$hr(),

          # ── Cuándo NO usar LM ──────────────────────
          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "¿Cuándo NO usar el modelo lineal general?"),

          layout_columns(
            col_widths = c(4, 4, 4),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("x-circle-fill", class = "me-2",
                      style = paste0("color:", colores$peligro)),
              strong("Y binaria (sí/no, vivo/muerto)"), br(),
              "Usa ", strong("GLM con familia binomial"),
              " (regresión logística)."
            ),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("x-circle-fill", class = "me-2",
                      style = paste0("color:", colores$peligro)),
              strong("Y = conteos (0, 1, 2, …)"), br(),
              "Usa ", strong("GLM con familia Poisson"),
              " o binomial negativa."
            ),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("x-circle-fill", class = "me-2",
                      style = paste0("color:", colores$peligro)),
              strong("Datos agrupados o repetidos"), br(),
              "Usa ", strong("modelos mixtos (LMM)"),
              " para manejar la estructura jerárquica."
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

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Supuestos del modelo lineal general"),
          p(class = "small text-muted mb-3",
            "Para que el modelo funcione bien y sus conclusiones sean ",
            "confiables, los datos deben cumplir cinco condiciones. ",
            "Si alguna falla, las estimaciones o los errores estándar ",
            "pueden ser incorrectos. La pestaña ",
            strong("Diagnóstico"),
            " verifica estos supuestos de forma automática una vez ",
            "ajustado el modelo."
          ),

          # ── Supuesto 1: Linealidad ──────────────────
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$primario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("graph-up",
                        style = paste0("color:", colores$primario,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$primario,
                                  "; font-weight:700;"),
                   "1. Linealidad")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("¿Qué significa?")),
                p(class = "small text-muted mb-0",
                  "La relación entre cada predictor (X) y la variable ",
                  "respuesta (Y) debe ser aproximadamente lineal — ",
                  "representable con una línea recta, no con una curva. ",
                  "No implica que la relación sea perfecta, sino que ",
                  "una línea es una aproximación razonable.")
              ),
              div(
                p(class = "small mb-1", strong("¿Cómo verificarlo?")),
                p(class = "small text-muted mb-1",
                  "Gráfico de ", strong("residuos vs. valores ajustados."),
                  " Los puntos deben distribuirse aleatoriamente ",
                  "alrededor del cero, sin patrón en forma de U, S u ola."),
                div(
                  class = "alert alert-warning small py-1 px-2 mb-0",
                  bs_icon("exclamation-triangle", class = "me-1"),
                  strong("Si falla:"), " transforma X con log() o ",
                  "raíz cuadrada, o usa un modelo GAM.")
              )
            )
          ),

          # ── Supuesto 2: Normalidad ──────────────────
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$secundario, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("bar-chart",
                        style = paste0("color:", colores$secundario,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$secundario,
                                  "; font-weight:700;"),
                   "2. Normalidad de los residuos")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("¿Qué significa?")),
                p(class = "small text-muted mb-0",
                  "Los errores del modelo (diferencia entre lo observado ",
                  "y lo predicho) deben seguir una distribución normal. ",
                  "Importante: ", strong("no es Y quien debe ser normal"),
                  ", sino los residuos. Y puede tener cualquier ",
                  "distribución siempre que los errores sean normales.")
              ),
              div(
                p(class = "small mb-1", strong("¿Cómo verificarlo?")),
                p(class = "small text-muted mb-1",
                  "Gráfico ", strong("Q-Q normal."),
                  " Los puntos deben seguir la línea diagonal. ",
                  "Desviaciones en los extremos indican colas más ",
                  "pesadas de lo esperado."),
                div(
                  class = "alert alert-info small py-1 px-2 mb-0",
                  bs_icon("info-circle", class = "me-1"),
                  strong("Con n > 100:"),
                  " el modelo es robusto a desviaciones moderadas ",
                  "gracias al teorema central del límite.")
              )
            )
          ),

          # ── Supuesto 3: Homocedasticidad ───────────
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$acento, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("arrows-expand",
                        style = paste0("color:", colores$acento,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$acento,
                                  "; font-weight:700;"),
                   "3. Homocedasticidad")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("¿Qué significa?")),
                p(class = "small text-muted mb-0",
                  "La varianza de los errores debe ser constante en ",
                  "toda la escala de predicción. Si los residuos se ",
                  "dispersan más cuando los valores predichos son altos ",
                  "(o bajos), hay ", strong("heterocedasticidad"),
                  " — lo opuesto a lo que necesitamos.")
              ),
              div(
                p(class = "small mb-1", strong("¿Cómo verificarlo?")),
                p(class = "small text-muted mb-1",
                  "Gráfico ", strong("scale-location."),
                  " La línea suavizada debe ser aproximadamente ",
                  "horizontal. Una línea creciente indica mayor varianza ",
                  "con valores altos."),
                div(
                  class = "alert alert-warning small py-1 px-2 mb-0",
                  bs_icon("exclamation-triangle", class = "me-1"),
                  strong("Si falla:"),
                  " transforma Y con log() o raíz cuadrada, ",
                  "o usa errores estándar robustos.")
              )
            )
          ),

          # ── Supuesto 4: Independencia ───────────────
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$peligro, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("link-45deg",
                        style = paste0("color:", colores$peligro,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$peligro,
                                  "; font-weight:700;"),
                   "4. Independencia de los errores")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("¿Qué significa?")),
                p(class = "small text-muted mb-0",
                  "El error de una observación no debe estar relacionado ",
                  "con el error de otra. Esto se viola cuando medimos ",
                  "el mismo individuo varias veces (medidas repetidas), ",
                  "muestreamos sitios cercanos entre sí (autocorrelación ",
                  "espacial), o tenemos datos de múltiples años ",
                  "(autocorrelación temporal).")
              ),
              div(
                p(class = "small mb-1", strong("¿Cómo verificarlo?")),
                p(class = "small text-muted mb-1",
                  "Se garantiza principalmente con un buen ",
                  strong("diseño de muestreo."),
                  " Si los datos tienen estructura temporal, espacial ",
                  "o jerárquica, la independencia probablemente se viola."),
                div(
                  class = "alert alert-warning small py-1 px-2 mb-0",
                  bs_icon("exclamation-triangle", class = "me-1"),
                  strong("Si falla:"),
                  " usa modelos mixtos (LMM) para datos agrupados ",
                  "o modelos de series de tiempo.")
              )
            )
          ),

          # ── Supuesto 5: Multicolinealidad ───────────
          div(
            class = "card-muestreo mb-3",
            style = paste0("border-left: 4px solid ", colores$texto, ";"),
            div(class = "d-flex align-items-center gap-2 mb-2",
                bs_icon("diagram-2",
                        style = paste0("color:", colores$texto,
                                       "; font-size:1.1rem")),
                h6(class = "mb-0",
                   style = paste0("color:", colores$texto,
                                  "; font-weight:700;"),
                   "5. Ausencia de multicolinealidad")),
            layout_columns(
              col_widths = c(6, 6),
              div(
                p(class = "small mb-1", strong("¿Qué significa?")),
                p(class = "small text-muted mb-0",
                  "Los predictores no deben estar muy correlacionados ",
                  "entre sí. Si dos variables X miden casi lo mismo, ",
                  "el modelo no puede distinguir el efecto de cada una ",
                  "con precisión — los errores estándar se inflan y ",
                  "los coeficientes se vuelven inestables.")
              ),
              div(
                p(class = "small mb-1", strong("¿Cómo verificarlo?")),
                p(class = "small text-muted mb-1",
                  strong("VIF (Factor de Inflación de Varianza)."),
                  " Valores de referencia:", br(),
                  "• VIF < 3 — sin problema", br(),
                  "• VIF 3–5 — moderado, aceptable", br(),
                  "• VIF > 5 — problemático, requiere acción"),
                div(
                  class = "alert alert-warning small py-1 px-2 mb-0",
                  bs_icon("exclamation-triangle", class = "me-1"),
                  strong("Si falla:"),
                  " elimina uno de los predictores correlacionados ",
                  "o crea un índice compuesto.")
              )
            )
          ),

          div(
            class = "alert alert-warning small py-2 px-3 mb-0",
            bs_icon("lightbulb-fill", class = "me-2"),
            strong("Perspectiva práctica:"),
            " no todos los supuestos tienen el mismo peso. ",
            "La independencia y la linealidad son los más críticos. ",
            "La normalidad es la más flexible, especialmente con ",
            "muestras grandes. La homocedasticidad importa principalmente ",
            "cuando hay grandes diferencias en la varianza entre grupos. ",
            "El VIF solo es relevante en modelos con más de un predictor."
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 3: Los datos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("table", class = "me-1"), "Los datos"),
        card_body(

          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("database", class = "me-1"),
                          "Fuente de datos"),
              card_body(
                radioButtons(
                  ns("fuente_datos"),
                  label   = NULL,
                  choices = c(
                    "Usar datos de ejemplo (palmerpenguins)" = "ejemplo",
                    "Cargar mis propios datos"               = "propio"
                  ),
                  selected = "ejemplo"
                ),
                conditionalPanel(
                  condition = paste0("input['", ns("fuente_datos"),
                                     "'] == 'propio'"),
                  tags$hr(),
                  fileInput(
                    ns("archivo"),
                    label       = "Seleccionar archivo:",
                    accept      = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = "Buscar…",
                    placeholder = "CSV o Excel"
                  ),
                  selectInput(
                    ns("separador"),
                    label    = "Separador (CSV):",
                    choices  = c(
                      "Coma (,)"         = ",",
                      "Punto y coma (;)" = ";",
                      "Tabulador"        = "\t"
                    ),
                    selected = ","
                  ),
                  p(class = "small text-muted mb-0",
                    bs_icon("info-circle", class = "me-1"),
                    "La primera fila debe contener los nombres ",
                    "de las columnas.")
                ),
                tags$hr(),
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
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Explorar relaciones entre variables"),

          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("sliders", class = "me-1"),
                          "Controles"),
              card_body(
                uiOutput(ns("sel_var_x")),
                uiOutput(ns("sel_color")),
                checkboxInput(ns("mostrar_linea"),
                              "Mostrar línea de regresión",
                              value = TRUE),
                checkboxInput(ns("linea_por_grupo"),
                              "Línea por grupo (si hay color)",
                              value = FALSE),
                tags$hr(),
                uiOutput(ns("cards_correlacion"))
              )
            ),

            div(
              plotOutput(ns("plot_scatter"), height = "340px"),
              uiOutput(ns("insight_scatter"))
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
                  "Selecciona la variable respuesta y los predictores. ",
                  "Las métricas se actualizan al ajustar."),
                uiOutput(ns("sel_var_y")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  "Predictores numéricos"),
                uiOutput(ns("checks_numericos")),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  "Predictores categóricos"),
                uiOutput(ns("checks_categoricos")),
                tags$hr(),
                actionButton(
                  ns("ajustar"),
                  "Ajustar modelo",
                  class = "btn-primary w-100",
                  icon  = icon("play")
                ),

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
          p(class = "small text-muted mb-3",
            "Cada ", strong("coeficiente β"), " indica cuánto cambia Y por ",
            "cada unidad adicional de X, manteniendo el resto constante. ",
            "Un ", strong("p-valor < 0.05"), " indica que el efecto es ",
            "estadísticamente significativo. El ",
            strong("intervalo de confianza 95% (IC)"),
            " muestra el rango plausible del verdadero efecto — ",
            "si incluye el cero, el efecto no es significativo."
          ),
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
                p(class = "small text-muted",
                  "Si la barra cruza el cero (línea punteada), ",
                  "el efecto no es estadísticamente significativo."),
                plotOutput(ns("plot_forest"), height = "260px")
              )
            )
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
      # PESTAÑA 6: Diagnóstico
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard-check", class = "me-1"),
                        "Diagnóstico"),
        card_body(
          p(class = "small text-muted mb-3",
            "Verificamos los ", strong("cinco supuestos del modelo lineal general"),
            " descritos en la pestaña ", strong("Fundamentos"), ". ",
            "Generado con ", strong("performance::check_model()"),
            " del ecosistema easystats."
          ),

          layout_columns(
            col_widths = c(4, 4, 4),

            # ── Col 1: semáforo ────────────────────────
            div(
              uiOutput(ns("semaforo_col1")),
              uiOutput(ns("semaforo_col2"))
            ),

            # ── Col 2: residuos + Q-Q ──────────────────
            div(
              card(
                class = "mb-2",
                card_header(
                  class = "py-1",
                  bs_icon("graph-up", class = "me-1"),
                  "Residuos vs. ajustados",
                  span(class = "text-muted small ms-1", "— linealidad")
                ),
                card_body(
                  class = "p-1",
                  plotOutput(ns("plot_resid"), height = "240px")
                )
              ),
              card(
                class = "mb-0",
                card_header(
                  class = "py-1",
                  bs_icon("bar-chart", class = "me-1"),
                  "Q-Q normal",
                  span(class = "text-muted small ms-1", "— normalidad")
                ),
                card_body(
                  class = "p-1",
                  plotOutput(ns("plot_qq"), height = "240px")
                )
              )
            ),

            # ── Col 3: scale-location + VIF ───────────
            div(
              card(
                class = "mb-2",
                card_header(
                  class = "py-1",
                  bs_icon("rulers", class = "me-1"),
                  "Scale-location",
                  span(class = "text-muted small ms-1",
                       "— homocedasticidad")
                ),
                card_body(
                  class = "p-1",
                  plotOutput(ns("plot_scale"), height = "240px")
                )
              ),
              card(
                class = "mb-0",
                card_header(
                  class = "py-1",
                  bs_icon("bar-chart-line", class = "me-1"),
                  "VIF",
                  span(class = "text-muted small ms-1",
                       "— multicolinealidad")
                ),
                card_body(
                  class = "p-1",
                  plotOutput(ns("plot_vif"), height = "240px")
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 7: Código R
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
        card_body(
          p(class = "text-muted small mb-3",
            "Script que reproduce este análisis en R usando ",
            strong("tidymodels"), " y ", strong("easystats"),
            ". Se actualiza automáticamente según las selecciones activas."
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
mod_lm_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ────────────────────────────────────────────────────
    # DATOS: ejemplo o propios
    # ────────────────────────────────────────────────────

    datos_activos <- reactive({
      if (input$fuente_datos == "ejemplo") {
        palmerpenguins::penguins |>
          dplyr::mutate(
            species = factor(species),
            island  = factor(island),
            sex     = factor(sex)
          ) |>
          tidyr::drop_na()
      } else {
        req(input$archivo)
        ext <- tools::file_ext(input$archivo$name)
        tryCatch({
          df <- if (ext %in% c("xlsx", "xls")) {
            readxl::read_excel(input$archivo$datapath)
          } else {
            readr::read_delim(
              input$archivo$datapath,
              delim     = input$separador,
              show_col_types = FALSE
            )
          }
          df |>
            dplyr::mutate(dplyr::across(
              where(is.character), factor
            ))
        }, error = function(e) {
          showNotification(
            paste("Error al leer el archivo:", conditionMessage(e)),
            type = "error", duration = 6
          )
          NULL
        })
      }
    })

    vars_numericas <- reactive({
      df <- datos_activos()
      req(df)
      names(df)[sapply(df, is.numeric)]
    })

    vars_categoricas <- reactive({
      df <- datos_activos()
      req(df)
      names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    })

    # ── Resumen y preview ────────────────────────────────

    output$resumen_datos <- renderUI({
      df <- datos_activos()
      if (is.null(df)) return(NULL)
      div(
        class = "small text-muted mt-2",
        bs_icon("check-circle-fill",
                style = paste0("color:", colores$exito),
                class = "me-1"),
        paste0(nrow(df), " filas · ", ncol(df), " columnas cargadas.")
      )
    })

    output$cards_datos <- renderUI({
      df <- datos_activos()
      req(df)
      nnum <- length(vars_numericas())
      ncat <- length(vars_categoricas())
      nna  <- sum(is.na(df))

      layout_columns(
        col_widths = c(4, 4, 4),
        card(
          class = "text-center",
          card_body(class = "p-2",
                    h3(style = paste0("color:", colores$primario,
                                      "; font-weight:700;"), nrow(df)),
                    p(class = "small text-muted mb-0", "Observaciones")
          )
        ),
        card(
          class = "text-center",
          card_body(class = "p-2",
                    h3(style = paste0("color:", colores$acento,
                                      "; font-weight:700;"), nnum),
                    p(class = "small text-muted mb-0", "Variables numéricas")
          )
        ),
        card(
          class = "text-center",
          card_body(class = "p-2",
                    h3(style = paste0("color:", colores$secundario,
                                      "; font-weight:700;"), ncat),
                    p(class = "small text-muted mb-0", "Variables categóricas")
          )
        )
      )
    })

    output$tabla_preview <- renderDT({
      df <- datos_activos()
      req(df)
      datatable(
        head(df, 8),
        rownames = FALSE,
        options  = list(dom = "t", scrollX = TRUE),
        class    = "table-sm table-striped"
      )
    })

    # ── Explorar: controles dinámicos ────────────────────

    output$sel_var_x <- renderUI({
      req(vars_numericas())
      selectInput(
        ns("var_x"),
        label   = "Variable X (predictor numérico):",
        choices = vars_numericas(),
        selected = vars_numericas()[1]
      )
    })

    output$sel_color <- renderUI({
      cats <- vars_categoricas()
      if (length(cats) == 0) return(NULL)
      selectInput(
        ns("var_color"),
        label   = "Colorear por (opcional):",
        choices = c("Ninguna" = "ninguna", cats),
        selected = "ninguna"
      )
    })

    output$cards_correlacion <- renderUI({
      df  <- datos_activos()
      req(df, input$var_x)
      yv  <- vars_numericas()
      req(length(yv) >= 2)
      # usar primera numérica distinta de X como Y provisional
      yvar <- yv[yv != input$var_x][1]
      req(yvar)
      cor_val <- cor(df[[yvar]], df[[input$var_x]],
                     use = "complete.obs")
      r2_val  <- cor_val^2
      layout_columns(
        col_widths = c(6, 6),
        card(
          class = "text-center border-0",
          style = paste0("background:", colores$fondo),
          card_body(class = "p-2",
                    h4(style = paste0("color:", colores$primario,
                                      "; font-weight:700;"),
                       round(cor_val, 2)),
                    p(class = "small text-muted mb-0", "Correlación (r)")
          )
        ),
        card(
          class = "text-center border-0",
          style = paste0("background:", colores$fondo),
          card_body(class = "p-2",
                    h4(style = paste0("color:", colores$acento,
                                      "; font-weight:700;"),
                       paste0(round(r2_val * 100, 0), "%")),
                    p(class = "small text-muted mb-0", "R² simple")
          )
        )
      )
    })

    output$plot_scatter <- renderPlot({
      df   <- datos_activos()
      req(df, input$var_x)
      yv   <- vars_numericas()
      req(length(yv) >= 2)
      yvar <- yv[yv != input$var_x][1]
      req(yvar)

      p <- ggplot(df, aes(x = .data[[input$var_x]],
                          y = .data[[yvar]]))

      usar_color <- !is.null(input$var_color) &&
        input$var_color != "ninguna" &&
        input$var_color %in% names(df)

      if (usar_color) {
        p <- p +
          aes(color = .data[[input$var_color]]) +
          scale_color_manual(values = colores$tableau,
                             name   = input$var_color)
      }

      p <- p + geom_point(alpha = 0.6, size = 2)

      if (isTRUE(input$mostrar_linea)) {
        if (usar_color && isTRUE(input$linea_por_grupo)) {
          p <- p + geom_smooth(aes(group = .data[[input$var_color]]),
                               method = "lm", se = TRUE,
                               alpha = 0.15, linewidth = 1)
        } else {
          p <- p + geom_smooth(method = "lm", se = TRUE,
                               color = colores$primario,
                               fill  = colores$secundario,
                               alpha = 0.15, linewidth = 1.2)
        }
      }

      p + labs(
        x        = input$var_x,
        y        = yvar,
        subtitle = paste0("n = ", nrow(df), " observaciones")
      ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.subtitle    = element_text(color = colores$texto,
                                          size  = 10),
          panel.grid.minor = element_blank(),
          legend.position  = "bottom"
        )
    }, res = 110)

    output$insight_scatter <- renderUI({
      df   <- datos_activos()
      req(df, input$var_x)
      yv   <- vars_numericas()
      req(length(yv) >= 2)
      yvar <- yv[yv != input$var_x][1]
      req(yvar)
      cor_val <- cor(df[[yvar]], df[[input$var_x]],
                     use = "complete.obs")
      r2_pct  <- round(cor_val^2 * 100, 0)
      dir <- if (cor_val >  0.5) "positiva y fuerte" else
        if (cor_val >  0.2) "positiva y moderada" else
          if (cor_val < -0.5) "negativa y fuerte"  else
            "débil o negativa"
      div(
        class = "alert alert-info small py-2 px-3 mt-2 mb-0",
        bs_icon("lightbulb-fill", class = "me-1"),
        paste0(
          "La relación entre ", input$var_x, " y ", yvar,
          " es ", dir, " (r = ", round(cor_val, 2), "). ",
          "Esta variable sola explica el ", r2_pct,
          "% de la variación en ", yvar, "."
        )
      )
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 3: Construir el modelo — UI dinámica
    # ────────────────────────────────────────────────────

    output$sel_var_y <- renderUI({
      req(vars_numericas())
      selectInput(
        ns("var_y"),
        label    = "Variable respuesta (Y):",
        choices  = vars_numericas(),
        selected = vars_numericas()[1]
      )
    })

    output$checks_numericos <- renderUI({
      req(vars_numericas(), input$var_y)
      opts <- vars_numericas()[vars_numericas() != input$var_y]
      if (length(opts) == 0)
        return(p(class = "small text-muted",
                 "No hay más variables numéricas disponibles."))
      checkboxGroupInput(
        ns("preds_num"),
        label    = NULL,
        choices  = opts,
        selected = opts[1]
      )
    })

    output$checks_categoricos <- renderUI({
      cats <- vars_categoricas()
      if (length(cats) == 0)
        return(p(class = "small text-muted",
                 "No hay variables categóricas en los datos."))
      checkboxGroupInput(
        ns("preds_cat"),
        label    = NULL,
        choices  = cats,
        selected = NULL
      )
    })


    # ── Ajuste del modelo ────────────────────────────────

    modelo_lm <- eventReactive(input$ajustar, {
      df  <- datos_activos()
      req(df, input$var_y)

      preds <- c(input$preds_num, input$preds_cat)
      if (length(preds) == 0) {
        showNotification("Selecciona al menos un predictor.",
                         type = "warning", duration = 4)
        return(NULL)
      }

      fm <- as.formula(
        paste(input$var_y, "~", paste(preds, collapse = " + "))
      )

      withProgress(message = "Ajustando modelo...", value = 0.5, {
        fit <- tryCatch(
          lm(fm, data = df),
          error = function(e) {
            showNotification(
              paste("Error al ajustar:", conditionMessage(e)),
              type = "error", duration = 6
            )
            NULL
          }
        )
        incProgress(0.5)
        fit
      })
    }, ignoreNULL = FALSE)

    # ── Métricas ─────────────────────────────────────────

    output$cards_metricas <- renderUI({
      fit <- modelo_lm()
      if (is.null(fit)) return(
        div(class = "text-muted small py-3",
            bs_icon("arrow-left-circle", class = "me-1"),
            "Selecciona predictores y haz clic en 'Ajustar modelo'.")
      )
      s     <- summary(fit)
      r2adj <- round(s$adj.r.squared, 3)
      sigma <- round(s$sigma, 2)
      aic_v <- round(AIC(fit), 0)
      np    <- length(coef(fit)) - 1L
      col_r2 <- if (r2adj > 0.8) colores$exito else
        if (r2adj > 0.5) colores$acento else colores$peligro

      layout_columns(
        col_widths = c(3, 3, 3, 3),
        card(class = "text-center", card_body(class = "p-2",
                                              h3(style = paste0("color:", col_r2, "; font-weight:700;"), r2adj),
                                              p(class = "small text-muted mb-0", strong("R² ajustado")),
                                              p(class = "small text-muted mb-0", "varianza explicada")
        )),
        card(class = "text-center", card_body(class = "p-2",
                                              h3(style = paste0("color:", colores$primario, "; font-weight:700;"),
                                                 sigma),
                                              p(class = "small text-muted mb-0", strong("Error estándar")),
                                              p(class = "small text-muted mb-0", "residual (σ)")
        )),
        card(class = "text-center", card_body(class = "p-2",
                                              h3(style = paste0("color:", colores$secundario, "; font-weight:700;"),
                                                 aic_v),
                                              p(class = "small text-muted mb-0", strong("AIC")),
                                              p(class = "small text-muted mb-0", "menor = mejor")
        )),
        card(class = "text-center", card_body(class = "p-2",
                                              h3(style = paste0("color:", colores$texto, "; font-weight:700;"), np),
                                              p(class = "small text-muted mb-0", strong("Predictores")),
                                              p(class = "small text-muted mb-0", "en el modelo")
        ))
      )
    })

    output$plot_predobs <- renderPlot({
      fit <- modelo_lm()
      req(fit)
      tibble::tibble(
        obs  = fitted(fit) + resid(fit),
        pred = fitted(fit)
      ) |>
        ggplot(aes(x = obs, y = pred)) +
        geom_abline(slope = 1, intercept = 0,
                    linetype = "dashed",
                    color = colores$texto, linewidth = 0.8) +
        geom_point(color = colores$primario, alpha = 0.5, size = 1.8) +
        labs(x = "Observado", y = "Predicho") +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
    }, res = 110)

    output$texto_modelo <- renderUI({
      fit <- modelo_lm()
      if (is.null(fit)) return(
        p(class = "small text-muted",
          "Ajusta el modelo para ver la interpretación.")
      )
      s     <- summary(fit)
      r2adj <- round(s$adj.r.squared, 3)
      sigma <- round(s$sigma, 2)
      np    <- length(coef(fit)) - 1L
      cal   <- if (r2adj > 0.8) "excelente" else
        if (r2adj > 0.6) "bueno" else "débil"

      tagList(
        p(class = "small",
          "El modelo con ", strong(np), " predictor(es) tiene un ajuste ",
          strong(cal), ". Explica el ",
          strong(paste0(round(r2adj * 100, 0), "%")),
          " de la variación en ", strong(input$var_y), "."),
        p(class = "small",
          "El error típico de predicción es ",
          strong(paste0("±", sigma, " unidades")), "."),
        if (r2adj < 0.5)
          div(class = "alert alert-warning small py-1 px-2 mt-2 mb-0",
              bs_icon("exclamation-triangle", class = "me-1"),
              "El ajuste es bajo. Prueba agregar más predictores.")
      )
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 4: Parámetros
    # ────────────────────────────────────────────────────

    output$tabla_params_ui <- renderUI({
      fit <- modelo_lm()
      if (is.null(fit)) return(
        div(class = "text-muted small py-3",
            "Ajusta el modelo primero.")
      )
      s        <- summary(fit)
      coef_mat <- coef(s)
      ci       <- confint(fit, level = 0.95)

      filas <- lapply(seq_len(nrow(coef_mat)), function(i) {
        nm   <- rownames(coef_mat)[i]
        est  <- round(coef_mat[i, 1], 3)
        se   <- round(coef_mat[i, 2], 3)
        pval <- coef_mat[i, 4]
        lo   <- round(ci[i, 1], 3)
        hi   <- round(ci[i, 2], 3)

        p_txt <- if (pval < 0.001) "< 0.001 ***" else
          if (pval < 0.01)  paste0(round(pval, 3), " **") else
            if (pval < 0.05)  paste0(round(pval, 3), " *")  else
              round(pval, 3)
        col_p <- if (pval < 0.001) colores$exito else
          if (pval < 0.05)  colores$acento else colores$texto

        tags$tr(
          style   = "cursor:pointer;",
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority:'event'})",
            ns("param_seleccionado"), nm
          ),
          tags$td(strong(nm)),
          tags$td(est),
          tags$td(se),
          tags$td(paste0("[", lo, ", ", hi, "]")),
          tags$td(style = paste0("color:", col_p, "; font-weight:600;"),
                  p_txt)
        )
      })

      tags$table(
        class = "table table-sm table-hover small mb-0",
        tags$thead(tags$tr(
          tags$th("Parámetro"), tags$th("Estimado"),
          tags$th("EE"), tags$th("IC 95%"), tags$th("p-valor")
        )),
        tags$tbody(filas)
      )
    })

    output$plot_forest <- renderPlot({
      fit <- modelo_lm()
      req(fit)
      ci    <- confint(fit, level = 0.95)
      coefs <- coef(fit)
      pvals <- coef(summary(fit))[, 4]
      nms   <- names(coefs)

      df_f <- tibble::tibble(
        term = factor(nms, levels = rev(nms)),
        est  = coefs,
        lo   = ci[, 1],
        hi   = ci[, 2],
        sig  = pvals < 0.05
      ) |> dplyr::filter(term != "(Intercept)")

      if (nrow(df_f) == 0) return(invisible(NULL))

      ggplot(df_f, aes(x = est, y = term,
                       xmin = lo, xmax = hi, color = sig)) +
        geom_vline(xintercept = 0, linetype = "dashed",
                   color = colores$texto, linewidth = 0.7) +
        geom_errorbar(aes(ymin = lo, ymax = hi),
                      width = 0.2, linewidth = 1,
                      orientation = "y") +
        geom_point(size = 3) +
        scale_color_manual(
          values = c(`TRUE`  = colores$acento,
                     `FALSE` = colores$primario),
          labels = c(`TRUE`  = "Significativo (p < 0.05)",
                     `FALSE` = "No significativo"),
          name   = NULL
        ) +
        labs(x = "Coeficiente (unidades de Y)", y = NULL,
             subtitle = "IC 95% — si incluye el 0, el efecto no es significativo") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position    = "bottom",
          plot.subtitle      = element_text(color = colores$texto, size = 9),
          legend.text        = element_text(size = 9)
        )
    }, res = 110)

    output$interp_coef <- renderUI({
      fit <- modelo_lm()
      req(fit)
      sel <- input$param_seleccionado
      if (is.null(sel) || sel == "") return(
        p(class = "small text-muted",
          "Haz clic en una fila de la tabla para ver la interpretación.")
      )
      coefs <- coef(fit)
      ci    <- confint(fit, level = 0.95)
      pvals <- coef(summary(fit))[, 4]
      req(sel %in% names(coefs))

      est  <- round(coefs[sel], 3)
      lo   <- round(ci[sel, 1], 3)
      hi   <- round(ci[sel, 2], 3)
      pval <- pvals[sel]
      sig  <- pval < 0.05
      p_txt <- if (pval < 0.001) "< 0.001" else round(pval, 3)
      col   <- if (sig) colores$exito else colores$advertencia

      interp <- if (sel == "(Intercept)") {
        paste0(
          "El intercepto (β₀ = ", est, ") es el valor predicho de ",
          input$var_y, " cuando todos los predictores son cero. ",
          "Generalmente no tiene interpretación práctica directa."
        )
      } else {
        es_cat <- grepl(paste(vars_categoricas(), collapse = "|"), sel)
        if (es_cat) {
          paste0(
            "El grupo '", sel, "' tiene en promedio ",
            ifelse(est >= 0, "+", ""), est, " unidades de ", input$var_y,
            " respecto a la categoría de referencia, ",
            "manteniendo el resto de variables igual. ",
            "IC 95%: [", lo, ", ", hi, "]. ",
            if (sig) "Diferencia estadísticamente significativa (p = "
            else "Diferencia NO estadísticamente significativa (p = ",
            p_txt, ")."
          )
        } else {
          paste0(
            "Por cada unidad adicional de ", sel, ", ",
            input$var_y, " cambia en promedio ",
            ifelse(est >= 0, "+", ""), est, " unidades, ",
            "manteniendo el resto de variables igual. ",
            "IC 95%: [", lo, ", ", hi, "]. ",
            if (sig) "Efecto estadísticamente significativo (p = "
            else "Efecto NO estadísticamente significativo (p = ",
            p_txt, ")."
          )
        }
      }

      div(
        class = "alert py-2 px-3 small mb-0",
        style = paste0(
          "border-left: 4px solid ", col, "; background: ",
          if (sig) "#f0f9f5" else "#fffbf0", ";"
        ),
        bs_icon(if (sig) "check-circle-fill" else "circle",
                class = "me-1",
                style = paste0("color:", col)),
        strong(sel), " — ", interp
      )
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 5: Diagnóstico
    # ────────────────────────────────────────────────────

    # Helper interno: construye un item del semáforo
    sem_item <- function(s, col_map, icon_map, label_map, bg_map) {
      col   <- col_map[[s$st]]
      icono <- icon_map[[s$st]]
      lbl   <- label_map[[s$st]]
      bg    <- bg_map[[s$st]]
      div(
        class = "d-flex align-items-start gap-2 p-2 rounded mb-2",
        style = paste0("background:", bg,
                       "; border-left: 4px solid ", col, ";"),
        bs_icon(icono, size = "1.1em",
                style = paste0("color:", col,
                               "; flex-shrink:0; margin-top:2px")),
        div(
          p(class = "small mb-0",
            strong(s$nombre), " ",
            tags$span(
              class = "badge",
              style = paste0("background:", col, "; font-size:0.7rem;"),
              lbl
            ), br(),
            tags$span(class = "text-muted",
                      style = "font-size:0.78rem;", s$def), br(),
            tags$span(style = "font-size:0.82rem;", s[[s$st]])
          )
        )
      )
    }

    # Datos compartidos para ambas columnas del semáforo
    supuestos_data <- reactive({
      fit <- modelo_lm()
      req(fit)
      res  <- resid(fit)
      n    <- length(res)
      mu   <- mean(res)
      skew <- mean((res - mu)^3) / mean((res - mu)^2)^1.5
      sw_p <- tryCatch(
        shapiro.test(sample(res, min(n, 5000)))$p.value,
        error = function(e) NA
      )
      vif_max <- tryCatch({
        if (length(coef(fit)) > 2) max(car::vif(fit), na.rm = TRUE) else 1
      }, error = function(e) 1)
      r2adj <- summary(fit)$adj.r.squared

      list(
        list(
          nombre = "Linealidad",
          def    = "La relación entre X e Y debe ser lineal.",
          st     = if (abs(skew) < 1) "ok" else "warn",
          ok     = "Los residuos no muestran patrón sistemático claro.",
          warn   = "Posible curvatura. Considera transformar algún predictor.",
          bad    = "Patrón de no linealidad. Usa GAM o transforma variables."
        ),
        list(
          nombre = "Normalidad de los residuos",
          def    = "Los errores deben seguir una distribución normal.",
          st     = if (is.na(sw_p) || sw_p > 0.05 || n > 100) "ok" else "warn",
          ok     = if (is.na(sw_p)) "No calculado (muestra muy grande)."
          else paste0("Shapiro-Wilk: p = ", round(sw_p, 3), "."),
          warn   = paste0("Shapiro-Wilk: p = ", round(sw_p, 3),
                          ". Revisa el Q-Q."),
          bad    = "Desviación severa. Los IC pueden ser incorrectos."
        ),
        list(
          nombre = "Homocedasticidad",
          def    = "La varianza de los errores debe ser constante.",
          st     = if (r2adj > 0.7) "ok" else "warn",
          ok     = "La dispersión de los residuos parece constante.",
          warn   = "Posible heterocedasticidad. Revisa scale-location.",
          bad    = "Heterocedasticidad clara. Transforma Y o usa errores robustos."
        ),
        list(
          nombre = "Independencia",
          def    = "Las observaciones no deben estar correlacionadas.",
          st     = "ok",
          ok     = "Asumida. Verifica si los datos son temporales o espaciales.",
          warn   = "Posible dependencia. Considera modelos mixtos (LMM).",
          bad    = "Dependencia clara. El LM no es apropiado."
        ),
        list(
          nombre = "Ausencia de multicolinealidad",
          def    = "Los predictores no deben estar muy correlacionados (VIF < 5).",
          st     = if (vif_max < 3) "ok" else if (vif_max < 5) "warn" else "bad",
          ok     = paste0("VIF máx. = ", round(vif_max, 1), " — sin problema."),
          warn   = paste0("VIF máx. = ", round(vif_max, 1), " — moderado."),
          bad    = paste0("VIF máx. = ", round(vif_max, 1), " — alto. Elimina predictores.")
        )
      )
    })

    col_map_sem   <- list(ok = colores$exito, warn = colores$acento,
                          bad = colores$peligro)
    icon_map_sem  <- list(ok = "check-circle-fill",
                          warn = "exclamation-triangle-fill",
                          bad = "x-circle-fill")
    label_map_sem <- list(ok = "Cumplido", warn = "Atención", bad = "Problema")
    bg_map_sem    <- list(ok = "#f0f9f5", warn = "#fffbf0", bad = "#fff0f2")

    # Columna izquierda: supuestos 1, 2, 3
    output$semaforo_col1 <- renderUI({
      sp <- supuestos_data()
      req(sp)
      do.call(tagList, lapply(sp[1:3], sem_item,
                              col_map_sem, icon_map_sem,
                              label_map_sem, bg_map_sem))
    })

    # Columna derecha: supuestos 4, 5
    output$semaforo_col2 <- renderUI({
      sp <- supuestos_data()
      req(sp)
      do.call(tagList, lapply(sp[4:5], sem_item,
                              col_map_sem, icon_map_sem,
                              label_map_sem, bg_map_sem))
    })


    output$plot_resid <- renderPlot({
      fit <- modelo_lm(); req(fit)
      tibble::tibble(fit_v = fitted(fit), res = resid(fit)) |>
        ggplot(aes(x = fit_v, y = res)) +
        geom_hline(yintercept = 0, linetype = "dashed",
                   color = colores$texto) +
        geom_point(color = colores$primario, alpha = 0.4, size = 1.5) +
        geom_smooth(method = "loess", se = FALSE,
                    color = colores$acento, linewidth = 1) +
        labs(x = "Valores ajustados", y = "Residuos") +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 15, 10, 10)
        )
    }, res = 96)

    output$plot_qq <- renderPlot({
      fit <- modelo_lm(); req(fit)
      tibble::tibble(res = resid(fit)) |>
        ggplot(aes(sample = res)) +
        stat_qq(color = colores$primario, alpha = 0.5, size = 1.5) +
        stat_qq_line(color = colores$acento, linewidth = 1) +
        labs(x = "Cuantiles teóricos N(0,1)",
             y = "Cuantiles observados") +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 15, 10, 10)
        )
    }, res = 96)

    output$plot_scale <- renderPlot({
      fit <- modelo_lm(); req(fit)
      tibble::tibble(
        fit_v = fitted(fit),
        sr    = sqrt(abs(rstandard(fit)))
      ) |>
        ggplot(aes(x = fit_v, y = sr)) +
        geom_point(color = colores$primario, alpha = 0.4, size = 1.5) +
        geom_smooth(method = "loess", se = FALSE,
                    color = colores$acento, linewidth = 1) +
        labs(x = "Valores ajustados",
             y = "\u221a|residuos estandarizados|") +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 15, 10, 10)
        )
    }, res = 96)

    output$plot_vif <- renderPlot({
      fit <- modelo_lm(); req(fit)
      if (length(coef(fit)) <= 2) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "VIF requiere\nm\u00e1s de un predictor",
                     color = colores$texto, size = 4) +
            theme_void()
        )
      }
      vif_vals <- tryCatch(car::vif(fit), error = function(e) NULL)
      if (is.null(vif_vals)) return(invisible(NULL))
      if (is.matrix(vif_vals)) vif_vals <- vif_vals[, 1]

      tibble::tibble(
        term  = names(vif_vals),
        vif   = as.numeric(vif_vals)
      ) |>
        dplyr::mutate(
          term  = factor(term, levels = rev(term)),
          nivel = dplyr::case_when(
            vif < 3 ~ "Bajo (< 3)",
            vif < 5 ~ "Moderado (3\u20135)",
            TRUE    ~ "Alto (> 5)"
          )
        ) |>
        ggplot(aes(x = vif, y = term, fill = nivel)) +
        geom_col(width = 0.6) +
        geom_vline(xintercept = 5, linetype = "dashed",
                   color = colores$peligro, linewidth = 0.8) +
        scale_fill_manual(
          values = c("Bajo (< 3)"      = colores$exito,
                     "Moderado (3\u20135)" = colores$acento,
                     "Alto (> 5)"      = colores$peligro),
          name = NULL
        ) +
        labs(x = "VIF", y = NULL) +
        theme_minimal(base_size = 13) +
        theme(
          panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position    = "bottom",
          legend.text        = element_text(size = 9),
          plot.margin        = margin(10, 15, 10, 10)
        )
    }, res = 110)

    # ────────────────────────────────────────────────────
    # PESTAÑA 6: Código R reproducible
    # ────────────────────────────────────────────────────

    codigo_generado <- reactive({
      req(input$var_y)
      preds <- c(input$preds_num, input$preds_cat)
      formula_txt <- if (length(preds) > 0)
        paste(input$var_y, "~", paste(preds, collapse = " + "))
      else paste(input$var_y, "~ 1")

      tiene_cat <- length(input$preds_cat) > 0

      if (input$fuente_datos == "ejemplo") {
        carga <- paste0(
          "library(palmerpenguins)\n",
          "datos <- penguins |> drop_na()\n"
        )
      } else {
        ext <- if (!is.null(input$archivo))
          tools::file_ext(input$archivo$name) else "csv"
        carga <- if (ext %in% c("xlsx", "xls")) paste0(
          "library(readxl)\n",
          "datos <- read_excel(\"tu_archivo.", ext, "\")\n"
        ) else paste0(
          "datos <- read_delim(\"tu_archivo.csv\",\n",
          "                    delim = \"", input$separador, "\")\n"
        )
      }

      encabezado <- encabezado_script("StatModels",
                                      "Modelo lineal general (LM)")

      paste0(
        encabezado,
        "# \u2500\u2500 Paquetes \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "library(tidymodels)\n",
        "library(parameters)   # easystats\n",
        "library(performance)  # easystats\n",
        if (!is.null(input$archivo) &&
            tools::file_ext(input$archivo$name) %in% c("xlsx","xls"))
          "library(readxl)\n" else "",
        "\n",
        "# \u2500\u2500 Datos \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        carga,
        "\n",
        "# \u2500\u2500 Dividir entrenamiento / prueba (75/25) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "set.seed(42)\n",
        "splits <- initial_split(datos, prop = 0.75)\n",
        "train  <- training(splits)\n",
        "test   <- testing(splits)\n\n",
        "# \u2500\u2500 Receta de preprocesamiento \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "rec <- recipe(", formula_txt, ", data = train) |>\n",
        if (tiene_cat)
          "  step_dummy(all_nominal_predictors()) |>\n" else "",
        "  step_impute_median(all_numeric_predictors()) |>\n",
        "  step_zv(all_predictors())\n\n",
        "# \u2500\u2500 Workflow: receta + modelo lineal \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "modelo <- workflow() |>\n",
        "  add_recipe(rec) |>\n",
        "  add_model(linear_reg() |> set_engine(\"lm\")) |>\n",
        "  fit(data = train)\n\n",
        "# \u2500\u2500 M\u00e9tricas en el conjunto de prueba \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "preds <- augment(modelo, new_data = test)\n",
        "metrics(preds, truth = ", input$var_y,
        ", estimate = .pred)\n\n",
        "# \u2500\u2500 Tabla de par\u00e1metros (easystats) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "lm_fit <- modelo |>\n",
        "  extract_fit_parsnip() |>\n",
        "  pluck(\"fit\")\n\n",
        "model_parameters(lm_fit)          # coeficientes + IC 95%\n",
        "standardize_parameters(lm_fit)    # betas estandarizados\n\n",
        "# \u2500\u2500 Diagn\u00f3stico (easystats) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "model_performance(lm_fit)         # R\u00b2, AIC, sigma\n",
        "check_model(lm_fit)               # 6 gr\u00e1ficos autom\u00e1ticos\n",
        "check_normality(lm_fit)           # Shapiro-Wilk\n",
        "check_heteroscedasticity(lm_fit)  # Breusch-Pagan\n",
        "check_collinearity(lm_fit)        # VIF\n"
      )
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descargar_script <- downloadHandler(
      filename = function()
        paste0("statmodels_lm_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content = function(file)
        writeLines(codigo_generado(), file)
    )

  }) # fin moduleServer
}
