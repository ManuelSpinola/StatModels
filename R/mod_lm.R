# ============================================================
# mod_lm.R — Modelo lineal general (LM)
# StatModels · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Familia: regresión simple, múltiple, ANOVA, ANCOVA
# Datos: birdabundance_lm / birthwt_lm (ejemplos) o CSV/XLSX propio
# Ecosistema: tidymodels + easystats
#
# Filosofía: didáctico, sin conocimiento previo de programación
# El código R es secundario (pestaña final, descargable)
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_lm_ui <- function(id) {
  ns <- NS(id)

  tagList(

    navset_card_tab(

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("book", class = "me-1"), "¿Qué es?"),
        card_body(

          div(
            class = "px-1 pb-2",
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

          # ── Contexto del dataset ────────────────────


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
                  tags$th("Ejemplo con datos de aves")
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
                    "¿La densidad de especie aumenta con el área del fragmento?"
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
                    "¿La densidad depende del área y la distancia al mismo tiempo?"
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
                    "¿Difiere la densidad promedio entre niveles de pastoreo?"
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
                    "¿Depende la densidad del área, controlando por pastoreo?"
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
            fill = FALSE,
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
        fillable = FALSE,
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
              fill = FALSE,
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
              fill = FALSE,
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
              fill = FALSE,
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
              fill = FALSE,
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
              fill = FALSE,
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
        fillable = FALSE,
        title = tagList(bs_icon("table", class = "me-1"), "Los datos"),
        card_body(
          navset_pill(

            nav_panel(
              fillable = FALSE,
              title = tagList(bs_icon("collection", class = "me-1"),
                              "Datos de ejemplo"),
              br(),
              layout_columns(
                col_widths = c(4, 8),
                fill = FALSE,
                div(
                  radioButtons(
                    ns("fuente_datos"),
                    label   = tagList(bs_icon("database", class = "me-1"),
                                      "Seleccionar dataset:"),
                    choices = c(
                      "Densidad de especie de ave (Loyn, 1987)"    = "ejemplo_ave",
                      "Peso al nacer \u2014 salud perinatal (Hosmer)" = "ejemplo_salud"
                    ),
                    selected = "ejemplo_ave"
                  ),
                  tags$hr(),
                  uiOutput(ns("info_dataset"))
                ),
                card(
                  fill = FALSE,
                  card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                  card_body(
                    style = "overflow: auto;",
                    uiOutput(ns("cards_datos")),
                    br(),
                    DTOutput(ns("tabla_preview"))
                  )
                )
              )
            ),

            nav_panel(
              fillable = FALSE,
              title = tagList(bs_icon("folder2-open", class = "me-1"),
                              "Mis datos"),
              br(),
              layout_columns(
                col_widths = c(4, 8),
                fill = FALSE,
                div(
                  p(class = "small text-muted mb-3",
                    bs_icon("info-circle", class = "me-1"),
                    "Sube un archivo CSV o Excel. ",
                    "La primera fila debe contener los nombres de las columnas."),
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
                      "Tabulador"        = "\t"
                    ),
                    selected = ","
                  ),
                  tags$hr(),
                  uiOutput(ns("resumen_datos_propio"))
                ),
                card(
                  fill = FALSE,
                  card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                  card_body(
                    style = "overflow: auto;",
                    uiOutput(ns("cards_datos_propio")),
                    br(),
                    DTOutput(ns("tabla_preview_propio"))
                  )
                )
              )
            ),

            nav_panel(
              fillable = FALSE,
              title = tagList(bs_icon("sliders2", class = "me-1"),
                              "Tipos de variables"),
              br(),
              p(class = "small text-muted mb-3",
                "Verifica que cada variable tenga el tipo correcto. ",
                "Las variables ", strong("categ\u00f3ricas"),
                " deben ser ", strong("Factor"), ". ",
                "Las variables codificadas como n\u00fameros pero que ",
                "representan grupos deben cambiarse a Factor antes de modelar."
              ),
              layout_columns(
                col_widths = c(10, 2),
                fill = FALSE,
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
              uiOutput(ns("tipos_aplicados_msg")),

              tags$hr(),
              layout_columns(
                col_widths = c(4, 8),
                fill = FALSE,
                radioButtons(
                  ns("manejo_na"),
                  label    = tagList(bs_icon("exclamation-diamond", class = "me-1"),
                                     "Valores perdidos (NA)"),
                  choices  = c(
                    "Conservar"             = "conservar",
                    "Eliminar filas con NA" = "eliminar"
                  ),
                  selected = "conservar"
                ),
                uiOutput(ns("na_info"))
              )
            )

          )
        )
      ),


      # ════════════════════════════════════════════════
      # PESTAÑA 4: Explorar
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("zoom-in", class = "me-1"),
                        "Explorar"),
        card_body(
          p(class = "small text-muted mb-3",
            "Visualiza las relaciones entre variables antes de ajustar ",
            "el modelo. Ayuda a identificar predictores relevantes y ",
            "detectar patrones que guíen la especificación del modelo."
          ),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            card(
              fill = FALSE,
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
              plotOutput(ns("plot_scatter"), height = "380px"),
              uiOutput(ns("insight_scatter"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 5: Ajustar modelo
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("gear", class = "me-1"),
                        "Ajustar modelo"),
        card_body(
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            card(
              fill = FALSE,
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
                div(
                  class = "mb-3",
                  p(class = "small fw-bold text-muted mb-1",
                    bs_icon("distribute-vertical", class = "me-1"),
                    "Estandarización"),
                  checkboxInput(
                    ns("estandarizar"),
                    label = tagList(
                      "Estandarizar predictores numéricos",
                      tags$small(class = "text-muted d-block mt-1",
                                 "Permite comparar el peso relativo de cada predictor ",
                                 "(β en unidades de SD). El modelo se ajusta en ",
                                 "escala original — los efectos marginales y ",
                                 "predicciones siempre en unidades reales.")
                    ),
                    value = FALSE
                  )
                ),
                actionButton(
                  ns("ajustar"),
                  "Ajustar modelo",
                  class = "btn-primary w-100",
                  icon  = icon("play")
                ),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  bs_icon("floppy", class = "me-1"),
                  "Guardar para comparar"),
                p(class = "small text-muted mb-2",
                  "Dale un nombre al modelo ajustado y guárdalo. ",
                  "Cambia los predictores, reajusta y guarda otro ",
                  "para comparar en la pestaña ",
                  strong("Comparar modelos"), "."),
                textInput(
                  ns("nombre_modelo_lm"),
                  label       = NULL,
                  placeholder = "Ej: solo_area, area+pastoreo…"
                ),
                actionButton(
                  ns("guardar_modelo_lm"),
                  "Guardar modelo",
                  class = "btn-outline-primary w-100 btn-sm",
                  icon  = icon("floppy-disk")
                )
              )
            ),

            div(
              uiOutput(ns("cards_metricas")),
              br(),
              layout_columns(
                col_widths = c(6, 6),
                fill = FALSE,
                card(
                  fill = FALSE,
                  card_header(bs_icon("bullseye", class = "me-1"),
                              "Predichos vs. observados"),
                  card_body(
                    p(class = "small text-muted",
                      "Puntos cerca de la diagonal = buenas predicciones."),
                    plotOutput(ns("plot_predobs"), height = "240px")
                  )
                ),
                card(
                  fill = FALSE,
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
      # PESTAÑA 6: Diagnóstico
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
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
            fill = FALSE,

            # ── Col 1: semáforo ────────────────────────
            div(
              uiOutput(ns("semaforo_col1")),
              uiOutput(ns("semaforo_col2"))
            ),

            # ── Col 2: residuos + Q-Q ──────────────────
            div(
              card(
                fill = FALSE,
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
                fill = FALSE,
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
                fill = FALSE,
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
                fill = FALSE,
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
      # PESTAÑA 7: Performance
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("speedometer2", class = "me-1"),
                        "Performance"),
        card_body(

          p(class = "small text-muted mb-3",
            "Métricas de rendimiento del modelo lineal: R², RMSE, MAE ",
            "y validación cruzada para estimar el error de predicción ",
            "en datos nuevos. Generadas con ",
            strong("performance::model_performance()"),
            " y ", strong("tidymodels (vfold_cv)"), "."
          ),

          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,

            card(
              fill = FALSE,
              card_header(
                bs_icon("speedometer2", class = "me-1"),
                "Métricas del modelo",
                span(class = "text-muted small ms-2",
                     "— model_performance() · easystats")
              ),
              card_body(uiOutput(ns("tabla_performance_lm")))
            ),

            div(
              card(
                fill = FALSE,
                class = "mb-3",
                card_header(
                  bs_icon("graph-up-arrow", class = "me-1"),
                  "Predicho vs. Observado",
                  span(class = "text-muted small ms-2",
                       "— entrenamiento completo")
                ),
                card_body(
                  plotOutput(ns("plot_predobs_lm"), height = "240px")
                )
              ),

              card(
                fill = FALSE,
                class = "mb-0",
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
                    fill = FALSE,
                    numericInput(
                      ns("cv_folds_lm"),
                      label = "Folds:",
                      value = 10, min = 3, max = 20
                    ),
                    div(class = "pt-4",
                        checkboxInput(ns("cv_estratificado_lm"),
                                      "Estratificar",
                                      value = FALSE)),
                    div(class = "pt-4",
                        actionButton(ns("correr_cv_lm"), "Correr CV",
                                     class = "btn-primary w-100",
                                     icon  = icon("rotate")))
                  ),
                  tags$hr(),
                  uiOutput(ns("resultado_cv_lm"))
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 8: Parámetros
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("table", class = "me-1"), "Parámetros"),
        div(
          class = "p-3",
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
            fill = FALSE,
            card(
              fill = FALSE,
              card_header(
                bs_icon("layout-text-sidebar", class = "me-1"),
                "Tabla de coeficientes",
                span(class = "text-muted small ms-2",
                     "— parameters (easystats)")
              ),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("tabla_params_ui"))
              )
            ),
            card(
              fill = FALSE,
              card_header(
                bs_icon("bar-chart-fill", class = "me-1"),
                "Forest plot",
                span(class = "text-muted small ms-2",
                     "— coeficiente ± IC 95%")
              ),
              card_body(
                style = "height: auto;",
                p(class = "small text-muted",
                  "Si la barra cruza el cero (línea punteada), ",
                  "el efecto no es estadísticamente significativo."),
                plotOutput(ns("plot_forest"), height = "300px")
              )
            )
          ),
          div(class = "mt-3",
              card(
                fill = FALSE,
                card_header(
                  bs_icon("bar-chart-steps", class = "me-1"),
                  "Importancia de variables",
                  span(class = "text-muted small ms-2",
                       "— β estandarizados · parameters (easystats)")
                ),
                card_body(
                  style = "height: auto;",
                  p(class = "small text-muted mb-2",
                    "Las barras muestran el peso relativo de cada predictor ",
                    "en unidades de desviación estándar (SD). ",
                    strong("Azul"), " = efecto positivo · ",
                    strong("rojo"), " = efecto negativo. ",
                    "Barras transparentes = no significativo (p ≥ 0.05)."
                  ),
                  plotOutput(ns("plot_importancia_lm"), height = "300px")
                )
              )
          ),
          div(class = "mt-3",
              card(
                fill = FALSE,
                card_header(bs_icon("chat-text", class = "me-1"),
                            "Interpretación — haz clic en una fila"),
                card_body(
                  style = "overflow: visible; height: auto;",
                  uiOutput(ns("interp_coef"))
                )
              )
          )
        )
      ),


      # ════════════════════════════════════════════════
      # PESTAÑA 9: Efectos marginales
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("graph-up-arrow", class = "me-1"),
                        "Efectos marginales"),
        card_body(
          p(class = "small text-muted mb-3",
            "Los ", strong("efectos marginales"),
            " muestran cómo cambia Y al variar un predictor, ",
            "manteniendo el resto en sus valores típicos (media para ",
            "numéricos, moda para categóricos). En el LM los efectos ",
            "son directamente los coeficientes, pero visualizarlos ",
            "facilita la interpretación, especialmente con interacciones. ",
            "Generados con ", strong("modelbased::estimate_relation()"),
            " de easystats."
          ),

          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            card(
              fill = FALSE,
              card_header(bs_icon("sliders", class = "me-1"),
                          "Controles"),
              card_body(
                style = "overflow: visible; height: auto;",
                p(class = "small text-muted mb-2",
                  "Selecciona el predictor focal. ",
                  "El resto se mantiene en sus valores típicos."),
                uiOutput(ns("sel_pred_marginal_lm")),
                tags$hr(),
                checkboxInput(ns("marginal_ci_lm"),
                              "Mostrar intervalo de confianza 95%",
                              value = TRUE),
                checkboxInput(ns("marginal_puntos_lm"),
                              "Mostrar datos observados",
                              value = TRUE),
                tags$hr(),
                uiOutput(ns("marginal_valores_tipicos_lm"))
              )
            ),

            div(
              card(
                fill = FALSE,
                card_header(
                  bs_icon("graph-up-arrow", class = "me-1"),
                  "Efecto marginal",
                  span(class = "text-muted small ms-2",
                       "— estimate_relation() · modelbased")
                ),
                card_body(
                  plotOutput(ns("plot_marginal_lm"), height = "380px")
                )
              ),
              br(),
              uiOutput(ns("marginal_interpretacion_lm"))
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Predicción puntual"),
          p(class = "small text-muted mb-3",
            "Ingresa valores específicos para cada predictor y obtén ",
            "el valor predicho de Y con su intervalo de confianza 95%. ",
            "Usa ", strong("modelbased::estimate_expectation()"),
            " de easystats."
          ),

          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            card(
              fill = FALSE,
              card_header(bs_icon("sliders", class = "me-1"),
                          "Valores de los predictores"),
              card_body(
                uiOutput(ns("inputs_prediccion_lm")),
                br(),
                actionButton(
                  ns("calcular_prediccion_lm"),
                  "Calcular predicción",
                  class = "btn-primary w-100",
                  icon  = icon("calculator")
                )
              )
            ),

            card(
              fill = FALSE,
              card_header(bs_icon("bullseye", class = "me-1"),
                          "Resultado"),
              card_body(uiOutput(ns("resultado_prediccion_lm")))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 10: Contrastes
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("arrows-angle-expand", class = "me-1"),
                        "Contrastes"),
        card_body(
          p(class = "small text-muted mb-3",
            "Los ", strong("contrastes"), " comparan el valor promedio ",
            "de Y entre grupos de un predictor categórico, controlando ",
            "por el resto de variables del modelo. Las diferencias se ",
            "expresan en las ", strong("unidades de Y"), ". Generados con ",
            strong("modelbased::estimate_contrasts()"), " de easystats."
          ),

          uiOutput(ns("contrasts_no_cat_msg_lm")),

          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            card(
              fill = FALSE,
              card_header(bs_icon("sliders", class = "me-1"),
                          "Controles"),
              card_body(
                uiOutput(ns("sel_var_contraste_lm")),
                tags$hr(),
                selectInput(
                  ns("metodo_ajuste_lm"),
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
                fill = FALSE,
                class = "mb-3",
                card_header(
                  bs_icon("table", class = "me-1"),
                  "Tabla de contrastes"
                ),
                card_body(uiOutput(ns("tabla_contrastes_lm")))
              ),
              card(
                fill = FALSE,
                class = "mb-0",
                card_header(
                  bs_icon("bar-chart-fill", class = "me-1"),
                  "Visualización de contrastes"
                ),
                card_body(
                  plotOutput(ns("plot_contrastes_lm"), height = "300px")
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 11: Comparar modelos
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("arrow-left-right", class = "me-1"),
                        "Comparar modelos"),
        card_body(
          p(class = "small text-muted mb-3",
            "Ajusta distintos modelos en la pestaña ",
            strong("Ajustar modelo"), ", guarda cada uno con un ",
            "nombre descriptivo y compáralos aquí por AIC, AICc, BIC y R²."
          ),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,

            card(
              fill = FALSE,
              card_header(bs_icon("list-check", class = "me-1"),
                          "Modelos guardados"),
              card_body(
                uiOutput(ns("lista_modelos_guardados_lm")),
                tags$hr(),
                actionButton(ns("limpiar_modelos_lm"),
                             "Limpiar todos",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon  = icon("trash"))
              )
            ),

            div(
              card(
                fill = FALSE,
                class = "mb-3",
                card_header(
                  bs_icon("table", class = "me-1"),
                  "Tabla comparativa",
                  span(class = "text-muted small ms-2",
                       "— compare_performance() · easystats")
                ),
                card_body(uiOutput(ns("tabla_comparacion_lm")))
              ),
              card(
                fill = FALSE,
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
                  plotOutput(ns("plot_comparacion_lm"),
                             height = "340px")
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 12: Código R
      # ════════════════════════════════════════════════
      nav_panel(
        fillable = FALSE,
        title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
        card_body(
          p(class = "text-muted small mb-3",
            "Script que reproduce este análisis en R usando ",
            strong("tidymodels"), " y ", strong("easystats"),
            ". Se actualiza automáticamente según las selecciones activas."
          ),
          card(
            fill = FALSE,
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

    # ── Info dinámica del dataset en pestaña ¿Qué es? ─────────
    tipos_usuario <- reactiveVal(NULL)

    # datos_activos_unif: prioriza datos propios si hay archivo
    datos_activos_unif <- reactive({
      if (!is.null(input$archivo)) {
        dp <- try(datos_propio(), silent = TRUE)
        if (!inherits(dp, "try-error") && !is.null(dp)) return(dp)
      }
      datos_activos()
    })

    datos_mod <- reactiveVal(NULL)
    observeEvent(datos_activos_unif(), {
      tipos_usuario(NULL)
      datos_mod(datos_activos_unif())
    })

    observeEvent(input$resetear_tipos, {
      tipos_usuario(NULL)
      datos_mod(datos_activos_unif())
      showNotification("Tipos restaurados.", type = "message", duration = 2)
    })

    observeEvent(input$aplicar_tipos, {
      req(datos_mod())
      d   <- datos_mod()
      nms <- names(d)
      nuevos <- lapply(nms, function(nm) input[[paste0("tipo_", nm)]])
      names(nuevos) <- nms
      tipos_usuario(nuevos)
      for (nm in names(nuevos)) {
        tipo <- nuevos[[nm]]
        if (is.null(tipo) || !nm %in% names(d)) next
        if (tipo == "factor" && !is.factor(d[[nm]]))
          d[[nm]] <- factor(d[[nm]])
        else if (tipo == "numeric" && !is.numeric(d[[nm]]))
          d[[nm]] <- suppressWarnings(as.numeric(as.character(d[[nm]])))
        else if (tipo == "excluir")
          d[[nm]] <- NULL
      }
      datos_mod(d)
      showNotification("Tipos aplicados.", type = "message", duration = 2)
    })

    # ── Manejo de NAs ────────────────────────────────────────────────────────
    datos_finales <- reactive({
      df <- datos_mod()
      req(df)
      if (isTRUE(input$manejo_na == "eliminar")) {
        df <- tidyr::drop_na(df)
      }
      df
    })

    output$na_info <- renderUI({
      df_orig  <- datos_mod()
      df_final <- datos_finales()
      req(df_orig)
      n_na <- sum(!stats::complete.cases(df_orig))
      if (n_na == 0) return(
        div(class = "alert alert-success small py-2 px-3 mb-0",
            bs_icon("check-circle", class = "me-1"), "Sin valores perdidos.")
      )
      n_elim <- nrow(df_orig) - nrow(df_final)
      if (input$manejo_na == "eliminar")
        div(class = "alert alert-warning small py-2 px-3 mb-0",
            bs_icon("exclamation-triangle", class = "me-1"),
            paste0(n_elim, " fila(s) eliminadas. Quedan ", nrow(df_final), " filas."))
      else
        div(class = "alert alert-info small py-2 px-3 mb-0",
            bs_icon("info-circle", class = "me-1"),
            paste0(n_na, " fila(s) con NA. El modelo puede fallar o excluirlas ",
                   "autom\u00e1ticamente \u2014 pod\u00e9s eliminarlas arriba para mayor control."))
    })

        output$tabla_tipos <- renderUI({
      df <- datos_mod(); req(df)
      tu <- tipos_usuario()
      filas <- lapply(names(df), function(nm) {
        col    <- df[[nm]]
        actual <- if (is.factor(col) || is.character(col)) "factor" else "numeric"
        icono  <- if (actual == "factor")
          bs_icon("tag-fill", style = paste0("color:", colores$acento))
        else
          bs_icon("123", style = paste0("color:", colores$primario))
        sel <- if (!is.null(tu) && !is.null(tu[[nm]])) tu[[nm]] else actual
        tags$tr(
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  div(class = "d-flex align-items-center gap-2", icono, strong(nm))),
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  tags$span(class = "badge",
                            style = paste0("background:",
                              if (actual == "factor") colores$acento
                              else colores$primario, "; font-size:0.75rem;"),
                            if (actual == "factor") "Factor" else "Num\u00e9rico")),
          tags$td(style = "padding:5px 8px;",
                  selectInput(
                    inputId  = paste0(ns("tipo_"), nm),
                    label    = NULL,
                    choices  = c("Num\u00e9rico" = "numeric",
                                 "Factor (categ\u00f3rico)" = "factor",
                                 "Excluir" = "excluir"),
                    selected = sel, width = "180px")),
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  if (!is.null(tu) && !is.null(tu[[nm]]) && tu[[nm]] != actual)
                    tags$span(class = "badge",
                              style = paste0("background:", colores$exito),
                              "Modificado")
                  else
                    tags$span(class = "text-muted small", "Sin cambios"))
        )
      })
      tags$table(
        class = "table table-sm table-hover small mb-0",
        tags$thead(
          style = paste0("background:", colores$primario,
                         " !important; color:#fff !important;"),
          tags$tr(
            tags$th(style = "padding:7px 8px;", "Variable"),
            tags$th(style = "padding:7px 8px;", "Tipo detectado"),
            tags$th(style = "padding:7px 8px;", "Tipo a usar"),
            tags$th(style = "padding:7px 8px;", "Estado")
          )
        ),
        tags$tbody(filas)
      )
    })

    output$tipos_aplicados_msg <- renderUI({
      tu <- tipos_usuario(); if (is.null(tu)) return(NULL)
      df <- datos_mod(); req(df)
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
          "El modelo usar\u00e1 estos tipos.")
    })

    output$info_dataset <- renderUI({
      fuente <- input$fuente_datos
      if (is.null(fuente) || fuente == "ejemplo_ave") {
        div(
          class = "alert alert-info small py-2 px-3 mb-3",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Dataset: Densidad de especie de ave (Loyn, 1987)."),
          " Abundancia de aves en ",
          strong("56 fragmentos de bosque"),
          " de Victoria, Australia. Variables: ",
          strong("densidad_especie"), " (aves/ha), ",
          strong("area_ha"), " (ha), ",
          strong("distancia_m"), " (m al fragmento más cercano), ",
          strong("altitud_m"), " (m s.n.m.) y ",
          strong("pastoreo"), " (5 niveles de intensidad). ",
          "Fuente: Quinn & Keough (2002). ",
          em("Experimental Design and Data Analysis for Biologists.")
        )
      } else if (fuente == "ejemplo_salud") {
        div(
          class = "alert alert-info small py-2 px-3 mb-3",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Dataset: Peso al nacer — salud perinatal (Hosmer & Lemeshow)."),
          " Datos de ",
          strong("189 neonatos"),
          " del Baystate Medical Center, Springfield, MA (1986). Variables: ",
          strong("peso_g"), " (peso al nacer en gramos), ",
          strong("edad_madre"), " (años), ",
          strong("peso_madre"), " (libras), ",
          strong("tabaco"), " y ", strong("hta"), " (factores de riesgo). ",
          "Fuente: MASS::birthwt."
        )
      }
    })

    # ────────────────────────────────────────────────────
    # DATOS: ejemplo o propios (separados)
    # ────────────────────────────────────────────────────

    datos_activos <- reactive({
      fuente <- input$fuente_datos
      req(!is.null(fuente) && nchar(fuente) > 0)
      if (fuente == "ejemplo_ave") {
        tryCatch({
          e <- new.env()
          load(system.file("app/data/birdabundance_lm.rda",
                           package = "StatModels"), envir = e)
          e$birdabundance_lm
        }, error = function(err) {
          showNotification("Archivo birdabundance_lm.rda no encontrado.",
                           type = "error", duration = 6)
          NULL
        })
      } else {
        tryCatch({
          e <- new.env()
          load(system.file("app/data/birthwt_lm.rda",
                           package = "StatModels"), envir = e)
          e$birthwt_lm
        }, error = function(err) {
          showNotification("Archivo birthwt_lm.rda no encontrado.",
                           type = "error", duration = 6)
          NULL
        })
      }
    })

    # datos propios
    datos_propio <- reactive({
      req(input$archivo)
      ext <- tools::file_ext(input$archivo$name)
      tryCatch({
        df <- if (ext %in% c("xlsx", "xls"))
          readxl::read_excel(input$archivo$datapath)
        else
          readr::read_delim(input$archivo$datapath,
                            delim = input$separador,
                            show_col_types = FALSE)
        df |> dplyr::mutate(dplyr::across(where(is.character), factor))
      }, error = function(e) {
        showNotification(paste("Error al leer el archivo:", conditionMessage(e)),
                         type = "error", duration = 6)
        NULL
      })
    })

    observeEvent(datos_propio(), {
      df <- datos_propio()
      req(df)
      tipos_usuario(NULL)
    })

    # vista previa datos propios
    output$resumen_datos_propio <- renderUI({
      req(datos_propio())
      d <- datos_propio()
      div(class = "small text-muted",
          bs_icon("check-circle-fill",
                  style = paste0("color:", colores$exito), class = "me-1"),
          paste0(nrow(d), " filas \u00b7 ", ncol(d), " columnas"))
    })

    output$cards_datos_propio <- renderUI({
      req(datos_propio())
      d    <- datos_propio()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
      layout_columns(col_widths = c(4, 4, 4), fill = FALSE,
        card(class = "text-center",
          card_body(class = "p-2",
            h3(style = paste0("color:", colores$primario, "; font-weight:700;"),
               nrow(d)),
            p(class = "small text-muted mb-0", "Observaciones"))),
        card(class = "text-center",
          card_body(class = "p-2",
            h3(style = paste0("color:", colores$acento, "; font-weight:700;"),
               nnum),
            p(class = "small text-muted mb-0", "Num\u00e9ricas"))),
        card(class = "text-center",
          card_body(class = "p-2",
            h3(style = paste0("color:", colores$secundario, "; font-weight:700;"),
               ncat),
            p(class = "small text-muted mb-0", "Categ\u00f3ricas")))
      )
    })

    output$tabla_preview_propio <- renderDT({
      req(datos_propio())
      datatable(head(datos_propio(), 8), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    vars_numericas <- reactive({
      df <- datos_finales()
      req(df)
      names(df)[sapply(df, is.numeric)]
    })

    vars_categoricas <- reactive({
      df <- datos_finales()
      req(df)
      names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    })

    # ── Resumen y preview ────────────────────────────────

    output$resumen_datos <- renderUI({
      df <- datos_finales()
      if (is.null(df)) return(NULL)
      div(
        class = "small text-muted mt-2",
        bs_icon("check-circle-fill",
                style = paste0("color:", colores$exito),
                class = "me-1"),
        paste0(nrow(df), " filas · ", ncol(df), " columnas")
      )
    })

    output$cards_datos <- renderUI({
      df <- datos_finales()
      req(df)
      nnum <- length(vars_numericas())
      ncat <- length(vars_categoricas())
      nna  <- sum(is.na(df))

      layout_columns(
        col_widths = c(4, 4, 4),
        fill = FALSE,
        card(
          fill = FALSE,
          class = "text-center",
          card_body(class = "p-2",
                    h3(style = paste0("color:", colores$primario,
                                      "; font-weight:700;"), nrow(df)),
                    p(class = "small text-muted mb-0", "Observaciones")
          )
        ),
        card(
          fill = FALSE,
          class = "text-center",
          card_body(class = "p-2",
                    h3(style = paste0("color:", colores$acento,
                                      "; font-weight:700;"), nnum),
                    p(class = "small text-muted mb-0", "Num\u00e9ricas")
          )
        ),
        card(
          fill = FALSE,
          class = "text-center",
          card_body(class = "p-2",
                    h3(style = paste0("color:", colores$secundario,
                                      "; font-weight:700;"), ncat),
                    p(class = "small text-muted mb-0", "Categ\u00f3ricas")
          )
        )
      )
    })

    output$tabla_preview <- renderDT({
      df <- datos_finales()
      req(df)
      datatable(
        df,
        rownames = FALSE,
        options  = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
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
      df  <- datos_finales()
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
        fill = FALSE,
        card(
          fill = FALSE,
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
          fill = FALSE,
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

    output$plot_scatter <- renderPlot(suppressWarnings({
      df   <- datos_finales()
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
                               method = "lm", formula = y ~ x, se = TRUE,
                               alpha = 0.15, linewidth = 1)
        } else {
          p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                               color = colores$primario,
                               fill  = colores$secundario,
                               alpha = 0.15, linewidth = 1.2,
                               show.legend = FALSE)
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
    }), res = 110)

    output$insight_scatter <- renderUI({
      df   <- datos_finales()
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
    # PESTAÑA 5: Ajustar modelo — UI dinámica
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

    output$checks_interacciones <- renderUI({
      preds <- c(input$preds_num, input$preds_cat)
      if (length(preds) < 2) return(NULL)
      pares     <- combn(preds, 2, simplify = FALSE)
      etiquetas <- sapply(pares, function(p)
        paste0(p[1], " × ", p[2]))
      valores   <- sapply(pares, function(p)
        paste0(p[1], "*", p[2]))
      checkboxGroupInput(ns("interacciones"), label = NULL,
                         choices  = setNames(valores, etiquetas),
                         selected = NULL)
    })

    modelo_lm <- eventReactive(input$ajustar, {
      df  <- datos_finales()
      req(df, input$var_y)

      preds <- c(input$preds_num, input$preds_cat)
      if (length(preds) == 0) {
        showNotification("Selecciona al menos un predictor.",
                         type = "warning", duration = 4)
        return(NULL)
      }

      ints     <- input$interacciones
      terminos <- if (!is.null(ints) && length(ints) > 0)
        c(preds, ints) else preds

      fm <- as.formula(
        paste(input$var_y, "~", paste(terminos, collapse = " + "))
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

    # ── Modelo estandarizado (para importancia de variables) ──

    modelo_lm_std <- eventReactive(input$ajustar, {
      df    <- datos_finales(); req(df, input$var_y)
      preds <- c(input$preds_num, input$preds_cat)
      req(length(preds) > 0)
      preds_num <- input$preds_num
      if (length(preds_num) == 0) return(NULL)

      ints     <- input$interacciones
      terminos <- if (!is.null(ints) && length(ints) > 0)
        c(preds, ints) else preds
      fm <- as.formula(
        paste(input$var_y, "~", paste(terminos, collapse = " + "))
      )

      tryCatch({
        df_std <- datawizard::standardize(df, select = preds_num)
        lm(fm, data = df_std)
      }, error = function(e) NULL)
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
        fill = FALSE,
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
    # PESTAÑA 6: Diagnóstico
    # ────────────────────────────────────────────────────

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
        geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
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
        geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
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
    # PESTAÑA 7: Performance
    # ────────────────────────────────────────────────────

    output$tabla_performance_lm <- renderUI({
      fit <- modelo_lm()
      if (is.null(fit)) return(
        div(class = "text-muted small py-3",
            bs_icon("arrow-left-circle", class = "me-1"),
            "Ajusta el modelo primero.")
      )
      tryCatch({
        s      <- summary(fit)
        r2     <- round(s$r.squared, 4)
        r2adj  <- round(s$adj.r.squared, 4)
        sigma  <- round(s$sigma, 3)
        aic_v  <- tryCatch(round(AIC(fit), 2),  error = function(e) NA)
        bic_v  <- tryCatch(round(BIC(fit), 2),  error = function(e) NA)
        aicc_v <- tryCatch(round(performance::performance_aicc(fit), 2),
                           error = function(e) NA)
        rmse_v <- tryCatch(round(performance::performance_rmse(fit, verbose = FALSE), 3),
                           error = function(e) NA)
        mae_v  <- tryCatch({
          round(mean(abs(resid(fit))), 3)
        }, error = function(e) NA)
        ll_v   <- tryCatch(round(as.numeric(logLik(fit)), 2), error = function(e) NA)
        np     <- length(coef(fit)) - 1L
        n      <- nrow(fit$model)

        col_r2 <- if (r2adj > 0.8) colores$exito else
          if (r2adj > 0.5) colores$acento else colores$peligro

        filas <- list(
          list(g = "MUESTRA", m = "n (observaciones)", v = n,
               i = "Tamaño de la muestra usada para ajustar el modelo."),
          list(g = NULL, m = "k (predictores)", v = np,
               i = "Número de predictores sin el intercepto."),
          list(g = "VARIANZA EXPLICADA", m = "R²", v = r2,
               i = paste0("Proporción de varianza en Y explicada por el modelo. ",
                          "No penaliza por número de predictores.")),
          list(g = NULL, m = "R² ajustado", v = r2adj,
               i = paste0("R² corregido por el número de predictores. ",
                          if (r2adj > 0.8) " Ajuste excelente."
                          else if (r2adj > 0.5) " Ajuste moderado."
                          else " Ajuste débil.")),
          list(g = "ERROR DE PREDICCIÓN", m = "σ (error estándar residual)", v = sigma,
               i = paste0("Error típico de predicción en unidades de Y. ",
                          "Menor = predicciones más precisas.")),
          list(g = NULL, m = "RMSE", v = rmse_v,
               i = "Raíz del error cuadrático medio. Menor = mejor."),
          list(g = NULL, m = "MAE", v = mae_v,
               i = "Error absoluto medio. Más robusto que RMSE ante outliers."),
          list(g = "CRITERIOS DE INFORMACIÓN", m = "AIC", v = aic_v,
               i = "Criterio de Akaike. Menor = mejor. Penaliza complejidad."),
          list(g = NULL, m = "AICc", v = aicc_v,
               i = "AIC corregido para muestras pequeñas (n/k < 40)."),
          list(g = NULL, m = "BIC", v = bic_v,
               i = "Criterio Bayesiano de Schwarz. Penaliza más que AIC."),
          list(g = "AJUSTE RELATIVO", m = "Log-verosimilitud", v = ll_v,
               i = "Mayor = mejor ajuste. Base del AIC y BIC.")
        )

        filas_html <- lapply(filas, function(f) {
          grupo_td <- if (!is.null(f$g))
            tags$td(tags$span(class = "badge bg-secondary", f$g))
          else
            tags$td()

          val_style <- if (!is.null(f$g) && f$g == "VARIANZA EXPLICADA" &&
                           f$m == "R² ajustado")
            paste0("color:", col_r2, "; font-weight:700;")
          else ""

          tags$tr(
            grupo_td,
            tags$td(strong(f$m)),
            tags$td(style = val_style, f$v),
            tags$td(class = "text-muted small", style = "font-size:0.78rem;", f$i)
          )
        })

        tags$table(
          class = "table table-sm table-hover small mb-0",
          tags$thead(tags$tr(
            tags$th("Grupo"), tags$th("Métrica"),
            tags$th("Valor"), tags$th("Interpretación")
          )),
          tags$tbody(filas_html)
        )
      }, error = function(e) {
        div(class = "text-danger small",
            paste("Error al calcular métricas:", conditionMessage(e)))
      })
    })

    output$plot_predobs_lm <- renderPlot({
      fit <- modelo_lm(); req(fit)
      tibble::tibble(
        obs  = fitted(fit) + resid(fit),
        pred = fitted(fit)
      ) |>
        ggplot(aes(x = obs, y = pred)) +
        geom_abline(slope = 1, intercept = 0,
                    linetype = "dashed",
                    color = colores$texto, linewidth = 0.8) +
        geom_point(color = colores$primario, alpha = 0.5, size = 2) +
        geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                    color = colores$acento, linewidth = 1) +
        labs(x = "Observado", y = "Predicho",
             subtitle = "Los puntos deben seguir la diagonal — desviaciones indican mal ajuste") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          plot.subtitle    = element_text(color = colores$texto, size = 9)
        )
    }, res = 110)

    # ── Validación cruzada ────────────────────────────────

    cv_resultados_lm <- reactiveVal(NULL)

    observeEvent(input$correr_cv_lm, {
      fit <- modelo_lm()
      if (is.null(fit)) {
        showNotification("Ajusta un modelo primero.",
                         type = "warning", duration = 3)
        return()
      }
      withProgress(message = "Corriendo validación cruzada...",
                   value = 0.2, {
                     tryCatch({
                       df_cv <- datos_finales()
                       preds <- c(input$preds_num, input$preds_cat)
                       req(length(preds) > 0, input$var_y)

                       fm <- as.formula(paste(input$var_y, "~",
                                              paste(preds, collapse = " + ")))

                       folds <- rsample::vfold_cv(
                         df_cv, v = input$cv_folds_lm,
                         strata = if (isTRUE(input$cv_estratificado_lm))
                           input$var_y else NULL
                       )

                       tiene_cat <- any(sapply(
                         df_cv[, preds, drop = FALSE],
                         function(x) is.factor(x) || is.character(x)
                       ))

                       rec <- recipes::recipe(fm, data = df_cv)
                       if (tiene_cat)
                         rec <- recipes::step_dummy(rec, recipes::all_nominal_predictors())
                       rec <- rec |>
                         recipes::step_impute_median(recipes::all_numeric_predictors()) |>
                         recipes::step_zv(recipes::all_predictors())

                       modelo_parsnip <- parsnip::linear_reg() |>
                         parsnip::set_engine("lm") |>
                         parsnip::set_mode("regression")

                       wf <- workflows::workflow() |>
                         workflows::add_recipe(rec) |>
                         workflows::add_model(modelo_parsnip)

                       incProgress(0.5, detail = "Evaluando folds...")

                       metricas <- yardstick::metric_set(
                         yardstick::rmse,
                         yardstick::rsq,
                         yardstick::mae
                       )

                       res_cv <- tune::fit_resamples(
                         wf, resamples = folds,
                         metrics = metricas,
                         control = tune::control_resamples()
                       )
                       cm <- tune::collect_metrics(res_cv)

                       cv_resultados_lm(list(
                         formula  = deparse(fm),
                         folds    = input$cv_folds_lm,
                         metricas = cm
                       ))

                     }, error = function(e) {
                       showNotification(paste("Error en CV:", conditionMessage(e)),
                                        type = "error", duration = 6)
                     })
                   })
    })

    output$resultado_cv_lm <- renderUI({
      res <- cv_resultados_lm()
      if (is.null(res)) return(
        div(class = "text-muted small py-3",
            bs_icon("arrow-repeat", class = "me-2"),
            "Haz clic en ", strong("Correr CV"),
            " para evaluar la capacidad predictiva.")
      )
      cm <- res$metricas

      label_map <- c(
        rmse = "RMSE (error cuadrático medio)",
        rsq  = "R² (varianza explicada)",
        mae  = "MAE (error absoluto medio)"
      )

      tarjetas <- lapply(seq_len(nrow(cm)), function(i) {
        met <- cm$.metric[i]
        col <- if (met == "rsq") colores$exito else colores$primario
        card(
          fill = FALSE,
          class = "text-center",
          card_body(class = "p-2",
                    h4(style = paste0("color:", col, "; font-weight:700;"),
                       round(cm$mean[i], 3)),
                    p(class = "small text-muted mb-0",
                      strong(toupper(met))),
                    p(class = "small text-muted mb-0",
                      paste0("\u00b1", round(cm$std_err[i], 3), " EE"))
          )
        )
      })

      tagList(
        do.call(layout_columns,
                c(list(col_widths = rep(4, nrow(cm))),
                  tarjetas)),
        div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
            bs_icon("info-circle", class = "me-1"),
            strong(paste0(res$folds, "-fold CV · ")),
            "Fórmula: ", code(res$formula), ". ",
            "Los valores de CV estiman la capacidad de ",
            strong("generalización"), " a datos no vistos.")
      )
    })

    # ── Comparar modelos ──────────────────────────────────

    modelos_guardados_lm <- reactiveVal(list())

    observeEvent(input$guardar_modelo_lm, {
      fit    <- modelo_lm()
      nombre <- trimws(input$nombre_modelo_lm)
      if (is.null(fit)) {
        showNotification("Ajusta un modelo primero.",
                         type = "warning", duration = 3)
        return()
      }
      if (nchar(nombre) == 0) {
        showNotification("Escribe un nombre para el modelo.",
                         type = "warning", duration = 3)
        return()
      }
      actual <- modelos_guardados_lm()
      actual[[nombre]] <- list(
        fit     = fit,
        formula = deparse(formula(fit)),
        preds   = c(input$preds_num, input$preds_cat),
        var_y   = input$var_y,
        datos   = datos_finales()
      )
      modelos_guardados_lm(actual)
      showNotification(paste0("Modelo '", nombre, "' guardado."),
                       type = "message", duration = 3)
      updateTextInput(session, "nombre_modelo_lm", value = "")
    })

    observeEvent(input$limpiar_modelos_lm, {
      modelos_guardados_lm(list())
      showNotification("Modelos eliminados.", type = "message", duration = 2)
    })

    output$lista_modelos_guardados_lm <- renderUI({
      mg <- modelos_guardados_lm()
      if (length(mg) == 0) return(
        p(class = "small text-muted mb-0", "Aún no hay modelos guardados.")
      )
      tagList(lapply(names(mg), function(nm) {
        m <- mg[[nm]]
        div(class = "d-flex align-items-center gap-2 mb-1",
            bs_icon("check-circle-fill",
                    style = paste0("color:", colores$exito)),
            div(
              p(class = "small mb-0", strong(nm)),
              p(class = "small text-muted mb-0",
                style = "font-size:0.75rem;", m$formula)
            ))
      }))
    })

    output$tabla_comparacion_lm <- renderUI({
      mg <- modelos_guardados_lm()
      if (length(mg) < 1) return(
        div(class = "text-muted small py-3",
            bs_icon("info-circle", class = "me-1"),
            "Guarda al menos un modelo para ver la comparación.")
      )
      rows <- lapply(names(mg), function(nm) {
        fit <- mg[[nm]]$fit
        pm  <- tryCatch(
          performance::model_performance(fit, verbose = FALSE),
          error = function(e) NULL
        )
        if (is.null(pm)) return(NULL)
        list(
          nm   = nm,
          aic  = round(pm$AIC, 1),
          aicc = tryCatch(round(performance::performance_aicc(fit), 1),
                          error = function(e) NA),
          bic  = round(pm$BIC, 1),
          r2   = round(pm$R2, 3),
          r2adj = round(pm$R2_adjusted, 3)
        )
      })
      rows <- rows[!sapply(rows, is.null)]
      if (length(rows) == 0) return(NULL)

      best_aicc <- which.min(sapply(rows, function(r) r$aicc))

      tags$table(
        class = "table table-sm table-hover small mb-0",
        tags$thead(
          style = paste0("background:", colores$primario, "; color:#fff;"),
          tags$tr(
            tags$th("Modelo"), tags$th("AIC"),
            tags$th("AICc"),  tags$th("BIC"),
            tags$th("R²"),    tags$th("R² adj.")
          )
        ),
        tags$tbody(lapply(seq_along(rows), function(i) {
          r  <- rows[[i]]
          bg <- if (i == best_aicc)
            "background:#f0f9f5; font-weight:600;" else ""
          tags$tr(
            style = bg,
            tags$td(
              if (i == best_aicc)
                tagList(bs_icon("trophy-fill",
                                style = paste0("color:", colores$acento,
                                               "; margin-right:4px")), r$nm)
              else r$nm
            ),
            tags$td(r$aic), tags$td(r$aicc), tags$td(r$bic),
            tags$td(r$r2),  tags$td(r$r2adj)
          )
        }))
      )
    })

    output$plot_comparacion_lm <- renderPlot({
      mg <- modelos_guardados_lm()
      req(length(mg) >= 2)
      fits <- lapply(mg, function(m) m$fit)
      tryCatch({
        comp <- do.call(performance::compare_performance,
                        c(fits, list(rank = TRUE, verbose = FALSE)))
        p <- plot(comp) +
          ggplot2::scale_color_manual(
            values = colores$tableau[seq_along(mg)]) +
          ggplot2::scale_fill_manual(
            values = paste0(colores$tableau[seq_along(mg)], "33")) +
          ggplot2::labs(title = NULL,
                        subtitle = "Métricas normalizadas 0–1 · mayor área = mejor") +
          see::theme_radar() +
          ggplot2::theme(
            legend.position  = "bottom",
            plot.subtitle    = ggplot2::element_text(color = colores$texto, size = 9),
            plot.margin      = ggplot2::margin(10, 10, 5, 10)
          )
        print(p)
      }, error = function(e) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste0("Guarda al menos 2 modelos\n",
                                  "para ver el gráfico radar."),
                   color = colores$texto, size = 5, hjust = 0.5) +
          theme_void()
      })
    }, res = 96)

    # ────────────────────────────────────────────────────
    # PESTAÑA 8: Parámetros
    # ────────────────────────────────────────────────────

    output$tabla_params_ui <- renderUI({
      fit <- modelo_lm()
      if (is.null(fit)) return(
        div(class = "text-muted small py-3",
            "Ajusta el modelo primero.")
      )

      std <- isTRUE(input$estandarizar)

      # Obtener parámetros — estandarizados o crudos
      mp <- tryCatch(
        parameters::model_parameters(
          fit, ci = 0.95,
          standardize = if (std) "refit" else NULL,
          verbose = FALSE
        ),
        error = function(e) NULL
      )

      if (is.null(mp)) {
        # Fallback to base R
        s        <- summary(fit)
        coef_mat <- coef(s)
        ci_mat   <- confint(fit, level = 0.95)
        mp_df <- data.frame(
          Parameter = rownames(coef_mat),
          Coefficient = coef_mat[,1],
          SE = coef_mat[,2],
          CI_low = ci_mat[,1],
          CI_high = ci_mat[,2],
          p = coef_mat[,4]
        )
      } else {
        mp_df <- as.data.frame(mp)
      }

      col_est <- if (std) "Std_Coefficient" else "Coefficient"
      if (!col_est %in% names(mp_df)) col_est <- "Coefficient"

      encabezado <- if (std)
        "β estandarizado (SD)" else "Estimado"

      filas <- lapply(seq_len(nrow(mp_df)), function(i) {
        nm   <- as.character(mp_df$Parameter[i])
        est  <- round(mp_df[[col_est]][i], 3)
        se   <- round(mp_df$SE[i], 3)
        pval <- mp_df$p[i]
        lo   <- round(mp_df$CI_low[i], 3)
        hi   <- round(mp_df$CI_high[i], 3)

        p_txt <- if (!is.na(pval)) {
          if (pval < 0.001) "< 0.001 ***" else
            if (pval < 0.01)  paste0(round(pval,3), " **") else
              if (pval < 0.05)  paste0(round(pval,3), " *") else
                round(pval, 3)
        } else "—"
        col_p <- if (!is.na(pval) && pval < 0.001) colores$exito else
          if (!is.na(pval) && pval < 0.05) colores$acento else colores$texto

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

      tagList(
        if (std) div(
          class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("distribute-vertical", class = "me-1"),
          strong("Coeficientes estandarizados (β)."),
          " Cada estimado está en unidades de desviación estándar — ",
          "mayor |β| indica mayor peso relativo del predictor. ",
          "Efectos marginales y predicciones siguen en escala original."
        ),
        tags$table(
          class = "table table-sm table-hover small mb-0",
          tags$thead(tags$tr(
            tags$th("Parámetro"), tags$th(encabezado),
            tags$th("EE"), tags$th("IC 95%"), tags$th("p-valor")
          )),
          tags$tbody(filas)
        )
      )
    })

    output$plot_forest <- renderPlot({
      fit <- modelo_lm()
      req(fit)
      std <- isTRUE(input$estandarizar)

      mp <- tryCatch(
        parameters::model_parameters(
          fit, ci = 0.95,
          standardize = if (std) "refit" else NULL,
          verbose = FALSE
        ),
        error = function(e) NULL
      )

      if (is.null(mp)) {
        ci    <- confint(fit, level = 0.95)
        coefs <- coef(fit)
        pvals <- coef(summary(fit))[, 4]
        df_f  <- tibble::tibble(
          term = names(coefs),
          est  = coefs, lo = ci[,1], hi = ci[,2],
          sig  = pvals < 0.05
        )
      } else {
        mp_df <- as.data.frame(mp)
        col_est <- if (std && "Std_Coefficient" %in% names(mp_df))
          "Std_Coefficient" else "Coefficient"
        df_f <- tibble::tibble(
          term = as.character(mp_df$Parameter),
          est  = mp_df[[col_est]],
          lo   = mp_df$CI_low,
          hi   = mp_df$CI_high,
          sig  = !is.na(mp_df$p) & mp_df$p < 0.05
        )
      }

      df_f <- df_f |>
        dplyr::filter(term != "(Intercept)") |>
        dplyr::mutate(term = factor(term, levels = rev(unique(term))))

      if (nrow(df_f) == 0) return(invisible(NULL))

      x_label <- if (std) "β estandarizado (SD)" else
        paste0("Coeficiente (unidades de ", input$var_y, ")")

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
        labs(x = x_label, y = NULL,
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

    output$plot_importancia_lm <- renderPlot({
      fit <- modelo_lm(); req(fit)

      fit_std <- modelo_lm_std()

      # Usar modelo estandarizado si está disponible
      mp <- if (!is.null(fit_std)) {
        tryCatch(
          parameters::model_parameters(fit_std, ci = 0.95, verbose = FALSE),
          error = function(e) NULL
        )
      } else NULL

      # Fallback a coeficientes crudos
      if (is.null(mp)) {
        mp <- tryCatch(
          parameters::model_parameters(fit, ci = 0.95, verbose = FALSE),
          error = function(e) NULL
        )
      }

      if (is.null(mp)) return(
        ggplot() + annotate("text", x=0.5, y=0.5,
                            label="No se pudieron calcular los parámetros.",
                            color=colores$texto, size=4) + theme_void()
      )

      df_imp <- as.data.frame(mp)
      col_est <- "Coefficient"
      es_std  <- !is.null(fit_std)

      df_imp <- df_imp |>
        dplyr::filter(Parameter != "(Intercept)") |>
        dplyr::mutate(
          abs_est   = abs(.data[[col_est]]),
          direccion = ifelse(.data[[col_est]] >= 0, "Positivo", "Negativo"),
          sig       = !is.na(p) & p < 0.05,
          Parameter = factor(Parameter,
                             levels = Parameter[order(abs_est)])
        ) |>
        dplyr::arrange(abs_est)

      if (nrow(df_imp) == 0) return(invisible(NULL))

      df_imp$sig_chr <- ifelse(df_imp$sig, "sig", "no_sig")

      x_label <- if (es_std) "Importancia (β estandarizado en SD)"
      else paste0("Importancia (coeficiente en unidades de ", input$var_y, ")")

      ggplot(df_imp,
             aes(x = abs_est, y = Parameter,
                 fill = direccion, alpha = sig_chr)) +
        geom_col(width = 0.65) +
        geom_text(aes(label = sprintf("%+.3f", .data[[col_est]])),
                  hjust = -0.15, size = 3.5,
                  color = colores$texto) +
        scale_fill_manual(
          values = c("Positivo" = colores$primario,
                     "Negativo" = colores$peligro),
          name = "Dirección"
        ) +
        scale_alpha_manual(
          values = c("sig" = 1, "no_sig" = 0.35),
          guide  = "none"
        ) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
        labs(
          x        = x_label,
          y        = NULL,
          subtitle = "Barras transparentes = p ≥ 0.05 · Mayor barra = mayor peso relativo"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position    = "bottom",
          plot.subtitle      = element_text(color = colores$texto, size = 9),
          legend.text        = element_text(size = 9),
          plot.margin        = margin(10, 20, 5, 10)
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
    # PESTAÑA 9: Efectos marginales
    # ────────────────────────────────────────────────────

    output$sel_pred_marginal_lm <- renderUI({
      fit <- modelo_lm(); req(fit)
      preds <- c(input$preds_num, input$preds_cat)
      req(length(preds) > 0)
      selectInput(ns("pred_marginal_lm"),
                  label    = "Predictor a explorar:",
                  choices  = preds,
                  selected = preds[1])
    })

    output$marginal_valores_tipicos_lm <- renderUI({
      fit   <- modelo_lm(); req(fit, input$pred_marginal_lm)
      df    <- datos_finales()
      preds <- c(input$preds_num, input$preds_cat)
      otros <- preds[preds != input$pred_marginal_lm]
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

    output$plot_marginal_lm <- renderPlot({
      fit  <- modelo_lm(); req(fit, input$pred_marginal_lm)
      df   <- datos_finales()
      pred <- input$pred_marginal_lm
      es_cat <- pred %in% vars_categoricas()

      tryCatch({
        rel    <- suppressWarnings(
          modelbased::estimate_relation(fit, by = pred, verbose = FALSE)
        )
        df_rel <- as.data.frame(rel)

        p <- ggplot(df_rel, aes(x = .data[[pred]], y = Predicted)) +
          theme_minimal(base_size = 13)

        if (es_cat) {
          if (isTRUE(input$marginal_ci_lm))
            p <- p + geom_errorbar(
              aes(ymin = CI_low, ymax = CI_high),
              width = 0.2, linewidth = 0.8,
              color = colores$primario)
          p <- p + geom_point(color = colores$primario, size = 3.5)
        } else {
          if (isTRUE(input$marginal_ci_lm))
            p <- p + geom_ribbon(aes(ymin = CI_low, ymax = CI_high),
                                 fill = colores$primario, alpha = 0.15)
          if (isTRUE(input$marginal_puntos_lm))
            p <- p + geom_point(
              data = data.frame(x_obs = df[[pred]],
                                y_obs = as.numeric(df[[input$var_y]])),
              aes(x = x_obs, y = y_obs),
              color = colores$primario, alpha = 0.3, size = 1.5,
              inherit.aes = FALSE)
          p <- p + geom_line(color = colores$primario, linewidth = 1.2)
        }

        p + labs(x = pred, y = input$var_y,
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
                            label = paste0("Error: ", conditionMessage(e)),
                            color = colores$texto, size = 3.5, hjust = 0.5) +
          theme_void()
      })
    }, res = 96)

    output$marginal_interpretacion_lm <- renderUI({
      fit  <- modelo_lm(); req(fit, input$pred_marginal_lm)
      pred <- input$pred_marginal_lm
      es_cat <- pred %in% vars_categoricas()
      tryCatch({
        coefs <- coef(fit)
        pvals <- coef(summary(fit))[, 4]
        if (es_cat) {
          filas_cat <- names(coefs)[grepl(pred, names(coefs), fixed = TRUE)]
          texto <- if (length(filas_cat) > 0) {
            paste0("La variable '", pred, "' genera diferencias en ",
                   input$var_y, ". Diferencias respecto a la referencia: ",
                   paste(paste0(gsub(pred, "", filas_cat),
                                " = ", round(coefs[filas_cat], 3)),
                         collapse = ", "), " unidades.")
          } else "Ver tabla de parámetros para la interpretación."
        } else {
          est <- round(coefs[pred], 3)
          sig <- pvals[pred] < 0.05
          texto <- paste0(
            "Por cada unidad adicional de '", pred, "', ",
            input$var_y, " cambia en promedio ",
            ifelse(est >= 0, "+", ""), est, " unidades",
            " (manteniendo el resto fijo). ",
            if (sig) "Efecto estadísticamente significativo."
            else "Efecto NO estadísticamente significativo."
          )
        }
        div(class = "alert alert-info small py-2 px-3 mb-0",
            bs_icon("lightbulb-fill", class = "me-1"), texto)
      }, error = function(e) NULL)
    })

    # ── Predicción puntual ────────────────────────────────

    output$inputs_prediccion_lm <- renderUI({
      fit   <- modelo_lm(); req(fit)
      df    <- datos_finales()
      preds <- c(input$preds_num, input$preds_cat)
      req(length(preds) > 0)
      inputs <- lapply(preds, function(nm) {
        col <- df[[nm]]
        if (is.numeric(col)) {
          numericInput(
            inputId = ns(paste0("pred_val_lm_", nm)),
            label   = paste0(nm, " (media = ",
                             round(mean(col, na.rm=TRUE), 1), "):"),
            value   = round(mean(col, na.rm=TRUE), 1),
            step    = round(sd(col, na.rm=TRUE) / 10, 2)
          )
        } else {
          moda <- names(sort(table(col), decreasing=TRUE))[1]
          selectInput(
            inputId  = ns(paste0("pred_val_lm_", nm)),
            label    = nm,
            choices  = levels(col),
            selected = moda
          )
        }
      })
      do.call(tagList, inputs)
    })

    resultado_prediccion_lm_data <- eventReactive(
      input$calcular_prediccion_lm, {
        fit   <- modelo_lm(); req(fit)
        df    <- datos_finales()
        preds <- c(input$preds_num, input$preds_cat)
        req(length(preds) > 0)
        nueva_obs <- tryCatch({
          vals <- lapply(preds, function(nm) {
            col <- df[[nm]]
            val <- input[[paste0("pred_val_lm_", nm)]]
            req(!is.null(val))
            if (is.numeric(col)) as.numeric(val)
            else factor(val, levels = levels(col))
          })
          names(vals) <- preds
          as.data.frame(vals)
        }, error = function(e) NULL)
        req(nueva_obs)
        tryCatch(
          modelbased::estimate_expectation(
            fit, data = nueva_obs, verbose = FALSE
          ),
          error = function(e) NULL
        )
      }, ignoreNULL = TRUE)

    output$resultado_prediccion_lm <- renderUI({
      res <- resultado_prediccion_lm_data()
      if (is.null(res)) return(
        div(class = "text-muted small py-3",
            bs_icon("calculator", class = "me-2"),
            "Define los valores y haz clic en ",
            strong("Calcular predicción"), ".")
      )
      df_res <- as.data.frame(res)
      pred   <- round(df_res$Predicted[1], 3)
      lo     <- round(df_res$CI_low[1], 3)
      hi     <- round(df_res$CI_high[1], 3)
      tagList(
        div(class = "text-center py-3",
            h2(style = paste0("color:", colores$primario,
                              "; font-weight:700; font-size:2.5rem;"),
               pred),
            p(class = "text-muted mb-1",
              strong(paste0(input$var_y, " predicho"))),
            p(class = "small text-muted",
              "IC 95%: ", strong(paste0("[", lo, ", ", hi, "]")))
        ),
        tags$hr(),
        div(class = "small text-muted",
            bs_icon("info-circle", class = "me-1"),
            "Valores usados: ",
            paste(sapply(c(input$preds_num, input$preds_cat), function(nm) {
              val <- input[[paste0("pred_val_lm_", nm)]]
              paste0(nm, " = ", val)
            }), collapse = " · ")
        )
      )
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 10: Contrastes
    # ────────────────────────────────────────────────────

    output$contrasts_no_cat_msg_lm <- renderUI({
      if (length(input$preds_cat) == 0)
        div(class = "alert alert-warning small py-2 px-3 mb-3",
            bs_icon("exclamation-triangle-fill", class = "me-1"),
            "El modelo no tiene predictores categóricos. ",
            "Ve a ", strong("Ajustar modelo"),
            " y agrega al menos una variable categórica.")
    })

    output$sel_var_contraste_lm <- renderUI({
      fit  <- modelo_lm(); req(fit)
      cats <- input$preds_cat; req(length(cats) > 0)
      selectInput(ns("var_contraste_lm"),
                  label    = "Variable para contrastar:",
                  choices  = cats, selected = cats[1])
    })

    output$tabla_contrastes_lm <- renderUI({
      fit <- modelo_lm(); req(fit, input$var_contraste_lm)
      tryCatch({
        ct    <- modelbased::estimate_contrasts(
          fit, contrast = input$var_contraste_lm,
          p_adjust = input$metodo_ajuste_lm, verbose = FALSE)
        df_ct <- as.data.frame(ct)
        char_cols <- names(df_ct)[sapply(df_ct, function(x)
          is.character(x) || is.factor(x))]
        if (length(char_cols) >= 2)
          etiqueta <- paste0(df_ct[[char_cols[1]]], " vs. ",
                             df_ct[[char_cols[2]]])
        else etiqueta <- paste0("Contraste ", seq_len(nrow(df_ct)))

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
            else if (pv < 0.01)  paste0(round(pv,3), " **")
            else if (pv < 0.05)  paste0(round(pv,3), " *")
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
                                 " !important; color:#fff !important;"),
                    "Contraste"),
            tags$th(style=paste0("background:", colores$primario,
                                 " !important; color:#fff !important;",
                                 "text-align:center;"),
                    paste0("Diferencia (", input$var_y, ")")),
            tags$th(style=paste0("background:", colores$primario,
                                 " !important; color:#fff !important;",
                                 "text-align:center;"), "IC 95%"),
            tags$th(style=paste0("background:", colores$primario,
                                 " !important; color:#fff !important;",
                                 "text-align:center;"), "p-valor")
          )),
          tags$tbody(filas)
        )
      }, error = function(e) {
        div(class="text-muted small py-3",
            "Ajusta el modelo con predictores categóricos.")
      })
    })

    output$plot_contrastes_lm <- renderPlot({
      fit <- modelo_lm(); req(fit, input$var_contraste_lm)
      tryCatch({
        ct    <- modelbased::estimate_contrasts(
          fit, contrast = input$var_contraste_lm,
          p_adjust = input$metodo_ajuste_lm, verbose = FALSE)
        df_ct <- as.data.frame(ct)
        char_cols <- names(df_ct)[sapply(df_ct, function(x)
          is.character(x) || is.factor(x))]
        if (length(char_cols) >= 2)
          etiqueta <- paste0(df_ct[[char_cols[1]]], " vs. ",
                             df_ct[[char_cols[2]]])
        else etiqueta <- paste0("Contraste ", seq_len(nrow(df_ct)))

        diff_col <- if ("Difference" %in% names(df_ct)) "Difference"
        else names(df_ct)[sapply(df_ct, is.numeric)][1]
        ci_lo <- if ("CI_low"  %in% names(df_ct)) df_ct$CI_low
        else df_ct[[diff_col]] - 1
        ci_hi <- if ("CI_high" %in% names(df_ct)) df_ct$CI_high
        else df_ct[[diff_col]] + 1
        p_vals <- if ("p" %in% names(df_ct)) df_ct$p
        else if ("p.value" %in% names(df_ct)) df_ct$p.value
        else rep(0.5, nrow(df_ct))

        df_plot <- data.frame(
          contraste = factor(etiqueta, levels = rev(unique(etiqueta))),
          diff      = df_ct[[diff_col]],
          lo = ci_lo, hi = ci_hi,
          sig = !is.na(p_vals) & p_vals < 0.05
        )

        ggplot(df_plot, aes(x=diff, y=contraste, xmin=lo, xmax=hi,
                            color=sig)) +
          geom_vline(xintercept=0, linetype="dashed",
                     color=colores$texto, linewidth=0.7) +
          geom_errorbar(aes(ymin=lo, ymax=hi), width=0.25,
                        linewidth=1.1, orientation="y") +
          geom_point(size=3.5) +
          scale_color_manual(
            values=c(`TRUE`=colores$acento, `FALSE`=colores$primario),
            labels=c(`TRUE`="Significativo", `FALSE`="No significativo"),
            name=NULL) +
          labs(x=paste0("Diferencia en ", input$var_y, " (unidades)"),
               y=NULL,
               subtitle=paste0("Ajuste p-valores: ",
                               input$metodo_ajuste_lm, " · IC 95%")) +
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
    # PESTAÑA 12: Código R reproducible
    # ────────────────────────────────────────────────────

    codigo_generado <- reactive({
      req(input$var_y)
      preds <- c(input$preds_num, input$preds_cat)
      ints  <- input$interacciones
      terminos <- if (!is.null(ints) && length(ints) > 0)
        c(preds, ints) else preds
      formula_txt <- if (length(terminos) > 0)
        paste(input$var_y, "~", paste(terminos, collapse = " + "))
      else paste(input$var_y, "~ 1")

      tiene_cat <- length(input$preds_cat) > 0

      if (input$fuente_datos == "ejemplo_ave") {
        carga <- paste0(
          "load(\"data/birdabundance_lm.rda\")\n",
          "datos <- birdabundance_lm\n"
        )
      } else if (input$fuente_datos == "ejemplo_salud") {
        carga <- paste0(
          "load(\"data/birthwt_lm.rda\")\n",
          "datos <- birthwt_lm\n"
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