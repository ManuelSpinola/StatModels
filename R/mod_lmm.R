# ============================================================
# mod_lmm.R — Modelos Lineales Mixtos (LMM / GLMM)
# StatModels · StatSuite
#
# Paquetes: lme4, lmerTest, performance (easystats)
# Datos:    rikz_lmm.rda / sleepstudy_lmm.rda (o propios)
# ============================================================

# ── UI ──────────────────────────────────────────────────────
mod_lmm_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "px-1 pt-2 pb-2",
      layout_columns(
        col_widths = c(9, 3),
        div(
          h4(style = paste0("color:", colores$primario,
                            "; font-weight:700; margin-bottom:4px;"),
             bs_icon("diagram-3", class = "me-2"),
             "Modelos Lineales y Generalizados Mixtos (LMM / GLMM)"),
          p(class = "text-muted small mb-0",
            "Extienden el LM y el GLM para datos con ",
            strong("estructura jer\u00e1rquica"), ": sitios dentro de playas, ",
            "individuos dentro de poblaciones, medidas repetidas. ",
            "Combinan ", strong("efectos fijos"), " (poblacionales) con ",
            strong("efectos aleatorios"), " (variabilidad entre grupos). ",
            "Paquete: ", strong("lme4"), " \u00b7 inferencia con ",
            strong("lmerTest"), " \u00b7 diagn\u00f3stico con ", strong("performance"),
            " y ", strong("easystats"), ".")
        ),
        div(
          class = "text-end pt-1",
          tags$span(
            class = "badge",
            style = paste0("background:", colores$primario,
                           "; font-size:0.8rem; padding:6px 12px;"),
            bs_icon("diagram-3", class = "me-1"), "lme4"
          )
        )
      )
    ),

    navset_card_tab(
      id = ns("pestanas"),

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "\u00bfQu\u00e9 es?"),
        card_body(
          h4(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Modelos mixtos \u2014 LMM / GLMM"),
          p(class = "text-muted small mb-3",
            "lme4 \u00b7 Bates et al. (2015) \u00b7 easystats"),

          layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,

            card(
              card_header(bs_icon("question-circle", class = "me-1"),
                          "\u00bfCu\u00e1ndo usar modelos mixtos?"),
              card_body(
                tags$ul(class = "small mb-0",
                  tags$li("Datos con ", strong("estructura jer\u00e1rquica"),
                          ": sitios dentro de playas, individuos ",
                          "dentro de poblaciones"),
                  tags$li(strong("Medidas repetidas"), " en el tiempo o espacio"),
                  tags$li("Observaciones dentro del mismo grupo ",
                          strong("no son independientes")),
                  tags$li("Quieres generalizar m\u00e1s all\u00e1 de los grupos ",
                          "muestreados (efectos aleatorios)")
                )
              )
            ),

            card(
              card_header(bs_icon("code-slash", class = "me-1"),
                          "Anatom\u00eda de la f\u00f3rmula"),
              card_body(
                p(class = "small text-muted mb-2",
                  "Los modelos mixtos tienen dos partes:"),
                div(class = "alert alert-secondary small py-2 px-3 mb-2",
                    style = "font-family: monospace;",
                    "Y ~ X\u2081 + X\u2082 + (1 | grupo)"),
                tags$table(class = "table table-sm small mb-0",
                  tags$tbody(
                    tags$tr(tags$td(code("Y")),
                            tags$td("Variable respuesta")),
                    tags$tr(tags$td(code("X\u2081 + X\u2082")),
                            tags$td("Efectos fijos (poblacionales)")),
                    tags$tr(tags$td(code("(1 | grupo)")),
                            tags$td("Intercepto aleatorio por grupo")),
                    tags$tr(tags$td(code("(1 + X | grupo)")),
                            tags$td("Intercepto + pendiente aleatoria")),
                    tags$tr(tags$td(code("(1 | A/B)")),
                            tags$td("Anidado: B dentro de A"))
                  )
                )
              )
            ),

            card(
              card_header(bs_icon("layers", class = "me-1"),
                          "LMM vs GLMM"),
              card_body(
                p(class = "small text-muted mb-2",
                  strong("lme4"), " ofrece dos funciones principales:"),
                tags$ul(class = "small mb-2",
                  tags$li(code("lmer()"), " \u2014 ",
                          strong("LMM"), ": respuesta continua gaussiana"),
                  tags$li(code("glmer()"), " \u2014 ",
                          strong("GLMM"), ": respuesta binomial o Poisson")
                ),
                div(class = "alert alert-info small py-2 px-3 mb-0",
                    bs_icon("info-circle", class = "me-1"),
                    "Para distribuciones m\u00e1s complejas (NB, ZIP, Beta) ",
                    "considera ", strong("glmmTMB"), ".")
              )
            )
          ),

          tags$hr(),

          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,

            card(
              card_header(bs_icon("shuffle", class = "me-1"),
                          "Efectos fijos vs aleatorios"),
              card_body(
                tags$table(class = "table table-sm small mb-0",
                  tags$thead(tags$tr(
                    tags$th("Aspecto"),
                    tags$th("Efectos fijos"),
                    tags$th("Efectos aleatorios")
                  )),
                  tags$tbody(
                    tags$tr(tags$td("Pregunta"),
                            tags$td("\u00bfCu\u00e1l es el efecto promedio?"),
                            tags$td("\u00bfCu\u00e1nto var\u00edan los grupos?")),
                    tags$tr(tags$td("Estimaci\u00f3n"),
                            tags$td("Coeficiente \u03b2"),
                            tags$td("Varianza \u03c3\u00b2")),
                    tags$tr(tags$td("Inferencia"),
                            tags$td("t-test, p-valor"),
                            tags$td("ICC, R\u00b2 Nakagawa")),
                    tags$tr(tags$td("Ejemplo"),
                            tags$td("NAP, Exposici\u00f3n"),
                            tags$td("Playa (intercept)"))
                  )
                )
              )
            ),

            card(
              card_header(bs_icon("bar-chart-steps", class = "me-1"),
                          "R\u00b2 Nakagawa \u2014 m\u00e9trica clave"),
              card_body(
                p(class = "small mb-2",
                  "Los modelos mixtos tienen dos R\u00b2:"),
                tags$ul(class = "small mb-2",
                  tags$li(strong("R\u00b2 marginal"), " \u2014 varianza explicada ",
                          "solo por los efectos ", strong("fijos")),
                  tags$li(strong("R\u00b2 condicional"), " \u2014 varianza explicada ",
                          "por efectos fijos ", strong("+ aleatorios"))
                ),
                div(class = "alert alert-info small py-2 px-3 mb-0",
                    bs_icon("info-circle", class = "me-1"),
                    "ICC = R\u00b2 condicional \u2212 R\u00b2 marginal = proporci\u00f3n ",
                    "de varianza debida a los grupos.")
              )
            )
          )
        )
      ), # /PESTAÑA 1

      # ════════════════════════════════════════════════
      # PESTAÑA 2: Fundamentos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("journal-bookmark", class = "me-1"),
                        "Fundamentos"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),

            div(
              h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
                 "1. El problema de la pseudorreplicaci\u00f3n"),
              p(class = "small",
                "En el dataset RIKZ hay 5 sitios por playa. Si ignoramos la ",
                "playa y hacemos un LM simple, asumimos que los 45 sitios son ",
                "independientes \u2014 pero los sitios de la misma playa est\u00e1n ",
                "m\u00e1s correlacionados entre s\u00ed que con sitios de otras playas. ",
                "Esto infla los grados de libertad y produce p-valores incorrectos."),
              div(class = "alert alert-warning small py-2 px-3 mb-3",
                  bs_icon("exclamation-triangle-fill", class = "me-1"),
                  "LM simple ignora la estructura: trata 45 obs como ",
                  "independientes cuando hay solo 9 playas."),

              h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
                 "2. Intercepto aleatorio"),
              p(class = "small",
                "La soluci\u00f3n m\u00e1s simple: cada playa tiene su propio intercepto. ",
                "Algunas playas tienen m\u00e1s especies en promedio (intercepto alto), ",
                "otras menos (intercepto bajo). El modelo estima la ",
                strong("distribuci\u00f3n"), " de esos interceptos, no cada uno por separado."),
              div(class = "alert alert-secondary small py-2 px-3 mb-3",
                  code("Richness ~ NAP + (1 | Beach)")),

              h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
                 "3. Pendiente aleatoria"),
              p(class = "small",
                "Adem\u00e1s del intercepto, el efecto de NAP puede variar entre playas. ",
                "En algunas playas la riqueza baja mucho con NAP, en otras poco."),
              div(class = "alert alert-secondary small py-2 px-3 mb-0",
                  code("Richness ~ NAP + (1 + NAP | Beach)"))
            ),

            div(
              h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
                 "4. M\u00e9todo de estimaci\u00f3n"),
              tags$table(class = "table table-sm small mb-3",
                tags$thead(tags$tr(
                  tags$th("M\u00e9todo"), tags$th("Uso recomendado")
                )),
                tags$tbody(
                  tags$tr(tags$td(strong("REML")),
                          tags$td("Estimaci\u00f3n de varianzas. ",
                                  "Por defecto en lmer(). ",
                                  "No usar para comparar modelos con distintos efectos fijos.")),
                  tags$tr(tags$td(strong("ML")),
                          tags$td("Comparaci\u00f3n de modelos con ",
                                  "distintos efectos fijos (AIC/LRT)."))
                )
              ),

              h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
                 "5. ICC \u2014 Correlaci\u00f3n Intraclase"),
              p(class = "small",
                "ICC = proporci\u00f3n de varianza total atribuible al grupo. ",
                "ICC alto (> 0.3) indica que el agrupamiento importa mucho ",
                "y justifica el modelo mixto."),
              div(class = "alert alert-secondary small py-2 px-3 mb-3",
                  code("performance::icc(modelo)")),

              h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
                 "6. Distribuci\u00f3n de los efectos aleatorios"),
              p(class = "small",
                "Los efectos aleatorios se asumen normales: ",
                code("b ~ N(0, \u03c3\u00b2)"), ". ",
                "No se estiman efectos por grupo individual, sino la varianza ",
                "de su distribuci\u00f3n. Esto se llama ", strong("shrinkage"),
                " o contraction hacia la media."),

              h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
                 "7. Singular fit"),
              p(class = "small mb-0",
                "Cuando la varianza de un efecto aleatorio se estima en 0 o ",
                "la correlaci\u00f3n entre efectos aleatorios es \u00b11, el modelo ",
                "tiene un ", strong("singular fit"), ". Indica sobreparametrizaci\u00f3n. ",
                "La soluci\u00f3n es simplificar la estructura de efectos aleatorios.")
            )
          )
        )
      ), # /PESTAÑA 2

      # ════════════════════════════════════════════════
      # PESTAÑA 3: Los datos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("table", class = "me-1"), "Los datos"),
        card_body(
          navset_pill(

            nav_panel(
              title = tagList(bs_icon("database", class = "me-1"),
                              "Cargar datos"),
              br(),
              layout_columns(
                col_widths = c(4, 8),
                card(
                  card_header(bs_icon("folder2-open", class = "me-1"),
                              "Fuente de datos"),
                  card_body(
                    style = "overflow: visible; height: auto;",
                    uiOutput(ns("sel_fuente_datos")),
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
                        "Tabulador"        = "\t"
                      ),
                      selected = ","
                    ),
                    p(class = "small text-muted mb-0",
                      bs_icon("info-circle", class = "me-1"),
                      "La primera fila debe contener los nombres de las columnas."),
                    tags$hr(),
                    uiOutput(ns("info_dataset")),
                    uiOutput(ns("resumen_datos"))
                  )
                ),
                card(
                  card_header(bs_icon("eye", class = "me-1"),
                              "Vista previa"),
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
              title = tagList(bs_icon("sliders2", class = "me-1"),
                              "Tipos de variables"),
              br(),
              p(class = "small text-muted mb-3",
                "Verifica que cada variable tenga el tipo correcto. ",
                "La variable de agrupamiento debe ser ", strong("Factor"), "."),
              layout_columns(
                col_widths = c(10, 2),
                uiOutput(ns("tabla_tipos")),
                div(
                  class = "pt-2",
                  actionButton(ns("aplicar_tipos"), "Aplicar tipos",
                               class = "btn-primary w-100",
                               icon  = icon("check")),
                  br(), br(),
                  actionButton(ns("resetear_tipos"), "Restaurar",
                               class = "btn-outline-secondary w-100 btn-sm",
                               icon  = icon("rotate-left"))
                )
              ),
              uiOutput(ns("tipos_aplicados_msg"))
            )
          )
        )
      ), # /PESTAÑA 3

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Explorar
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("zoom-in", class = "me-1"), "Explorar"),
        card_body(
          p(class = "small text-muted mb-3",
            "Visualiza la estructura jer\u00e1rquica de los datos. El gr\u00e1fico ",
            "de spaguetti muestra la relaci\u00f3n X\u2192Y por grupo, revelando ",
            "si los interceptos y/o pendientes var\u00edan entre grupos."
          ),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("sel_var_y_exp")),
                uiOutput(ns("sel_var_x_exp")),
                uiOutput(ns("sel_grupo_exp")),
                checkboxInput(ns("mostrar_global"),
                              "Mostrar l\u00ednea global (LM simple)",
                              value = TRUE),
                checkboxInput(ns("mostrar_grupos"),
                              "Mostrar l\u00edneas por grupo",
                              value = TRUE),
                tags$hr(),
                uiOutput(ns("cards_icc_previo"))
              )
            ),
            div(
              plotOutput(ns("plot_spaghetti"), height = "420px"),
              uiOutput(ns("insight_estructura"))
            )
          )
        )
      ), # /PESTAÑA 4

      # ════════════════════════════════════════════════
      # PESTAÑA 5: Ajustar modelo
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("gear", class = "me-1"), "Ajustar modelo"),
        card_body(
          layout_columns(
            col_widths = c(4, 8),

            card(
              card_header(bs_icon("sliders", class = "me-1"),
                          "Especificaci\u00f3n del modelo"),
              card_body(
                style = "overflow: visible; height: auto;",

                uiOutput(ns("sel_var_y")),
                uiOutput(ns("sel_familia")),
                tags$hr(),

                p(class = "small fw-bold text-muted mb-1",
                  bs_icon("graph-up", class = "me-1"),
                  "Efectos fijos"),
                p(class = "small text-muted mb-2",
                  "Predictores con efecto poblacional (\u03b2)."),
                uiOutput(ns("sel_efectos_fijos")),
                tags$hr(),

                p(class = "small fw-bold text-muted mb-1",
                  bs_icon("diagram-3", class = "me-1"),
                  "Efectos aleatorios"),
                p(class = "small text-muted mb-1",
                  "Variable de agrupamiento y estructura aleatoria:"),
                uiOutput(ns("sel_grupo")),
                selectInput(
                  ns("estructura_aleatoria"),
                  label = "Estructura:",
                  choices = c(
                    "Intercepto aleatorio: (1 | grupo)"           = "intercepto",
                    "Intercepto + pendiente: (1 + X | grupo)"     = "pendiente",
                    "Solo pendiente: (0 + X | grupo)"             = "solo_pendiente",
                    "Anidado: (1 | A/B)"                          = "anidado"
                  ),
                  selected = "intercepto"
                ),
                conditionalPanel(
                  condition = paste0(
                    "input['", ns("estructura_aleatoria"),
                    "'] == 'pendiente' || input['",
                    ns("estructura_aleatoria"), "'] == 'solo_pendiente'"
                  ),
                  uiOutput(ns("sel_pendiente_var"))
                ),
                conditionalPanel(
                  condition = paste0(
                    "input['", ns("estructura_aleatoria"), "'] == 'anidado'"
                  ),
                  uiOutput(ns("sel_grupo_b"))
                ),
                tags$hr(),

                selectInput(
                  ns("metodo_lmm"),
                  label = "M\u00e9todo de estimaci\u00f3n:",
                  choices = c(
                    "REML (inferencia)"        = "REML",
                    "ML (comparar modelos)"    = "ML"
                  ),
                  selected = "REML"
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
                textInput(ns("nombre_modelo"),
                          label = NULL,
                          placeholder = "Ej: nulo, nap, nap+expo\u2026"),
                actionButton(ns("guardar_modelo"), "Guardar modelo",
                             class = "btn-outline-primary w-100 btn-sm",
                             icon  = icon("floppy-disk"))
              )
            ),

            div(
              uiOutput(ns("aviso_singular")),
              uiOutput(ns("cards_metricas_lmm")),
              br(),
              card(
                card_header(bs_icon("list-ol", class = "me-1"),
                            "F\u00f3rmula ajustada"),
                card_body(
                  style = "overflow: visible; height: auto;",
                  uiOutput(ns("formula_ajustada"))
                )
              )
            )
          )
        )
      ), # /PESTAÑA 5

      # ════════════════════════════════════════════════
      # PESTAÑA 6: Diagnóstico
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard-check", class = "me-1"),
                        "Diagn\u00f3stico"),
        div(
          class = "p-3",
          p(class = "small text-muted mb-3",
            "Verificaci\u00f3n de supuestos del modelo mixto. ",
            "Generado con ", strong("performance::check_model()"), "."),
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            card(
              card_header(bs_icon("graph-up", class = "me-1"),
                          "Gr\u00e1ficos de diagn\u00f3stico"),
              card_body(
                style = "height: auto;",
                plotOutput(ns("plot_diagnostico"), height = "420px")
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("diagram-3", class = "me-1"),
                            "Efectos aleatorios (caterpillar plot)"),
                card_body(
                  style = "height: auto;",
                  plotOutput(ns("plot_ranef"), height = "240px")
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
        )
      ), # /PESTAÑA 6

      # ════════════════════════════════════════════════
      # PESTAÑA 7: Performance
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("speedometer2", class = "me-1"),
                        "Performance"),
        div(
          class = "p-3",
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            card(
              card_header(
                bs_icon("speedometer2", class = "me-1"),
                "M\u00e9tricas del modelo",
                span(class = "text-muted small ms-2",
                     "\u2014 performance \u00b7 easystats")
              ),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("tabla_performance"))
              )
            ),
            card(
              card_header(
                bs_icon("pie-chart", class = "me-1"),
                "ICC \u2014 Correlaci\u00f3n intraclase",
                span(class = "text-muted small ms-2",
                     "\u2014 performance::icc()")
              ),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("tabla_icc")),
                br(),
                plotOutput(ns("plot_icc"), height = "200px")
              )
            )
          )
        )
      ), # /PESTAÑA 9

      # ════════════════════════════════════════════════
      # PESTAÑA 8: Parámetros
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("list-ol", class = "me-1"),
                        "Par\u00e1metros"),
        div(
          class = "p-3",
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            card(
              card_header(
                bs_icon("layout-text-sidebar", class = "me-1"),
                "Efectos fijos",
                span(class = "text-muted small ms-2",
                     "\u2014 coeficientes \u03b2 \u00b7 lmerTest / parameters")
              ),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("tabla_efectos_fijos"))
              )
            ),
            card(
              card_header(
                bs_icon("diagram-3", class = "me-1"),
                "Efectos aleatorios",
                span(class = "text-muted small ms-2",
                     "\u2014 varianzas \u03c3\u00b2 \u00b7 lme4")
              ),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("tabla_efectos_aleatorios"))
              )
            )
          ),
          div(class = "mt-3",
            card(
              card_header(
                bs_icon("bar-chart-steps", class = "me-1"),
                "Importancia de variables",
                span(class = "text-muted small ms-2",
                     "\u2014 coeficientes estandarizados \u00b7 parameters")
              ),
              card_body(
                style = "height: auto;",
                plotOutput(ns("plot_importancia"), height = "260px")
              )
            )
          )
        )
      ), # /PESTAÑA 7

      # ════════════════════════════════════════════════
      # PESTAÑA 9: Efectos marginales
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("graph-up-arrow", class = "me-1"),
                        "Efectos marginales"),
        div(
          class = "p-3",
          p(class = "small text-muted mb-3",
            "Visualizaci\u00f3n de los efectos fijos del modelo. ",
            "Las predicciones se muestran promediando sobre los efectos aleatorios ",
            "(efectos marginales poblacionales). ",
            "Generado con ", strong("modelbased::estimate_relation()"), "."),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("sel_pred_efecto")),
                checkboxInput(ns("mostrar_datos_efecto"),
                              "Mostrar datos observados",
                              value = TRUE),
                checkboxInput(ns("mostrar_grupos_efecto"),
                              "Colorear por grupo",
                              value = TRUE),
                tags$hr(),
                p(class = "small fw-bold text-muted mb-1",
                  "Predicci\u00f3n puntual"),
                uiOutput(ns("inputs_prediccion")),
                actionButton(ns("calcular_prediccion"),
                             "Calcular",
                             class = "btn-primary w-100 btn-sm mt-2",
                             icon  = icon("calculator"))
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("graph-up-arrow", class = "me-1"),
                            "Efecto marginal"),
                card_body(
                  style = "height: auto;",
                  plotOutput(ns("plot_efecto"), height = "340px")
                )
              ),
              uiOutput(ns("resultado_prediccion"))
            )
          )
        )
      ), # /PESTAÑA 8

      # ════════════════════════════════════════════════
      # PESTAÑA 10: Contrastes
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("arrows-angle-expand", class = "me-1"),
                        "Contrastes"),
        div(
          class = "p-3",
          p(class = "small text-muted mb-3",
            "Comparaci\u00f3n entre niveles de predictores categ\u00f3ricos, ",
            "controlando por el resto del modelo. ",
            "Generado con ", strong("modelbased::estimate_contrasts()"), "."),
          uiOutput(ns("contrasts_no_cat_msg")),
          layout_columns(
            col_widths = c(4, 8),
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_var_contraste")),
                tags$hr(),
                selectInput(
                  ns("metodo_ajuste"),
                  label   = "Ajuste de p-valores:",
                  choices = c("Sin ajuste" = "none",
                              "Bonferroni"  = "bonferroni",
                              "Holm"        = "holm",
                              "FDR (BH)"    = "fdr"),
                  selected = "none"
                )
              )
            ),
            div(
              card(class = "mb-3",
                card_header(bs_icon("table", class = "me-1"),
                            "Tabla de contrastes"),
                card_body(uiOutput(ns("tabla_contrastes")))
              ),
              card(class = "mb-0",
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "Visualizaci\u00f3n"),
                card_body(
                  plotOutput(ns("plot_contrastes"), height = "280px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 11

      # ════════════════════════════════════════════════
      # PESTAÑA 11: Comparar modelos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("arrow-left-right", class = "me-1"),
                        "Comparar modelos"),
        div(
          class = "p-3",
          p(class = "small text-muted mb-3",
            "Ajusta distintos modelos en ", strong("Ajustar modelo"),
            ", gu\u00e1rdalos con nombre y comp\u00e1ralos. ",
            "Para comparar modelos con distintos efectos fijos usa ",
            strong("ML"), "; para distintos efectos aleatorios usa ",
            strong("REML"), "."),
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
              card(
                class = "mb-3",
                card_header(bs_icon("table", class = "me-1"),
                            "Tabla comparativa"),
                card_body(
                  style = "overflow: visible; height: auto;",
                  uiOutput(ns("tabla_comparacion"))
                )
              ),
              card(
                class = "mb-0",
                card_header(bs_icon("diagram-3", class = "me-1"),
                            "Gr\u00e1fico radar"),
                card_body(
                  style = "height: auto;",
                  plotOutput(ns("plot_comparacion"), height = "300px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 10

      # ════════════════════════════════════════════════
      # PESTAÑA 12: Código R
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"),
                        "C\u00f3digo R"),
        card_body(
          p(class = "text-muted small mb-3",
            "C\u00f3digo R reproducible para el modelo ajustado."),
          layout_columns(
            col_widths = c(10, 2),
            verbatimTextOutput(ns("codigo_r")),
            div(
              downloadButton(ns("descargar_codigo"), "Descargar .R",
                             class = "btn-outline-secondary btn-sm w-100")
            )
          )
        )
      ) # /PESTAÑA 12

    ) # /navset_card_tab
  ) # /tagList
} # /mod_lmm_ui


# ── SERVER ──────────────────────────────────────────────────
mod_lmm_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ────────────────────────────────────────────────────
    # DATOS
    # ────────────────────────────────────────────────────

    output$sel_fuente_datos <- renderUI({
      radioButtons(
        ns("fuente_datos"),
        label   = tagList(bs_icon("database", class = "me-1"),
                          "Dataset de ejemplo:"),
        choices = c(
          "Riqueza en playas \u2014 RIKZ (ecolog\u00eda)"  = "rikz",
          "Privaci\u00f3n de sue\u00f1o \u2014 sleepstudy (psicolog\u00eda)" = "sleepstudy",
          "Cargar mis propios datos"                  = "propio"
        ),
        selected = "rikz"
      )
    })

    datos_activos <- reactive({
      fuente <- input$fuente_datos
      req(!is.null(fuente) && nchar(fuente) > 0)
      tu <- tipos_usuario()

      df <- if (fuente == "rikz") {
        tryCatch({
          e <- new.env()
          load(system.file("app/data/rikz_lmm.rda",
                           package = "StatModels"), envir = e)
          e$rikz_lmm
        }, error = function(err) {
          showNotification("Archivo rikz_lmm.rda no encontrado.",
                           type = "error", duration = 6)
          NULL
        })
      } else if (fuente == "sleepstudy") {
        tryCatch({
          df <- as.data.frame(lme4::sleepstudy)
          df$Subject <- as.factor(df$Subject)
          df
        }, error = function(err) {
          showNotification("lme4::sleepstudy no disponible.",
                           type = "error", duration = 6)
          NULL
        })
      } else {
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
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error", duration = 6)
          NULL
        })
      }

      req(df)
      # Aplicar tipos de usuario
      if (!is.null(tu)) {
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

    # ── Tipos de variables ────────────────────────────────
    tipos_usuario <- reactiveVal(NULL)
    observeEvent(input$fuente_datos, { tipos_usuario(NULL) })
    observeEvent(input$resetear_tipos, {
      tipos_usuario(NULL)
      showNotification("Tipos restaurados.", type = "message", duration = 2)
    })
    observeEvent(input$aplicar_tipos, {
      df <- datos_activos(); req(df)
      nuevos <- lapply(names(df), function(nm) input[[paste0("tipo_", nm)]])
      names(nuevos) <- names(df)
      tipos_usuario(nuevos)
      showNotification("Tipos aplicados.", type = "message", duration = 2)
    })

    output$tabla_tipos <- renderUI({
      df <- datos_activos(); req(df)
      tu <- tipos_usuario()
      filas <- lapply(names(df), function(nm) {
        col    <- df[[nm]]
        actual <- if (is.factor(col) || is.character(col)) "factor" else "numeric"
        icono  <- if (actual == "factor")
          bs_icon("tag-fill", style = paste0("color:", colores$acento))
        else bs_icon("123", style = paste0("color:", colores$primario))
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
                            if (actual == "factor") "Factor" else "Num\u00e9rico")),
          tags$td(style = "padding:5px 8px;",
                  selectInput(
                    inputId  = paste0(ns("tipo_"), nm),
                    label    = NULL,
                    choices  = c("Num\u00e9rico" = "numeric",
                                 "Factor" = "factor",
                                 "Excluir" = "excluir"),
                    selected = sel, width = "160px")),
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  if (!is.null(tu) && !is.null(tu[[nm]]) && tu[[nm]] != actual)
                    tags$span(class = "badge",
                              style = paste0("background:", colores$exito),
                              "Modificado")
                  else tags$span(class = "text-muted small", "Sin cambios"))
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
      n_exc <- sum(sapply(tu, function(t) !is.null(t) && t == "excluir"))
      if (n_exc == 0) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("check-circle-fill", class = "me-1"),
          paste0(n_exc, " variable(s) excluida(s)."))
    })

    # ── Info dataset ──────────────────────────────────────

    output$info_dataset <- renderUI({
      fuente <- input$fuente_datos
      if (is.null(fuente) || fuente == "rikz") {
        div(class = "alert alert-info small py-2 px-3 mb-2",
            bs_icon("info-circle-fill", class = "me-1"),
            strong("Dataset: RIKZ \u2014 Riqueza en playas (Janssen & Mulder, 2005)."),
            " 45 sitios en 9 playas de los Pa\u00edses Bajos. ",
            "Variables: Richness (riqueza), NAP (altura sobre nivel del mar), ",
            "Exposure (exposici\u00f3n), Beach (playa), Site (sitio).")
      } else if (fuente == "sleepstudy") {
        div(class = "alert alert-info small py-2 px-3 mb-2",
            bs_icon("info-circle-fill", class = "me-1"),
            strong("Dataset: sleepstudy (Belenky et al., 2003)."),
            " 180 observaciones, 18 sujetos \u00d7 10 d\u00edas. ",
            "Variables: Reaction (tiempo de reacci\u00f3n ms), Days (d\u00edas), ",
            "Subject (sujeto).")
      } else {
        div(class = "alert alert-info small py-2 px-3 mb-2",
            "Datos cargados por el usuario.")
      }
    })

    output$resumen_datos <- renderUI({
      df <- datos_activos(); req(df)
      div(class = "small text-muted mt-2",
          bs_icon("check-circle-fill", class = "me-1",
                  style = paste0("color:", colores$exito)),
          paste0(nrow(df), " filas \u00b7 ", ncol(df), " columnas"))
    })

    output$cards_datos <- renderUI({
      df <- datos_activos(); req(df)
      nnum <- length(vars_numericas())
      ncat <- length(vars_categoricas())
      layout_columns(
        col_widths = c(4, 4, 4),
        card(class = "text-center border-0",
             style = paste0("background:", colores$fondo),
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$primario,
                                 "; font-weight:700;"), nrow(df)),
               p(class = "small text-muted mb-0", "Observaciones"))),
        card(class = "text-center border-0",
             style = paste0("background:", colores$fondo),
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$acento,
                                 "; font-weight:700;"), nnum),
               p(class = "small text-muted mb-0", "Num\u00e9ricas"))),
        card(class = "text-center border-0",
             style = paste0("background:", colores$fondo),
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$secundario,
                                 "; font-weight:700;"), ncat),
               p(class = "small text-muted mb-0", "Categ\u00f3ricas")))
      )
    })

    output$tabla_preview <- renderDT({
      df <- datos_activos(); req(df)
      datatable(head(df, 15),
                options = list(scrollX = TRUE, dom = "t", pageLength = 15),
                rownames = FALSE,
                class    = "table-sm table-striped")
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 4: Explorar
    # ────────────────────────────────────────────────────

    output$sel_var_y_exp <- renderUI({
      req(vars_numericas())
      selectInput(ns("var_y_exp"), "Variable Y:",
                  choices = vars_numericas(), selected = vars_numericas()[1])
    })
    output$sel_var_x_exp <- renderUI({
      nums <- vars_numericas(); req(nums, input$var_y_exp)
      opts <- nums[nums != input$var_y_exp]
      req(length(opts) > 0)
      selectInput(ns("var_x_exp"), "Variable X:",
                  choices = opts, selected = opts[1])
    })
    output$sel_grupo_exp <- renderUI({
      cats <- vars_categoricas()
      if (length(cats) == 0) return(
        p(class = "small text-muted",
          "No hay variables categ\u00f3ricas. Convierta la variable de grupo en la pesta\u00f1a Tipos.")
      )
      selectInput(ns("grupo_exp"), "Variable de grupo:",
                  choices = cats, selected = cats[1])
    })

    output$plot_spaghetti <- renderPlot({
      df <- datos_activos()
      req(df, input$var_y_exp, input$var_x_exp, input$grupo_exp)
      tryCatch({
        p <- ggplot2::ggplot(df,
               ggplot2::aes(x = .data[[input$var_x_exp]],
                            y = .data[[input$var_y_exp]],
                            color = .data[[input$grupo_exp]],
                            group = .data[[input$grupo_exp]])) +
          ggplot2::geom_point(alpha = 0.5, size = 2)

        if (isTRUE(input$mostrar_grupos))
          p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                                         se = FALSE, linewidth = 0.8, alpha = 0.7)
        if (isTRUE(input$mostrar_global))
          p <- p + ggplot2::geom_smooth(
            data = df,
            ggplot2::aes(x = .data[[input$var_x_exp]],
                         y = .data[[input$var_y_exp]]),
            method = "lm", formula = y ~ x, se = TRUE,
            color = "black", linewidth = 1.3, linetype = "dashed",
            inherit.aes = FALSE)

        p + ggplot2::labs(
              x        = input$var_x_exp,
              y        = input$var_y_exp,
              color    = input$grupo_exp,
              subtitle = "L\u00edneas por grupo (color) \u00b7 L\u00ednea global (negro punteado)"
            ) +
          ggplot2::scale_color_manual(values = colores$tableau) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(panel.grid.minor  = ggplot2::element_blank(),
                         legend.position   = "bottom",
                         plot.subtitle     = ggplot2::element_text(
                           color = colores$texto, size = 9))
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label = "Selecciona variables para visualizar.",
                            color = colores$texto, size=4) +
          ggplot2::theme_void()
      })
    }, res = 96)

    output$insight_estructura <- renderUI({
      df <- datos_activos()
      req(df, input$grupo_exp, input$var_y_exp)
      tryCatch({
        grupos <- unique(df[[input$grupo_exp]])
        n_grupos <- length(grupos)
        n_obs <- nrow(df)
        div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
            bs_icon("lightbulb-fill", class = "me-1"),
            strong(n_grupos, " grupos"), " (", input$grupo_exp, ") con ",
            strong(round(n_obs / n_grupos, 1)), " observaciones en promedio. ",
            "Si las l\u00edneas de grupo tienen pendientes e interceptos distintos, ",
            "el modelo mixto es adecuado.")
      }, error = function(e) NULL)
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 5: Ajustar modelo
    # ────────────────────────────────────────────────────

    output$sel_var_y <- renderUI({
      req(vars_numericas())
      selectInput(ns("var_y"), "Variable respuesta (Y):",
                  choices = vars_numericas(), selected = vars_numericas()[1])
    })

    output$sel_familia <- renderUI({
      selectInput(ns("familia"), "Familia (distribuci\u00f3n):",
                  choices = c(
                    "Gaussian \u2014 lmer() (continua)" = "gaussian",
                    "Poisson \u2014 glmer() (conteos)"  = "poisson",
                    "Binomial \u2014 glmer() (0/1)"     = "binomial"
                  ),
                  selected = "gaussian")
    })

    output$sel_efectos_fijos <- renderUI({
      df <- datos_activos(); req(df, input$var_y)
      opts <- names(df)[names(df) != input$var_y]
      checkboxGroupInput(ns("efectos_fijos"), label = NULL,
                         choices = opts,
                         selected = opts[1])
    })

    output$sel_grupo <- renderUI({
      cats <- vars_categoricas()
      if (length(cats) == 0) return(
        div(class = "alert alert-warning small py-2 px-3",
            "No hay variables categ\u00f3ricas. Convierte la variable de grupo ",
            "en la pesta\u00f1a Tipos de variables.")
      )
      selectInput(ns("var_grupo"), "Variable de agrupamiento:",
                  choices = cats, selected = cats[length(cats)])
    })

    output$sel_pendiente_var <- renderUI({
      nums <- vars_numericas(); req(nums, input$var_y)
      opts <- nums[nums != input$var_y]
      req(length(opts) > 0)
      selectInput(ns("pendiente_var"),
                  "Predictor con pendiente aleatoria:",
                  choices = opts, selected = opts[1])
    })

    output$sel_grupo_b <- renderUI({
      cats <- vars_categoricas(); req(cats, input$var_grupo)
      opts <- cats[cats != input$var_grupo]
      req(length(opts) > 0)
      selectInput(ns("var_grupo_b"), "Sub-grupo (B anidado en A):",
                  choices = opts, selected = opts[1])
    })

    # ── Construcción de la fórmula de efectos aleatorios ──

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

    # ── Ajuste del modelo ─────────────────────────────────

    modelo_lmm <- eventReactive(input$ajustar, {
      df <- datos_activos(); req(df, input$var_y, input$efectos_fijos)
      fijos <- input$efectos_fijos
      re    <- tryCatch(formula_re(), error = function(e) NULL)
      req(re)

      fm_txt <- paste(
        input$var_y, "~",
        paste(fijos, collapse = " + "),
        "+", re
      )
      fm <- tryCatch(as.formula(fm_txt), error = function(e) NULL)
      req(fm)

      withProgress(message = "Ajustando modelo mixto\u2026", value = 0.5, {
        tryCatch({
          fit <- if (input$familia == "gaussian") {
            lme4::lmer(fm, data = df, REML = (input$metodo_lmm == "REML"))
          } else if (input$familia == "poisson") {
            lme4::glmer(fm, data = df, family = poisson(link = "log"))
          } else {
            lme4::glmer(fm, data = df, family = binomial(link = "logit"))
          }
          incProgress(0.5)
          fit
        }, error = function(e) {
          showNotification(paste("Error al ajustar:", conditionMessage(e)),
                           type = "error", duration = 6)
          NULL
        })
      })
    }, ignoreNULL = TRUE)

    # ── Aviso singular fit ────────────────────────────────

    output$aviso_singular <- renderUI({
      fm <- modelo_lmm(); req(fm)
      if (lme4::isSingular(fm)) {
        div(class = "alert alert-warning small py-2 px-3 mb-2",
            bs_icon("exclamation-triangle-fill", class = "me-1"),
            strong("Singular fit"), " \u2014 la varianza de alg\u00fan efecto aleatorio ",
            "se estim\u00f3 en 0 o la correlaci\u00f3n es \u00b11. ",
            "Considera simplificar la estructura aleatoria.")
      }
    })

    output$formula_ajustada <- renderUI({
      fm <- modelo_lmm(); req(fm)
      fm_str <- paste(deparse(formula(fm)), collapse = " ")
      div(class = "alert alert-secondary small py-2 px-3 mb-0",
          code(fm_str))
    })

    output$cards_metricas_lmm <- renderUI({
      fm <- modelo_lmm()
      if (is.null(fm)) return(
        div(class = "alert alert-info small py-2 px-3",
            bs_icon("play-circle", class = "me-1"),
            "Ajusta el modelo para ver las m\u00e9tricas.")
      )
      tryCatch({
        pm   <- performance::model_performance(fm, verbose = FALSE)
        r2   <- tryCatch(performance::r2_nakagawa(fm, verbose = FALSE),
                         error = function(e) NULL)
        r2m  <- if (!is.null(r2)) round(r2$R2_marginal, 3) else NA
        r2c  <- if (!is.null(r2)) round(r2$R2_conditional, 3) else NA
        icc_ <- tryCatch(round(performance::icc(fm, verbose = FALSE)$ICC_adjusted, 3),
                         error = function(e) NA)

        layout_columns(
          col_widths = c(6, 6),
          card(class = "text-center border-0",
               style = paste0("background:", colores$fondo),
               card_body(class = "p-2",
                 h3(style = paste0("color:", colores$primario,
                                   "; font-weight:700;"),
                    if (!is.na(r2m)) r2m else "\u2014"),
                 p(class = "small text-muted mb-0", "R\u00b2 marginal"))),
          card(class = "text-center border-0",
               style = paste0("background:", colores$fondo),
               card_body(class = "p-2",
                 h3(style = paste0("color:", colores$acento,
                                   "; font-weight:700;"),
                    if (!is.na(r2c)) r2c else "\u2014"),
                 p(class = "small text-muted mb-0", "R\u00b2 condicional")))
        )
      }, error = function(e) {
        div(class = "text-muted small", "Ajusta el modelo primero.")
      })
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 6: Diagnóstico
    # ────────────────────────────────────────────────────

    output$plot_diagnostico <- renderPlot({
      fm <- modelo_lmm(); req(fm)
      tryCatch({
        p <- performance::check_model(fm, verbose = FALSE)
        plot(p)
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label = "Diagn\u00f3stico no disponible.",
                            color = colores$texto, size=4) +
          ggplot2::theme_void()
      })
    }, res = 96)

    output$plot_ranef <- renderPlot({
      fm <- modelo_lmm(); req(fm)
      tryCatch({
        re <- lme4::ranef(fm, condVar = TRUE)
        lattice::dotplot(re)[[1]]
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label = "Caterpillar plot no disponible.",
                            color = colores$texto, size=4) +
          ggplot2::theme_void()
      })
    }, res = 96)

    output$tabla_supuestos <- renderUI({
      fm <- modelo_lmm(); req(fm)
      tryCatch({
        sing <- lme4::isSingular(fm)
        filas <- list(
          list(s = "Singular fit",
               v = if (sing) "\u26a0 S\u00ed" else "\u2713 No",
               c = if (sing) colores$peligro else colores$exito,
               i = "Varianza de efecto aleatorio \u2248 0."),
          list(s = "N\u00famero de grupos",
               v = length(unique(lme4::getME(fm, "flist")[[1]])),
               c = colores$texto,
               i = "Se recomiendan \u226510 grupos.")
        )
        tags$table(
          class = "table table-sm small mb-0",
          tags$tbody(lapply(filas, function(f) {
            tags$tr(
              tags$td(strong(f$s)),
              tags$td(style = paste0("color:", f$c, "; font-weight:600;"), f$v),
              tags$td(class = "text-muted small", f$i)
            )
          }))
        )
      }, error = function(e) NULL)
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 7: Performance
    # ────────────────────────────────────────────────────

    output$tabla_performance <- renderUI({
      fm <- modelo_lmm(); req(fm)
      tryCatch({
        pm  <- performance::model_performance(fm, verbose = FALSE)
        r2  <- tryCatch(performance::r2_nakagawa(fm, verbose = FALSE),
                        error = function(e) NULL)
        r2m <- if (!is.null(r2)) round(r2$R2_marginal, 4) else NA
        r2c <- if (!is.null(r2)) round(r2$R2_conditional, 4) else NA
        n   <- nrow(fm@frame)

        filas <- list(
          list(m = "n (observaciones)", v = n,
               i = "Tama\u00f1o de la muestra."),
          list(m = "R\u00b2 marginal",
               v = if (!is.na(r2m)) r2m else "\u2014",
               i = "Varianza explicada por efectos fijos."),
          list(m = "R\u00b2 condicional",
               v = if (!is.na(r2c)) r2c else "\u2014",
               i = "Varianza explicada por efectos fijos + aleatorios."),
          list(m = "AIC",
               v = round(pm$AIC, 2),
               i = "Criterio de Akaike. Menor = mejor."),
          list(m = "BIC",
               v = round(pm$BIC, 2),
               i = "Criterio Bayesiano."),
          list(m = "Singular fit",
               v = if (lme4::isSingular(fm)) "\u26a0 S\u00ed" else "\u2713 No",
               i = "Sobreparametrizaci\u00f3n de efectos aleatorios.")
        )
        tags$table(
          class = "table table-sm table-hover small mb-0",
          tags$thead(tags$tr(
            tags$th("M\u00e9trica"), tags$th("Valor"),
            tags$th("Interpretaci\u00f3n")
          )),
          tags$tbody(lapply(filas, function(f) {
            tags$tr(
              tags$td(strong(f$m)),
              tags$td(f$v),
              tags$td(class = "text-muted small", f$i)
            )
          }))
        )
      }, error = function(e) {
        div(class="text-muted small", "Ajusta el modelo primero.")
      })
    })

    output$tabla_icc <- renderUI({
      fm <- modelo_lmm(); req(fm)
      tryCatch({
        icc_ <- performance::icc(fm, verbose = FALSE)
        layout_columns(
          col_widths = c(6, 6),
          card(class = "text-center border-0",
               style = paste0("background:", colores$fondo),
               card_body(class = "p-2",
                 h3(style = paste0("color:", colores$primario,
                                   "; font-weight:700;"),
                    round(icc_$ICC_adjusted, 3)),
                 p(class = "small text-muted mb-0", "ICC ajustado"))),
          card(class = "text-center border-0",
               style = paste0("background:", colores$fondo),
               card_body(class = "p-2",
                 h3(style = paste0("color:", colores$acento,
                                   "; font-weight:700;"),
                    round(icc_$ICC_unadjusted, 3)),
                 p(class = "small text-muted mb-0", "ICC no ajustado")))
        )
      }, error = function(e) {
        p(class = "small text-muted", "ICC no disponible.")
      })
    })

    output$plot_icc <- renderPlot({
      fm <- modelo_lmm(); req(fm)
      tryCatch({
        r2  <- performance::r2_nakagawa(fm, verbose = FALSE)
        r2m <- r2$R2_marginal
        r2c <- r2$R2_conditional
        icc_ <- r2c - r2m
        res  <- 1 - r2c

        df_pie <- data.frame(
          componente = c("Efectos fijos", "Efectos aleatorios", "Residual"),
          valor      = c(r2m, icc_, res)
        )
        df_pie$componente <- factor(df_pie$componente,
                                     levels = df_pie$componente)

        ggplot2::ggplot(df_pie,
                        ggplot2::aes(x = 2, y = valor,
                                     fill = componente)) +
          ggplot2::geom_col(width = 1) +
          ggplot2::coord_polar(theta = "y") +
          ggplot2::xlim(0.5, 2.5) +
          ggplot2::scale_fill_manual(
            values = c(colores$primario, colores$acento, "#CCCCCC")) +
          ggplot2::labs(fill = "Componente",
                        subtitle = "Descomposici\u00f3n de la varianza") +
          ggplot2::theme_void(base_size = 11) +
          ggplot2::theme(legend.position = "right",
                         plot.subtitle = ggplot2::element_text(
                           color = colores$texto, size = 9))
      }, error = function(e) {
        ggplot2::ggplot() + ggplot2::theme_void()
      })
    }, res = 96)

    # ────────────────────────────────────────────────────
    # PESTAÑA 8: Parámetros
    # ────────────────────────────────────────────────────

    output$tabla_efectos_fijos <- renderUI({
      fm <- modelo_lmm(); req(fm)
      tryCatch({
        mp  <- parameters::model_parameters(fm, verbose = FALSE)
        df  <- as.data.frame(mp)
        filas <- lapply(seq_len(nrow(df)), function(i) {
          pval <- df$p[i]
          p_txt <- if (!is.na(pval)) {
            if (pval < 0.001) "< 0.001 ***"
            else if (pval < 0.01)  paste0(round(pval,3), " **")
            else if (pval < 0.05)  paste0(round(pval,3), " *")
            else round(pval, 3)
          } else "\u2014"
          col_p <- if (!is.na(pval) && pval < 0.05) colores$exito else colores$texto
          tags$tr(
            tags$td(strong(df$Parameter[i])),
            tags$td(style="text-align:center;", round(df$Coefficient[i],3)),
            tags$td(style="text-align:center;", round(df$SE[i],3)),
            tags$td(style="text-align:center;",
                    paste0("[",round(df$CI_low[i],3),", ",
                           round(df$CI_high[i],3),"]")),
            tags$td(style=paste0("color:",col_p,";font-weight:600;",
                                 "text-align:center;"), p_txt)
          )
        })
        tags$table(
          class = "table table-sm table-hover small mb-0",
          tags$thead(tags$tr(
            tags$th("Par\u00e1metro"), tags$th("Estimado"),
            tags$th("EE"), tags$th("IC 95%"), tags$th("p-valor")
          )),
          tags$tbody(filas)
        )
      }, error = function(e) {
        p(class = "small text-muted", "Ajusta el modelo primero.")
      })
    })

    output$tabla_efectos_aleatorios <- renderUI({
      fm <- modelo_lmm(); req(fm)
      tryCatch({
        vc  <- as.data.frame(lme4::VarCorr(fm))
        filas <- lapply(seq_len(nrow(vc)), function(i) {
          tags$tr(
            tags$td(code(vc$grp[i])),
            tags$td(if (!is.na(vc$var1[i])) vc$var1[i] else "\u2014"),
            tags$td(style="text-align:center;", round(vc$vcov[i],3)),
            tags$td(style="text-align:center;", round(vc$sdcor[i],3))
          )
        })
        tags$table(
          class = "table table-sm small mb-0",
          tags$thead(tags$tr(
            tags$th("Grupo"), tags$th("Variable"),
            tags$th("Varianza (\u03c3\u00b2)"), tags$th("SD / Cor")
          )),
          tags$tbody(filas)
        )
      }, error = function(e) {
        p(class = "small text-muted", "Ajusta el modelo primero.")
      })
    })

    output$plot_importancia <- renderPlot({
      fm <- modelo_lmm(); req(fm)
      tryCatch({
        mp <- parameters::model_parameters(fm, standardize = "refit",
                                           verbose = FALSE)
        df <- as.data.frame(mp)
        df <- df[!grepl("Intercept", df$Parameter), ]
        df$abs_est <- abs(df$Coefficient)
        df$dir     <- ifelse(df$Coefficient >= 0, "Positivo", "Negativo")
        df$Parameter <- factor(df$Parameter,
                                levels = df$Parameter[order(df$abs_est)])
        ggplot2::ggplot(df,
                        ggplot2::aes(x = abs_est, y = Parameter, fill = dir)) +
          ggplot2::geom_col(width = 0.65) +
          ggplot2::scale_fill_manual(
            values = c("Positivo" = colores$primario,
                       "Negativo" = colores$peligro),
            name = "Direcci\u00f3n") +
          ggplot2::labs(x = "|\u03b2 estandarizado|", y = NULL,
                        subtitle = "Efectos fijos estandarizados") +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(panel.grid.minor   = ggplot2::element_blank(),
                         panel.grid.major.y = ggplot2::element_blank(),
                         legend.position    = "bottom")
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="Ajusta el modelo primero.",
                            color=colores$texto, size=4) +
          ggplot2::theme_void()
      })
    }, res = 96)

    # ────────────────────────────────────────────────────
    # PESTAÑA 9: Efectos marginales
    # ────────────────────────────────────────────────────

    output$sel_pred_efecto <- renderUI({
      req(input$efectos_fijos)
      selectInput(ns("pred_efecto"),
                  "Predictor a visualizar:",
                  choices  = input$efectos_fijos,
                  selected = input$efectos_fijos[1])
    })

    output$plot_efecto <- renderPlot({
      fm <- modelo_lmm(); req(fm, input$pred_efecto)
      df <- datos_activos(); req(df)
      tryCatch({
        rel <- modelbased::estimate_relation(fm,
                 by = input$pred_efecto, verbose = FALSE)
        df_rel <- as.data.frame(rel)

        p <- ggplot2::ggplot(df_rel,
               ggplot2::aes(x = .data[[input$pred_efecto]],
                            y = Predicted)) +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = CI_low, ymax = CI_high),
                               fill = colores$primario, alpha = 0.15) +
          ggplot2::geom_line(color = colores$primario, linewidth = 1.2)

        if (isTRUE(input$mostrar_datos_efecto)) {
          if (isTRUE(input$mostrar_grupos_efecto) &&
              !is.null(input$var_grupo) && input$var_grupo %in% names(df)) {
            p <- p + ggplot2::geom_point(
              data = df,
              ggplot2::aes(x    = .data[[input$pred_efecto]],
                           y    = .data[[input$var_y]],
                           color = .data[[input$var_grupo]]),
              alpha = 0.5, size = 2, inherit.aes = FALSE) +
              ggplot2::scale_color_manual(values = colores$tableau,
                                          name = input$var_grupo)
          } else {
            p <- p + ggplot2::geom_point(
              data = df,
              ggplot2::aes(x = .data[[input$pred_efecto]],
                           y = .data[[input$var_y]]),
              color = colores$primario, alpha = 0.4, size = 2,
              inherit.aes = FALSE)
          }
        }

        p + ggplot2::labs(x = input$pred_efecto, y = input$var_y,
                          subtitle = "Efecto marginal (promediando efectos aleatorios) \u00b7 IC 95%") +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         legend.position  = "bottom",
                         plot.subtitle    = ggplot2::element_text(
                           color = colores$texto, size = 9))
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="Ajusta el modelo primero.",
                            color=colores$texto, size=4) +
          ggplot2::theme_void()
      })
    }, res = 96)

    output$inputs_prediccion <- renderUI({
      fm <- modelo_lmm(); req(fm)
      df <- datos_activos()
      preds <- input$efectos_fijos; req(length(preds) > 0)
      inputs <- lapply(preds, function(nm) {
        col <- df[[nm]]
        if (is.numeric(col))
          numericInput(ns(paste0("pv_", nm)),
                       paste0(nm, " (media=", round(mean(col,na.rm=T),1), "):"),
                       value = round(mean(col, na.rm=T), 1))
        else
          selectInput(ns(paste0("pv_", nm)), nm,
                      choices = levels(col), selected = levels(col)[1])
      })
      do.call(tagList, inputs)
    })

    resultado_pred <- eventReactive(input$calcular_prediccion, {
      fm <- modelo_lmm(); req(fm)
      df <- datos_activos()
      preds <- input$efectos_fijos; req(length(preds) > 0)
      tryCatch({
        vals <- lapply(preds, function(nm) {
          col <- df[[nm]]
          val <- input[[paste0("pv_", nm)]]
          if (is.numeric(col)) as.numeric(val)
          else factor(val, levels = levels(col))
        })
        names(vals) <- preds
        nueva_obs <- as.data.frame(vals)
        modelbased::estimate_expectation(fm, data = nueva_obs,
                                         verbose = FALSE)
      }, error = function(e) NULL)
    }, ignoreNULL = TRUE)

    output$resultado_prediccion <- renderUI({
      res <- resultado_pred(); if (is.null(res)) return(NULL)
      df_res <- as.data.frame(res)
      card(class = "mt-2",
           card_header(bs_icon("bullseye", class = "me-1"), "Resultado"),
           card_body(
             div(class = "text-center py-2",
                 h3(style = paste0("color:", colores$primario,
                                   "; font-weight:700;"),
                    round(df_res$Predicted[1], 3)),
                 p(class = "text-muted mb-1",
                   strong(paste0(input$var_y, " predicho"))),
                 p(class = "small text-muted",
                   "IC 95%: ", strong(paste0("[",
                     round(df_res$CI_low[1],3), ", ",
                     round(df_res$CI_high[1],3), "]")))
             )
           ))
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 10: Contrastes
    # ────────────────────────────────────────────────────

    output$contrasts_no_cat_msg <- renderUI({
      fijos <- input$efectos_fijos
      cats  <- vars_categoricas()
      if (is.null(fijos) || !any(fijos %in% cats))
        div(class = "alert alert-warning small py-2 px-3 mb-3",
            bs_icon("exclamation-triangle-fill", class = "me-1"),
            "El modelo no tiene predictores categ\u00f3ricos en efectos fijos.")
    })

    output$sel_var_contraste <- renderUI({
      fm   <- modelo_lmm(); req(fm)
      cats <- vars_categoricas()
      fijos <- input$efectos_fijos
      cats <- cats[cats %in% fijos]
      req(length(cats) > 0)
      selectInput(ns("var_contraste"),
                  "Variable para contrastar:",
                  choices = cats, selected = cats[1])
    })

    output$tabla_contrastes <- renderUI({
      fm <- modelo_lmm(); req(fm, input$var_contraste)
      tryCatch({
        ct    <- modelbased::estimate_contrasts(
          fm, contrast = input$var_contraste,
          p_adjust = input$metodo_ajuste, verbose = FALSE)
        df_ct <- as.data.frame(ct)
        char_cols <- names(df_ct)[sapply(df_ct, function(x)
          is.character(x) || is.factor(x))]
        etiqueta <- if (length(char_cols) >= 2)
          paste0(df_ct[[char_cols[1]]], " vs. ", df_ct[[char_cols[2]]])
        else paste0("Contraste ", seq_len(nrow(df_ct)))

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
          } else "\u2014"
          col_p <- if (sig) colores$exito else colores$texto
          tags$tr(
            tags$td(strong(etiqueta[i])),
            tags$td(style="text-align:center;",
                    round(df_ct[[diff_col]][i], 3)),
            tags$td(style="text-align:center;",
                    if (!is.na(ci_lo))
                      paste0("[",round(df_ct[[ci_lo]][i],3),", ",
                             round(df_ct[[ci_hi]][i],3),"]")
                    else "\u2014"),
            tags$td(style=paste0("text-align:center;color:",col_p,
                                 ";font-weight:600;"), p_txt)
          )
        })
        tags$table(
          class = "table table-sm table-hover small mb-0",
          tags$thead(tags$tr(
            tags$th("Contraste"),
            tags$th(style="text-align:center;",
                    paste0("Diferencia (", input$var_y, ")")),
            tags$th(style="text-align:center;", "IC 95%"),
            tags$th(style="text-align:center;", "p-valor")
          )),
          tags$tbody(filas)
        )
      }, error = function(e) {
        div(class="text-muted small py-3",
            "Ajusta el modelo con predictores categ\u00f3ricos en efectos fijos.")
      })
    })

    output$plot_contrastes <- renderPlot({
      fm <- modelo_lmm(); req(fm, input$var_contraste)
      tryCatch({
        ct    <- modelbased::estimate_contrasts(
          fm, contrast = input$var_contraste,
          p_adjust = input$metodo_ajuste, verbose = FALSE)
        df_ct <- as.data.frame(ct)
        char_cols <- names(df_ct)[sapply(df_ct,
                       function(x) is.character(x) || is.factor(x))]
        etiqueta <- if (length(char_cols) >= 2)
          paste0(df_ct[[char_cols[1]]], " vs. ", df_ct[[char_cols[2]]])
        else paste0("Contraste ", seq_len(nrow(df_ct)))

        diff_col <- if ("Difference" %in% names(df_ct)) "Difference"
          else names(df_ct)[sapply(df_ct, is.numeric)][1]
        ci_lo <- if ("CI_low" %in% names(df_ct)) df_ct$CI_low
          else df_ct[[diff_col]] - 1
        ci_hi <- if ("CI_high" %in% names(df_ct)) df_ct$CI_high
          else df_ct[[diff_col]] + 1
        p_vals <- if ("p" %in% names(df_ct)) df_ct$p
          else if ("p.value" %in% names(df_ct)) df_ct$p.value
          else rep(0.5, nrow(df_ct))

        df_plot <- data.frame(
          contraste = factor(etiqueta, levels = rev(unique(etiqueta))),
          diff = df_ct[[diff_col]],
          lo = ci_lo, hi = ci_hi,
          sig = !is.na(p_vals) & p_vals < 0.05
        )
        ggplot2::ggplot(df_plot,
                        ggplot2::aes(x=diff, y=contraste,
                                     xmin=lo, xmax=hi, color=sig)) +
          ggplot2::geom_vline(xintercept=0, linetype="dashed",
                              color=colores$texto, linewidth=0.7) +
          ggplot2::geom_errorbar(width=0.25, linewidth=1.1) +
          ggplot2::geom_point(size=3.5) +
          ggplot2::scale_color_manual(
            values=c(`TRUE`=colores$acento, `FALSE`=colores$primario),
            labels=c(`TRUE`="Significativo", `FALSE`="No sig."),
            name=NULL) +
          ggplot2::labs(x=paste0("Diferencia en ", input$var_y),
                        y=NULL,
                        subtitle=paste0("Ajuste: ", input$metodo_ajuste,
                                        " \u00b7 IC 95%")) +
          ggplot2::theme_minimal(base_size=12) +
          ggplot2::theme(
            panel.grid.minor   = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            legend.position    = "bottom",
            plot.subtitle      = ggplot2::element_text(
              color=colores$texto, size=9))
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="Sin contrastes disponibles.",
                            color=colores$texto, size=4) +
          ggplot2::theme_void()
      })
    }, res = 96)

    # ────────────────────────────────────────────────────
    # PESTAÑA 11: Comparar modelos
    # ────────────────────────────────────────────────────

    modelos_guardados <- reactiveVal(list())

    observeEvent(input$guardar_modelo, {
      fm     <- modelo_lmm()
      nombre <- trimws(input$nombre_modelo)
      if (is.null(fm)) {
        showNotification("Ajusta un modelo primero.",
                         type = "warning", duration = 3); return()
      }
      if (nchar(nombre) == 0) {
        showNotification("Escribe un nombre.",
                         type = "warning", duration = 3); return()
      }
      lst <- modelos_guardados()
      lst[[nombre]] <- list(fit     = fm,
                            formula = paste(deparse(formula(fm)),
                                            collapse = " "))
      modelos_guardados(lst)
      updateTextInput(session, "nombre_modelo", value = "")
      showNotification(paste0("Modelo '", nombre, "' guardado."),
                       type = "message", duration = 3)
    })

    observeEvent(input$limpiar_modelos, {
      modelos_guardados(list())
      showNotification("Modelos eliminados.", type = "message", duration = 2)
    })

    output$lista_modelos_guardados <- renderUI({
      mg <- modelos_guardados()
      if (length(mg) == 0) return(
        p(class = "small text-muted", "A\u00fan no hay modelos guardados.")
      )
      tagList(lapply(names(mg), function(nm) {
        div(class = "d-flex align-items-center gap-2 mb-1",
            bs_icon("check-circle-fill",
                    style = paste0("color:", colores$exito)),
            div(p(class = "small mb-0", strong(nm)),
                p(class = "small text-muted mb-0",
                  style = "font-size:0.75rem;", mg[[nm]]$formula)))
      }))
    })

    output$tabla_comparacion <- renderUI({
      mg <- modelos_guardados()
      if (length(mg) < 1) return(
        div(class = "text-muted small py-3",
            "Guarda al menos un modelo para ver la comparaci\u00f3n.")
      )
      rows <- lapply(names(mg), function(nm) {
        fm  <- mg[[nm]]$fit
        pm  <- tryCatch(
          performance::model_performance(fm, verbose = FALSE),
          error = function(e) NULL)
        if (is.null(pm)) return(NULL)
        r2 <- tryCatch(performance::r2_nakagawa(fm, verbose = FALSE),
                       error = function(e) NULL)
        list(nm   = nm,
             aic  = round(pm$AIC, 1),
             bic  = round(pm$BIC, 1),
             r2m  = if (!is.null(r2)) round(r2$R2_marginal, 3) else NA,
             r2c  = if (!is.null(r2)) round(r2$R2_conditional, 3) else NA)
      })
      rows <- rows[!sapply(rows, is.null)]
      if (length(rows) == 0) return(NULL)
      best <- which.min(sapply(rows, function(r) r$aic))
      tags$table(
        class = "table table-sm table-hover small mb-0",
        tags$thead(
          style = paste0("background:", colores$primario,
                         "; color:#fff;"),
          tags$tr(tags$th("Modelo"), tags$th("AIC"), tags$th("BIC"),
                  tags$th("R\u00b2 marg."), tags$th("R\u00b2 cond."))
        ),
        tags$tbody(lapply(seq_along(rows), function(i) {
          r  <- rows[[i]]
          bg <- if (i == best) "background:#f0f9f5; font-weight:600;" else ""
          tags$tr(style = bg,
            tags$td(if (i == best)
              tagList(bs_icon("trophy-fill",
                              style = paste0("color:", colores$acento,
                                             "; margin-right:4px")), r$nm)
              else r$nm),
            tags$td(r$aic), tags$td(r$bic),
            tags$td(r$r2m), tags$td(r$r2c))
        }))
      )
    })

    output$plot_comparacion <- renderPlot({
      mg <- modelos_guardados(); req(length(mg) >= 2)
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
                        subtitle = "M\u00e9tricas normalizadas \u00b7 mayor \u00e1rea = mejor") +
          see::theme_radar() +
          ggplot2::theme(legend.position = "bottom",
                         plot.subtitle = ggplot2::element_text(
                           color = colores$texto, size = 9))
        print(p)
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="Guarda al menos 2 modelos.",
                            color=colores$texto, size=4) +
          ggplot2::theme_void()
      })
    }, res = 96)

    # ────────────────────────────────────────────────────
    # PESTAÑA 12: Código R
    # ────────────────────────────────────────────────────

    codigo_generado <- reactive({
      req(modelo_lmm())
      fm      <- modelo_lmm()
      fuente  <- input$fuente_datos
      familia <- input$familia
      metodo  <- input$metodo_lmm

      carga <- if (fuente == "rikz")
        paste0('load(system.file("app/data/rikz_lmm.rda",\n',
               '               package = "StatModels"))\n',
               'datos <- rikz_lmm\n')
      else if (fuente == "sleepstudy")
        'datos <- as.data.frame(lme4::sleepstudy)\n'
      else
        'datos <- read.csv("tu_archivo.csv")\n'

      fm_str <- paste(deparse(formula(fm)), collapse = " ")

      ajuste <- if (familia == "gaussian")
        paste0("lme4::lmer(\n  formula = ", fm_str, ",\n",
               "  data    = datos,\n",
               "  REML    = ", (metodo == "REML"), "\n)")
      else
        paste0("lme4::glmer(\n  formula = ", fm_str, ",\n",
               "  data    = datos,\n",
               "  family  = ", familia, "()\n)")

      paste0(
        "# ── Modelos mixtos con lme4 ─────────────────────────────\n",
        "library(lme4)\n",
        "library(lmerTest)  # p-valores para LMM\n",
        "library(parameters)  # easystats\n",
        "library(performance) # easystats\n\n",
        "# Cargar datos\n", carga, "\n",
        "# Ajustar modelo\n",
        "fm <- ", ajuste, "\n\n",
        "# Resumen\n",
        "summary(fm)\n\n",
        "# Parámetros (easystats)\n",
        "model_parameters(fm)\n\n",
        "# R² Nakagawa\n",
        "performance::r2_nakagawa(fm)\n\n",
        "# ICC\n",
        "performance::icc(fm)\n\n",
        "# Efectos aleatorios\n",
        "lme4::ranef(fm)\n\n",
        "# Diagnóstico\n",
        "performance::check_model(fm)\n"
      )
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descargar_codigo <- downloadHandler(
      filename = function() paste0("lmm_", Sys.Date(), ".R"),
      content  = function(file) writeLines(codigo_generado(), file)
    )

  }) # /moduleServer
} # /mod_lmm_server
