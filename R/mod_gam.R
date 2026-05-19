# ============================================================
# mod_gam.R — Modelo Aditivo Generalizado (GAM)
# StatModels · StatSuite
#
# Paquetes: mgcv, gratia, easystats
# Datos:    birdabundance_lm / birthwt_lm (o propios)
# ============================================================

# ── UI ──────────────────────────────────────────────────────
mod_gam_ui <- function(id) {
  ns <- NS(id)

  tagList(
  div(
    class = "px-1 pt-2 pb-2",
    layout_columns(
      col_widths = c(9, 3),
      div(
        h4(style = paste0("color:", colores$primario,
                          "; font-weight:700; margin-bottom:4px;"),
           bs_icon("bezier2", class = "me-2"),
           "Modelo Aditivo Generalizado (GAM)"),
        p(class = "text-muted small mb-0",
          "Extiende el modelo lineal reemplazando los efectos lineales por ",
          strong("funciones suaves no param\u00e9tricas"), " (splines). ",
          "Ideal cuando la relaci\u00f3n entre X e Y es no lineal \u2014 curva, ",
          "sinuosa o con un m\u00e1ximo/m\u00ednimo. El modelo aprende la forma ",
          "de la relaci\u00f3n directamente de los datos. Paquete: ",
          strong("mgcv"), " \u00b7 visualizaci\u00f3n con ", strong("gratia"),
          " y ", strong("easystats"), "."
        )
      ),
      div(
        class = "text-end pt-1",
        tags$span(
          class = "badge",
          style = paste0("background:", colores$primario,
                         "; font-size:0.8rem; padding:6px 12px;"),
          bs_icon("bezier2", class = "me-1"), "mgcv"
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

        # ── Intro ────────────────────────────────────
        h4(style = paste0("color:", colores$primario, "; font-weight:700;"),
           "Modelo Aditivo Generalizado (GAM)"),
        p(class = "text-muted small mb-3",
          "mgcv \u00b7 Wood (2017) \u00b7 easystats"),
        p("Un ", strong("GAM"), " extiende el modelo lineal reemplazando los ",
          "efectos lineales \u03b2X por ", strong("funciones suaves"), " s(X). ",
          "En lugar de asumir que la relaci\u00f3n entre X e Y es una l\u00ednea recta, ",
          "el GAM aprende la forma de esa relaci\u00f3n directamente de los datos."),

        # ── Fila 1: ¿Cuándo usar? + Anatomía fórmula ─
        layout_columns(
          col_widths = c(4, 8),
          fill = FALSE,

          card(
            card_header(
              bs_icon("question-circle", class = "me-1"),
              "\u00bfCu\u00e1ndo usar un GAM?"
            ),
            card_body(
              tags$ul(
                class = "small mb-2",
                tags$li("La relaci\u00f3n entre X e Y es ",
                        strong("no lineal"),
                        " (curva, sinuosa, con m\u00e1ximo/m\u00ednimo)"),
                tags$li("Quieres ", strong("descubrir la forma"),
                        " sin asumir una funci\u00f3n espec\u00edfica"),
                tags$li("El LM tiene ", strong("residuos con patrones"),
                        " sistem\u00e1ticos"),
                tags$li("Relaci\u00f3n especie-\u00e1rea, gradientes altitudinales, ",
                        "respuestas unimodales")
              ),
              div(class = "alert alert-info small py-2 px-3 mb-0",
                  bs_icon("lightbulb-fill", class = "me-1"),
                  "Si la relaci\u00f3n densidad-\u00e1rea es curva, el LM subestima ",
                  "en los extremos. El GAM captura esa curva autom\u00e1ticamente.")
            )
          ),

          card(
            card_header(
              bs_icon("code-slash", class = "me-1"),
              "Anatom\u00eda de la f\u00f3rmula"
            ),
            card_body(
              p(class = "small text-muted mb-2",
                "Cada elemento de la f\u00f3rmula tiene un significado espec\u00edfico:"),
              div(
                class = "alert alert-secondary small py-2 px-3 mb-3",
                style = "font-family: monospace; font-size: 0.9rem;",
                "Y ~ s(X\u2081, bs = \u0022tp\u0022, k = 10) + X\u2082"
              ),
              tags$table(
                class = "table table-sm small mb-2",
                tags$tbody(
                  tags$tr(
                    tags$td(code("Y")),
                    tags$td("Variable respuesta (continua)")
                  ),
                  tags$tr(
                    tags$td(code("s(...)")),
                    tags$td("Funci\u00f3n suave (spline) \u2014 la relaci\u00f3n se aprende de los datos")
                  ),
                  tags$tr(
                    tags$td(code("X\u2081")),
                    tags$td("Predictor modelado con spline (no lineal)")
                  ),
                  tags$tr(
                    tags$td(code("bs = \u0022tp\u0022")),
                    tags$td(tagList(strong("basis"), " \u2014 tipo de funci\u00f3n base del spline"))
                  ),
                  tags$tr(
                    tags$td(code("k = 10")),
                    tags$td("Knots m\u00e1ximos \u2014 techo de flexibilidad")
                  ),
                  tags$tr(
                    tags$td(code("X\u2082")),
                    tags$td("Predictor lineal tradicional (\u03b2\u2082 fijo)")
                  )
                )
              ),
              p(class = "small text-muted mb-0",
                "Los tipos de base (bs) m\u00e1s usados en ecolog\u00eda:",
                br(),
                code("tp"), " \u2014 thin plate (por defecto, recomendado) \u00b7 ",
                code("cr"), " \u2014 cubic regression (r\u00e1pido con n grande) \u00b7 ",
                code("ps"), " \u2014 P-spline (bueno con ruido) \u00b7 ",
                code("te()"), " \u2014 tensor product (interacci\u00f3n entre 2 variables)")
            )
          )
        ),

        # ── Fila 2: k ────────────────────────────────
        div(class = "mt-3",
          card(
            card_header(
              bs_icon("sliders", class = "me-1"),
              "Par\u00e1metro k \u2014 techo de flexibilidad"
            ),
            card_body(
              layout_columns(
                col_widths = c(6, 6),
                fill = FALSE,
                div(
                  p(class = "small mb-1",
                    strong("k es un techo, no un valor fijo."),
                    " Define cu\u00e1ntos segmentos puede usar el spline, pero mgcv ",
                    "decide autom\u00e1ticamente cu\u00e1nta flexibilidad necesita ",
                    "mediante penalizaci\u00f3n (REML/GCV)."),
                  tags$ul(
                    class = "small text-muted mb-0",
                    tags$li(strong("k peque\u00f1o (3-5)"),
                            " \u2014 solo curvas simples"),
                    tags$li(strong("k = 10 (por defecto)"),
                            " \u2014 suficiente para la mayor\u00eda de casos"),
                    tags$li(strong("k grande (15-20)"),
                            " \u2014 formas muy complejas; mgcv suaviza si no son necesarias"),
                    tags$li(strong("EDF \u2248 k-1"),
                            " \u2014 el modelo us\u00f3 toda la flexibilidad; aumenta k")
                  )
                ),
                div(
                  class = "alert alert-info small py-2 px-3 mb-0 h-100",
                  bs_icon("info-circle-fill", class = "me-1"),
                  p(class = "mb-1",
                    strong("Flujo recomendado:"),
                    " empieza con k = 10 (por defecto)."),
                  p(class = "mb-1",
                    "La pestaña ", strong("Ajustar modelo"),
                    " avisa autom\u00e1ticamente si k es insuficiente."),
                  p(class = "mb-0",
                    "La pestaña ", strong("Diagn\u00f3stico"),
                    " verifica formalmente con el test de k.")
                )
              )
            )
          )
        ),

        tags$hr(),

        # ── Tabla LM vs GAM ──────────────────────────
        h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
           "Comparaci\u00f3n: LM vs GAM"),
        tags$table(
          class = "table table-sm small",
          tags$thead(tags$tr(
            tags$th("Aspecto"),
            tags$th("LM"),
            tags$th("GAM")
          )),
          tags$tbody(
            tags$tr(
              tags$td("Forma de la relaci\u00f3n"),
              tags$td("Lineal (fija)"),
              tags$td("Flexible (aprendida)")
            ),
            tags$tr(
              tags$td("Interpretaci\u00f3n"),
              tags$td("\u03b2: cambio por unidad"),
              tags$td("EDF: grados de libertad del spline")
            ),
            tags$tr(
              tags$td("Selecci\u00f3n de complejidad"),
              tags$td("Manual (interacciones)"),
              tags$td("Autom\u00e1tica (penalizaci\u00f3n GCV/REML)")
            ),
            tags$tr(
              tags$td("Paquete"),
              tags$td(code("stats::lm()")),
              tags$td(code("mgcv::gam()"))
            ),
            tags$tr(
              tags$td("M\u00e9todo de estimaci\u00f3n"),
              tags$td("OLS"),
              tags$td("REML / ML / GCV")
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
               "1. Funciones suaves (splines)"),
            p(class = "small",
              "Un spline es una funci\u00f3n construida por tramos de polinomios ",
              "unidos suavemente en puntos llamados ", strong("knots"), ". ",
              "mgcv usa ", strong("thin plate regression splines"),
              " por defecto, que minimizan la curvatura total."),
            div(class = "alert alert-secondary small py-2 px-3 mb-3",
                code("s(x, k=10, bs='tp')"),
                br(),
                "k = dimensi\u00f3n de la base (knots m\u00e1ximos), bs = tipo de base"),

            h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
               "2. Penalizaci\u00f3n y suavizado"),
            p(class = "small",
              "El GAM minimiza:"),
            div(class = "alert alert-secondary small py-2 px-3 mb-3",
                code("\u2211(y\u1d62 - f(x\u1d62))\u00b2 + \u03bb \u222b [f''(x)]\u00b2 dx")),
            p(class = "small",
              "\u03bb es el par\u00e1metro de suavizado. mgcv lo selecciona autom\u00e1ticamente ",
              "minimizando el ", strong("GCV"), " o maximizando el ", strong("REML"), "."),

            h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
               "3. EDF — Grados de libertad efectivos"),
            p(class = "small",
              "EDF = 1 \u2192 relaci\u00f3n lineal. EDF > 1 \u2192 no lineal. ",
              "EDF \u2248 k-1 \u2192 el spline usa toda su flexibilidad (aumentar k). ",
              "La significancia del t\u00e9rmino suave se prueba con un ",
              strong("test aproximado de Chi-cuadrado"), ".")
          ),

          div(
            h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
               "4. M\u00e9todos de estimaci\u00f3n"),
            tags$table(
              class = "table table-sm small mb-3",
              tags$thead(tags$tr(
                tags$th("M\u00e9todo"), tags$th("Uso recomendado")
              )),
              tags$tbody(
                tags$tr(tags$td(strong("REML")),
                        tags$td("Inferencia \u2014 preferido para estimaci\u00f3n de \u03bb")),
                tags$tr(tags$td(strong("ML")),
                        tags$td("Comparaci\u00f3n de modelos con distinta parte fija")),
                tags$tr(tags$td(strong("GCV")),
                        tags$td("R\u00e1pido, pero puede sobresuavizar"))
              )
            ),

            h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
               "5. Concurvidad"),
            p(class = "small",
              "Equivalente del VIF para GAMs. Mide si un t\u00e9rmino suave puede ",
              "ser aproximado por una combinaci\u00f3n de los otros. ",
              "Valores > 0.8 indican problemas. Se calcula con ",
              code("mgcv::concurvity()"), "."),

            h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
               "6. Devianza explicada"),
            p(class = "small",
              "Equivalente del R\u00b2 para GAMs. Se interpreta como la proporci\u00f3n ",
              "de devianza total explicada por el modelo. Disponible en el ",
              "resumen como ", code("Deviance explained"), "."),

            h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
               "7. Verificaci\u00f3n de k"),
            p(class = "small",
              code("gam.check()"), " realiza un test estad\u00edstico para verificar ",
              "si k es suficientemente grande. Si p < 0.05, aumentar k. ",
              "El ratio k'/k debe estar cerca de 1.")
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
            title = tagList(bs_icon("database", class = "me-1"), "Cargar datos"),
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
    ), # /PESTAÑA 3
    # ════════════════════════════════════════════════
    # PESTAÑA 4: Explorar
    # ════════════════════════════════════════════════
    nav_panel(
      title = tagList(bs_icon("zoom-in", class = "me-1"), "Explorar"),
      card_body(
        p(class = "small text-muted mb-3",
          "Visualiza las relaciones entre variables. Las curvas LOESS ayudan a ",
          "identificar qu\u00e9 variables muestran relaciones no lineales y son ",
          "buenas candidatas para llevar un spline ", code("s()"), " en el GAM."
        ),
        layout_columns(
          col_widths = c(4, 8),
          fill = FALSE,
          card(
            card_header(bs_icon("sliders", class = "me-1"), "Controles"),
            card_body(
              style = "overflow: visible; height: auto;",
              uiOutput(ns("sel_var_x")),
              uiOutput(ns("sel_color")),
              checkboxInput(ns("mostrar_suavizado"),
                            "Mostrar curva LOESS",
                            value = TRUE),
              checkboxInput(ns("linea_lm"),
                            "Superponer l\u00ednea LM (comparar)",
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
              tags$hr(),
              p(class = "small fw-bold text-muted mb-1",
                bs_icon("bezier2", class = "me-1"),
                "Predictores con spline s()"),
              p(class = "small text-muted mb-2",
                "Estas variables se modelar\u00e1n con una funci\u00f3n suave no param\u00e9trica."),
              uiOutput(ns("sel_preds_spline")),
              tags$hr(),
              p(class = "small fw-bold text-muted mb-1",
                bs_icon("graph-up", class = "me-1"),
                "Predictores lineales"),
              p(class = "small text-muted mb-2",
                "Estas variables tendr\u00e1n efecto lineal tradicional (\u03b2)."),
              uiOutput(ns("sel_preds_lineal")),
              tags$hr(),
              p(class = "small fw-bold text-muted mb-1",
                bs_icon("sliders2", class = "me-1"),
                "Opciones del spline"),
              p(class = "small text-muted mb-2",
                strong("bs"), " (basis) = tipo de funci\u00f3n base del spline. ",
                strong("k"), " = n\u00famero m\u00e1ximo de segmentos (mgcv ",
                "decide cu\u00e1ntos usar realmente)."),
              numericInput(
                  ns("k_spline"),
                  label = "k (knots m\u00e1x.):",
                  value = 10, min = 3, max = 30
                ),
                selectInput(
                  ns("bs_spline"),
                  label = "Base (bs):",
                  choices = c(
                    "Thin plate — tp (recomendado)" = "tp",
                    "Cubic regression — cr"          = "cr",
                    "P-spline — ps"                  = "ps"
                  ),
                  selected = "tp"
                ),
              div(class = "mt-2",
                checkboxInput(
                  ns("usar_tensor"),
                  label = tagList(
                    "Usar ", code("te()"), " para interacci\u00f3n entre 2 variables",
                    tags$small(class = "text-muted d-block mt-1",
                      "Solo aplica cuando hay \u22652 predictores con spline.")
                  ),
                  value = FALSE
                )
              ),
              tags$hr(),
              selectInput(
                ns("metodo_gam"),
                label = "M\u00e9todo de estimaci\u00f3n:",
                choices = c(
                  "REML (recomendado para inferencia)" = "REML",
                  "ML (comparaci\u00f3n de modelos)"       = "ML",
                  "GCV.Cp (r\u00e1pido)"                   = "GCV.Cp"
                ),
                selected = "REML"
              ),
              actionButton(
                ns("ajustar"),
                "Ajustar modelo GAM",
                class = "btn-primary w-100",
                icon  = icon("play")
              ),
              div(class = "alert alert-info small py-2 px-3 mt-2",
                  bs_icon("info-circle", class = "me-1"),
                  "Al ajustar el modelo, las covariables num\u00e9ricas se estandarizan ",
                  "autom\u00e1ticamente en segundo plano para mejorar la convergencia. ",
                  strong("Los gr\u00e1ficos y predicciones siempre en escala original.")
              ),
              tags$hr(),
              p(class = "small fw-bold text-muted mb-1",
                bs_icon("floppy", class = "me-1"),
                "Guardar para comparar"),
              p(class = "small text-muted mb-2",
                "Dale un nombre al modelo y gu\u00e1rdalo para comparar en ",
                strong("Comparar modelos"), "."),
              textInput(
                ns("nombre_modelo"),
                label       = NULL,
                placeholder = "Ej: nulo, area, area+dist\u2026"
              ),
              actionButton(
                ns("guardar_modelo"),
                "Guardar modelo",
                class = "btn-outline-primary w-100 btn-sm",
                icon  = icon("floppy-disk")
              )
            )
          ),

          div(
            uiOutput(ns("cards_metricas_gam")),
            br(),
            uiOutput(ns("aviso_k")),
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
          "Verificaci\u00f3n de supuestos del GAM: normalidad de residuos, ",
          "patrones sistem\u00e1ticos, y si k es suficientemente grande. ",
          "Generado con ", strong("mgcv::gam.check()"), "."
        ),
        layout_columns(
          col_widths = c(6, 6),
          fill = FALSE,
          card(
            card_header(bs_icon("graph-up", class = "me-1"),
                        "Gr\u00e1ficos de diagn\u00f3stico",
                        span(class = "text-muted small ms-2",
                             "— gam.check() \u00b7 mgcv")),
            card_body(
              style = "height: auto;",
              plotOutput(ns("plot_diagnostico"), height = "420px")
            )
          ),
          div(
            card(
              class = "mb-3",
              card_header(bs_icon("table", class = "me-1"),
                          "Verificaci\u00f3n de k",
                          span(class = "text-muted small ms-2",
                               "— test de k \u00b7 mgcv")),
              card_body(
                style = "overflow: visible; height: auto;",
                p(class = "small text-muted mb-2",
                  "Si p < 0.05 o k'/k < 0.85, aumenta k en Ajustar modelo."),
                uiOutput(ns("tabla_k_check"))
              )
            ),
            card(
              class = "mb-0",
              card_header(bs_icon("diagram-3", class = "me-1"),
                          "Concurvidad",
                          span(class = "text-muted small ms-2",
                               "— concurvity() \u00b7 mgcv")),
              card_body(
                style = "overflow: visible; height: auto;",
                p(class = "small text-muted mb-2",
                  "Equivalente del VIF para GAMs. Valores > 0.8 indican que ",
                  "un t\u00e9rmino suave puede ser aproximado por los otros."),
                uiOutput(ns("tabla_concurvidad"))
              )
            )
          )
        )
      )
    ), # /PESTAÑA 6

    # ════════════════════════════════════════════════
    # PESTAÑA 7: Parámetros
    # ════════════════════════════════════════════════
    nav_panel(
      title = tagList(bs_icon("list-ol", class = "me-1"),
                      "Par\u00e1metros"),
      div(
        class = "p-3",
        p(class = "small text-muted mb-3",
          "Los GAMs tienen dos tipos de t\u00e9rminos: ",
          strong("param\u00e9tricos"), " (coeficientes \u03b2 interpretables como en LM) y ",
          strong("suaves"), " (caracterizados por sus EDF). Un EDF = 1 indica ",
          "efecto lineal; EDF > 1 indica no linealidad."
        ),
        layout_columns(
          col_widths = c(6, 6),
          fill = FALSE,
          card(
            card_header(
              bs_icon("layout-text-sidebar", class = "me-1"),
              "T\u00e9rminos param\u00e9tricos",
              span(class = "text-muted small ms-2",
                   "— coeficientes \u03b2 \u00b7 parameters (easystats)")
            ),
            card_body(
              style = "overflow: visible; height: auto;",
              uiOutput(ns("tabla_params_parametricos"))
            )
          ),
          card(
            card_header(
              bs_icon("bezier2", class = "me-1"),
              "T\u00e9rminos suaves (EDF)",
              span(class = "text-muted small ms-2",
                   "— estimated degrees of freedom \u00b7 mgcv")
            ),
            card_body(
              style = "overflow: visible; height: auto;",
              p(class = "small text-muted mb-2",
                "EDF \u2248 1 = lineal \u00b7 EDF > 1 = no lineal \u00b7 EDF \u2248 k-1 = aumentar k"),
              uiOutput(ns("tabla_params_suaves"))
            )
          )
        ),
        div(class = "mt-3",
          card(
            card_header(
              bs_icon("bar-chart-steps", class = "me-1"),
              "Importancia de variables",
              span(class = "text-muted small ms-2",
                   "— coeficientes estandarizados \u00b7 parameters (easystats)")
            ),
            card_body(
              style = "height: auto;",
              p(class = "small text-muted mb-2",
                strong("Azul"), " = efecto positivo \u00b7 ",
                strong("rojo"), " = efecto negativo. ",
                "Para t\u00e9rminos suaves se muestra el EDF normalizado."
              ),
              plotOutput(ns("plot_importancia"), height = "280px")
            )
          )
        )
      )
    ), # /PESTAÑA 7

    # ════════════════════════════════════════════════
    # PESTAÑA 8: Efectos suaves
    # ════════════════════════════════════════════════
    nav_panel(
      title = tagList(bs_icon("bezier2", class = "me-1"),
                      "Efectos suaves"),
      div(
        class = "p-3",
        p(class = "small text-muted mb-3",
          "Visualizaci\u00f3n de los efectos suaves estimados por el GAM. ",
          "La banda gris muestra el IC 95%. La l\u00ednea punteada en cero indica ",
          "ausencia de efecto. Generado con ",
          strong("gratia::draw()"), " y ", strong("modelbased::estimate_relation()"), "."
        ),
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
              checkboxInput(ns("mostrar_residuos_parciales"),
                            "Mostrar residuos parciales",
                            value = FALSE),
              tags$hr(),
              p(class = "small fw-bold text-muted mb-1",
                "Predicci\u00f3n puntual"),
              uiOutput(ns("inputs_prediccion")),
              actionButton(
                ns("calcular_prediccion"),
                "Calcular predicci\u00f3n",
                class = "btn-primary w-100 btn-sm mt-2",
                icon  = icon("calculator")
              )
            )
          ),
          div(
            card(
              class = "mb-3",
              card_header(
                bs_icon("bezier2", class = "me-1"),
                "Efecto suave estimado",
                span(class = "text-muted small ms-2",
                     "— gratia::draw() \u00b7 IC 95%")
              ),
              card_body(
                style = "height: auto;",
                plotOutput(ns("plot_efecto_suave"), height = "360px")
              )
            ),
            uiOutput(ns("resultado_prediccion"))
          )
        ),
        div(class = "mt-3",
          card(
            card_header(
              bs_icon("graph-up-arrow", class = "me-1"),
              "Predicho vs. Observado",
              span(class = "text-muted small ms-2",
                   "— escala original")
            ),
            card_body(
              style = "height: auto;",
              plotOutput(ns("plot_predobs"), height = "280px")
            )
          )
        )
      )
    ), # /PESTAÑA 8

    # ════════════════════════════════════════════════
    # PESTAÑA 9: Contrastes
    # ════════════════════════════════════════════════
    nav_panel(
      title = tagList(bs_icon("arrows-angle-expand", class = "me-1"),
                      "Contrastes"),
      div(
        class = "p-3",
        p(class = "small text-muted mb-3",
          "Los ", strong("contrastes"), " comparan el valor promedio ",
          "de Y entre grupos de un predictor categórico, controlando ",
          "por el resto de variables del modelo. Solo disponible para ",
          "los ", strong("términos paramétricos"), " (factores) del GAM. ",
          "Generados con ",
          strong("modelbased::estimate_contrasts()"), " de easystats."
        ),
        uiOutput(ns("contrasts_no_cat_msg_gam")),
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header(bs_icon("sliders", class = "me-1"), "Controles"),
            card_body(
              uiOutput(ns("sel_var_contraste_gam")),
              tags$hr(),
              selectInput(
                ns("metodo_ajuste_gam"),
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
              card_header(bs_icon("table", class = "me-1"),
                          "Tabla de contrastes"),
              card_body(uiOutput(ns("tabla_contrastes_gam")))
            ),
            card(
              class = "mb-0",
              card_header(bs_icon("bar-chart-fill", class = "me-1"),
                          "Visualización de contrastes"),
              card_body(
                plotOutput(ns("plot_contrastes_gam"), height = "300px")
              )
            )
          )
        )
      )
    ), # /PESTAÑA 9

    # ════════════════════════════════════════════════
    # PESTAÑA 10: Performance
    # ════════════════════════════════════════════════
    nav_panel(
      title = tagList(bs_icon("speedometer2", class = "me-1"),
                      "Performance"),
      div(
        class = "p-3",
        p(class = "small text-muted mb-3",
          "M\u00e9tricas de rendimiento del GAM: devianza explicada, R\u00b2, RMSE, ",
          "y validaci\u00f3n cruzada para estimar el error de predicci\u00f3n en datos nuevos."
        ),
        layout_columns(
          col_widths = c(6, 6),
          fill = FALSE,
          card(
            card_header(
              bs_icon("speedometer2", class = "me-1"),
              "M\u00e9tricas del modelo",
              span(class = "text-muted small ms-2",
                   "— performance \u00b7 easystats")
            ),
            card_body(
              style = "overflow: visible; height: auto;",
              uiOutput(ns("tabla_performance"))
            )
          ),
          card(
            card_header(
              bs_icon("arrow-repeat", class = "me-1"),
              "Validaci\u00f3n cruzada",
              span(class = "text-muted small ms-2",
                   "— vfold_cv() \u00b7 tidymodels")
            ),
            card_body(
              style = "overflow: visible; height: auto;",
              p(class = "small text-muted mb-2",
                "\u00bfCu\u00e1nto error cometo al predecir ",
                strong("datos nuevos"), "?"),
              layout_columns(
                col_widths = c(4, 4, 4),
                numericInput(ns("cv_folds"), "Folds:", value = 10,
                             min = 3, max = 20),
                div(class = "pt-4",
                    checkboxInput(ns("cv_estratificado"),
                                  "Estratificar", value = FALSE)),
                div(class = "pt-4",
                    actionButton(ns("correr_cv"), "Correr CV",
                                 class = "btn-primary w-100",
                                 icon = icon("rotate")))
              ),
              tags$hr(),
              uiOutput(ns("resultado_cv"))
            )
          )
        )
      )
    ), # /PESTAÑA 9

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
          ", gu\u00e1rdalos con nombre y comp\u00e1ralos por AIC, AICc, BIC, ",
          "R\u00b2 y devianza explicada."
        ),
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
              card_header(
                bs_icon("table", class = "me-1"),
                "Tabla comparativa",
                span(class = "text-muted small ms-2",
                     "— compare_performance() \u00b7 easystats")
              ),
              card_body(
                style = "overflow: visible; height: auto;",
                uiOutput(ns("tabla_comparacion"))
              )
            ),
            card(
              class = "mb-0",
              card_header(
                bs_icon("diagram-3", class = "me-1"),
                "Gr\u00e1fico radar",
                span(class = "text-muted small ms-2",
                     "— see \u00b7 easystats")
              ),
              card_body(
                style = "height: auto;",
                p(class = "small text-muted mb-2",
                  "Mayor \u00e1rea = mejor modelo. Requiere \u22652 modelos."),
                plotOutput(ns("plot_comparacion"), height = "320px")
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
      title = tagList(bs_icon("code-slash", class = "me-1"), "C\u00f3digo R"),
      card_body(
        p(class = "text-muted small mb-3",
          "C\u00f3digo R reproducible para el modelo ajustado. ",
          "Copia y ejecuta en tu propia sesi\u00f3n de R."),
        layout_columns(
          col_widths = c(10, 2),
          verbatimTextOutput(ns("codigo_r")),
          div(
            downloadButton(ns("descargar_codigo"), "Descargar .R",
                           class = "btn-outline-secondary btn-sm w-100")
          )
        )
      )
    ) # /PESTAÑA 11

  ) # /navset_card_tab
  ) # /tagList

} # /mod_gam_ui


# ── SERVER ──────────────────────────────────────────────────
mod_gam_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ────────────────────────────────────────────────────
    # DATOS
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
      } else if (fuente == "ejemplo_salud") {
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
      } else {
        req(input$archivo)
        ext <- tools::file_ext(input$archivo$name)
        tryCatch({
          if (ext == "csv")
            readr::read_delim(input$archivo$datapath,
                              delim = input$separador,
                              show_col_types = FALSE) |>
              as.data.frame()
          else
            readxl::read_excel(input$archivo$datapath) |>
              as.data.frame()
        }, error = function(e) {
          showNotification(paste("Error al leer archivo:", conditionMessage(e)),
                           type = "error", duration = 6)
          NULL
        })
      }
    })

    vars_numericas <- reactive({
      df <- datos_activos(); req(df)
      names(df)[sapply(df, is.numeric)]
    })

    vars_categoricas <- reactive({
      df <- datos_activos(); req(df)
      names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    })

    # ── Info dataset ──────────────────────────────────────

        # Tipos de variables
    tipos_usuario <- reactiveVal(NULL)
    observeEvent(input$fuente_datos, { tipos_usuario(NULL) })
    observeEvent(input$resetear_tipos, {
      tipos_usuario(NULL)
      showNotification("Tipos restaurados.", type = "message", duration = 2)
    })
    observeEvent(input$aplicar_tipos, {
      df  <- datos_activos(); req(df)
      nms <- names(df)
      nuevos <- lapply(nms, function(nm) input[[paste0("tipo_", nm)]])
      names(nuevos) <- nms
      tipos_usuario(nuevos)
      showNotification("Tipos aplicados.", type = "message", duration = 2)
    })
    output$sel_fuente_datos <- renderUI({
      radioButtons(
        ns("fuente_datos"),
        label   = tagList(bs_icon("database", class = "me-1"),
                          "Dataset de ejemplo:"),
        choices = c(
          "Densidad de especie de ave (Loyn, 1987)"  = "ejemplo_ave",
          "Peso al nacer \u2014 salud perinatal (Hosmer)" = "ejemplo_salud",
          "Cargar mis propios datos"                  = "propio"
        ),
        selected = "ejemplo_ave"
      )
    })

        output$tabla_tipos <- renderUI({
      df <- datos_activos(); req(df)
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
      df <- datos_activos(); req(df)
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
        div(class = "alert alert-info small py-2 px-3 mb-2",
            bs_icon("info-circle-fill", class = "me-1"),
            strong("Dataset: Densidad de especie de ave (Loyn, 1987)."),
            " 56 fragmentos de bosque en Victoria, Australia. ",
            "Variables: densidad_especie, area_ha, distancia_m, altitud_m, pastoreo.")
      } else if (fuente == "ejemplo_salud") {
        div(class = "alert alert-info small py-2 px-3 mb-2",
            bs_icon("info-circle-fill", class = "me-1"),
            strong("Dataset: Peso al nacer (Hosmer & Lemeshow)."),
            " 189 neonatos. Variables: peso_g, edad_madre, peso_madre, tabaco, hta.")
      } else {
        div(class = "alert alert-info small py-2 px-3 mb-2",
            bs_icon("info-circle-fill", class = "me-1"),
            "Datos cargados por el usuario.")
      }
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
               h3(style = paste0("color:", colores$primario, "; font-weight:700;"),
                  nrow(df)),
               p(class = "small text-muted mb-0", "Observaciones")
             )),
        card(class = "text-center border-0",
             style = paste0("background:", colores$fondo),
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$acento, "; font-weight:700;"),
                  nnum),
               p(class = "small text-muted mb-0", "Num\u00e9ricas")
             )),
        card(class = "text-center border-0",
             style = paste0("background:", colores$fondo),
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$secundario, "; font-weight:700;"),
                  ncat),
               p(class = "small text-muted mb-0", "Categ\u00f3ricas")
             ))
      )
    })

    output$resumen_datos <- renderUI({
      df <- datos_activos(); req(df)
      div(class = "small text-muted mt-2",
          bs_icon("info-circle", class = "me-1"),
          paste0(nrow(df), " filas \u00b7 ", ncol(df), " columnas"))
    })

    output$tabla_preview <- renderDT({
      df <- datos_activos(); req(df)
      datatable(head(df, 20),
                options = list(scrollX = TRUE, dom = "t",
                               pageLength = 20),
                rownames = FALSE,
                class    = "table-sm table-striped")
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 4: Explorar
    # ────────────────────────────────────────────────────

    output$sel_var_x <- renderUI({
      req(vars_numericas())
      selectInput(ns("var_x"), "Variable X:",
                  choices  = vars_numericas(),
                  selected = vars_numericas()[1])
    })

    output$sel_color <- renderUI({
      cats <- vars_categoricas()
      if (length(cats) == 0) return(NULL)
      selectInput(ns("var_color"), "Colorear por (opcional):",
                  choices  = c("Ninguna" = "ninguna", cats),
                  selected = "ninguna")
    })

    output$cards_correlacion <- renderUI({
      df <- datos_activos(); req(df, input$var_x)
      nums <- vars_numericas(); req(length(nums) >= 2)
      yvar <- nums[nums != input$var_x][1]; req(yvar)
      cor_val <- cor(df[[yvar]], df[[input$var_x]], use = "complete.obs")
      layout_columns(
        col_widths = c(6, 6),
        card(class = "text-center border-0",
             style = paste0("background:", colores$fondo),
             card_body(class = "p-2",
               h4(style = paste0("color:", colores$primario, "; font-weight:700;"),
                  round(cor_val, 2)),
               p(class = "small text-muted mb-0", "Correlaci\u00f3n (r)")
             )),
        card(class = "text-center border-0",
             style = paste0("background:", colores$fondo),
             card_body(class = "p-2",
               h4(style = paste0("color:", colores$acento, "; font-weight:700;"),
                  paste0(round(cor_val^2 * 100, 0), "%")),
               p(class = "small text-muted mb-0", "R\u00b2 simple")
             ))
      )
    })

    output$plot_scatter <- renderPlot(suppressWarnings({
      df <- datos_activos(); req(df, input$var_x)
      nums <- vars_numericas(); req(length(nums) >= 2)
      yvar <- nums[nums != input$var_x][1]; req(yvar)
      usar_color <- !is.null(input$var_color) &&
        input$var_color != "ninguna" &&
        input$var_color %in% names(df)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[input$var_x]],
                                             y = .data[[yvar]]))
      if (usar_color)
        p <- p + ggplot2::aes(color = .data[[input$var_color]]) +
          ggplot2::scale_color_manual(values = colores$tableau,
                                      name = input$var_color)
      p <- p + ggplot2::geom_point(alpha = 0.5, size = 2)

      if (isTRUE(input$mostrar_suavizado))
        p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                                       se = TRUE, color = colores$primario,
                                       fill = colores$secundario,
                                       alpha = 0.15, linewidth = 1.2,
                                       show.legend = FALSE)
      if (isTRUE(input$linea_lm))
        p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                                       se = FALSE, color = colores$peligro,
                                       linetype = "dashed", linewidth = 0.8,
                                       show.legend = FALSE)
      p + ggplot2::labs(x = input$var_x, y = yvar,
                        subtitle = paste0("n = ", nrow(df),
                                          " observaciones \u00b7 curva LOESS vs l\u00ednea LM")) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       legend.position  = "bottom",
                       plot.subtitle    = ggplot2::element_text(
                         color = colores$texto, size = 9))
    }), res = 96)

    output$insight_scatter <- renderUI({
      df <- datos_activos(); req(df, input$var_x)
      nums <- vars_numericas(); req(length(nums) >= 2)
      yvar <- nums[nums != input$var_x][1]; req(yvar)
      cor_val <- cor(df[[yvar]], df[[input$var_x]], use = "complete.obs")
      dir <- if (cor_val > 0.5) "positiva y fuerte" else
        if (cor_val > 0.2) "positiva y moderada" else
          if (cor_val < -0.5) "negativa y fuerte" else "d\u00e9bil"
      div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
          bs_icon("lightbulb-fill", class = "me-1"),
          paste0("La relaci\u00f3n entre ", input$var_x, " y ", yvar,
                 " es ", dir, " (r = ", round(cor_val, 2), "). ",
                 "Si la curva LOESS se aleja de la l\u00ednea LM, considera usar ",
                 code("s("), input$var_x, code(")"), " en el GAM."))
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 5: Ajustar modelo
    # ────────────────────────────────────────────────────

    output$sel_var_y <- renderUI({
      req(vars_numericas())
      selectInput(ns("var_y"), "Variable respuesta (Y):",
                  choices  = vars_numericas(),
                  selected = vars_numericas()[1])
    })

    output$sel_preds_spline <- renderUI({
      nums <- vars_numericas(); req(nums, input$var_y)
      opts <- nums[nums != input$var_y]
      if (length(opts) == 0) return(
        p(class = "small text-muted", "No hay m\u00e1s variables num\u00e9ricas.")
      )
      checkboxGroupInput(
        ns("sel_preds_spline_input"),
        label    = NULL,
        choices  = opts,
        selected = opts[1]
      )
    })

    output$sel_preds_lineal <- renderUI({
      df   <- datos_activos(); req(df, input$var_y)
      nums <- vars_numericas()
      cats <- vars_categoricas()
      spline_sel <- input$sel_preds_spline_input
      opts <- c(
        nums[!nums %in% c(input$var_y, spline_sel)],
        cats
      )
      if (length(opts) == 0) return(
        p(class = "small text-muted", "No hay variables disponibles para efecto lineal.")
      )
      checkboxGroupInput(
        ns("preds_lineal"),
        label    = NULL,
        choices  = opts,
        selected = NULL
      )
    })

    # ── Modelo GAM ───────────────────────────────────────

    modelo_gam <- eventReactive(input$ajustar, {
      df <- datos_activos(); req(df, input$var_y)
      splines <- input$sel_preds_spline_input
      lineales <- input$preds_lineal
      if (is.null(splines) || length(splines) == 0) {
        showNotification("Selecciona al menos una variable con spline.",
                         type = "warning", duration = 4)
        return(NULL)
      }

      tryCatch({
        # Términos suaves
        if (isTRUE(input$usar_tensor) && length(splines) >= 2) {
          terminos_s <- paste0("te(", paste(splines[1:2], collapse = ", "),
                               ", k = ", input$k_spline, ")")
          if (length(splines) > 2)
            terminos_s <- c(terminos_s,
                            paste0("s(", splines[3:length(splines)],
                                   ", k = ", input$k_spline,
                                   ", bs = '", input$bs_spline, "')"))
        } else {
          terminos_s <- paste0("s(", splines, ", k = ", input$k_spline,
                               ", bs = '", input$bs_spline, "')")
        }

        # Términos lineales
        terminos_l <- if (!is.null(lineales) && length(lineales) > 0)
          lineales else character(0)

        todos <- c(terminos_s, terminos_l)
        fm <- as.formula(paste(input$var_y, "~", paste(todos, collapse = " + ")))

        mgcv::gam(fm, data = df, method = input$metodo_gam)
      }, error = function(e) {
        showNotification(paste("Error al ajustar GAM:", conditionMessage(e)),
                         type = "error", duration = 6)
        NULL
      })
    }, ignoreNULL = TRUE)

    # Modelo estandarizado (para importancia)
    modelo_gam_std <- eventReactive(input$ajustar, {
      df <- datos_activos(); req(df, input$var_y)
      splines  <- input$sel_preds_spline_input
      lineales <- input$preds_lineal
      req(!is.null(splines) && length(splines) > 0)
      tryCatch({
        nums_pred <- c(splines,
                       lineales[lineales %in% vars_numericas()])
        if (length(nums_pred) > 0)
          df[, nums_pred] <- scale(df[, nums_pred, drop = FALSE])
        terminos_s <- paste0("s(", splines, ", k = ", input$k_spline,
                             ", bs = '", input$bs_spline, "')")
        terminos_l <- if (!is.null(lineales) && length(lineales) > 0)
          lineales else character(0)
        fm <- as.formula(paste(input$var_y, "~",
                               paste(c(terminos_s, terminos_l),
                                     collapse = " + ")))
        mgcv::gam(fm, data = df, method = input$metodo_gam)
      }, error = function(e) NULL)
    }, ignoreNULL = TRUE)

    output$aviso_k <- renderUI({
      fm <- modelo_gam(); req(fm)
      tryCatch({
        kc  <- mgcv::k.check(fm)
        # Rows where k is potentially insufficient
        prob <- kc[kc[,"k'"] / kc[,"k"] > 0.85 |
                   kc[,"p-value"] < 0.05, , drop = FALSE]
        if (nrow(prob) == 0) return(NULL)
        terminos <- paste(rownames(prob), collapse = ", ")
        div(class = "alert alert-warning small py-2 px-3 mb-2",
            bs_icon("exclamation-triangle-fill", class = "me-1"),
            strong("k posiblemente insuficiente"), " en: ",
            code(terminos), ". ",
            "El modelo quería más flexibilidad. Aumenta ",
            strong("k"), " en los controles y reajusta. ",
            "Verifica en la pestaña ", strong("Diagnóstico"), ".")
      }, error = function(e) NULL)
    })

    output$formula_ajustada <- renderUI({
      fm <- modelo_gam(); req(fm)
      fm_str <- paste(deparse(formula(fm)), collapse = " ")
      div(class = "alert alert-secondary small py-2 px-3 mb-0",
          code(fm_str))
    })

    output$cards_metricas_gam <- renderUI({
      fm <- modelo_gam()
      if (is.null(fm)) return(
        div(class = "alert alert-info small py-2 px-3",
            bs_icon("play-circle", class = "me-1"),
            "Ajusta el modelo para ver las m\u00e9tricas.")
      )
      s      <- summary(fm)
      dev_ex <- round(s$dev.expl * 100, 1)
      r2_adj <- round(s$r.sq, 3)
      aic_v  <- round(AIC(fm), 2)
      reml_v <- if (fm$method == "REML") round(fm$gcv.ubre, 2) else NA

      col_dev <- if (dev_ex > 80) colores$exito else
        if (dev_ex > 50) colores$acento else colores$peligro

      layout_columns(
        col_widths = c(6, 6),
        card(class = "text-center border-0 mb-0",
             style = paste0("background:", colores$fondo),
             card_body(class = "p-2",
               h3(style = paste0("color:", col_dev, "; font-weight:700;"),
                  paste0(dev_ex, "%")),
               p(class = "small text-muted mb-0", "Devianza explicada")
             )),
        card(class = "text-center border-0 mb-0",
             style = paste0("background:", colores$fondo),
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$primario, "; font-weight:700;"),
                  r2_adj),
               p(class = "small text-muted mb-0", "R\u00b2 ajustado")
             ))
      )
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 6: Diagnóstico
    # ────────────────────────────────────────────────────

    output$plot_diagnostico <- renderPlot({
      fm <- modelo_gam(); req(fm)
      old_par <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
      on.exit(par(old_par))
      mgcv::gam.check(fm, rep = 500)
    }, res = 96)

    output$tabla_k_check <- renderUI({
      fm <- modelo_gam(); req(fm)
      tryCatch({
        kc <- capture.output(mgcv::k.check(fm))
        df_k <- tryCatch({
          kch   <- mgcv::k.check(fm)
          data.frame(
            Termino = rownames(kch),
            k_prima = round(kch[, "k'"], 0),
            k       = round(kch[, "k"], 0),
            ratio   = round(kch[, "k'"]/kch[, "k"], 3),
            p_valor = round(kch[, "p-value"], 4)
          )
        }, error = function(e) NULL)
        req(df_k)
        filas <- lapply(seq_len(nrow(df_k)), function(i) {
          ok    <- df_k$ratio[i] >= 0.85 && df_k$p_valor[i] >= 0.05
          col_p <- if (!ok) colores$peligro else colores$exito
          tags$tr(
            tags$td(code(df_k$Termino[i])),
            tags$td(style = "text-align:center;", df_k$k_prima[i]),
            tags$td(style = "text-align:center;", df_k$k[i]),
            tags$td(style = "text-align:center;", df_k$ratio[i]),
            tags$td(style = paste0("color:", col_p, "; font-weight:600;",
                                   "text-align:center;"),
                    if (!ok) paste0(df_k$p_valor[i], " \u26a0") else df_k$p_valor[i])
          )
        })
        tags$table(
          class = "table table-sm small mb-0",
          tags$thead(tags$tr(
            tags$th("T\u00e9rmino"), tags$th("k'"), tags$th("k"),
            tags$th("k'/k"), tags$th("p-valor")
          )),
          tags$tbody(filas)
        )
      }, error = function(e) {
        p(class = "small text-muted", "Ajusta el modelo primero.")
      })
    })

    output$tabla_concurvidad <- renderUI({
      fm <- modelo_gam(); req(fm)
      tryCatch({
        cv  <- mgcv::concurvity(fm, full = FALSE)
        df_cv <- as.data.frame(cv$worst)
        df_cv$termino <- rownames(df_cv)
        filas <- lapply(seq_len(nrow(df_cv)), function(i) {
          val   <- round(max(unlist(df_cv[i, -ncol(df_cv)]),
                             na.rm = TRUE), 3)
          col_v <- if (val > 0.8) colores$peligro else
            if (val > 0.5) colores$acento else colores$exito
          estado <- if (val > 0.8) "\u26a0 Severo" else
            if (val > 0.5) "\u26a0 Moderado" else "\u2713 OK"
          tags$tr(
            tags$td(code(df_cv$termino[i])),
            tags$td(style = "text-align:center;", val),
            tags$td(style = paste0("color:", col_v,
                                   "; font-weight:600; text-align:center;"),
                    estado)
          )
        })
        tags$table(
          class = "table table-sm small mb-0",
          tags$thead(tags$tr(
            tags$th("T\u00e9rmino"), tags$th("Concurvidad m\u00e1x."),
            tags$th("Estado")
          )),
          tags$tbody(filas)
        )
      }, error = function(e) {
        p(class = "small text-muted",
          "Concurvidad disponible con \u22652 t\u00e9rminos suaves.")
      })
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 7: Parámetros
    # ────────────────────────────────────────────────────

    output$tabla_params_parametricos <- renderUI({
      fm <- modelo_gam(); req(fm)
      tryCatch({
        mp <- parameters::model_parameters(fm, verbose = FALSE)
        df <- as.data.frame(mp)
        # Solo términos paramétricos
        df <- df[df$Component == "parametric" | is.na(df$Component), ]
        if (nrow(df) == 0) return(
          p(class = "small text-muted",
            "No hay t\u00e9rminos param\u00e9tricos (solo splines).")
        )
        filas <- lapply(seq_len(nrow(df)), function(i) {
          pval <- df$p[i]
          p_txt <- if (!is.na(pval)) {
            if (pval < 0.001) "< 0.001 ***" else
            if (pval < 0.01)  paste0(round(pval,3), " **") else
            if (pval < 0.05)  paste0(round(pval,3), " *") else
            round(pval, 3)
          } else "\u2014"
          col_p <- if (!is.na(pval) && pval < 0.001) colores$exito else
            if (!is.na(pval) && pval < 0.05) colores$acento else colores$texto
          tags$tr(
            tags$td(strong(df$Parameter[i])),
            tags$td(style="text-align:center;", round(df$Coefficient[i],3)),
            tags$td(style="text-align:center;", round(df$SE[i],3)),
            tags$td(style="text-align:center;",
                    paste0("[",round(df$CI_low[i],3),", ",round(df$CI_high[i],3),"]")),
            tags$td(style=paste0("color:",col_p,";font-weight:600;text-align:center;"),
                    p_txt)
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

    output$tabla_params_suaves <- renderUI({
      fm <- modelo_gam(); req(fm)
      tryCatch({
        s    <- summary(fm)
        df_s <- as.data.frame(s$s.table)
        df_s$termino <- rownames(df_s)
        filas <- lapply(seq_len(nrow(df_s)), function(i) {
          pval  <- df_s[i, "p-value"]
          edf   <- round(df_s[i, "edf"], 2)
          lineal <- edf <= 1.5
          p_txt <- if (!is.na(pval)) {
            if (pval < 0.001) "< 0.001 ***" else
            if (pval < 0.01)  paste0(round(pval,4), " **") else
            if (pval < 0.05)  paste0(round(pval,4), " *") else
            round(pval, 4)
          } else "\u2014"
          col_p <- if (!is.na(pval) && pval < 0.05) colores$exito else colores$texto
          col_edf <- if (lineal) colores$texto else colores$acento
          tags$tr(
            tags$td(code(df_s$termino[i])),
            tags$td(style = paste0("color:",col_edf,
                                   ";font-weight:600;text-align:center;"),
                    edf),
            tags$td(style="text-align:center;",
                    round(df_s[i,"Ref.df"], 2)),
            tags$td(style="text-align:center;",
                    round(df_s[i,"F"], 2)),
            tags$td(style=paste0("color:",col_p,
                                 ";font-weight:600;text-align:center;"),
                    p_txt),
            tags$td(style="text-align:center; font-size:0.8rem;",
                    class="text-muted",
                    if (lineal) "lineal" else "no lineal")
          )
        })
        tags$table(
          class = "table table-sm table-hover small mb-0",
          tags$thead(tags$tr(
            tags$th("T\u00e9rmino"), tags$th("EDF"),
            tags$th("Ref.df"), tags$th("F"), tags$th("p-valor"),
            tags$th("Forma")
          )),
          tags$tbody(filas)
        )
      }, error = function(e) {
        p(class = "small text-muted", "Ajusta el modelo primero.")
      })
    })

    output$plot_importancia <- renderPlot({
      fm_std <- modelo_gam_std(); req(fm_std)
      tryCatch({
        s    <- summary(fm_std)
        # Términos paramétricos
        df_p <- as.data.frame(s$p.table)
        df_p$termino  <- rownames(df_p)
        df_p$abs_est  <- abs(df_p$Estimate)
        df_p$dir      <- ifelse(df_p$Estimate >= 0, "Positivo", "Negativo")
        df_p$sig      <- df_p[,"Pr(>|t|)"] < 0.05
        df_p$tipo     <- "Param\u00e9trico"

        # Términos suaves — usar EDF normalizado
        df_s <- as.data.frame(s$s.table)
        df_s$termino <- rownames(df_s)
        df_s$abs_est <- df_s$edf / max(df_s$edf)
        df_s$dir     <- "Positivo"
        df_s$sig     <- df_s[,"p-value"] < 0.05
        df_s$tipo    <- "Suave (EDF norm.)"

        # Combinar
        cols <- c("termino","abs_est","dir","sig","tipo")
        df_all <- rbind(
          df_p[!grepl("Intercept", df_p$termino), cols],
          df_s[, cols]
        )
        df_all$sig_chr <- ifelse(df_all$sig, "sig", "no_sig")
        df_all$termino <- factor(df_all$termino,
                                 levels = df_all$termino[order(df_all$abs_est)])

        ggplot2::ggplot(df_all,
                        ggplot2::aes(x = abs_est, y = termino,
                                     fill = dir, alpha = sig_chr)) +
          ggplot2::geom_col(width = 0.65) +
          ggplot2::facet_grid(tipo ~ ., scales = "free_y", space = "free_y") +
          ggplot2::scale_fill_manual(
            values = c("Positivo" = colores$primario,
                       "Negativo" = colores$peligro),
            name = "Direcci\u00f3n") +
          ggplot2::scale_alpha_manual(
            values = c("sig" = 1, "no_sig" = 0.35), guide = "none") +
          ggplot2::scale_x_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.15))) +
          ggplot2::labs(
            x = "Importancia relativa",
            y = NULL,
            subtitle = "Param\u00e9tricos: |\u03b2 estand.| \u00b7 Suaves: EDF normalizado \u00b7 Transparente = p \u2265 0.05") +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(
            panel.grid.minor   = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            legend.position    = "bottom",
            strip.text         = ggplot2::element_text(
              color = colores$primario, face = "bold"),
            plot.subtitle      = ggplot2::element_text(
              color = colores$texto, size = 8))
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "Ajusta el modelo primero.",
                            color = colores$texto, size = 4) +
          ggplot2::theme_void()
      })
    }, res = 96)

    # ────────────────────────────────────────────────────
    # PESTAÑA 8: Efectos suaves
    # ────────────────────────────────────────────────────

    output$sel_pred_efecto <- renderUI({
      fm <- modelo_gam(); req(fm)
      splines <- input$sel_preds_spline_input
      req(splines)
      selectInput(ns("pred_efecto"), "Predictor a visualizar:",
                  choices = splines, selected = splines[1])
    })

    output$plot_efecto_suave <- renderPlot({
      fm <- modelo_gam(); req(fm, input$pred_efecto)
      tryCatch({
        # Usar gratia::draw para el término específico
        p <- gratia::draw(fm,
                          select    = paste0("s(", input$pred_efecto, ")"),
                          residuals = isTRUE(input$mostrar_residuos_parciales),
                          rug       = isTRUE(input$mostrar_datos_efecto)) &
          ggplot2::theme_minimal(base_size = 13) &
          ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            plot.subtitle    = ggplot2::element_text(
              color = colores$texto, size = 9)
          )
        print(p)
      }, error = function(e) {
        # Fallback con modelbased
        tryCatch({
          rel <- modelbased::estimate_relation(fm, by = input$pred_efecto,
                                               verbose = FALSE)
          df_rel <- as.data.frame(rel)
          df_dat <- datos_activos()

          ggplot2::ggplot(df_rel,
                          ggplot2::aes(x = .data[[input$pred_efecto]],
                                       y = Predicted)) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = CI_low, ymax = CI_high),
                                 fill = colores$primario, alpha = 0.15) +
            {if (isTRUE(input$mostrar_datos_efecto))
              ggplot2::geom_point(
                data = df_dat,
                ggplot2::aes(x = .data[[input$pred_efecto]],
                             y = .data[[input$var_y]]),
                color = colores$primario, alpha = 0.3, size = 1.5,
                inherit.aes = FALSE)} +
            ggplot2::geom_line(color = colores$primario, linewidth = 1.2) +
            ggplot2::labs(x = input$pred_efecto, y = input$var_y,
                          subtitle = paste0("Efecto suave s(", input$pred_efecto,
                                            ") \u00b7 IC 95%")) +
            ggplot2::theme_minimal(base_size = 13) +
            ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
        }, error = function(e2) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x=0.5, y=0.5,
                              label = "Error al graficar el efecto.",
                              color = colores$texto, size=4) +
            ggplot2::theme_void()
        })
      })
    }, res = 96)

    output$plot_predobs <- renderPlot({
      fm <- modelo_gam(); req(fm)
      tibble::tibble(
        obs  = fitted(fm) + resid(fm),
        pred = fitted(fm)
      ) |>
        ggplot2::ggplot(ggplot2::aes(x = obs, y = pred)) +
        ggplot2::geom_abline(slope = 1, intercept = 0,
                             linetype = "dashed",
                             color = colores$texto, linewidth = 0.8) +
        ggplot2::geom_point(color = colores$primario, alpha = 0.5, size = 2) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                             se = FALSE, color = colores$acento, linewidth = 1) +
        ggplot2::labs(x = "Observado", y = "Predicho",
                      subtitle = "Los puntos deben seguir la diagonal") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       plot.subtitle    = ggplot2::element_text(
                         color = colores$texto, size = 9))
    }, res = 96)

    # Predicción puntual
    output$inputs_prediccion <- renderUI({
      fm <- modelo_gam(); req(fm)
      df <- datos_activos()
      preds <- c(input$sel_preds_spline_input, input$preds_lineal)
      req(length(preds) > 0)
      inputs <- lapply(preds, function(nm) {
        col <- df[[nm]]
        if (is.numeric(col))
          numericInput(inputId = ns(paste0("pv_", nm)),
                       label = paste0(nm, " (media=", round(mean(col,na.rm=T),1), "):"),
                       value = round(mean(col, na.rm=T), 1))
        else
          selectInput(inputId = ns(paste0("pv_", nm)),
                      label = nm, choices = levels(col),
                      selected = levels(col)[1])
      })
      do.call(tagList, inputs)
    })

    resultado_pred_data <- eventReactive(input$calcular_prediccion, {
      fm <- modelo_gam(); req(fm)
      df <- datos_activos()
      preds <- c(input$sel_preds_spline_input, input$preds_lineal)
      req(length(preds) > 0)
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
      res <- resultado_pred_data()
      if (is.null(res)) return(NULL)
      df_res <- as.data.frame(res)
      pred   <- round(df_res$Predicted[1], 3)
      lo     <- round(df_res$CI_low[1], 3)
      hi     <- round(df_res$CI_high[1], 3)
      card(
        class = "mt-2",
        card_header(bs_icon("bullseye", class = "me-1"), "Resultado"),
        card_body(
          div(class = "text-center py-2",
              h3(style = paste0("color:", colores$primario,
                                "; font-weight:700;"), pred),
              p(class = "text-muted mb-1",
                strong(paste0(input$var_y, " predicho"))),
              p(class = "small text-muted",
                "IC 95%: ", strong(paste0("[", lo, ", ", hi, "]")))
          )
        )
      )
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 9: Contrastes
    # ────────────────────────────────────────────────────

    output$contrasts_no_cat_msg_gam <- renderUI({
      if (length(input$preds_lineal) == 0 ||
          !any(input$preds_lineal %in% vars_categoricas()))
        div(class = "alert alert-warning small py-2 px-3 mb-3",
            bs_icon("exclamation-triangle-fill", class = "me-1"),
            "El modelo no tiene predictores categóricos (lineales). ",
            "Ve a ", strong("Ajustar modelo"),
            " y agrega al menos una variable categórica en los predictores lineales.")
    })

    output$sel_var_contraste_gam <- renderUI({
      fit  <- modelo_gam(); req(fit)
      cats <- input$preds_lineal
      cats <- cats[cats %in% vars_categoricas()]
      req(length(cats) > 0)
      selectInput(ns("var_contraste_gam"),
                  label   = "Variable para contrastar:",
                  choices = cats, selected = cats[1])
    })

    output$tabla_contrastes_gam <- renderUI({
      fit <- modelo_gam(); req(fit, input$var_contraste_gam)
      tryCatch({
        ct    <- modelbased::estimate_contrasts(
          fit, contrast = input$var_contraste_gam,
          p_adjust = input$metodo_ajuste_gam, verbose = FALSE)
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
                      paste0("[", round(df_ct[[ci_lo]][i],3), ", ",
                             round(df_ct[[ci_hi]][i],3), "]")
                    else "\u2014"),
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
            "Ajusta el modelo con predictores categóricos lineales.")
      })
    })

    output$plot_contrastes_gam <- renderPlot({
      fit <- modelo_gam(); req(fit, input$var_contraste_gam)
      tryCatch({
        ct    <- modelbased::estimate_contrasts(
          fit, contrast = input$var_contraste_gam,
          p_adjust = input$metodo_ajuste_gam, verbose = FALSE)
        df_ct <- as.data.frame(ct)
        char_cols <- names(df_ct)[sapply(df_ct, function(x)
          is.character(x) || is.factor(x))]
        etiqueta <- if (length(char_cols) >= 2)
          paste0(df_ct[[char_cols[1]]], " vs. ", df_ct[[char_cols[2]]])
        else paste0("Contraste ", seq_len(nrow(df_ct)))

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
            labels=c(`TRUE`="Significativo", `FALSE`="No significativo"),
            name=NULL) +
          ggplot2::labs(
            x = paste0("Diferencia en ", input$var_y),
            y = NULL,
            subtitle = paste0("Ajuste p-valores: ",
                              input$metodo_ajuste_gam, " \u00b7 IC 95%")) +
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
    # PESTAÑA 10: Performance
    # ────────────────────────────────────────────────────

    output$tabla_performance <- renderUI({
      fm <- modelo_gam(); req(fm)
      tryCatch({
        s      <- summary(fm)
        dev_ex <- round(s$dev.expl * 100, 1)
        r2_adj <- round(s$r.sq, 4)
        aic_v  <- round(AIC(fm), 2)
        gcv_v  <- round(fm$gcv.ubre, 3)
        rmse_v <- round(performance::performance_rmse(fm, verbose = FALSE), 3)
        n      <- nrow(fm$model)
        edf_tot <- round(sum(s$edf), 2)

        filas <- list(
          list(m = "n (observaciones)", v = n,
               i = "Tama\u00f1o de la muestra."),
          list(m = "EDF total", v = edf_tot,
               i = "Suma de grados de libertad efectivos de todos los splines."),
          list(m = "Devianza explicada (%)", v = paste0(dev_ex, "%"),
               i = "Proporci\u00f3n de devianza explicada. Equivalente del R\u00b2 para GAMs."),
          list(m = "R\u00b2 ajustado", v = r2_adj,
               i = "R\u00b2 ajustado por complejidad del modelo."),
          list(m = "RMSE", v = rmse_v,
               i = "Ra\u00edz del error cuadr\u00e1tico medio."),
          list(m = "AIC", v = aic_v,
               i = "Criterio de Akaike. Menor = mejor."),
          list(m = paste0("GCV/REML (", fm$method, ")"), v = gcv_v,
               i = "Puntuaci\u00f3n usada para seleccionar el par\u00e1metro de suavizado.")
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
        div(class = "text-muted small", "Ajusta el modelo primero.")
      })
    })

    cv_resultados <- reactiveVal(NULL)

    observeEvent(input$correr_cv, {
      fm <- modelo_gam()
      if (is.null(fm)) {
        showNotification("Ajusta un modelo primero.",
                         type = "warning", duration = 3)
        return()
      }
      withProgress(message = "Corriendo validaci\u00f3n cruzada...",
                   value = 0.2, {
        tryCatch({
          df_cv   <- datos_activos()
          splines <- input$sel_preds_spline_input
          lineal  <- input$preds_lineal
          req(length(splines) > 0, input$var_y)

          terminos_s <- paste0("s(", splines, ", k=", input$k_spline,
                               ", bs='", input$bs_spline, "')")
          terminos_l <- if (!is.null(lineal) && length(lineal) > 0)
            lineal else character(0)
          fm_formula <- as.formula(
            paste(input$var_y, "~",
                  paste(c(terminos_s, terminos_l), collapse = " + "))
          )

          rec <- recipes::recipe(fm_formula, data = df_cv) |>
            recipes::step_dummy(recipes::all_nominal_predictors()) |>
            recipes::step_impute_median(recipes::all_numeric_predictors()) |>
            recipes::step_zv(recipes::all_predictors())

          modelo_p <- parsnip::gen_additive_mod(
            select_features = FALSE,
            adjust_deg_free = input$k_spline
          ) |>
            parsnip::set_engine("mgcv", method = input$metodo_gam) |>
            parsnip::set_mode("regression")

          wf <- workflows::workflow() |>
            workflows::add_recipe(rec) |>
            workflows::add_model(modelo_p, formula = fm_formula)

          folds <- rsample::vfold_cv(df_cv, v = input$cv_folds,
                                     strata = if (isTRUE(input$cv_estratificado))
                                       input$var_y else NULL)

          incProgress(0.5, detail = "Evaluando folds...")

          metricas <- yardstick::metric_set(
            yardstick::rmse, yardstick::rsq, yardstick::mae
          )

          res_cv <- tune::fit_resamples(
            wf, resamples = folds, metrics = metricas,
            control = tune::control_resamples()
          )
          cv_resultados(tune::collect_metrics(res_cv))
        }, error = function(e) {
          showNotification(paste("Error en CV:", conditionMessage(e)),
                           type = "error", duration = 6)
        })
      })
    })

    output$resultado_cv <- renderUI({
      cm <- cv_resultados()
      if (is.null(cm)) return(
        div(class = "text-muted small py-3",
            bs_icon("arrow-repeat", class = "me-2"),
            "Haz clic en ", strong("Correr CV"), ".")
      )
      tarjetas <- lapply(seq_len(nrow(cm)), function(i) {
        col <- if (cm$.metric[i] == "rsq") colores$exito else colores$primario
        card(class = "text-center",
             card_body(class = "p-2",
               h4(style = paste0("color:",col,"; font-weight:700;"),
                  round(cm$mean[i], 3)),
               p(class = "small text-muted mb-0", toupper(cm$.metric[i])),
               p(class = "small text-muted mb-0",
                 paste0("\u00b1", round(cm$std_err[i], 3), " EE"))
             ))
      })
      tagList(
        do.call(layout_columns,
                c(list(col_widths = rep(4, nrow(cm))), tarjetas)),
        div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
            bs_icon("info-circle", class = "me-1"),
            strong(paste0(input$cv_folds, "-fold CV")),
            " \u2014 estimaci\u00f3n del error en datos no vistos.")
      )
    })

    # ────────────────────────────────────────────────────
    # PESTAÑA 11: Comparar modelos
    # ────────────────────────────────────────────────────

    modelos_guardados <- reactiveVal(list())

    observeEvent(input$guardar_modelo, {
      fm     <- modelo_gam()
      nombre <- trimws(input$nombre_modelo)
      if (is.null(fm)) {
        showNotification("Ajusta un modelo primero.",
                         type = "warning", duration = 3); return()
      }
      if (nchar(nombre) == 0) {
        showNotification("Escribe un nombre para el modelo.",
                         type = "warning", duration = 3); return()
      }
      lst <- modelos_guardados()
      lst[[nombre]] <- list(
        fit     = fm,
        formula = deparse(formula(fm)),
        splines = input$sel_preds_spline_input,
        lineal  = input$preds_lineal
      )
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
            div(
              p(class = "small mb-0", strong(nm)),
              p(class = "small text-muted mb-0",
                style = "font-size:0.75rem;", mg[[nm]]$formula)
            ))
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
          error = function(e) NULL
        )
        if (is.null(pm)) return(NULL)
        s <- summary(fm)
        list(nm   = nm,
             aic  = round(pm$AIC, 1),
             aicc = tryCatch(round(performance::performance_aicc(fm), 1),
                             error = function(e) NA),
             bic  = round(pm$BIC, 1),
             r2   = round(pm$R2, 3),
             dev  = round(s$dev.expl * 100, 1))
      })
      rows <- rows[!sapply(rows, is.null)]
      if (length(rows) == 0) return(NULL)
      best <- which.min(sapply(rows, function(r) r$aicc))
      tags$table(
        class = "table table-sm table-hover small mb-0",
        tags$thead(
          style = paste0("background:", colores$primario, "; color:#fff;"),
          tags$tr(tags$th("Modelo"), tags$th("AIC"), tags$th("AICc"),
                  tags$th("BIC"), tags$th("R\u00b2"), tags$th("Dev. exp. (%)"))
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
            tags$td(r$aic), tags$td(r$aicc), tags$td(r$bic),
            tags$td(r$r2),  tags$td(paste0(r$dev, "%"))
          )
        }))
      )
    })

    output$plot_comparacion <- renderPlot({
      mg <- modelos_guardados()
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
                        subtitle = "M\u00e9tricas normalizadas 0\u20131 \u00b7 mayor \u00e1rea = mejor") +
          see::theme_radar() +
          ggplot2::theme(legend.position = "bottom",
                         plot.subtitle = ggplot2::element_text(
                           color = colores$texto, size = 9))
        print(p)
      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label = "Guarda al menos 2 modelos.",
                            color = colores$texto, size=4) +
          ggplot2::theme_void()
      })
    }, res = 96)

    # ────────────────────────────────────────────────────
    # PESTAÑA 12: Código R reproducible
    # ────────────────────────────────────────────────────

    codigo_generado <- reactive({
      req(modelo_gam())
      fm      <- modelo_gam()
      splines <- input$sel_preds_spline_input
      lineal  <- input$preds_lineal
      k       <- input$k_spline
      bs      <- input$bs_spline
      metodo  <- input$metodo_gam
      yvar    <- input$var_y

      fuente <- input$fuente_datos
      carga <- if (fuente == "ejemplo_ave")
        paste0('load(system.file("app/data/birdabundance_lm.rda",\n',
               '               package = "StatModels"))\n',
               'datos <- birdabundance_lm\n')
      else if (fuente == "ejemplo_salud")
        paste0('load(system.file("app/data/birthwt_lm.rda",\n',
               '               package = "StatModels"))\n',
               'datos <- birthwt_lm\n')
      else 'datos <- read.csv("tu_archivo.csv")\n'

      terminos_s <- paste0("s(", splines, ", k=", k, ", bs='", bs, "')")
      terminos_l <- if (!is.null(lineal) && length(lineal) > 0) lineal else character(0)
      formula_str <- paste(yvar, "~",
                           paste(c(terminos_s, terminos_l), collapse = " + "))

      paste0(
        "# ── GAM con mgcv ───────────────────────────────────────\n",
        "library(mgcv)\n",
        "library(gratia)\n",
        "library(parameters)  # easystats\n",
        "library(performance) # easystats\n\n",
        "# Cargar datos\n",
        carga, "\n",
        "# Ajustar modelo\n",
        "fm <- gam(\n",
        "  formula = ", formula_str, ",\n",
        "  data    = datos,\n",
        "  method  = '", metodo, "'\n",
        ")\n\n",
        "# Resumen\n",
        "summary(fm)\n\n",
        "# Diagnóstico\n",
        "gam.check(fm)\n",
        "concurvity(fm, full = FALSE)\n\n",
        "# Visualizar efectos suaves\n",
        "draw(fm)  # gratia\n\n",
        "# Parámetros (easystats)\n",
        "model_parameters(fm)\n\n",
        "# Performance\n",
        "model_performance(fm)\n\n",
        "# Predicción\n",
        "predict(fm, newdata = datos, type = 'response')\n"
      )
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descargar_codigo <- downloadHandler(
      filename = function() paste0("gam_", Sys.Date(), ".R"),
      content  = function(file) writeLines(codigo_generado(), file)
    )

  }) # /moduleServer
} # /mod_gam_server
