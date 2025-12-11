# app_completo_integrado.R
# Integración: app_completo_shiny.R + módulo "Ficha y gráficos simulados"
# Mantengo íntegro tu script original y agrego el módulo como pestaña 6.

library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(ggplot2)
library(DT)
library(tidyr)

# -------------------------------
# Módulo: Formulación de hipótesis (tu código original)
# -------------------------------
modHipotesisUI <- function(id) {
  ns <- NS(id)
  tagList(
    HTML("
    <div style='background-color:#f9f9f9; padding:15px; border-left:5px solid #2c3e50; margin-bottom:20px;'>
      <h4><strong>📘 Cómo usar el módulo C.A.L.C.A.:</strong></h4>
      <p>Este espacio interactivo te permite aprender a formular hipótesis de forma clara, contrastable y relevante. Está dividido en cinco secciones:</p>
      <ol>
        <li><strong>Clasifica la hipótesis</strong> – Clasificación.</li>
        <li><strong>Anatomía</strong> – Construye y evalúa tu hipòtesis.</li>
        <li><strong>Laboratorio</strong> – Formula y guarda.</li>
        <li><strong>Clínica</strong> – Revisa hipótesis de otros grupos.</li>
        <li><strong>Anàlisis de hipotesis</strong> – Analisis y mapeo de una hipòtesis.</li>
      </ol>
    </div>
    "),
    
    tabsetPanel(
      # Tab 1: Clasificación
      tabPanel("1. Clasifica la hipótesis",
               h4("Clasifica el enunciado"),br(),
               p("Ejemplo: 'Las MIPYME lideradas por mujeres en Uruguay enfrentan mayores barreras de acceso al crédito formal.'"),br(),
               checkboxGroupInput(ns("hipo_tipo"), "¿Qué tipo de hipótesis es?",br(),
                                  choices = c("Descriptiva", "Explicativa", "Normativa")),
               actionButton(ns("evaluar1"), "Evaluar"),
               verbatimTextOutput(ns("feedback1"))
      ),
      
      # --- TAB 2: ANATOMÍA ---
      tabPanel("2. Anatomía de una hipótesis",
               
               h4("Anatomía de una hipótesis"),br(),
               p("Una hipótesis se compone de tres partes esenciales: una variable independiente (causa), una variable dependiente (efecto) y un contexto (variable contextual)."),
               br(),
               
               # Tabla explicativa (modelo)
               tableOutput(ns("tabla_anatomia")),
               tags$hr(),br(),
               
               h4("✏️ Construí tu hipótesis"),br(),
               p("Completá las tres partes de tu hipótesis en la tabla y luego hacé clic en “Generar hipótesis completa”."),br(),
               DT::dataTableOutput(ns("tabla_construccion")),
               br(),
               actionButton(ns("generar_hipótesis"), "Generar hipótesis completa", icon = icon("flask")),
               verbatimTextOutput(ns("hipotesis_generada")),
               
               
               br(),
               br(),
               br(),
               # Criterios de evaluación
               h4("🧭 Evalúa tu hipótesis"),br(),
               checkboxGroupInput(ns("criterios"), "¿Cumple con estos criterios?",
                                  choices = c("Claridad", "Contrastabilidad", "Relevancia", "Delimitación")),
               actionButton(ns("evaluar2"), "Evaluar"),
               verbatimTextOutput(ns("feedback2"))
      ),
      
      
      
      # Tab 3: Laboratorio
      tabPanel("3. Laboratorio de hipótesis",
               h4("Formula, justifica y guarda tu hipótesis"),br(),
               textAreaInput(ns("hipotesis_lab"), "1. Escribe tu hipótesis", rows = 3),
               helpText(em("Ejemplo: 'La mayor concientización ciudadana es la causa del aumento de denuncias por estafa digital en Uruguay.'")),br(),br(),
               selectInput(ns("tipo_lab"), "2. Tipo de hipótesis",
                           choices = c("Descriptiva", "Explicativa", "Correlacional")),
               helpText(em("Ejemplo: Esta hipótesis es explicativa porque propone una causa del aumento de denuncias.")),br(),br(),
               textAreaInput(ns("justificacion_lab"), "3. ¿Por qué es relevante?", rows = 2),
               helpText(em("Ejemplo: 'Es relevante porque permite investigar la causa del aumento de denuncias.'")),br(),br(),
               textAreaInput(ns("fuentes_lab"), "4. ¿Cómo podrías contrastarla?", rows = 2),
               helpText(em("Ejemplo: 'Podría contrastarla mediante la comparación de datos sobre campañas de concientización ciudadana y el número de denuncias registradas en distintos períodos, aplicando encuestas para medir el nivel de conocimiento sobre estafas digitales y verificando si existe una relación estadísticamente significativa entre mayor concientización y aumento de denuncias.'")),
               actionButton(ns("guardar_lab"), "Guardar hipótesis"),
               br(), br(),br(),br(),
               dataTableOutput(ns("tabla_hipotesis"))
      ),
      
      # Tab 4: Clínica de hipótesis
      tabPanel("4. Clínica de hipótesis",
               h3("Revisa hipótesis de otros grupos"),br(),br(),
               dataTableOutput(ns("tabla_hipotesis_clinica")),br(),br(),
               textAreaInput(ns("comentario_clinica"), "Escribe tu retroalimentación", rows = 2),
               actionButton(ns("enviar_comentario"), "Enviar"),
               verbatimTextOutput(ns("feedback_clinica"))
      ),
      
      # Tab 5: Investigación jurídica
      tabPanel("5. Anàlisis de hipotesis",
               h3("Mapa de hipótesis"),br(),
               textAreaInput(ns("hipotesis_final"), "Hipótesis seleccionada", rows = 2),br(),
               textInput(ns("variables"), "Variables involucradas"),br(),
               textAreaInput(ns("fuentes_final"), "Fuentes", rows = 2),br(),
               textAreaInput(ns("metodo"), "Método de contraste", rows = 2),
               actionButton(ns("guardar_final"), "Guardar diseño"),br(),br(),br(),br(),
               dataTableOutput(ns("tabla_final"))
      )
    )
  )
}

# -------------------------------
# Servidor del módulo (hipótesis)
# -------------------------------
modHipotesisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Tab 1: Clasificación ---
    observeEvent(input$evaluar1, {
      output$feedback1 <- renderText({
        if (is.null(input$hipo_tipo) || length(input$hipo_tipo) == 0) {
          "Selecciona al menos una opción."
        } else if ("Descriptiva" %in% input$hipo_tipo) {
          "¡Correcto! Es una hipótesis Descriptiva porque está afirmando una situación observable en la realidad (las barreras de acceso al crédito).."
        } else {
          "Revisa tu clasificación. Este enunciado describe un fenómeno social sin entrar en causas ni en explicaciones. , por lo tanto es Descriptiva."
        }
      })
    })
    
    # --- Tab 2: Anatomía ---
    output$tabla_anatomia <- renderTable({
      data.frame(
        Aspecto = c("Descripción", "Ejemplo"),
        `Variable independiente` = c(
          "Es la causa o el factor que se supone influye o explica el cambio.",
          "Mayor concientización ciudadana"
        ),
        `Variable dependiente` = c(
          "Es el efecto o fenómeno que se observa o mide.",
          "Aumento de denuncias por estafa digital"
        ),
        `Variable contextual` = c(
          "Es el marco o entorno en el que se da la relación causa-efecto.",
          "En Uruguay, entre 2018 y 2024"
        ),
        stringsAsFactors = FALSE
      )
    })
    
    # --- Tabla editable del estudiante (segura y estable) ---
    tabla_construccion_base <- reactiveVal(
      data.frame(
        independiente = rep("", 5),
        dependiente = rep("", 5),
        contextual = rep("", 5),
        stringsAsFactors = FALSE
      )
    ) 
    
    output$tabla_construccion <- DT::renderDT({
      DT::datatable(
        tabla_construccion_base(),
        colnames = c("Variable independiente", "Variable dependiente", "Variable contextual"),
        editable = TRUE,
        rownames = FALSE,
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE,
          autoWidth = TRUE
        )
      )
    }, server = TRUE)
    
    # --- Captura de edición segura ---
    observeEvent(input$tabla_construccion_cell_edit, {
      info <- input$tabla_construccion_cell_edit
      tabla <- tabla_construccion_base()
      tabla_actualizada <- DT::editData(tabla, info, rownames = FALSE)
      tabla_construccion_base(tabla_actualizada)
    })
    
    # --- Generar hipótesis completa ---
    observeEvent(input$generar_hipotesis, {
      tabla <- tabla_construccion_base()
      
      # Filtrar filas completas
      completas <- tabla[
        apply(tabla, 1, function(row) all(nchar(row) > 0)),
      ]
      
      if (nrow(completas) == 0) {
        output$hipotesis_generada <- renderText("⚠️ Por favor, completa al menos una fila.")
      } else {
        hipotesis <- apply(completas, 1, function(row) {
          paste0("📘 Si ", tolower(row["independiente"]),
                 ", entonces ", tolower(row["dependiente"]),
                 " en ", row["contextual"], ".")
        })
        
        output$hipotesis_generada <- renderText(paste(hipotesis, collapse = "\n\n"))
      }
    })
    # --- Tab 2.2: Anatomía ---
    observeEvent(input$evaluar2, {
      output$feedback2 <- renderText({
        faltantes <- setdiff(c("Claridad", "Contrastabilidad", "Relevancia", "Delimitación"), input$criterios)
        if (length(faltantes) == 0) {
          "¡Excelente! Tu hipótesis cumple con todos los criterios fundamentales."
        } else {
          paste("Tu hipótesis podría mejorar en:", paste(faltantes, collapse = ", "))
        }
      })
    })
    
    # --- Tab 3: Laboratorio ---
    hipotesis_df <- reactiveVal(data.frame(
      Hipótesis = character(),
      Tipo = character(),
      Justificación = character(),
      Fuentes = character(),
      stringsAsFactors = FALSE
    ))
    
    observeEvent(input$guardar_lab, {
      nueva <- data.frame(
        Hipótesis = input$hipotesis_lab,
        Tipo = input$tipo_lab,
        Justificación = input$justificacion_lab,
        Fuentes = input$fuentes_lab,
        stringsAsFactors = FALSE
      )
      hipotesis_df(rbind(hipotesis_df(), nueva))
    })
    
    output$tabla_hipotesis <- DT::renderDT({
      DT::datatable(hipotesis_df(), options = list(pageLength = 5, autoWidth = TRUE), rownames = FALSE)
    })
    
    # --- Tab 4: Clínica ---
    output$tabla_hipotesis_clinica <- DT::renderDT({
      DT::datatable(hipotesis_df(), options = list(pageLength = 7, autoWidth = TRUE), rownames = FALSE)
    })
    
    observeEvent(input$enviar_comentario, {
      output$feedback_clinica <- renderText({
        texto <- trimws(input$comentario_clinica %||% "")
        if (nchar(texto) < 10) {
          "Tu comentario es muy breve. Intenta dar una retroalimentación más detallada."
        } else {
          "¡Comentario enviado! Gracias por tu aporte."
        }
      })
    })
    
    # --- Tab 5: Investigación jurídica ---
    mapa_df <- reactiveVal(data.frame(
      Hipótesis = character(),
      Variables = character(),
      Fuentes = character(),
      Método = character(),
      stringsAsFactors = FALSE
    ))
    
    observeEvent(input$guardar_final, {
      nuevo <- data.frame(
        Hipótesis = input$hipotesis_final,
        Variables = input$variables,
        Fuentes = input$fuentes_final,
        Método = input$metodo,
        stringsAsFactors = FALSE
      )
      mapa_df(rbind(mapa_df(), nuevo))
    })
    
    output$tabla_final <- DT::renderDT({
      DT::datatable(mapa_df(), options = list(pageLength = 7, autoWidth = TRUE), rownames = FALSE)
    })
  })
}

# -------------------------------
# Módulo nuevo: Ficha y gráficos simulados
# Basado en app_ficha_grafico_boxplot.R (tu segundo script)
# -------------------------------

modFichaGraficoUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Generador de gráficos interactivos"),
      sidebarLayout(
        sidebarPanel(
          h4("Ficha (completa los campos mínimos)"),
          textInput(ns("grupo"), "Grupo:", value = ""),
          textInput(ns("tema"), "Tema:", value = ""),
          textInput(ns("vi"), "Variable independiente (VI) - etiqueta:", value = "VI"),
          textInput(ns("vd"), "Variable dependiente (VD) - etiqueta:", value = "VD"),
          textInput(ns("contexto"), "Variable contextual (opcional) - etiqueta:", value = ""),
          hr(),
          h4("Parámetros de simulación"),br(),
          sliderInput(ns("n"), "Tamaño de muestra (n):", min = 30, max = 2000, value = 200, step = 10),br(),
          numericInput(ns("effect"), "Tamaño del efecto (pendiente verdadera):", value = 0.5, step = 0.1),br(),
          numericInput(ns("noise"), "Desviación estándar del ruido:", value = 1, step = 0.1),br(),
          checkboxInput(ns("make_contextual"), "Incluir variable contextual (como factor)", value = TRUE),
          textInput(ns("contexto_niveles"), "Niveles de la variable contextual (separar con ;)", value = "L1;L2;L3"),
          hr(),
          h4("Tipo de gráfico"),br(),
          selectInput(ns("plot_type"), "Elegir tipo:", choices = c(
            "Scatter" = "scatter",
            "Barras" = "bar",
            "Torta" = "pie",
            "Boxplot" = "box"
          )),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'bar'", ns("plot_type")),
            selectInput(ns("bar_agg"), "Agrupar barras por:", choices = c("VI (cuartiles)" = "vi_bin", "Contexto" = "contexto"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'pie'", ns("plot_type")),
            selectInput(ns("pie_by"), "Torta por:", choices = c("Contexto" = "contexto", "VI (cuartiles)" = "vi_bin"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'box'", ns("plot_type")),
            checkboxInput(ns("show_outliers"), "Mostrar outliers", value = TRUE),
            checkboxInput(ns("add_jitter"), "Superponer puntos (jitter)", value = TRUE)
          ),
          hr(),
          actionButton(ns("gen"), "Generar gráfico"),
          br(), br(),
          downloadButton(ns("download_data"), "Descargar datos (CSV)")
        ),
        mainPanel(
          h4("Vista previa de la ficha"),
          verbatimTextOutput(ns("preview_ficha")),
          hr(),
          tags$details(
            tags$summary("📘 Instrucciones para usar la app (clic para desplegar)"),br(),
            tags$div(style = "margin-top:10px;",
                     HTML("
    <strong>1. Completar la ficha del proyecto</strong><br/>
    Escribí el nombre del grupo, el tema, y las variables VI (independiente), VD (dependiente) y contextual (opcional).<br/><br/>

    <strong>2. Definir niveles de la variable contextual</strong><br/>
    Si usás una variable contextual, escribí los niveles separados por punto y coma. Ejemplo:<br/>
    <code>Montevideo; Interior urbano; Interior rural</code><br/><br/>

    <strong>3. Simular datos</strong><br/>
    Ajustá el tamaño de muestra, el efecto y el ruido para generar datos simulados.<br/><br/>

    <strong>4. Elegir el tipo de gráfico</strong><br/>
    Podés generar:<br/>
    - 📈 Scatter: relación VI → VD<br/>
    - 📊 Barras: promedio de VD por grupo<br/>
    - 🥧 Torta: proporción por grupo<br/>
    - 📦 Boxplot: distribución de VD por grupo<br/><br/>

    <strong>5. Generar el gráfico</strong><br/>
    Hacé clic en <em>Generar gráfico</em> para visualizarlo.<br/><br/>

    <strong>6. Descargar los datos</strong><br/>
    Usá el botón <em>Descargar datos (CSV)</em> para guardar los datos simulados.<br/><br/>

    <strong>7. Exportar la ficha</strong><br/>
    Completá la ficha editable y descargala como archivo .docx para entregar.<br/><br/>

    <em>Consejo:</em> Podés cambiar parámetros y volver a generar el gráfico todas las veces que quieras.
    ")
            )
          ),br(),br(),
          h4("Gráfico interactivo"),br(),
          plotlyOutput(ns("plotly_main"), height = "560px"),
          hr(),
          h5("Resumen estadístico (solo para Scatter)"),
          verbatimTextOutput(ns("lm_summary")),
          hr(),
          tags$div(style = "background-color:#f4f4f4; padding:20px; border-left:5px solid #2c3e50; border-radius:5px;",
                   HTML("
  <h4>📘 Nota metodológica: Simulación e interpretación de gráficos</h4><br>

  <p><strong>1. Simulación de datos:</strong> Esta app genera datos simulados con la fórmula:</p>
  <pre>VD = efecto × VI + ruido</pre>
 <br><ul>
    <br><li><strong>VI</strong>: Variable independiente (por ejemplo, consumo de drogas), generada con distribución normal estándar.</li>
   <br><li><strong>VD</strong>: Variable dependiente (por ejemplo, violencia), calculada como función de VI más un componente aleatorio.</li>
   <br><li><strong>Ruido</strong>: Variabilidad no explicada por VI, que simula otros factores (educación, contexto, etc.).</li>
  </ul><br>

  <p><strong>2. Parámetros clave:</strong></p>
  <ul>
    <br><li><strong>Tamaño del efecto</strong>: cuánto influye VI sobre VD.</li>
    <br><li><strong>Ruido</strong>: dispersión aleatoria que afecta la claridad de la relación.</li>
  </ul><br>

  <p><strong>3. Gráfico scatter:</strong> Muestra la relación entre VI y VD. Cada punto representa un caso. Si el efecto es positivo, los puntos tienden a subir hacia la derecha. El ruido afecta la dispersión. La línea de regresión indica la tendencia general.</p>

  <br><p><strong>4. Gráfico de barras:</strong> Agrupa los valores de VI en cuartiles (Q1 a Q4) y muestra la <em>media de VD</em> en cada grupo. Si el efecto es positivo, se espera que la media de VD aumente de Q1 a Q4. Valores negativos en Q1 indican que, en promedio, los casos con menor VI (por ejemplo, bajo consumo de drogas) tienen menor VD (menos violencia).</p>

  <br><p><strong>5. Gráfico de torta:</strong> Muestra la proporción de casos por grupo (por ejemplo, regiones o cuartiles de VI). No representa relación causal, solo distribución. Es útil para visualizar cómo se reparte la muestra entre categorías.</p>

  <br><p><strong>6. Gráfico boxplot:</strong> Muestra la distribución de VD por grupo (por ejemplo, por cuartiles de VI o por contexto). Cada caja representa el rango intercuartílico (Q1 a Q3), la línea central es la mediana, y los puntos fuera de la caja son posibles outliers. Si se activa el jitter, se superponen los puntos individuales. Este gráfico permite ver la variabilidad interna de cada grupo.</p>

  <br><p><strong>7. Interpretación:</strong> Estos gráficos permiten explorar cómo se construyen relaciones estadísticas simples y cómo influyen el efecto y el ruido. Son útiles para reflexionar sobre correlación, causalidad y variabilidad. También ayudan a visualizar cómo se agrupan los datos y qué tan fuerte es la relación entre las variables.</p>
  ")
          )
        )
      )
    )
  )
}

modFichaGraficoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    simulated_data <- eventReactive(input$gen, {
      req(input$vi, input$vd)
      set.seed(123 + as.integer(Sys.time()) %% 10000)
      n <- max(10, round(input$n))
      x <- rnorm(n)
      y <- input$effect * x + rnorm(n, sd = input$noise)
      
      df <- tibble::tibble(vi_val = x, vd_val = y)
      
      if (nzchar(input$contexto) && input$make_contextual) {
        if (nzchar(input$contexto_niveles)) {
          niveles <- unlist(strsplit(input$contexto_niveles, ";"))
          niveles <- trimws(niveles)
          niveles <- niveles[niveles != ""]
          if (length(niveles) >= 2) {
            df <- df %>% mutate(contexto_val = factor(sample(niveles, n, replace = TRUE)))
          } else {
            k <- sample(2:4, 1)
            niveles <- paste0("L", seq_len(k))
            df <- df %>% mutate(contexto_val = factor(sample(niveles, n, replace = TRUE)))
          }
        } else {
          k <- sample(2:4, 1)
          niveles <- paste0("L", seq_len(k))
          df <- df %>% mutate(contexto_val = factor(sample(niveles, n, replace = TRUE)))
        }
      }
      
      df <- df %>% mutate(vi_bin = ntile(vi_val, 4) %>% paste0("Q", .))
      
      attr(df, "labels") <- list(
        vi_label = input$vi,
        vd_label = input$vd,
        contexto_label = ifelse(nzchar(input$contexto), input$contexto, NA),
        grupo = input$grupo,
        tema = input$tema
      )
      df
    }, ignoreNULL = FALSE)
    
    output$preview_ficha <- renderText({
      paste0(
        "Grupo: ", input$grupo, "\n",
        "Tema: ", input$tema, "\n",
        "VI: ", input$vi, " | VD: ", input$vd,
        ifelse(nzchar(input$contexto), paste0(" | Contexto: ", input$contexto), ""), "\n",
        "Tipo de gráfico: ", input$plot_type
      )
    })
    
    output$plotly_main <- renderPlotly({
      df <- simulated_data()
      req(nrow(df) > 0)
      labels <- attr(df, "labels")
      vi_lab <- labels$vi_label
      vd_lab <- labels$vd_label
      ctx_lab <- labels$contexto_label
      ctx_present <- "contexto_val" %in% names(df)
      pt <- input$plot_type
      
      if (pt == "scatter") {
        p <- ggplot(df, aes(x = vi_val, y = vd_val)) +
          geom_point(alpha = 0.7) +
          geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
          theme_minimal() +
          labs(x = vi_lab, y = vd_lab, title = paste0(vi_lab, " → ", vd_lab))
        
        if (ctx_present) {
          p <- ggplot(df, aes(x = vi_val, y = vd_val, color = contexto_val)) +
            geom_point(alpha = 0.8) +
            geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
            theme_minimal() +
            labs(x = vi_lab, y = vd_lab, color = ctx_lab, title = paste0(vi_lab, " → ", vd_lab))
        }
        
        ggplotly(p)
        
      } else if (pt == "bar") {
        if (input$bar_agg == "contexto" && ctx_present) {
          agg <- df %>% group_by(contexto_val) %>% summarise(mean_vd = mean(vd_val))
          p <- ggplot(agg, aes(x = contexto_val, y = mean_vd)) +
            geom_col() +
            theme_minimal() +
            labs(x = ctx_lab, y = paste0("Media de ", vd_lab), title = paste0("Media de ", vd_lab, " por ", ctx_lab))
        } else {
          agg <- df %>% group_by(vi_bin) %>% summarise(mean_vd = mean(vd_val))
          p <- ggplot(agg, aes(x = vi_bin, y = mean_vd)) +
            geom_col() +
            theme_minimal() +
            labs(x = paste0(vi_lab, " (cuartiles)"), y = paste0("Media de ", vd_lab), title = paste0("Media de ", vd_lab, " por ", vi_lab))
        }
        ggplotly(p)
        
      } else if (pt == "pie") {
        if (input$pie_by == "contexto" && ctx_present) {
          agg <- df %>% count(contexto_val)
          plot_ly(agg, labels = ~contexto_val, values = ~n, type = 'pie', textinfo = 'label+percent') %>%
            layout(title = paste0("Distribución por ", ctx_lab))
        } else {
          agg <- df %>% count(vi_bin)
          plot_ly(agg, labels = ~vi_bin, values = ~n, type = 'pie', textinfo = 'label+percent') %>%
            layout(title = paste0("Distribución por ", vi_lab, " (cuartiles)"))
        }
        
      } else if (pt == "box") {
        show_out <- input$show_outliers
        add_jit <- input$add_jitter
        
        if (ctx_present) {
          p <- ggplot(df, aes(x = contexto_val, y = vd_val, fill = contexto_val)) +
            geom_boxplot(outlier.shape = if (show_out) 16 else NA, alpha = 0.6) +
            theme_minimal() +
            labs(x = ctx_lab, y = vd_lab, title = paste0("Distribución de ", vd_lab, " por ", ctx_lab))
          if (add_jit) {
            p <- p + geom_jitter(width = 0.2, alpha = 0.4, size = 1)
          }
        } else {
          p <- ggplot(df, aes(x = vi_bin, y = vd_val, fill = vi_bin)) +
            geom_boxplot(outlier.shape = if (show_out) 16 else NA, alpha = 0.6) +
            theme_minimal() +
            labs(
              x = paste0(vi_lab, " (cuartiles)"),
              y = vd_lab,
              title = paste0("Distribución de ", vd_lab, " por ", vi_lab, " (cuartiles)")
            )
          if (add_jit) {
            p <- p + geom_jitter(width = 0.2, alpha = 0.4, size = 1)
          }
        }
        ggplotly(p)
      } else {
        plotly_empty(type = "scatter", mode = "markers")
      }
    })
    
    output$lm_summary <- renderPrint({
      req(input$plot_type == "scatter")
      df <- simulated_data()
      req(nrow(df) > 1)
      if ("contexto_val" %in% names(df)) {
        fmla <- as.formula("vd_val ~ vi_val + contexto_val")
      } else {
        fmla <- as.formula("vd_val ~ vi_val")
      }
      fit <- try(lm(fmla, data = df), silent = TRUE)
      if (inherits(fit, "try-error")) {
        cat("No se pudo ajustar el modelo.")
      } else {
        s <- summary(fit)
        cat("Regresión lineal (resumen):\n")
        print(s$call)
        cat("\nCoeficientes:\n")
        print(s$coefficients)
        cat("\nR-squared:", round(s$r.squared, 4), " | Adj R-squared:", round(s$adj.r.squared, 4), "\n")
      }
    })
    
    output$download_data <- downloadHandler(
      filename = function() {
        grp <- ifelse(nzchar(input$grupo), gsub("[^A-Za-z0-9]", "_", input$grupo), "grupo")
        paste0(Sys.Date(), "_simdata_", grp, ".csv")
      },
      content = function(file) {
        df <- simulated_data()
        readr::write_csv(df, file)
      }
    )
  })
}

# -------------------------------
# UI principal (tu código original)
# -------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-size: 18px; line-height: 1.45; background-color: #f9f9f9; }
      h1,h2,h3,h4 { color:#003366; font-weight:700; margin-top:0.6em; margin-bottom:0.4em; }
      table, th, td, label, .control-label { font-size: 17px; }
      .tab-content, .container-fluid { padding: 20px 24px; }
      iframe { border-radius: 12px; min-height: 450px; }
      .shiny-input-container input, .shiny-input-container textarea { font-size: 17px !important; }
    "))
  ),
  titlePanel("Formulación de hipótesis - FCS - Mag. José González"),
  tabsetPanel(
    tabPanel("Análisis de datos",
             sidebarLayout(
               sidebarPanel(
                 textInput("nombre_estudiante", "Nombre del estudiante:", placeholder = "Ej: María Rodríguez"),
                 textInput("hipotesis", "Escribe tu hipótesis:", placeholder = "Ej: La mayor concientización ciudadana es la causa del aumento de denuncias por estafa digital en Uruguay"),
                 textAreaInput("reflexion", "Reflexión guiada:",
                               placeholder = "Analizá si los datos respaldan tu hipótesis. ¿Qué otros factores podrían influir?",
                               rows = 5),
                 textAreaInput("conclusion", "Escribe tu conclusión final:",
                               placeholder = "Ej: Los datos respaldan parcialmente la hipótesis, pero podrían influir otros factores...",
                               rows = 4),
                 selectInput("tipo_delito", "Tipo de ciberdelito:",
                             choices = c("Estafa", "Acceso indebido", "Suplantación de identidad")),
                 sliderInput("rango_anios", "Rango de años:",
                             min = 2018, max = 2025, value = c(2020, 2024), sep = ""),
                 actionButton("evaluar", "Evaluar hipótesis"),
                 downloadButton("descargar_analisis", "Descargar hipótesis y análisis")
               ),
               mainPanel(
                 plotlyOutput("grafico"),
                 verbatimTextOutput("analisis"),br(),br(),br(),
                 DTOutput("tabla_datos"),br(),br(),br(),
                 htmlOutput("nota_metodologica")
               )
             )
    ),
    tabPanel("Formulación de hipótesis", modHipotesisUI("hipo")),
    # -------------------------------
    # Inserto aquí la nueva pestaña como pediste (se mantiene todo lo demás intacto)
    # -------------------------------
    tabPanel("Gráficos", modFichaGraficoUI("ficha"))
  )
)

# -------------------------------
# Server principal (tu código original) + llamada al nuevo módulo
# -------------------------------
server <- function(input, output, session) {
  # Iniciar módulo original
  modHipotesisServer("hipo")
  
  # Iniciar módulo nuevo (ficha y gráficos)
  modFichaGraficoServer("ficha")
  
  # Datos reactivos (archivo local) - tu original
  datos <- reactive({
    req(file.exists("ciberdelitos_uruguay.csv"))
    read_csv("ciberdelitos_uruguay.csv", show_col_types = FALSE) |>
      filter(tipo_delito == input$tipo_delito,
             año >= input$rango_anios[1],
             año <= input$rango_anios[2])
  })
  
  # Gráfico (original)
  output$grafico <- renderPlotly({
    df <- datos()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = año, y = denuncias)) +
      geom_line(linewidth = 1.2, color= "pink") +
      geom_point(size = 2) +
      labs(title = paste("Denuncias por", input$tipo_delito),
           x = "Año", y = "Cantidad de denuncias") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Análisis textual (original)
  output$analisis <- renderText({
    req(input$evaluar)
    df <- datos()
    req(nrow(df) > 0)
    resumen <- df |>
      summarise(promedio = mean(denuncias, na.rm = TRUE),
                maximo = max(denuncias, na.rm = TRUE),
                minimo = min(denuncias, na.rm = TRUE))
    paste0("📌 Hipótesis planteada:\n", input$hipotesis, "\n\n",
           "📊 Datos analizados:\n",
           "Promedio de denuncias: ", round(resumen$promedio, 1), "\n",
           "Máximo: ", resumen$maximo, "\n",
           "Mínimo: ", resumen$minimo, "\n\n",
           "🧠 Reflexión:\n¿Los datos respaldan tu hipótesis? ¿Qué otros factores podrían influir?")
  })
  
  # Tabla de datos (DT) (original)
  output$tabla_datos <- DT::renderDT({
    df <- datos()
    DT::datatable(df, options = list(pageLength = 7, autoWidth = TRUE), rownames = FALSE)
  })
  
  # Nota metodológica (HTML) (original)
  output$nota_metodologica <- renderUI({
    HTML("
      <div style='background:#f0f8ff; padding:10px; border-radius:6px; margin-top:10px;'>
        <h5>Nota metodológica</h5>
        <p>Los datos provienen del Observatorio de Criminalidad del Ministerio del Interior y del Poder Judicial. Las denuncias no implican resolución judicial. La categoría 'Estafa' incluye fraudes por redes sociales y plataformas de pago. Los datos fueron filtrados por tipo de delito y año para facilitar el análisis pedagógico.</p>
      </div>
    ")
  })
  
  # Descarga del análisis (archivo .txt) (original)
  output$descargar_analisis <- downloadHandler(
    filename = function() {
      paste0("hipotesis_analisis_", Sys.Date(), ".txt")
    },
    content = function(file) {
      req(input$evaluar)
      df <- datos()
      req(nrow(df) > 0)
      resumen <- df |>
        summarise(promedio = mean(denuncias, na.rm = TRUE),
                  maximo = max(denuncias, na.rm = TRUE),
                  minimo = min(denuncias, na.rm = TRUE))
      tabla_txt <- df |>
        arrange(año) |>
        mutate(linea = paste(año, tipo_delito, denuncias, sep = " | ")) |>
        pull(linea)
      
      contenido <- c(
        paste("👤 Nombre del estudiante:", input$nombre_estudiante),
        paste("📅 Fecha de entrega:", format(Sys.Date(), "%d/%m/%Y")),
        strrep("-", 50),
        "",
        "📌 Hipótesis planteada:",
        input$hipotesis,
        "",
        paste("📊 Tipo de ciberdelito:", input$tipo_delito),
        paste("📊 Rango de años:", input$rango_anios[1], "a", input$rango_anios[2]),
        "",
        "📈 Estadísticas:",
        paste("Promedio de denuncias:", round(resumen$promedio, 1)),
        paste("Máximo:", resumen$maximo),
        paste("Mínimo:", resumen$minimo),
        "",
        "📋 Datos filtrados:",
        "Año | Tipo de delito | Denuncias",
        tabla_txt,
        "",
        "🧠 Reflexión guiada:",
        input$reflexion,
        "",
        "📝 Conclusión final del estudiante:",
        input$conclusion,
        "",
        strrep("-", 50),
        "Nota metodológica: Los datos provienen del Observatorio de Criminalidad del Ministerio del Interior y del Poder Judicial. Las denuncias no implican resolución judicial."
      )
      
      writeLines(contenido, file)
    }
  )
}

# Run app
shinyApp(ui, server)