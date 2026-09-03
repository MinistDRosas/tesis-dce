library(shiny)
library(bslib)
library(shinyWidgets)
library(readxl)

source("R/helpers.R")
source("R/db_supabase.R")


# ==================================================
# CARGA DEL INSTRUMENTO
# ==================================================

ruta_instrumento <- "data/content/instrumento_DCE.xlsx"

instrumento <- cargar_instrumento_xlsx(
  ruta_instrumento
)

preguntas <- instrumento$preguntas
opciones <- instrumento$opciones
textos <- instrumento$textos
categorias <- instrumento$categorias
atributos <- instrumento$atributos
niveles <- instrumento$niveles


# ==================================================
# VALIDACIONES DE ESTRUCTURA
# ==================================================

validar_archivos_contenido(
  preguntas = preguntas,
  opciones = opciones,
  textos = textos,
  categorias = categorias,
  atributos = atributos,
  niveles = niveles
)

validar_catalogo_dce(
  categorias = categorias,
  atributos = atributos,
  niveles = niveles
)

validar_configuracion_restricciones_dce(
  categorias = categorias,
  atributos = atributos,
  niveles = niveles
)


# --------------------------------------------------
# Preparar categorías DCE
# --------------------------------------------------

categorias_ordenadas <- categorias[
  !is.na(categorias$categoria_id) &
    categorias$categoria_id != "",
]

categorias_ordenadas <- categorias_ordenadas[
  order(categorias_ordenadas$orden),
]

categorias_dce <- as.character(
  categorias_ordenadas$categoria_id
)

if (length(categorias_dce) == 0) {
  stop(
    "No existen categorías DCE válidas en la hoja 'categorias'."
  )
}


# ==================================================
# CONFIGURACIÓN GENERAL DE LA ENCUESTA
# ==================================================

paginas <- c(
  "bienvenida",
  "caracterizacion",
  "dce_intro",
  "dce_categoria_intro",
  "dce",
  "finalizacion"
)

# Diseño inicial eficiente: 16 choice sets por categoría,
# distribuidos en 4 bloques de 4. Cada persona responde
# 4 tareas por categoría = 12 tareas DCE en total.
# Se usan 16 sets porque Gestión y Marketing requiere estimar
# 16 parámetros (15 de atributos + 1 ASC de no contratación).
n_tareas_dce <- 4
n_bloques_dce <- 4

ruta_diseno_dce <- "data/design/diseno_dce.csv"

diseno_dce <- cargar_diseno_dce(
  ruta = ruta_diseno_dce,
  categorias_ids = categorias_dce,
  atributos = atributos,
  niveles = niveles,
  n_blocks_esperados = n_bloques_dce,
  tareas_por_bloque = n_tareas_dce
)

cat(
  "Diseño experimental cargado desde: ",
  ruta_diseno_dce,
  "\n",
  sep = ""
)


# ==================================================
# CONFIGURACIÓN DE GUARDADO DE RESPUESTAS
# ==================================================

ruta_respuestas_dce <- "data/responses"

dir.create(
  ruta_respuestas_dce,
  recursive = TRUE,
  showWarnings = FALSE
)

# Supabase se activa automáticamente cuando están presentes todas
# las variables de conexión en .Renviron / secretos del servidor.
supabase_habilitado_dce <- supabase_configurado_dce()

cat(
  "Guardado Supabase: ",
  if (isTRUE(supabase_habilitado_dce)) "habilitado" else "no configurado (solo respaldo local)",
  "\n",
  sep = ""
)


# ==================================================
# INTERFAZ GENERAL
# ==================================================

ui <- page_fluid(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  tags$head(
    tags$style(
      HTML("
        /* ----------------------------------------
           CONTENEDOR PRINCIPAL
        ---------------------------------------- */

        .contenedor-encuesta {
          width: 100%;
          max-width: 1100px;
          margin: 0 auto;
          padding: 20px 30px;
        }


        /* ----------------------------------------
           ANCHO GENERAL DE LOS CONTROLES
        ---------------------------------------- */

        .form-group {
          width: 100%;
        }

        .shiny-input-container {
          width: 100% !important;
          max-width: 100% !important;
        }

        .selectize-control,
        .selectize-input,
        .form-control {
          width: 100% !important;
        }


        /* ----------------------------------------
           TEXTOS DE PREGUNTAS Y AYUDAS
        ---------------------------------------- */

        .control-label {
          width: 100%;
          line-height: 1.4;
        }

        .radio,
        .checkbox {
          line-height: 1.4;
          margin-bottom: 8px;
        }

        .text-muted {
          max-width: 900px;
          line-height: 1.5;
        }


        /* ----------------------------------------
           INTRODUCCIÓN GENERAL AL DCE
        ---------------------------------------- */

        .dce-caja {
          border: 1px solid #d9e2e8;
          border-radius: 10px;
          padding: 18px 20px;
          margin: 18px 0;
          background: #f8fafb;
        }

        .dce-opciones-ejemplo {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 16px;
          margin: 18px 0;
        }

        .dce-opcion-ejemplo {
          border: 1px solid #ced7de;
          border-radius: 10px;
          padding: 16px;
          background: white;
        }

        .dce-ninguna {
          border: 1px dashed #9aa8b2;
          border-radius: 10px;
          padding: 14px 16px;
          margin: 14px 0 20px 0;
        }


        /* ----------------------------------------
           INTRODUCCIÓN DE CATEGORÍA
        ---------------------------------------- */

        .categoria-intro {
          border-left: 4px solid #2c3e50;
          padding: 16px 20px;
          margin: 18px 0 24px 0;
          background: #f8fafb;
        }

        .dce-descripcion-categoria {
          font-size: 1.02rem;
          line-height: 1.55;
          max-width: 950px;
        }

        .dce-atributos-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 16px;
          margin-top: 20px;
          margin-bottom: 24px;
        }

        .dce-atributo-card {
          border: 1px solid #d9e2e8;
          border-radius: 10px;
          padding: 18px;
          background: #ffffff;
        }

        .dce-atributo-titulo {
          margin-top: 0;
          margin-bottom: 8px;
        }

        .dce-atributo-descripcion {
          margin-bottom: 12px;
          line-height: 1.45;
        }

        .dce-ayuda-niveles {
          border-top: 1px solid #e7ecef;
          padding-top: 10px;
        }

        .dce-ayuda-niveles summary {
          cursor: pointer;
          font-weight: 600;
          color: #2c3e50;
          list-style: none;
          user-select: none;
        }

        .dce-ayuda-niveles summary::-webkit-details-marker {
          display: none;
        }

        .dce-icono-info {
          font-size: 1.05rem;
          margin-right: 4px;
        }

        .dce-niveles-contenido {
          margin-top: 12px;
        }

        .dce-lista-niveles {
          margin-bottom: 0;
          padding-left: 20px;
        }

        .dce-lista-niveles li {
          margin-bottom: 9px;
          line-height: 1.45;
        }

        /* ----------------------------------------
           COMPARACIÓN DE ALTERNATIVAS A/B
        ---------------------------------------- */

        .dce-instruccion-tarea {
          margin-bottom: 16px;
          line-height: 1.5;
        }

        .dce-comparacion-wrapper {
          width: 100%;
          overflow-x: auto;
          margin: 18px 0 22px 0;
          border: 1px solid #d9e2e8;
          border-radius: 10px;
          background: #ffffff;
        }

        .dce-comparacion {
          width: 100%;
          border-collapse: collapse;
          table-layout: fixed;
        }

        .dce-comparacion th,
        .dce-comparacion td {
          padding: 14px 16px;
          border-bottom: 1px solid #e7ecef;
          vertical-align: middle;
        }

        .dce-comparacion tbody tr:last-child td {
          border-bottom: none;
        }

        .dce-comparacion th {
          background: #f3f6f8;
          font-weight: 700;
          text-align: center;
        }

        .dce-encabezado-atributo {
          width: 30%;
          text-align: left !important;
        }

        .dce-encabezado-alternativa {
          width: 35%;
        }

        .dce-celda-atributo {
          background: #fafbfc;
          line-height: 1.35;
        }

        .dce-celda-alternativa {
          text-align: center;
          line-height: 1.4;
        }

        .dce-nivel-etiqueta {
          font-weight: 400;
        }

        .dce-nivel-detalle {
          display: inline;
          margin-left: 5px;
        }

        .dce-nivel-detalle summary {
          display: inline-block;
          list-style: none;
        }

        .dce-nivel-detalle summary::-webkit-details-marker {
          display: none;
        }

        .dce-nivel-info {
          display: inline-block;
          margin-left: 4px;
          cursor: pointer;
          color: #6c757d;
          font-size: 0.92rem;
          font-weight: 400;
          user-select: none;
        }

        .dce-nivel-info:hover,
        .dce-nivel-info:focus {
          color: #2c3e50;
        }

        .dce-nivel-detalle[open] {
          display: block;
          margin: 7px auto 0 auto;
        }

        .dce-nivel-detalle[open] .dce-nivel-info {
          margin-left: 0;
        }

        .dce-nivel-descripcion {
          margin-top: 6px;
          padding: 8px 10px;
          border-left: 3px solid #cbd5dc;
          background: #f8fafb;
          color: #495057;
          font-weight: 400;
          font-size: 0.94rem;
          line-height: 1.4;
          text-align: left;
        }

        .dce-fila-precio td {
          background: #f8fafb;
          border-top: 2px solid #cbd5dc;
        }

        .dce-fila-precio .dce-nivel-etiqueta {
          font-weight: 400;
        }

        .dce-eleccion {
          margin-top: 12px;
          padding: 16px 18px;
          border: 1px solid #d9e2e8;
          border-radius: 10px;
          background: #f8fafb;
        }

        .dce-eleccion .form-group {
          margin-bottom: 0;
        }

        .modo-desarrollo {
          font-size: 0.92rem;
          color: #6c757d;
          margin-top: 16px;
        }


        /* ----------------------------------------
           RESPONSIVE
        ---------------------------------------- */

        @media (max-width: 700px) {
          
          .contenedor-encuesta {
            padding: 15px;
          }

          .dce-opciones-ejemplo,
          .dce-atributos-grid {
            grid-template-columns: 1fr;
          }

          /* En móviles, cada atributo se presenta verticalmente:
             nombre del atributo -> Alternativa A -> Alternativa B.
             Así se evita el desplazamiento horizontal de la tabla. */
          .dce-comparacion-wrapper {
            overflow-x: visible;
          }

          .dce-comparacion {
            min-width: 0;
            width: 100%;
            table-layout: auto;
          }

          .dce-comparacion thead {
            display: none;
          }

          .dce-comparacion tbody,
          .dce-comparacion tr,
          .dce-comparacion td {
            display: block;
            width: 100%;
          }

          .dce-comparacion tbody tr {
            padding: 0;
          }

          .dce-comparacion tbody tr:not(:last-child) {
            border-bottom: 1px solid #d9e2e8;
          }

          .dce-comparacion th,
          .dce-comparacion td,
          .dce-comparacion tbody tr:last-child td {
            border-bottom: none;
          }

          .dce-celda-atributo {
            padding: 12px 14px 9px 14px;
            background: #f3f6f8;
          }

          .dce-celda-alternativa {
            padding: 10px 14px;
            text-align: left;
          }

          .dce-celda-alternativa:nth-child(2) {
            border-top: 1px solid #eef2f4;
          }

          .dce-celda-alternativa:nth-child(3) {
            border-top: 1px dashed #e1e7eb;
          }

          .dce-celda-alternativa:nth-child(2)::before,
          .dce-celda-alternativa:nth-child(3)::before {
            display: block;
            margin-bottom: 4px;
            color: #2c3e50;
            font-size: 0.88rem;
            font-weight: 700;
          }

          .dce-celda-alternativa:nth-child(2)::before {
            content: 'Alternativa A';
          }

          .dce-celda-alternativa:nth-child(3)::before {
            content: 'Alternativa B';
          }

          .dce-fila-precio {
            border-top: 2px solid #cbd5dc;
          }

          .dce-fila-precio td {
            border-top: none;
          }

          .dce-nivel-detalle[open] {
            margin-left: 0;
            margin-right: 0;
          }

          /* Las opciones de respuesta también se apilan en pantallas angostas. */
          .dce-eleccion .radio-inline {
            display: block;
            margin-left: 0;
            margin-bottom: 8px;
          }
        }
      ")
    )
  ),
  
  div(
    class = "contenedor-encuesta",
    uiOutput("pantalla_ui")
  )
)


# ==================================================
# SERVIDOR
# ==================================================

server <- function(input, output, session) {
  
  
  # --------------------------------------------------
  # IDENTIFICADOR ÚNICO DE SESIÓN
  # --------------------------------------------------
  
  id_sesion <- paste0(
    format(
      Sys.time(),
      "%Y%m%d%H%M%S"
    ),
    "_",
    sample(
      100000:999999,
      1
    )
  )
  
  
  # --------------------------------------------------
  # ORDEN ALEATORIO DE CATEGORÍAS
  # --------------------------------------------------
  
  orden_categorias <- sample(
    categorias_dce
  )
  
  
  # --------------------------------------------------
  # ASIGNAR VERSIÓN/BLOQUE Y PREPARAR TAREAS FIJAS
  # --------------------------------------------------

  bloque_dce <- sample(
    seq_len(n_bloques_dce),
    1
  )

  tareas_generadas <- construir_tareas_desde_diseno_dce(
    diseno = diseno_dce,
    bloque = bloque_dce,
    categorias_ids = categorias_dce,
    atributos = atributos,
    randomizar_lados = TRUE,
    randomizar_orden = TRUE
  )


  # --------------------------------------------------
  # ESTADO GENERAL DE LA ENCUESTA
  # --------------------------------------------------
  
  estado <- reactiveValues(
    pagina = 1,
    id_sesion = id_sesion,
    inicio = Sys.time(),
    fin = NULL,
    bloque_dce = bloque_dce,
    respuestas = list(),
    elegibilidad = NULL,
    consentimiento_aceptado = FALSE,
    consentimiento_fecha = NULL,
    orden_categorias = orden_categorias,
    indice_categoria = 1,
    categoria_actual = orden_categorias[1],
    tarea_actual = 1,
    tareas_dce = tareas_generadas,
    elecciones_dce = list(),
    guardado_final = FALSE,
    rutas_guardado = NULL,
    supabase_guardado = FALSE,
    supabase_error = NULL,
    supabase_pendiente = FALSE
  )
  
  
  # --------------------------------------------------
  # INFORMACIÓN TEMPORAL EN CONSOLA
  # --------------------------------------------------
  
  cat(
    "Nueva sesión iniciada:",
    id_sesion,
    "\n"
  )
  
  cat(
    "Inicio de sesión:",
    format(Sys.time()),
    "\n"
  )
  
  cat(
    "Orden de categorías:",
    paste(
      orden_categorias,
      collapse = " -> "
    ),
    "\n"
  )
  
  cat(
    "Categoría inicial:",
    orden_categorias[1],
    "\n"
  )

  cat(
    "Bloque experimental asignado:",
    bloque_dce,
    "de",
    n_bloques_dce,
    "\n"
  )
  
  # El borrador comienza a guardarse solo después de que
  # el participante acepte explícitamente participar.

  total_paginas <- length(
    paginas
  )
  
  
  # ==================================================
  # INTERFAZ SEGÚN PÁGINA
  # ==================================================
  
  output$pantalla_ui <- renderUI({
    
    pagina_actual <- estado$pagina
    nombre_pagina <- paginas[pagina_actual]
    
    
    # --------------------------------------------------
    # Elección y tarea DCE previamente guardadas
    # --------------------------------------------------

    eleccion_guardada <- NULL
    tarea_dce_actual <- NULL

    if (nombre_pagina == "dce") {

      eleccion_guardada <-
        estado$elecciones_dce[[id_tarea_dce(estado)]]

      tarea_dce_actual <-
        obtener_tarea_generada_dce(
          tareas_dce = estado$tareas_dce,
          categoria_id = estado$categoria_actual,
          tarea_numero = estado$tarea_actual
        )
    }


    # --------------------------------------------------
    # Información de categoría actual
    # --------------------------------------------------
    
    posicion_categoria <- match(
      estado$categoria_actual,
      categorias_ordenadas$categoria_id
    )
    
    nombre_categoria_actual <-
      if (!is.na(posicion_categoria)) {
        
        as.character(
          categorias_ordenadas$nombre[[posicion_categoria]]
        )
        
      } else {
        
        estado$categoria_actual
      }
    
    
    # --------------------------------------------------
    # Progreso global
    # --------------------------------------------------
    
    total_progreso <-
      4 +
      length(estado$orden_categorias) *
      (n_tareas_dce + 1)
    
    progreso_actual <- switch(
      nombre_pagina,
      
      "bienvenida" = 1,
      
      "caracterizacion" = 2,
      
      "dce_intro" = 3,
      
      "dce_categoria_intro" =
        4 +
        (estado$indice_categoria - 1) *
        (n_tareas_dce + 1),
      
      "dce" =
        4 +
        (estado$indice_categoria - 1) *
        (n_tareas_dce + 1) +
        estado$tarea_actual,
      
      "finalizacion" = total_progreso
    )
    
    
    # ==================================================
    # PÁGINA COMPLETA
    # ==================================================
    
    tagList(
      
      progressBar(
        id = "progreso",
        value = progreso_actual,
        total = total_progreso,
        display_pct = TRUE
      ),
      
      br(),
      
      switch(
        nombre_pagina,
        
        
        # ------------------------------------------------
        # BIENVENIDA
        # ------------------------------------------------
        
        "bienvenida" = tagList(
          
          crear_textos_pagina(
            nombre_pagina = "bienvenida",
            textos = textos
          )
        ),
        
        
        # ------------------------------------------------
        # CARACTERIZACIÓN
        # ------------------------------------------------
        
        "caracterizacion" = tagList(
          
          h2(
            "Caracterización"
          ),
          
          crear_textos_pagina(
            nombre_pagina = "caracterizacion",
            textos = textos
          ),
          
          crear_preguntas_pagina(
            nombre_pagina = "caracterizacion",
            preguntas = preguntas,
            opciones = opciones,
            respuestas = estado$respuestas
          )
        ),
        
        
        # ------------------------------------------------
        # INTRODUCCIÓN GENERAL AL DCE
        # ------------------------------------------------
        
        "dce_intro" = tagList(
          
          h2(
            "Ejercicios de elección"
          ),
          
          crear_textos_pagina(
            nombre_pagina = "dce_intro",
            textos = textos
          ),
          
          div(
            class = "dce-caja",
            
            p(
              strong(
                "¿Cómo funcionan los ejercicios?"
              )
            ),
            
            p(
              paste0(
                "La encuesta está dividida en tres secciones. ",
                "En cada una verá cuatro situaciones hipotéticas ",
                "relacionadas con servicios de inteligencia artificial."
              )
            ),
            
            p(
              paste0(
                "En cada tarea se mostrarán dos alternativas, A y B. ",
                "Ambas representarán servicios posibles para su empresa, ",
                "pero sus características y su precio mensual podrán ser distintos."
              )
            )
          ),
          
          div(
            class = "dce-opciones-ejemplo",
            
            div(
              class = "dce-opcion-ejemplo",
              h4("Alternativa A"),
              p(
                paste0(
                  "Un servicio hipotético con una combinación ",
                  "determinada de características y precio."
                )
              )
            ),
            
            div(
              class = "dce-opcion-ejemplo",
              h4("Alternativa B"),
              p(
                paste0(
                  "Otro servicio hipotético con una combinación ",
                  "distinta de características y precio."
                )
              )
            )
          ),
          
          div(
            class = "dce-ninguna",
            
            strong(
              "No contrataría ninguna"
            ),
            
            p(
              style = "margin-bottom: 0; margin-top: 6px;",
              paste0(
                "Si en una tarea no contrataría ninguna de las dos ",
                "alternativas para su empresa, puede seleccionar ",
                "esta opción."
              )
            )
          ),
          
          p(
            paste0(
              "No existen respuestas correctas o incorrectas. ",
              "Seleccione en cada caso la alternativa que represente ",
              "mejor lo que elegiría para la empresa."
            )
          )
        ),
        
        
        # ------------------------------------------------
        # INTRODUCCIÓN DINÁMICA DE CATEGORÍA
        # ------------------------------------------------
        
        "dce_categoria_intro" = tagList(
          
          p(
            class = "text-muted",
            paste0(
              "Sección ",
              estado$indice_categoria,
              " de ",
              length(estado$orden_categorias)
            )
          ),
          
          crear_introduccion_categoria_dce(
            categoria_id = estado$categoria_actual,
            categorias = categorias_ordenadas,
            atributos = atributos,
            niveles = niveles
          )
        ),
        
        
        # ------------------------------------------------
        # DCE CON DISEÑO EXPERIMENTAL BLOQUEADO
        # ------------------------------------------------

        "dce" = tagList(

          p(
            class = "text-muted",
            paste0(
              "Sección ",
              estado$indice_categoria,
              " de ",
              length(estado$orden_categorias)
            )
          ),

          h2(
            nombre_categoria_actual
          ),

          h4(
            paste0(
              "Tarea ",
              estado$tarea_actual,
              " de ",
              n_tareas_dce
            )
          ),

          p(
            class = "dce-instruccion-tarea",
            paste0(
              "Compare las características de ambos servicios y seleccione ",
              "la alternativa que preferiría contratar para la empresa. ",
              "Puede hacer clic en ⓘ para ver la explicación de cualquier nivel."
            )
          ),

          crear_comparacion_tarea_dce(
            tarea = tarea_dce_actual,
            categoria_id = estado$categoria_actual,
            atributos = atributos,
            niveles = niveles
          ),

          div(
            class = "dce-eleccion",

            radioButtons(
              inputId = "eleccion_dce",
              label = "¿Cuál elegiría?",
              choices = c(
                "Alternativa A" = "A",
                "Alternativa B" = "B",
                "No contrataría ninguna" = "ninguna"
              ),
              selected =
                if (is.null(eleccion_guardada)) {
                  character(0)
                } else {
                  eleccion_guardada
                },
              inline = TRUE
            )
          ),

          br()
        ),


        # ------------------------------------------------
        # FINALIZACIÓN
        # ------------------------------------------------
        
        "finalizacion" = tagList(
          
          crear_textos_pagina(
            nombre_pagina = "finalizacion",
            textos = textos
          ),

          if (isTRUE(estado$guardado_final)) {
            div(
              class = "dce-caja",
              p(
                style = "margin-bottom: 0;",
                paste0(
                  "Código de respuesta: ",
                  estado$id_sesion
                )
              )
            )
          }
        )
      ),
      
      hr(),
      
      
      # ==================================================
      # BOTONES DE NAVEGACIÓN
      # ==================================================
      
      fluidRow(
        
        column(
          width = 6,
          
          if (pagina_actual > 1) {
            
            actionButton(
              inputId = "anterior",
              label = "Anterior"
            )
          }
        ),
        
        column(
          width = 6,
          
          div(
            style = "text-align: right;",
            
            if (nombre_pagina == "bienvenida") {
              
              actionButton(
                inputId = "aceptar_consentimiento",
                label = if (isTRUE(estado$consentimiento_aceptado)) {
                  "Continuar"
                } else {
                  "Acepto participar"
                },
                class = "btn-primary"
              )
              
            } else if (pagina_actual < total_paginas) {
              
              actionButton(
                inputId = "siguiente",
                label = "Siguiente",
                class = "btn-primary"
              )
            }
          )
        )
      )
    )
  })
  
  
  # ==================================================
  # CONSENTIMIENTO INFORMADO
  # ==================================================
  
  observeEvent(
    input$aceptar_consentimiento,
    {
      
      if (!isTRUE(estado$consentimiento_aceptado)) {
        estado$consentimiento_aceptado <- TRUE
        estado$consentimiento_fecha <- Sys.time()
      }
      
      guardar_borrador_sesion_dce(
        estado = estado,
        ruta_base = ruta_respuestas_dce
      )
      
      estado$pagina <- match(
        "caracterizacion",
        paginas
      )
    }
  )
  
  
  # ==================================================
  # SISTEMAS DIGITALES:
  # "NINGUNO" MUTUAMENTE EXCLUYENTE
  # ==================================================
  
  seleccion_sistemas_anterior <- reactiveVal(
    character(0)
  )
  
  observeEvent(
    input$sistemas_digitales,
    {
      
      seleccion_actual <- input$sistemas_digitales
      
      if (is.null(seleccion_actual)) {
        seleccion_actual <- character(0)
      }
      
      seleccion_anterior <-
        seleccion_sistemas_anterior()
      
      nuevas <- setdiff(
        seleccion_actual,
        seleccion_anterior
      )
      
      if ("ninguno" %in% nuevas) {
        
        seleccion_corregida <- "ninguno"
        
        seleccion_sistemas_anterior(
          seleccion_corregida
        )
        
        updateCheckboxGroupInput(
          session = session,
          inputId = "sistemas_digitales",
          selected = seleccion_corregida
        )
        
        return()
      }
      
      if (
        "ninguno" %in% seleccion_actual &&
        length(seleccion_actual) > 1
      ) {
        
        seleccion_corregida <- setdiff(
          seleccion_actual,
          "ninguno"
        )
        
        seleccion_sistemas_anterior(
          seleccion_corregida
        )
        
        updateCheckboxGroupInput(
          session = session,
          inputId = "sistemas_digitales",
          selected = seleccion_corregida
        )
        
        return()
      }
      
      seleccion_sistemas_anterior(
        seleccion_actual
      )
    },
    ignoreInit = TRUE
  )
  
  
  # ==================================================
  # BOTÓN SIGUIENTE
  # ==================================================
  
  observeEvent(
    input$siguiente,
    {
      
      nombre_pagina_actual <-
        paginas[estado$pagina]
      
      
      # ------------------------------------------------
      # Validar página
      # ------------------------------------------------
      
      validacion <- validar_pagina(
        nombre_pagina_actual,
        input,
        preguntas
      )
      
      if (!validacion$valido) {
        
        showNotification(
          validacion$mensaje,
          type = "error",
          duration = 4
        )
        
        return()
      }
      
      
      # ------------------------------------------------
      # Guardar respuestas antes de avanzar
      # ------------------------------------------------
      
      guardar_pagina(
        nombre_pagina_actual,
        estado,
        input,
        preguntas
      )
      
      
      # ------------------------------------------------
      # Calcular elegibilidad
      # ------------------------------------------------
      
      if (nombre_pagina_actual == "caracterizacion") {
        
        estado$elegibilidad <-
          evaluar_elegibilidad(
            estado$respuestas
          )
      }

      # Actualizar borrador después de guardar la página actual.
      guardar_borrador_sesion_dce(
        estado = estado,
        ruta_base = ruta_respuestas_dce
      )
      
      
      # ==================================================
      # NAVEGACIÓN
      # ==================================================
      
      if (nombre_pagina_actual == "dce_intro") {
        
        estado$pagina <- match(
          "dce_categoria_intro",
          paginas
        )
        
        return()
      }
      
      if (nombre_pagina_actual == "dce_categoria_intro") {
        
        estado$pagina <- match(
          "dce",
          paginas
        )
        
        return()
      }
      
      if (nombre_pagina_actual == "dce") {
        
        # Siguiente tarea de la misma categoría
        
        if (estado$tarea_actual < n_tareas_dce) {
          
          estado$tarea_actual <-
            estado$tarea_actual + 1
          
          return()
        }
        
        # Siguiente categoría
        
        if (
          estado$indice_categoria <
          length(estado$orden_categorias)
        ) {
          
          estado$indice_categoria <-
            estado$indice_categoria + 1
          
          actualizar_categoria_dce(
            estado
          )
          
          estado$tarea_actual <- 1
          
          estado$pagina <- match(
            "dce_categoria_intro",
            paginas
          )
          
          return()
        }
        
        # Terminar DCE y guardar resultados finales

        estado$fin <- Sys.time()

        resultado_guardado <- tryCatch(
          {
            guardar_resultados_dual_dce(
              estado = estado,
              preguntas = preguntas,
              categorias = categorias_ordenadas,
              atributos = atributos,
              niveles = niveles,
              ruta_base = ruta_respuestas_dce,
              usar_supabase = supabase_habilitado_dce
            )
          },
          error = function(e) {
            showNotification(
              paste0(
                "No fue posible guardar las respuestas: ",
                conditionMessage(e)
              ),
              type = "error",
              duration = 8
            )
            NULL
          }
        )

        if (is.null(resultado_guardado)) {
          estado$fin <- NULL
          return()
        }

        estado$rutas_guardado <- resultado_guardado$local
        estado$guardado_final <- TRUE
        estado$supabase_guardado <- isTRUE(resultado_guardado$supabase_ok)
        estado$supabase_error <- resultado_guardado$supabase_error
        estado$supabase_pendiente <- !is.null(resultado_guardado$pendiente_supabase)

        if (isTRUE(estado$supabase_guardado)) {
          cat(
            "Sesión sincronizada con Supabase: ",
            estado$id_sesion,
            "\n",
            sep = ""
          )
        } else if (isTRUE(estado$supabase_pendiente)) {
          cat(
            "ADVERTENCIA: sesión guardada localmente, sincronización Supabase pendiente: ",
            estado$id_sesion,
            " | ",
            estado$supabase_error,
            "\n",
            sep = ""
          )

          showNotification(
            "Las respuestas se guardaron localmente, pero la sincronización online quedó pendiente.",
            type = "warning",
            duration = 8
          )
        }

        estado$pagina <- match(
          "finalizacion",
          paginas
        )
        
        return()
      }
      
      
      # ------------------------------------------------
      # Navegación general
      # ------------------------------------------------
      
      if (estado$pagina < total_paginas) {
        
        estado$pagina <-
          estado$pagina + 1
      }
    }
  )
  
  
  # ==================================================
  # BOTÓN ANTERIOR
  # ==================================================
  
  observeEvent(
    input$anterior,
    {
      
      nombre_pagina_actual <-
        paginas[estado$pagina]
      
      
      # ------------------------------------------------
      # Guardar antes de retroceder
      # ------------------------------------------------
      
      guardar_pagina(
        nombre_pagina_actual,
        estado,
        input,
        preguntas
      )

      guardar_borrador_sesion_dce(
        estado = estado,
        ruta_base = ruta_respuestas_dce
      )
      
      
      # ==================================================
      # NAVEGACIÓN HACIA ATRÁS
      # ==================================================
      
      if (nombre_pagina_actual == "dce") {
        
        # Tarea anterior
        
        if (estado$tarea_actual > 1) {
          
          estado$tarea_actual <-
            estado$tarea_actual - 1
          
          return()
        }
        
        # Primera tarea -> introducción de categoría
        
        estado$pagina <- match(
          "dce_categoria_intro",
          paginas
        )
        
        return()
      }
      
      if (nombre_pagina_actual == "dce_categoria_intro") {
        
        # Primera categoría -> introducción general
        
        if (estado$indice_categoria == 1) {
          
          estado$pagina <- match(
            "dce_intro",
            paginas
          )
          
          return()
        }
        
        # Categoría anterior -> última tarea
        
        estado$indice_categoria <-
          estado$indice_categoria - 1
        
        actualizar_categoria_dce(
          estado
        )
        
        estado$tarea_actual <-
          n_tareas_dce
        
        estado$pagina <- match(
          "dce",
          paginas
        )
        
        return()
      }
      
      if (nombre_pagina_actual == "finalizacion") {

        # Si el participante vuelve a editar la última tarea,
        # el guardado final deja de considerarse definitivo hasta
        # que vuelva a avanzar a la pantalla de finalización.
        resultado_invalidacion <- invalidar_guardado_dual_dce(
          id_sesion = estado$id_sesion,
          ruta_base = ruta_respuestas_dce,
          usar_supabase = supabase_habilitado_dce
        )

        if (
          isTRUE(supabase_habilitado_dce) &&
          !isTRUE(resultado_invalidacion$supabase_ok)
        ) {
          cat(
            "ADVERTENCIA: invalidación Supabase pendiente para sesión ",
            estado$id_sesion,
            " | ",
            resultado_invalidacion$supabase_error,
            "\n",
            sep = ""
          )
        }

        estado$guardado_final <- FALSE
        estado$fin <- NULL
        estado$rutas_guardado <- NULL
        estado$supabase_guardado <- FALSE
        estado$supabase_error <- resultado_invalidacion$supabase_error
        estado$supabase_pendiente <- !is.null(resultado_invalidacion$pendiente_supabase)
        
        estado$pagina <- match(
          "dce",
          paginas
        )
        
        return()
      }
      
      
      # ------------------------------------------------
      # Navegación general hacia atrás
      # ------------------------------------------------
      
      if (estado$pagina > 1) {
        
        estado$pagina <-
          estado$pagina - 1
      }
    }
  )

  session$onSessionEnded(function() {
    guardado_final_actual <- shiny::isolate(
      estado$guardado_final
    )

    consentimiento_actual <- shiny::isolate(
      estado$consentimiento_aceptado
    )

    if (
      !isTRUE(guardado_final_actual) &&
      isTRUE(consentimiento_actual)
    ) {
      try(
        guardar_borrador_sesion_dce(
          estado = estado,
          ruta_base = ruta_respuestas_dce
        ),
        silent = TRUE
      )
    }
  })
}


# ==================================================
# EJECUTAR APLICACIÓN
# ==================================================

shinyApp(
  ui = ui,
  server = server
)
