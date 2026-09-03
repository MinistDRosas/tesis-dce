# ==================================================
# CARGA DEL INSTRUMENTO DESDE XLSX
# ==================================================

cargar_instrumento_xlsx <- function(ruta) {
  
  hojas_requeridas <- c(
    "preguntas",
    "opciones",
    "textos"
  )
  
  hojas_disponibles <- readxl::excel_sheets(ruta)
  
  hojas_faltantes <- setdiff(
    hojas_requeridas,
    hojas_disponibles
  )
  
  if (length(hojas_faltantes) > 0) {
    stop(
      paste0(
        "Faltan las siguientes hojas en el archivo XLSX: ",
        paste(hojas_faltantes, collapse = ", ")
      )
    )
  }
  
  contenido <- list(
    
    preguntas = readxl::read_excel(
      ruta,
      sheet = "preguntas"
    ),
    
    opciones = readxl::read_excel(
      ruta,
      sheet = "opciones"
    ),
    
    textos = readxl::read_excel(
      ruta,
      sheet = "textos"
    )
  )
  
  
  # Hojas que utilizaremos más adelante.
  # Se cargan solo si existen.
  
  if ("categorias" %in% hojas_disponibles) {
    
    contenido$categorias <- readxl::read_excel(
      ruta,
      sheet = "categorias"
    )
  }
  
  if ("atributos" %in% hojas_disponibles) {
    
    contenido$atributos <- readxl::read_excel(
      ruta,
      sheet = "atributos"
    )
  }
  
  if ("niveles" %in% hojas_disponibles) {
    
    contenido$niveles <- readxl::read_excel(
      ruta,
      sheet = "niveles"
    )
  }
  
  contenido
}


# ==================================================
# GUARDADO DE RESPUESTAS GENERALES
# ==================================================

guardar_respuesta <- function(
    estado,
    nombre,
    valor
) {
  
  estado$respuestas[[nombre]] <- valor
}


guardar_pagina <- function(
    nombre_pagina,
    estado,
    input,
    preguntas
) {
  
  preguntas_pagina <- preguntas[
    !is.na(preguntas$pagina) &
      preguntas$pagina == nombre_pagina,
  ]
  
  if (nrow(preguntas_pagina) > 0) {
    
    for (id in preguntas_pagina$id) {
      
      if (is.na(id) || id == "") {
        next
      }
      
      valor <- input[[id]]
      
      guardar_respuesta(
        estado,
        id,
        valor
      )
    }
  }
  
  
  # Elecciones de prueba del DCE
  
  if (nombre_pagina == "dce") {
    
    guardar_eleccion_dce(
      estado,
      input$eleccion_dce
    )
  }
}


# ==================================================
# VALIDACIÓN DE RESPUESTAS
# ==================================================

es_obligatoria <- function(valor) {
  
  if (is.logical(valor)) {
    return(isTRUE(valor))
  }
  
  if (length(valor) == 0 || is.na(valor)) {
    return(FALSE)
  }
  
  valor <- tolower(
    trimws(
      as.character(valor)
    )
  )
  
  valor %in% c(
    "true",
    "verdadero",
    "1",
    "si",
    "sí",
    "yes"
  )
}


respuesta_vacia <- function(valor) {
  
  if (is.null(valor)) {
    return(TRUE)
  }
  
  if (length(valor) == 0) {
    return(TRUE)
  }
  
  if (all(is.na(valor))) {
    return(TRUE)
  }
  
  if (
    all(
      trimws(
        as.character(valor)
      ) == ""
    )
  ) {
    return(TRUE)
  }
  
  FALSE
}


validar_pagina <- function(
    nombre_pagina,
    input,
    preguntas
) {
  
  pertenece_pagina <-
    !is.na(preguntas$pagina) &
    preguntas$pagina == nombre_pagina
  
  obligatorias <- vapply(
    preguntas$obligatoria,
    es_obligatoria,
    logical(1)
  )
  
  preguntas_pagina <- preguntas[
    pertenece_pagina & obligatorias,
  ]
  
  
  if (nrow(preguntas_pagina) > 0) {
    
    for (i in seq_len(nrow(preguntas_pagina))) {
      
      fila <- preguntas_pagina[i, ]
      
      id <- as.character(
        fila$id[[1]]
      )
      
      texto <- as.character(
        fila$texto[[1]]
      )
      
      tipo <- as.character(
        fila$tipo[[1]]
      )
      
      valor <- input[[id]]
      
      
      # --------------------------------------------
      # Pregunta obligatoria sin respuesta
      # --------------------------------------------
      
      if (respuesta_vacia(valor)) {
        
        return(
          list(
            valido = FALSE,
            mensaje = paste0(
              "Por favor, responda: ",
              texto
            )
          )
        )
      }
      
      
      # --------------------------------------------
      # Validación adicional de campos numéricos
      # --------------------------------------------
      
      if (tipo == "numeric") {
        
        valor_numerico <- suppressWarnings(
          as.numeric(valor)
        )
        
        if (
          length(valor_numerico) == 0 ||
          is.na(valor_numerico)
        ) {
          
          return(
            list(
              valido = FALSE,
              mensaje = paste0(
                "Ingrese un valor numérico válido en: ",
                texto
              )
            )
          )
        }
        
        
        # Mínimo definido en Excel
        
        if (
          "min" %in% names(fila) &&
          !is.na(fila$min[[1]])
        ) {
          
          minimo <- as.numeric(
            fila$min[[1]]
          )
          
          if (valor_numerico < minimo) {
            
            return(
              list(
                valido = FALSE,
                mensaje = paste0(
                  "El valor mínimo permitido en \"",
                  texto,
                  "\" es ",
                  minimo,
                  "."
                )
              )
            )
          }
        }
        
        
        # Si step = 1, exigimos número entero
        
        if (
          "step" %in% names(fila) &&
          !is.na(fila$step[[1]])
        ) {
          
          paso <- as.numeric(
            fila$step[[1]]
          )
          
          if (
            paso == 1 &&
            abs(
              valor_numerico -
              round(valor_numerico)
            ) > sqrt(.Machine$double.eps)
          ) {
            
            return(
              list(
                valido = FALSE,
                mensaje = paste0(
                  "Ingrese un número entero en: ",
                  texto
                )
              )
            )
          }
        }
      }
    }
  }
  
  
  # Validación temporal para las tareas DCE de prueba
  
  if (nombre_pagina == "dce") {
    
    if (respuesta_vacia(input$eleccion_dce)) {
      
      return(
        list(
          valido = FALSE,
          mensaje =
            "Seleccione una alternativa antes de continuar."
        )
      )
    }
  }
  
  
  list(
    valido = TRUE,
    mensaje = NULL
  )
}


# ==================================================
# ESTADO DEL DCE
# ==================================================

actualizar_categoria_dce <- function(estado) {
  
  estado$categoria_actual <-
    estado$orden_categorias[
      estado$indice_categoria
    ]
}


id_tarea_dce <- function(estado) {
  
  paste0(
    estado$categoria_actual,
    "_tarea_",
    estado$tarea_actual
  )
}


guardar_eleccion_dce <- function(
    estado,
    valor
) {
  
  id <- id_tarea_dce(estado)
  
  estado$elecciones_dce[[id]] <- valor
}


# ==================================================
# VALIDACIÓN DE LA ESTRUCTURA DEL INSTRUMENTO
# ==================================================

validar_archivos_contenido <- function(
    preguntas,
    opciones,
    textos,
    categorias = NULL,
    atributos = NULL,
    niveles = NULL
) {
  
  columnas_preguntas <- c(
    "id",
    "pagina",
    "orden",
    "texto",
    "tipo",
    "obligatoria",
    "ayuda",
    "placeholder",
    "min",
    "step"
  )
  
  columnas_opciones <- c(
    "pregunta_id",
    "valor",
    "etiqueta",
    "orden"
  )
  
  columnas_textos <- c(
    "id",
    "pagina",
    "orden",
    "tipo",
    "texto"
  )
  
  
  faltantes_preguntas <- setdiff(
    columnas_preguntas,
    names(preguntas)
  )
  
  faltantes_opciones <- setdiff(
    columnas_opciones,
    names(opciones)
  )
  
  faltantes_textos <- setdiff(
    columnas_textos,
    names(textos)
  )
  
  
  if (length(faltantes_preguntas) > 0) {
    
    stop(
      paste0(
        "Faltan columnas en la hoja 'preguntas': ",
        paste(
          faltantes_preguntas,
          collapse = ", "
        )
      )
    )
  }
  
  
  if (length(faltantes_opciones) > 0) {
    
    stop(
      paste0(
        "Faltan columnas en la hoja 'opciones': ",
        paste(
          faltantes_opciones,
          collapse = ", "
        )
      )
    )
  }
  
  
  if (length(faltantes_textos) > 0) {
    
    stop(
      paste0(
        "Faltan columnas en la hoja 'textos': ",
        paste(
          faltantes_textos,
          collapse = ", "
        )
      )
    )
  }
  
  
  # Comprobación de identificadores duplicados
  
  ids_preguntas <- preguntas$id[
    !is.na(preguntas$id) &
      preguntas$id != ""
  ]
  
  if (anyDuplicated(ids_preguntas) > 0) {
    
    stop(
      "Existen identificadores duplicados en la hoja 'preguntas'."
    )
  }
  
  
  ids_textos <- textos$id[
    !is.na(textos$id) &
      textos$id != ""
  ]
  
  if (anyDuplicated(ids_textos) > 0) {
    
    stop(
      "Existen identificadores duplicados en la hoja 'textos'."
    )
  }
  
  
  TRUE
}


# ==================================================
# CREACIÓN DE UNA PREGUNTA
# ==================================================

crear_pregunta <- function(
    fila,
    opciones,
    valor_guardado = NULL
) {
  
  id <- as.character(
    fila$id[[1]]
  )
  
  tipo <- as.character(
    fila$tipo[[1]]
  )
  
  texto <- as.character(
    fila$texto[[1]]
  )
  
  ayuda <- as.character(
    fila$ayuda[[1]]
  )
  
  
  # ------------------------------------------------
  # Validaciones básicas
  # ------------------------------------------------
  
  if (is.na(id) || id == "") {
    
    return(
      div(
        style = "color:red;",
        "Error: pregunta sin ID."
      )
    )
  }
  
  
  if (is.na(tipo) || tipo == "") {
    
    return(
      div(
        style = "color:red;",
        paste(
          "Error: la pregunta",
          id,
          "no tiene tipo."
        )
      )
    )
  }
  
  
  if (is.na(texto)) {
    texto <- ""
  }
  
  
  if (is.na(ayuda)) {
    ayuda <- ""
  }
  
  
  # ------------------------------------------------
  # Opciones asociadas a la pregunta
  # ------------------------------------------------
  
  opciones_pregunta <-
    opciones[
      !is.na(opciones$pregunta_id) &
        opciones$pregunta_id == id,
      ,
      drop = FALSE
    ]
  
  
  if (
    nrow(opciones_pregunta) > 0 &&
    "orden" %in% names(opciones_pregunta)
  ) {
    
    opciones_pregunta <-
      opciones_pregunta[
        order(opciones_pregunta$orden),
        ,
        drop = FALSE
      ]
  }
  
  
  choices <- NULL
  
  if (nrow(opciones_pregunta) > 0) {
    
    choices <- stats::setNames(
      as.character(
        opciones_pregunta$valor
      ),
      as.character(
        opciones_pregunta$etiqueta
      )
    )
  }
  
  
  # ------------------------------------------------
  # Texto de ayuda
  # ------------------------------------------------
  
  label_ui <- texto
  
  if (nzchar(ayuda)) {
    
    label_ui <- tagList(
      texto,
      tags$div(
        class = "text-muted",
        style = "
          font-weight: normal;
          margin-top: 4px;
          margin-bottom: 4px;
        ",
        ayuda
      )
    )
  }
  
  
  # ------------------------------------------------
  # PLACEHOLDER
  # ------------------------------------------------
  
  placeholder_texto <- ""
  
  if (
    "placeholder" %in% names(fila) &&
    !is.na(fila$placeholder[[1]]) &&
    as.character(fila$placeholder[[1]]) != ""
  ) {
    
    placeholder_texto <-
      as.character(
        fila$placeholder[[1]]
      )
  }
  
  
  # ------------------------------------------------
  # RADIO
  # ------------------------------------------------
  
  if (tipo == "radio") {
    
    return(
      radioButtons(
        inputId = id,
        label = label_ui,
        choices = choices,
        selected =
          if (is.null(valor_guardado)) {
            character(0)
          } else {
            valor_guardado
          }
      )
    )
  }
  
  
  # ------------------------------------------------
  # SELECT
  # ------------------------------------------------
  
  if (tipo == "select") {
    
    return(
      selectInput(
        inputId = id,
        label = label_ui,
        choices = c(
          "Seleccione una alternativa" = "",
          choices
        ),
        selected =
          if (is.null(valor_guardado)) {
            ""
          } else {
            valor_guardado
          }
      )
    )
  }
  
  
  # ------------------------------------------------
  # NUMERIC
  # ------------------------------------------------
  
  if (tipo == "numeric") {
    
    minimo <- 0
    
    if (
      "min" %in% names(fila) &&
      !is.na(fila$min[[1]])
    ) {
      
      minimo <- as.numeric(
        fila$min[[1]]
      )
    }
    
    
    paso <- 1
    
    if (
      "step" %in% names(fila) &&
      !is.na(fila$step[[1]])
    ) {
      
      paso <- as.numeric(
        fila$step[[1]]
      )
    }
    
    
    return(
      numericInput(
        inputId = id,
        label = label_ui,
        value =
          if (is.null(valor_guardado)) {
            NA_real_
          } else {
            valor_guardado
          },
        min = minimo,
        step = paso
      )
    )
  }
  
  
  # ------------------------------------------------
  # CHECKBOX
  # ------------------------------------------------
  
  if (tipo == "checkbox") {
    
    return(
      checkboxGroupInput(
        inputId = id,
        label = label_ui,
        choices = choices,
        selected =
          if (is.null(valor_guardado)) {
            character(0)
          } else {
            valor_guardado
          }
      )
    )
  }
  
  
  # ------------------------------------------------
  # TEXT
  # ------------------------------------------------
  
  if (tipo == "text") {
    
    return(
      textInput(
        inputId = id,
        label = label_ui,
        value =
          if (is.null(valor_guardado)) {
            ""
          } else {
            valor_guardado
          },
        placeholder = placeholder_texto
      )
    )
  }
  
  
  # ------------------------------------------------
  # TIPO NO RECONOCIDO
  # ------------------------------------------------
  
  tags$p(
    style = "color:red;",
    paste(
      "Tipo de pregunta no reconocido:",
      tipo
    )
  )
}


# ==================================================
# CREACIÓN DE TODAS LAS PREGUNTAS DE UNA PÁGINA
# ==================================================

crear_preguntas_pagina <- function(
    nombre_pagina,
    preguntas,
    opciones,
    respuestas
) {
  
  preguntas_pagina <- preguntas[
    !is.na(preguntas$pagina) &
      preguntas$pagina == nombre_pagina,
  ]
  
  
  if (nrow(preguntas_pagina) == 0) {
    return(NULL)
  }
  
  
  preguntas_pagina <- preguntas_pagina[
    order(preguntas_pagina$orden),
  ]
  
  
  controles <- lapply(
    seq_len(nrow(preguntas_pagina)),
    function(i) {
      
      fila <- preguntas_pagina[i, ]
      
      id <- as.character(
        fila$id[[1]]
      )
      
      valor_guardado <-
        estado_valor_seguro(
          respuestas,
          id
        )
      
      crear_pregunta(
        fila = fila,
        opciones = opciones,
        valor_guardado = valor_guardado
      )
    }
  )
  
  tagList(controles)
}


estado_valor_seguro <- function(
    respuestas,
    id
) {
  
  if (
    is.null(respuestas) ||
    is.null(respuestas[[id]])
  ) {
    
    return(NULL)
  }
  
  respuestas[[id]]
}


# ==================================================
# CREACIÓN DE TEXTOS DE UNA PÁGINA
# ==================================================

crear_textos_pagina <- function(
    nombre_pagina,
    textos
) {
  
  textos_pagina <- textos[
    !is.na(textos$pagina) &
      textos$pagina == nombre_pagina,
  ]
  
  
  if (nrow(textos_pagina) == 0) {
    return(NULL)
  }
  
  
  textos_pagina <- textos_pagina[
    order(textos_pagina$orden),
  ]
  
  
  elementos <- lapply(
    seq_len(nrow(textos_pagina)),
    function(i) {
      
      fila <- textos_pagina[i, ]
      
      id <- as.character(
        fila$id[[1]]
      )
      
      tipo <- fila$tipo[[1]]
      contenido <- fila$texto[[1]]
      
      
      # Celda tipo vacía
      
      if (
        is.na(tipo) ||
        as.character(tipo) == ""
      ) {
        
        return(
          tags$p(
            style = "color:red;",
            paste(
              "El texto",
              id,
              "no tiene un tipo definido."
            )
          )
        )
      }
      
      
      # Texto vacío
      
      if (
        is.na(contenido) ||
        as.character(contenido) == ""
      ) {
        
        return(
          tags$p(
            style = "color:red;",
            paste(
              "El texto",
              id,
              "no tiene contenido."
            )
          )
        )
      }
      
      
      tipo <- as.character(tipo)
      contenido <- as.character(contenido)
      
      
      if (tipo == "titulo") {
        
        return(
          h2(contenido)
        )
      }
      
      
      if (tipo == "subtitulo") {
        
        return(
          h4(contenido)
        )
      }
      
      
      if (tipo == "parrafo") {
        
        return(
          p(contenido)
        )
      }
      
      
      tags$p(
        style = "color:red;",
        paste(
          "Tipo de texto no reconocido:",
          tipo
        )
      )
    }
  )
  
  
  tagList(elementos)
}


# ==================================================
# ELEGIBILIDAD PARA LA MUESTRA PRINCIPAL
# ==================================================

evaluar_elegibilidad <- function(respuestas) {
  
  # ----------------------------------------------
  # Actividad principal
  # ----------------------------------------------
  
  cumple_actividad <-
    !is.null(respuestas$actividad_principal) &&
    respuestas$actividad_principal ==
    "comercio_minorista"
  
  
  # ----------------------------------------------
  # Ventas anuales
  # ----------------------------------------------
  
  ventas_validas <- c(
    "hasta_2400_uf",
    "2400_25000_uf",
    "25000_100000_uf"
  )
  
  cumple_ventas <-
    !is.null(respuestas$ventas_anuales) &&
    respuestas$ventas_anuales %in%
    ventas_validas
  
  
  # ----------------------------------------------
  # Comuna
  # ----------------------------------------------
  
  comunas_validas <- c(
    "vina_del_mar",
    "quilpue"
  )
  
  cumple_comuna <-
    !is.null(respuestas$comuna) &&
    respuestas$comuna %in%
    comunas_validas
  
  
  # ----------------------------------------------
  # Cargo
  # ----------------------------------------------
  
  cargos_validos <- c(
    "dueno_socio",
    "gerencia",
    "jefatura",
    "otro"
  )
  
  cumple_cargo <-
    !is.null(respuestas$cargo) &&
    respuestas$cargo %in%
    cargos_validos
  
  
  # ----------------------------------------------
  # Elegibilidad general
  # ----------------------------------------------
  
  elegible_muestra <-
    cumple_actividad &&
    cumple_ventas &&
    cumple_comuna &&
    cumple_cargo
  
  
  list(
    cumple_actividad = cumple_actividad,
    cumple_ventas = cumple_ventas,
    cumple_comuna = cumple_comuna,
    cumple_cargo = cumple_cargo,
    elegible_muestra = elegible_muestra
  )
}


# ==================================================
# OPCIÓN "NINGUNO" EN CHECKBOXES
# ==================================================

resolver_seleccion_ninguno <- function(
    seleccion,
    valor_ninguno = "ninguno"
) {
  
  if (
    is.null(seleccion) ||
    length(seleccion) == 0
  ) {
    
    return(character(0))
  }
  
  seleccion <- as.character(
    seleccion
  )
  
  if (
    valor_ninguno %in% seleccion &&
    length(seleccion) > 1
  ) {
    
    seleccion <- setdiff(
      seleccion,
      valor_ninguno
    )
  }
  
  seleccion
}

# ==================================================
# VALIDACIÓN DEL CATÁLOGO DCE
# ==================================================

validar_catalogo_dce <- function(
    categorias,
    atributos,
    niveles
) {
  
  columnas_categorias <- c(
    "categoria_id",
    "nombre",
    "descripcion",
    "orden"
  )
  
  columnas_atributos <- c(
    "categoria_id",
    "atributo_id",
    "nombre",
    "descripcion",
    "orden"
  )
  
  columnas_niveles <- c(
    "categoria_id",
    "atributo_id",
    "nivel_id",
    "etiqueta",
    "descripcion",
    "orden"
  )
  
  if (
    is.null(categorias) ||
    !all(columnas_categorias %in% names(categorias))
  ) {
    
    stop(
      paste0(
        "La hoja 'categorias' debe contener las columnas: ",
        paste(columnas_categorias, collapse = ", ")
      )
    )
  }
  
  if (
    is.null(atributos) ||
    !all(columnas_atributos %in% names(atributos))
  ) {
    
    stop(
      paste0(
        "La hoja 'atributos' debe contener las columnas: ",
        paste(columnas_atributos, collapse = ", ")
      )
    )
  }
  
  if (
    is.null(niveles) ||
    !all(columnas_niveles %in% names(niveles))
  ) {
    
    stop(
      paste0(
        "La hoja 'niveles' debe contener las columnas: ",
        paste(columnas_niveles, collapse = ", ")
      )
    )
  }
  
  categorias_validas <- as.character(
    categorias$categoria_id[
      !is.na(categorias$categoria_id) &
      categorias$categoria_id != ""
    ]
  )
  
  if (length(categorias_validas) == 0) {
    stop("No existen categorías DCE válidas.")
  }
  
  claves_atributos <- paste(
    atributos$categoria_id,
    atributos$atributo_id,
    sep = "::"
  )
  
  if (anyDuplicated(claves_atributos) > 0) {
    
    stop(
      paste0(
        "Existen atributos DCE duplicados. ",
        "La combinación categoria_id + atributo_id debe ser única."
      )
    )
  }
  
  categorias_atributos <- unique(
    as.character(
      atributos$categoria_id[
        !is.na(atributos$categoria_id)
      ]
    )
  )
  
  categorias_atributos_invalidas <- setdiff(
    categorias_atributos,
    categorias_validas
  )
  
  if (length(categorias_atributos_invalidas) > 0) {
    
    stop(
      paste0(
        "La hoja 'atributos' contiene categoria_id inexistentes: ",
        paste(categorias_atributos_invalidas, collapse = ", ")
      )
    )
  }
  
  claves_niveles <- paste(
    niveles$categoria_id,
    niveles$atributo_id,
    niveles$nivel_id,
    sep = "::"
  )
  
  if (anyDuplicated(claves_niveles) > 0) {
    
    stop(
      paste0(
        "Existen niveles DCE duplicados. ",
        "La combinación categoria_id + atributo_id + nivel_id debe ser única."
      )
    )
  }
  
  claves_nivel_atributo <- paste(
    niveles$categoria_id,
    niveles$atributo_id,
    sep = "::"
  )
  
  atributos_inexistentes <- setdiff(
    unique(claves_nivel_atributo),
    unique(claves_atributos)
  )
  
  if (length(atributos_inexistentes) > 0) {
    
    stop(
      paste0(
        "La hoja 'niveles' contiene referencias a atributos inexistentes: ",
        paste(atributos_inexistentes, collapse = ", ")
      )
    )
  }
  
  for (clave in unique(claves_atributos)) {
    
    cantidad_niveles <- sum(
      claves_nivel_atributo == clave,
      na.rm = TRUE
    )
    
    if (cantidad_niveles < 2) {
      
      stop(
        paste0(
          "El atributo DCE '",
          clave,
          "' debe tener al menos dos niveles."
        )
      )
    }
  }
  
  TRUE
}


# ==================================================
# CONSULTAS DEL CATÁLOGO DCE
# ==================================================

obtener_categoria_dce <- function(
    categoria_id,
    categorias
) {
  
  filas <- categorias[
    !is.na(categorias$categoria_id) &
    categorias$categoria_id == categoria_id,
    ,
    drop = FALSE
  ]
  
  if (nrow(filas) == 0) {
    return(NULL)
  }
  
  filas[1, , drop = FALSE]
}


obtener_atributos_categoria <- function(
    categoria_id,
    atributos
) {
  
  resultado <- atributos[
    !is.na(atributos$categoria_id) &
    atributos$categoria_id == categoria_id,
    ,
    drop = FALSE
  ]
  
  if (nrow(resultado) == 0) {
    return(resultado)
  }
  
  resultado[
    order(resultado$orden),
    ,
    drop = FALSE
  ]
}


obtener_niveles_atributo <- function(
    categoria_id,
    atributo_id,
    niveles
) {
  
  resultado <- niveles[
    !is.na(niveles$categoria_id) &
    !is.na(niveles$atributo_id) &
    niveles$categoria_id == categoria_id &
    niveles$atributo_id == atributo_id,
    ,
    drop = FALSE
  ]
  
  if (nrow(resultado) == 0) {
    return(resultado)
  }
  
  resultado[
    order(resultado$orden),
    ,
    drop = FALSE
  ]
}


# ==================================================
# AYUDA VISUAL DE ATRIBUTOS Y NIVELES DCE
# ==================================================

crear_tarjeta_atributo_dce <- function(
    fila_atributo,
    niveles_atributo
) {
  
  nombre_atributo <- as.character(
    fila_atributo$nombre[[1]]
  )
  
  descripcion_atributo <- as.character(
    fila_atributo$descripcion[[1]]
  )
  
  if (is.na(nombre_atributo)) {
    nombre_atributo <- ""
  }
  
  if (is.na(descripcion_atributo)) {
    descripcion_atributo <- ""
  }
  
  elementos_niveles <- lapply(
    seq_len(nrow(niveles_atributo)),
    function(i) {
      
      fila_nivel <- niveles_atributo[i, ]
      
      etiqueta_nivel <- as.character(
        fila_nivel$etiqueta[[1]]
      )
      
      descripcion_nivel <- as.character(
        fila_nivel$descripcion[[1]]
      )
      
      if (is.na(etiqueta_nivel)) {
        etiqueta_nivel <- ""
      }
      
      if (is.na(descripcion_nivel)) {
        descripcion_nivel <- ""
      }
      
      tags$li(
        tags$strong(etiqueta_nivel),
        if (nzchar(descripcion_nivel)) {
          tags$span(
            " — ",
            descripcion_nivel
          )
        }
      )
    }
  )
  
  div(
    class = "dce-atributo-card",
    
    h4(
      class = "dce-atributo-titulo",
      nombre_atributo
    ),
    
    if (nzchar(descripcion_atributo)) {
      p(
        class = "dce-atributo-descripcion",
        descripcion_atributo
      )
    },
    
    tags$details(
      class = "dce-ayuda-niveles",
      
      tags$summary(
        tags$span(
          class = "dce-icono-info",
          "ⓘ"
        ),
        " Ver niveles"
      ),
      
      div(
        class = "dce-niveles-contenido",
        
        tags$ul(
          class = "dce-lista-niveles",
          elementos_niveles
        )
      )
    )
  )
}


# ==================================================
# INTRODUCCIÓN DINÁMICA DE UNA CATEGORÍA DCE
# ==================================================

crear_introduccion_categoria_dce <- function(
    categoria_id,
    categorias,
    atributos,
    niveles
) {
  
  categoria <- obtener_categoria_dce(
    categoria_id,
    categorias
  )
  
  if (is.null(categoria)) {
    
    return(
      tags$p(
        style = "color:red;",
        paste0(
          "No se encontró la categoría DCE: ",
          categoria_id
        )
      )
    )
  }
  
  nombre_categoria <- as.character(
    categoria$nombre[[1]]
  )
  
  descripcion_categoria <- as.character(
    categoria$descripcion[[1]]
  )
  
  if (is.na(nombre_categoria)) {
    nombre_categoria <- categoria_id
  }
  
  if (is.na(descripcion_categoria)) {
    descripcion_categoria <- ""
  }
  
  atributos_categoria <- obtener_atributos_categoria(
    categoria_id,
    atributos
  )
  
  if (nrow(atributos_categoria) == 0) {
    
    return(
      tagList(
        h2(nombre_categoria),
        tags$p(
          style = "color:red;",
          "Esta categoría no tiene atributos cargados."
        )
      )
    )
  }
  
  tarjetas <- lapply(
    seq_len(nrow(atributos_categoria)),
    function(i) {
      
      fila_atributo <- atributos_categoria[i, ]
      
      atributo_id <- as.character(
        fila_atributo$atributo_id[[1]]
      )
      
      niveles_atributo <- obtener_niveles_atributo(
        categoria_id = categoria_id,
        atributo_id = atributo_id,
        niveles = niveles
      )
      
      crear_tarjeta_atributo_dce(
        fila_atributo = fila_atributo,
        niveles_atributo = niveles_atributo
      )
    }
  )
  
  tagList(
    
    h2(
      nombre_categoria
    ),
    
    if (nzchar(descripcion_categoria)) {
      p(
        class = "dce-descripcion-categoria",
        descripcion_categoria
      )
    },
    
    div(
      class = "categoria-intro",
      
      p(
        paste0(
          "En las tareas de esta sección, cada alternativa estará descrita ",
          "mediante las características que se muestran a continuación."
        )
      ),
      
      p(
        style = "margin-bottom: 0;",
        paste0(
          "En cada característica aparecerá uno de sus niveles. ",
          "Puede abrir “ⓘ Ver niveles” para revisar qué significa cada uno."
        )
      )
    ),
    
    div(
      class = "dce-atributos-grid",
      tarjetas
    )
  )
}

# ==================================================
# DEFINICIÓN DE RESTRICCIONES DCE
# ==================================================

obtener_restricciones_dce <- function() {
  
  list(
    
    list(
      restriccion_id = "GM-R1",
      categoria_id = "gestion_marketing",
      descripcion = paste0(
        "El servicio no puede excluir simultáneamente análisis e inteligencia comercial, ",
        "predicción de demanda, gestión de precios y generación de contenido."
      ),
      condiciones = c(
        bi_analisis = "no_incluido",
        prediccion_demanda = "no_incluida",
        gestion_precios = "no_incluida",
        generacion_contenido = "no_incluida"
      )
    ),
    
    list(
      restriccion_id = "AC-R1",
      categoria_id = "atencion_cliente",
      descripcion = paste0(
        "El servicio debe disponer de al menos un canal de atención: ",
        "canal digital o atención telefónica con IA."
      ),
      condiciones = c(
        canales_digitales = "no_incluidos",
        atencion_telefonica = "no_incluida"
      )
    ),
    
    list(
      restriccion_id = "AC-R2",
      categoria_id = "atencion_cliente",
      descripcion = paste0(
        "Un servicio que gestiona la solicitud no puede combinar simultáneamente ",
        "sin integración y sin transferencia integrada a atención humana."
      ),
      condiciones = c(
        autonomia = "gestiona_solicitud",
        integracion = "sin_integracion",
        continuidad_humana = "sin_transferencia"
      )
    ),
    
    list(
      restriccion_id = "IL-R1",
      categoria_id = "inventario_logistica",
      descripcion = paste0(
        "El servicio no puede excluir simultáneamente control de inventario, ",
        "reposición y distribución/ruteo."
      ),
      condiciones = c(
        control_inventario = "no_incluido",
        reposicion = "no_incluida",
        distribucion = "no_incluida"
      )
    ),
    
    list(
      restriccion_id = "IL-R2",
      categoria_id = "inventario_logistica",
      descripcion = paste0(
        "La reposición automática requiere integración automática; ",
        "no es compatible con carga manual de datos."
      ),
      condiciones = c(
        reposicion = "reposicion_automatica",
        integracion = "carga_manual"
      )
    ),
    
    list(
      restriccion_id = "IL-R3",
      categoria_id = "inventario_logistica",
      descripcion = paste0(
        "La optimización dinámica de rutas requiere integración automática; ",
        "no es compatible con carga manual de datos."
      ),
      condiciones = c(
        distribucion = "optimizacion_dinamica",
        integracion = "carga_manual"
      )
    )
  )
}


# ==================================================
# RESUMEN LEGIBLE DE RESTRICCIONES
# ==================================================

resumen_restricciones_dce <- function() {
  
  restricciones <- obtener_restricciones_dce()
  
  data.frame(
    restriccion_id = vapply(
      restricciones,
      function(x) x$restriccion_id,
      character(1)
    ),
    categoria_id = vapply(
      restricciones,
      function(x) x$categoria_id,
      character(1)
    ),
    descripcion = vapply(
      restricciones,
      function(x) x$descripcion,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}


# ==================================================
# VALIDAR QUE LAS RESTRICCIONES COINCIDAN CON EL XLSX
# ==================================================

validar_configuracion_restricciones_dce <- function(
    categorias,
    atributos,
    niveles
) {
  
  restricciones <- obtener_restricciones_dce()
  
  ids_restricciones <- vapply(
    restricciones,
    function(x) x$restriccion_id,
    character(1)
  )
  
  if (anyDuplicated(ids_restricciones) > 0) {
    stop(
      "Existen restriccion_id duplicados en la configuración DCE."
    )
  }
  
  categorias_validas <- as.character(
    categorias$categoria_id[
      !is.na(categorias$categoria_id) &
        categorias$categoria_id != ""
    ]
  )
  
  for (restriccion in restricciones) {
    
    restriccion_id <- restriccion$restriccion_id
    categoria_id <- restriccion$categoria_id
    condiciones <- restriccion$condiciones
    
    if (!(categoria_id %in% categorias_validas)) {
      stop(
        paste0(
          "La restricción ",
          restriccion_id,
          " utiliza una categoría inexistente: ",
          categoria_id,
          "."
        )
      )
    }
    
    if (
      is.null(names(condiciones)) ||
      any(names(condiciones) == "")
    ) {
      stop(
        paste0(
          "La restricción ",
          restriccion_id,
          " contiene condiciones sin atributo_id."
        )
      )
    }
    
    for (atributo_id in names(condiciones)) {
      
      nivel_id <- as.character(
        condiciones[[atributo_id]]
      )
      
      existe_atributo <- any(
        !is.na(atributos$categoria_id) &
        !is.na(atributos$atributo_id) &
        atributos$categoria_id == categoria_id &
        atributos$atributo_id == atributo_id
      )
      
      if (!existe_atributo) {
        stop(
          paste0(
            "La restricción ",
            restriccion_id,
            " referencia un atributo inexistente: ",
            categoria_id,
            "::",
            atributo_id,
            "."
          )
        )
      }
      
      existe_nivel <- any(
        !is.na(niveles$categoria_id) &
        !is.na(niveles$atributo_id) &
        !is.na(niveles$nivel_id) &
        niveles$categoria_id == categoria_id &
        niveles$atributo_id == atributo_id &
        niveles$nivel_id == nivel_id
      )
      
      if (!existe_nivel) {
        stop(
          paste0(
            "La restricción ",
            restriccion_id,
            " referencia un nivel inexistente: ",
            categoria_id,
            "::",
            atributo_id,
            "::",
            nivel_id,
            "."
          )
        )
      }
    }
  }
  
  TRUE
}


# ==================================================
# NORMALIZAR UN PERFIL DCE
# ==================================================

normalizar_perfil_dce <- function(perfil) {
  
  if (is.data.frame(perfil)) {
    
    if (nrow(perfil) != 1) {
      stop(
        "Un perfil DCE entregado como data.frame debe contener exactamente una fila."
      )
    }
    
    perfil <- as.list(
      perfil[1, , drop = FALSE]
    )
  }
  
  if (
    is.atomic(perfil) &&
    !is.null(names(perfil))
  ) {
    perfil <- as.list(perfil)
  }
  
  if (!is.list(perfil)) {
    stop(
      paste0(
        "El perfil DCE debe entregarse como lista, vector nombrado ",
        "o data.frame de una fila."
      )
    )
  }
  
  if (
    is.null(names(perfil)) ||
    any(names(perfil) == "")
  ) {
    stop(
      "Todos los valores de un perfil DCE deben tener un atributo_id como nombre."
    )
  }
  
  if (anyDuplicated(names(perfil)) > 0) {
    stop(
      "Un perfil DCE no puede contener atributo_id duplicados."
    )
  }
  
  lapply(
    perfil,
    function(x) {
      
      if (length(x) != 1 || is.na(x)) {
        return(NA_character_)
      }
      
      as.character(x)
    }
  )
}


# ==================================================
# VALIDAR UN PERFIL DCE
# ==================================================

validar_perfil_dce <- function(
    perfil,
    categoria_id,
    atributos = NULL,
    niveles = NULL,
    exigir_perfil_completo = TRUE
) {
  
  perfil <- normalizar_perfil_dce(
    perfil
  )
  
  errores_estructura <- character(0)
  
  if (!is.null(atributos)) {
    
    atributos_categoria <- obtener_atributos_categoria(
      categoria_id = categoria_id,
      atributos = atributos
    )
    
    if (nrow(atributos_categoria) == 0) {
      
      errores_estructura <- c(
        errores_estructura,
        paste0(
          "No existen atributos para la categoría '",
          categoria_id,
          "'."
        )
      )
      
    } else {
      
      ids_atributos <- as.character(
        atributos_categoria$atributo_id
      )
      
      if (isTRUE(exigir_perfil_completo)) {
        
        faltantes <- setdiff(
          ids_atributos,
          names(perfil)
        )
        
        if (length(faltantes) > 0) {
          errores_estructura <- c(
            errores_estructura,
            paste0(
              "Faltan atributos en el perfil: ",
              paste(faltantes, collapse = ", "),
              "."
            )
          )
        }
      }
      
      sobrantes <- setdiff(
        names(perfil),
        ids_atributos
      )
      
      if (length(sobrantes) > 0) {
        errores_estructura <- c(
          errores_estructura,
          paste0(
            "El perfil contiene atributos que no pertenecen a la categoría: ",
            paste(sobrantes, collapse = ", "),
            "."
          )
        )
      }
    }
  }
  
  if (
    !is.null(atributos) &&
    !is.null(niveles)
  ) {
    
    atributos_presentes <- intersect(
      names(perfil),
      as.character(
        atributos$atributo_id[
          !is.na(atributos$categoria_id) &
          atributos$categoria_id == categoria_id
        ]
      )
    )
    
    for (atributo_id in atributos_presentes) {
      
      nivel_id <- perfil[[atributo_id]]
      
      if (
        length(nivel_id) != 1 ||
        is.na(nivel_id) ||
        !nzchar(nivel_id)
      ) {
        
        errores_estructura <- c(
          errores_estructura,
          paste0(
            "El atributo '",
            atributo_id,
            "' no tiene un nivel válido."
          )
        )
        
        next
      }
      
      existe_nivel <- any(
        !is.na(niveles$categoria_id) &
        !is.na(niveles$atributo_id) &
        !is.na(niveles$nivel_id) &
        niveles$categoria_id == categoria_id &
        niveles$atributo_id == atributo_id &
        niveles$nivel_id == nivel_id
      )
      
      if (!existe_nivel) {
        errores_estructura <- c(
          errores_estructura,
          paste0(
            "Nivel inexistente para ",
            categoria_id,
            "::",
            atributo_id,
            ": ",
            nivel_id,
            "."
          )
        )
      }
    }
  }
  
  if (length(errores_estructura) > 0) {
    
    return(
      list(
        valido = FALSE,
        estructura_valida = FALSE,
        restricciones_incumplidas = character(0),
        mensajes = errores_estructura
      )
    )
  }
  
  restricciones_categoria <- Filter(
    function(x) {
      identical(
        x$categoria_id,
        categoria_id
      )
    },
    obtener_restricciones_dce()
  )
  
  restricciones_incumplidas <- character(0)
  mensajes <- character(0)
  
  for (restriccion in restricciones_categoria) {
    
    condiciones <- restriccion$condiciones
    
    coincide_completa <- all(
      vapply(
        names(condiciones),
        function(atributo_id) {
          
          if (is.null(perfil[[atributo_id]])) {
            return(FALSE)
          }
          
          identical(
            as.character(perfil[[atributo_id]]),
            as.character(condiciones[[atributo_id]])
          )
        },
        logical(1)
      )
    )
    
    if (isTRUE(coincide_completa)) {
      
      restricciones_incumplidas <- c(
        restricciones_incumplidas,
        restriccion$restriccion_id
      )
      
      mensajes <- c(
        mensajes,
        restriccion$descripcion
      )
    }
  }
  
  list(
    valido = length(restricciones_incumplidas) == 0,
    estructura_valida = TRUE,
    restricciones_incumplidas = restricciones_incumplidas,
    mensajes = mensajes
  )
}


# ==================================================
# GENERAR EL UNIVERSO COMPLETO DE PERFILES DE UNA CATEGORÍA
# ==================================================

generar_perfiles_categoria_dce <- function(
    categoria_id,
    atributos,
    niveles
) {
  
  atributos_categoria <- obtener_atributos_categoria(
    categoria_id = categoria_id,
    atributos = atributos
  )
  
  if (nrow(atributos_categoria) == 0) {
    stop(
      paste0(
        "No existen atributos para la categoría '",
        categoria_id,
        "'."
      )
    )
  }
  
  listas_niveles <- lapply(
    seq_len(nrow(atributos_categoria)),
    function(i) {
      
      atributo_id <- as.character(
        atributos_categoria$atributo_id[[i]]
      )
      
      niveles_atributo <- obtener_niveles_atributo(
        categoria_id = categoria_id,
        atributo_id = atributo_id,
        niveles = niveles
      )
      
      if (nrow(niveles_atributo) == 0) {
        stop(
          paste0(
            "El atributo '",
            categoria_id,
            "::",
            atributo_id,
            "' no tiene niveles."
          )
        )
      }
      
      as.character(
        niveles_atributo$nivel_id
      )
    }
  )
  
  names(listas_niveles) <- as.character(
    atributos_categoria$atributo_id
  )
  
  argumentos_expand_grid <- c(
    listas_niveles,
    list(
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
  )
  
  do.call(
    expand.grid,
    argumentos_expand_grid
  )
}


# ==================================================
# EVALUAR TODOS LOS PERFILES DE UNA CATEGORÍA
# ==================================================

evaluar_universo_perfiles_dce <- function(
    categoria_id,
    atributos,
    niveles
) {
  
  perfiles <- generar_perfiles_categoria_dce(
    categoria_id = categoria_id,
    atributos = atributos,
    niveles = niveles
  )
  
  resultados <- lapply(
    seq_len(nrow(perfiles)),
    function(i) {
      validar_perfil_dce(
        perfil = perfiles[i, , drop = FALSE],
        categoria_id = categoria_id,
        atributos = atributos,
        niveles = niveles,
        exigir_perfil_completo = TRUE
      )
    }
  )
  
  valido <- vapply(
    resultados,
    function(x) isTRUE(x$valido),
    logical(1)
  )
  
  restricciones <- vapply(
    resultados,
    function(x) {
      paste(
        x$restricciones_incumplidas,
        collapse = ";"
      )
    },
    character(1)
  )
  
  perfiles$valido <- valido
  perfiles$restricciones_incumplidas <- restricciones
  
  perfiles
}

# ==================================================
# BLOQUE 6: PERFILES VÁLIDOS Y TAREAS A/B DE PRUEBA
# ==================================================

# --------------------------------------------------
# Obtener únicamente perfiles válidos de una categoría
# --------------------------------------------------

obtener_perfiles_validos_categoria_dce <- function(
    categoria_id,
    atributos,
    niveles
) {

  universo <- evaluar_universo_perfiles_dce(
    categoria_id = categoria_id,
    atributos = atributos,
    niveles = niveles
  )

  atributos_categoria <- obtener_atributos_categoria(
    categoria_id = categoria_id,
    atributos = atributos
  )

  ids_atributos <- as.character(
    atributos_categoria$atributo_id
  )

  validos <- universo[
    universo$valido,
    ids_atributos,
    drop = FALSE
  ]

  rownames(validos) <- NULL

  validos
}


# --------------------------------------------------
# Crear un identificador reproducible para un perfil
# --------------------------------------------------

id_perfil_dce <- function(
    perfil,
    categoria_id,
    atributos
) {

  perfil <- normalizar_perfil_dce(
    perfil
  )

  atributos_categoria <- obtener_atributos_categoria(
    categoria_id = categoria_id,
    atributos = atributos
  )

  ids_atributos <- as.character(
    atributos_categoria$atributo_id
  )

  valores <- vapply(
    ids_atributos,
    function(atributo_id) {

      valor <- perfil[[atributo_id]]

      if (
        is.null(valor) ||
        length(valor) != 1 ||
        is.na(valor)
      ) {
        valor <- ""
      }

      paste0(
        atributo_id,
        "=",
        as.character(valor)
      )
    },
    character(1)
  )

  paste(
    valores,
    collapse = "|"
  )
}


# --------------------------------------------------
# Contar atributos distintos entre dos perfiles
# --------------------------------------------------

contar_diferencias_perfiles_dce <- function(
    perfil_a,
    perfil_b,
    categoria_id,
    atributos
) {

  perfil_a <- normalizar_perfil_dce(
    perfil_a
  )

  perfil_b <- normalizar_perfil_dce(
    perfil_b
  )

  atributos_categoria <- obtener_atributos_categoria(
    categoria_id = categoria_id,
    atributos = atributos
  )

  ids_atributos <- as.character(
    atributos_categoria$atributo_id
  )

  diferencias <- vapply(
    ids_atributos,
    function(atributo_id) {

      valor_a <- as.character(
        perfil_a[[atributo_id]]
      )

      valor_b <- as.character(
        perfil_b[[atributo_id]]
      )

      !identical(
        valor_a,
        valor_b
      )
    },
    logical(1)
  )

  sum(diferencias)
}


# --------------------------------------------------
# Generar tareas A/B aleatorias a partir de perfiles válidos
# --------------------------------------------------

generar_tareas_prueba_categoria_dce <- function(
    categoria_id,
    atributos,
    niveles,
    n_tareas = 3,
    min_diferencias = 2,
    perfiles_validos = NULL,
    max_intentos = 5000
) {

  n_tareas <- as.integer(
    n_tareas
  )

  min_diferencias <- as.integer(
    min_diferencias
  )

  if (
    is.na(n_tareas) ||
    n_tareas < 1
  ) {
    stop(
      "n_tareas debe ser un entero mayor o igual a 1."
    )
  }

  if (
    is.na(min_diferencias) ||
    min_diferencias < 1
  ) {
    stop(
      "min_diferencias debe ser un entero mayor o igual a 1."
    )
  }

  if (is.null(perfiles_validos)) {

    perfiles_validos <-
      obtener_perfiles_validos_categoria_dce(
        categoria_id = categoria_id,
        atributos = atributos,
        niveles = niveles
      )
  }

  if (
    !is.data.frame(perfiles_validos) ||
    nrow(perfiles_validos) < 2
  ) {

    stop(
      paste0(
        "No existen suficientes perfiles válidos para generar tareas en la categoría '",
        categoria_id,
        "'."
      )
    )
  }

  if (
    nrow(perfiles_validos) <
      2 * n_tareas
  ) {

    stop(
      paste0(
        "La categoría '",
        categoria_id,
        "' no dispone de suficientes perfiles distintos para generar ",
        n_tareas,
        " tareas sin repetir perfiles."
      )
    )
  }

  usados <- integer(0)
  tareas <- vector(
    "list",
    n_tareas
  )

  for (i in seq_len(n_tareas)) {

    tarea_creada <- FALSE

    for (intento in seq_len(max_intentos)) {

      disponibles <- setdiff(
        seq_len(
          nrow(perfiles_validos)
        ),
        usados
      )

      if (length(disponibles) < 2) {
        break
      }

      indices <- sample(
        disponibles,
        size = 2,
        replace = FALSE
      )

      perfil_a <- perfiles_validos[
        indices[[1]],
        ,
        drop = FALSE
      ]

      perfil_b <- perfiles_validos[
        indices[[2]],
        ,
        drop = FALSE
      ]

      diferencias <-
        contar_diferencias_perfiles_dce(
          perfil_a = perfil_a,
          perfil_b = perfil_b,
          categoria_id = categoria_id,
          atributos = atributos
        )

      if (
        diferencias <
          min_diferencias
      ) {
        next
      }

      validacion_a <- validar_perfil_dce(
        perfil = perfil_a,
        categoria_id = categoria_id,
        atributos = atributos,
        niveles = niveles,
        exigir_perfil_completo = TRUE
      )

      validacion_b <- validar_perfil_dce(
        perfil = perfil_b,
        categoria_id = categoria_id,
        atributos = atributos,
        niveles = niveles,
        exigir_perfil_completo = TRUE
      )

      if (
        !isTRUE(validacion_a$valido) ||
        !isTRUE(validacion_b$valido)
      ) {
        next
      }

      id_a <- id_perfil_dce(
        perfil = perfil_a,
        categoria_id = categoria_id,
        atributos = atributos
      )

      id_b <- id_perfil_dce(
        perfil = perfil_b,
        categoria_id = categoria_id,
        atributos = atributos
      )

      tareas[[i]] <- list(
        tarea_id = paste0(
          categoria_id,
          "_tarea_",
          i
        ),
        categoria_id = categoria_id,
        perfil_A_id = id_a,
        perfil_B_id = id_b,
        perfil_A = perfil_a,
        perfil_B = perfil_b,
        diferencias = diferencias
      )

      usados <- c(
        usados,
        indices
      )

      tarea_creada <- TRUE
      break
    }

    if (!isTRUE(tarea_creada)) {

      stop(
        paste0(
          "No fue posible generar la tarea ",
          i,
          " para la categoría '",
          categoria_id,
          "' respetando min_diferencias = ",
          min_diferencias,
          "."
        )
      )
    }
  }

  tareas
}


# --------------------------------------------------
# Generar tareas para todas las categorías
# --------------------------------------------------

generar_tareas_prueba_dce <- function(
    categorias_ids,
    perfiles_validos_por_categoria,
    atributos,
    niveles,
    n_tareas = 3,
    min_diferencias = 2
) {

  categorias_ids <- as.character(
    categorias_ids
  )

  resultado <- lapply(
    categorias_ids,
    function(categoria_id) {

      pool <- perfiles_validos_por_categoria[[categoria_id]]

      if (is.null(pool)) {
        stop(
          paste0(
            "No existe un pool de perfiles válidos para la categoría '",
            categoria_id,
            "'."
          )
        )
      }

      generar_tareas_prueba_categoria_dce(
        categoria_id = categoria_id,
        atributos = atributos,
        niveles = niveles,
        n_tareas = n_tareas,
        min_diferencias = min_diferencias,
        perfiles_validos = pool
      )
    }
  )

  names(resultado) <- categorias_ids

  resultado
}


# --------------------------------------------------
# Recuperar una tarea ya generada
# --------------------------------------------------

obtener_tarea_generada_dce <- function(
    tareas_dce,
    categoria_id,
    tarea_numero
) {

  if (
    is.null(tareas_dce) ||
    is.null(tareas_dce[[categoria_id]])
  ) {

    stop(
      paste0(
        "No existen tareas generadas para la categoría '",
        categoria_id,
        "'."
      )
    )
  }

  tareas_categoria <-
    tareas_dce[[categoria_id]]

  tarea_numero <- as.integer(
    tarea_numero
  )

  if (
    is.na(tarea_numero) ||
    tarea_numero < 1 ||
    tarea_numero > length(tareas_categoria)
  ) {

    stop(
      paste0(
        "Número de tarea fuera de rango para la categoría '",
        categoria_id,
        "'."
      )
    )
  }

  tareas_categoria[[tarea_numero]]
}


# --------------------------------------------------
# Obtener etiqueta y descripción de un nivel
# --------------------------------------------------

obtener_detalle_nivel_dce <- function(
    categoria_id,
    atributo_id,
    nivel_id,
    niveles
) {

  filas <- niveles[
    !is.na(niveles$categoria_id) &
      !is.na(niveles$atributo_id) &
      !is.na(niveles$nivel_id) &
      niveles$categoria_id == categoria_id &
      niveles$atributo_id == atributo_id &
      niveles$nivel_id == nivel_id,
    ,
    drop = FALSE
  ]

  if (nrow(filas) == 0) {

    return(
      list(
        etiqueta = as.character(nivel_id),
        descripcion = ""
      )
    )
  }

  etiqueta <- as.character(
    filas$etiqueta[[1]]
  )

  descripcion <- as.character(
    filas$descripcion[[1]]
  )

  if (is.na(etiqueta)) {
    etiqueta <- as.character(
      nivel_id
    )
  }

  if (is.na(descripcion)) {
    descripcion <- ""
  }

  list(
    etiqueta = etiqueta,
    descripcion = descripcion
  )
}


# --------------------------------------------------
# Crear la tabla visual de comparación A/B
# --------------------------------------------------

crear_comparacion_tarea_dce <- function(
    tarea,
    categoria_id,
    atributos,
    niveles
) {

  if (
    is.null(tarea$perfil_A) ||
    is.null(tarea$perfil_B)
  ) {

    return(
      tags$p(
        style = "color:red;",
        "La tarea DCE no contiene ambos perfiles."
      )
    )
  }

  perfil_a <- normalizar_perfil_dce(
    tarea$perfil_A
  )

  perfil_b <- normalizar_perfil_dce(
    tarea$perfil_B
  )

  atributos_categoria <- obtener_atributos_categoria(
    categoria_id = categoria_id,
    atributos = atributos
  )

  filas <- lapply(
    seq_len(
      nrow(atributos_categoria)
    ),
    function(i) {

      fila_atributo <- atributos_categoria[
        i,
        ,
        drop = FALSE
      ]

      atributo_id <- as.character(
        fila_atributo$atributo_id[[1]]
      )

      nombre_atributo <- as.character(
        fila_atributo$nombre[[1]]
      )

      if (is.na(nombre_atributo)) {
        nombre_atributo <- atributo_id
      }

      nivel_a_id <- as.character(
        perfil_a[[atributo_id]]
      )

      nivel_b_id <- as.character(
        perfil_b[[atributo_id]]
      )

      detalle_a <- obtener_detalle_nivel_dce(
        categoria_id = categoria_id,
        atributo_id = atributo_id,
        nivel_id = nivel_a_id,
        niveles = niveles
      )

      detalle_b <- obtener_detalle_nivel_dce(
        categoria_id = categoria_id,
        atributo_id = atributo_id,
        nivel_id = nivel_b_id,
        niveles = niveles
      )

      clase_fila <-
        if (
          identical(
            atributo_id,
            "precio_mensual"
          )
        ) {
          "dce-fila-precio"
        } else {
          NULL
        }

      celda_nivel <- function(detalle) {

        tagList(
          tags$span(
            class = "dce-nivel-etiqueta",
            detalle$etiqueta
          ),

          if (nzchar(detalle$descripcion)) {
            tags$details(
              class = "dce-nivel-detalle",

              tags$summary(
                class = "dce-nivel-info",
                title = "Haga clic para ver la explicación de este nivel",
                "ⓘ"
              ),

              tags$div(
                class = "dce-nivel-descripcion",
                detalle$descripcion
              )
            )
          }
        )
      }

      tags$tr(
        class = clase_fila,

        tags$td(
          class = "dce-celda-atributo",
          tags$strong(
            nombre_atributo
          )
        ),

        tags$td(
          class = "dce-celda-alternativa",
          celda_nivel(
            detalle_a
          )
        ),

        tags$td(
          class = "dce-celda-alternativa",
          celda_nivel(
            detalle_b
          )
        )
      )
    }
  )

  div(
    class = "dce-comparacion-wrapper",

    tags$table(
      class = "dce-comparacion",

      tags$thead(
        tags$tr(
          tags$th(
            class = "dce-encabezado-atributo",
            "Característica"
          ),
          tags$th(
            class = "dce-encabezado-alternativa",
            "Alternativa A"
          ),
          tags$th(
            class = "dce-encabezado-alternativa",
            "Alternativa B"
          )
        )
      ),

      tags$tbody(
        filas
      )
    )
  )
}



# ==================================================
# BLOQUE 7: DISEÑO EXPERIMENTAL EFICIENTE Y BLOQUEADO
# ==================================================

# --------------------------------------------------
# Convertir un nivel de precio a unidades de $100.000 CLP
# --------------------------------------------------

precio_nivel_a_100k_dce <- function(nivel_id) {

  nivel_id <- as.character(nivel_id)

  digitos <- gsub(
    "[^0-9]",
    "",
    nivel_id
  )

  if (
    length(digitos) != 1 ||
    is.na(digitos) ||
    digitos == ""
  ) {
    stop(
      paste0(
        "No fue posible extraer el precio desde el nivel_id '",
        nivel_id,
        "'."
      )
    )
  }

  as.numeric(digitos) / 100000
}


# --------------------------------------------------
# Preparar candidatos válidos en el formato de idefix
# --------------------------------------------------

preparar_candidatos_idefix_dce <- function(
    categoria_id,
    atributos,
    niveles
) {

  if (!requireNamespace("idefix", quietly = TRUE)) {
    stop(
      paste0(
        "El paquete 'idefix' es necesario para generar el diseño experimental. ",
        "Instálelo una vez con install.packages(\"idefix\")."
      )
    )
  }

  atributos_categoria <- obtener_atributos_categoria(
    categoria_id = categoria_id,
    atributos = atributos
  )

  if (nrow(atributos_categoria) == 0) {
    stop(
      paste0(
        "No existen atributos para la categoría '",
        categoria_id,
        "'."
      )
    )
  }

  ids_atributos <- as.character(
    atributos_categoria$atributo_id
  )

  universo <- evaluar_universo_perfiles_dce(
    categoria_id = categoria_id,
    atributos = atributos,
    niveles = niveles
  )

  perfiles_validos <- universo[
    universo$valido,
    ids_atributos,
    drop = FALSE
  ]

  rownames(perfiles_validos) <- NULL

  lvls <- integer(
    nrow(atributos_categoria)
  )

  coding <- character(
    nrow(atributos_categoria)
  )

  niveles_por_atributo <- vector(
    "list",
    nrow(atributos_categoria)
  )

  c_lvls <- list()

  for (i in seq_len(nrow(atributos_categoria))) {

    atributo_id <- as.character(
      atributos_categoria$atributo_id[[i]]
    )

    niveles_atributo <- obtener_niveles_atributo(
      categoria_id = categoria_id,
      atributo_id = atributo_id,
      niveles = niveles
    )

    lvls[[i]] <- nrow(
      niveles_atributo
    )

    niveles_por_atributo[[i]] <- as.character(
      niveles_atributo$nivel_id
    )

    if (identical(atributo_id, "precio_mensual")) {

      coding[[i]] <- "C"

      c_lvls[[length(c_lvls) + 1]] <- vapply(
        niveles_por_atributo[[i]],
        precio_nivel_a_100k_dce,
        numeric(1)
      )

    } else {

      coding[[i]] <- "E"
    }
  }

  nombres_niveles <- setNames(
    niveles_por_atributo,
    ids_atributos
  )

  if (length(c_lvls) == 0) {
    c_lvls <- NULL
  }

  candidatos_completos <- idefix::Profiles(
    lvls = lvls,
    coding = coding,
    c.lvls = c_lvls
  )

  if (
    nrow(candidatos_completos) !=
    nrow(universo)
  ) {
    stop(
      paste0(
        "El catálogo codificado de '",
        categoria_id,
        "' no coincide en filas con el universo de perfiles."
      )
    )
  }

  candidatos_validos <- candidatos_completos[
    universo$valido,
    ,
    drop = FALSE
  ]

  ids_perfiles <- vapply(
    seq_len(nrow(perfiles_validos)),
    function(i) {
      id_perfil_dce(
        perfil = perfiles_validos[i, , drop = FALSE],
        categoria_id = categoria_id,
        atributos = atributos
      )
    },
    character(1)
  )

  list(
    categoria_id = categoria_id,
    perfiles = perfiles_validos,
    perfil_ids = ids_perfiles,
    cand_set = candidatos_validos,
    lvls = lvls,
    coding = coding,
    c_lvls = c_lvls,
    niveles_por_atributo = nombres_niveles
  )
}


# --------------------------------------------------
# Encontrar qué candidato corresponde a una fila codificada
# --------------------------------------------------

buscar_candidato_codificado_dce <- function(
    fila_codificada,
    cand_set,
    tolerancia = 1e-9
) {

  fila_codificada <- as.numeric(
    fila_codificada
  )

  if (
    length(fila_codificada) !=
    ncol(cand_set)
  ) {
    stop(
      "La fila codificada no tiene el mismo número de columnas que cand_set."
    )
  }

  diferencias <- abs(
    sweep(
      cand_set,
      2,
      fila_codificada,
      "-"
    )
  )

  coincidencias <- which(
    apply(
      diferencias <= tolerancia,
      1,
      all
    )
  )

  if (length(coincidencias) != 1) {
    stop(
      paste0(
        "Se esperaba una única coincidencia de candidato y se encontraron ",
        length(coincidencias),
        "."
      )
    )
  }

  coincidencias[[1]]
}


# --------------------------------------------------
# Transformar los bloques de idefix a un diseño largo legible
# --------------------------------------------------

extraer_bloques_idefix_dce <- function(
    resultado_idefix,
    candidatos,
    categoria_id,
    atributos,
    niveles,
    n_alts = 3,
    min_diferencias = 2
) {

  bloques <- resultado_idefix$BestDesign$Blocks

  if (
    is.null(bloques) ||
    length(bloques) == 0
  ) {
    stop(
      paste0(
        "idefix no devolvió bloques para la categoría '",
        categoria_id,
        "'."
      )
    )
  }

  atributos_categoria <- obtener_atributos_categoria(
    categoria_id = categoria_id,
    atributos = atributos
  )

  ids_atributos <- as.character(
    atributos_categoria$atributo_id
  )

  salida <- list()
  contador <- 1

  for (b in seq_along(bloques)) {

    bloque <- as.matrix(
      bloques[[b]]
    )

    if (nrow(bloque) %% n_alts != 0) {
      stop(
        paste0(
          "El bloque ",
          b,
          " de '",
          categoria_id,
          "' no contiene un número entero de choice sets."
        )
      )
    }

    n_tareas_bloque <- nrow(bloque) / n_alts

    # Con no.choice = TRUE y alt.cte = c(0,0,1),
    # la primera columna es la constante de la opción de no compra.
    columnas_candidatos <- seq.int(
      from = ncol(bloque) - ncol(candidatos$cand_set) + 1,
      to = ncol(bloque)
    )

    for (t in seq_len(n_tareas_bloque)) {

      filas_set <-
        (t - 1) * n_alts +
        seq_len(n_alts)

      perfiles_tarea <- vector(
        "list",
        2
      )

      ids_tarea <- character(2)

      for (a in 1:2) {

        fila_codificada <- bloque[
          filas_set[[a]],
          columnas_candidatos,
          drop = TRUE
        ]

        indice <- buscar_candidato_codificado_dce(
          fila_codificada = fila_codificada,
          cand_set = candidatos$cand_set
        )

        perfiles_tarea[[a]] <- candidatos$perfiles[
          indice,
          ,
          drop = FALSE
        ]

        ids_tarea[[a]] <- candidatos$perfil_ids[[indice]]
      }

      diferencias <- contar_diferencias_perfiles_dce(
        perfil_a = perfiles_tarea[[1]],
        perfil_b = perfiles_tarea[[2]],
        categoria_id = categoria_id,
        atributos = atributos
      )

      if (diferencias < min_diferencias) {
        stop(
          paste0(
            "La categoría '",
            categoria_id,
            "', bloque ",
            b,
            ", tarea ",
            t,
            " tiene solo ",
            diferencias,
            " atributo(s) diferentes."
          )
        )
      }

      for (a in 1:2) {

        alternativa <- if (a == 1) "A" else "B"
        perfil <- perfiles_tarea[[a]]

        validacion <- validar_perfil_dce(
          perfil = perfil,
          categoria_id = categoria_id,
          atributos = atributos,
          niveles = niveles,
          exigir_perfil_completo = TRUE
        )

        if (!isTRUE(validacion$valido)) {
          stop(
            paste0(
              "El diseño eficiente produjo un perfil inválido en '",
              categoria_id,
              "'."
            )
          )
        }

        for (atributo_id in ids_atributos) {

          salida[[contador]] <- data.frame(
            categoria_id = categoria_id,
            bloque = b,
            tarea_diseno = t,
            alternativa = alternativa,
            perfil_id = ids_tarea[[a]],
            atributo_id = atributo_id,
            nivel_id = as.character(
              perfil[[atributo_id]]
            ),
            stringsAsFactors = FALSE
          )

          contador <- contador + 1
        }
      }
    }
  }

  do.call(
    rbind,
    salida
  )
}


# --------------------------------------------------
# Generar un diseño D-eficiente inicial para una categoría
# --------------------------------------------------

generar_diseno_eficiente_categoria_dce <- function(
    categoria_id,
    atributos,
    niveles,
    n_sets = 16,
    n_blocks = 4,
    seed = 20260830,
    n_start = 8,
    max_iter = 30,
    blocking_iter = 100,
    min_diferencias = 2,
    max_reintentos = 3
) {

  if (!requireNamespace("idefix", quietly = TRUE)) {
    stop(
      paste0(
        "El paquete 'idefix' es necesario para generar el diseño. ",
        "Ejecute install.packages(\"idefix\") y vuelva a intentarlo."
      )
    )
  }

  candidatos <- preparar_candidatos_idefix_dce(
    categoria_id = categoria_id,
    atributos = atributos,
    niveles = niveles
  )

  n_par_atributos <- ncol(
    candidatos$cand_set
  )

  n_par_total <- n_par_atributos + 1

  # idefix::Modfed exige que el número de choice sets no sea
  # menor que el número total de parámetros a estimar.
  # La ASC corresponde a la opción fija de no contratación.
  if (n_sets < n_par_total) {
    stop(
      paste0(
        "La categoría '",
        categoria_id,
        "' requiere estimar ",
        n_par_total,
        " parámetros (",
        n_par_atributos,
        " de atributos + 1 ASC), pero solo se solicitaron ",
        n_sets,
        " choice sets. Aumente n_sets al menos a ",
        n_par_total,
        "."
      )
    )
  }

  # Una constante específica para la opción de no contratación
  # y prior local cero para todos los parámetros.
  par_draws <- list(
    matrix(
      0,
      nrow = 1,
      ncol = 1
    ),
    matrix(
      0,
      nrow = 1,
      ncol = n_par_atributos
    )
  )

  ultimo_error <- NULL

  for (intento in seq_len(max_reintentos)) {

    set.seed(
      seed + intento - 1
    )

    resultado <- idefix::Modfed(
      cand.set = candidatos$cand_set,
      n.sets = n_sets,
      n.alts = 3,
      par.draws = par_draws,
      optim = "D",
      alt.cte = c(0, 0, 1),
      no.choice = TRUE,
      parallel = FALSE,
      max.iter = max_iter,
      n.start = n_start,
      n.blocks = n_blocks,
      blocking.iter = blocking_iter
    )

    extraido <- tryCatch(
      extraer_bloques_idefix_dce(
        resultado_idefix = resultado,
        candidatos = candidatos,
        categoria_id = categoria_id,
        atributos = atributos,
        niveles = niveles,
        n_alts = 3,
        min_diferencias = min_diferencias
      ),
      error = function(e) e
    )

    if (!inherits(extraido, "error")) {

      return(
        list(
          diseno = extraido,
          DB_error = resultado$BestDesign$DB.error,
          ortogonalidad = resultado$BestDesign$Orthogonality,
          intento = intento,
          seed_usada = seed + intento - 1,
          n_perfiles_validos = nrow(candidatos$perfiles),
          n_parametros_atributos = n_par_atributos,
          n_parametros_total = n_par_total
        )
      )
    }

    ultimo_error <- conditionMessage(
      extraido
    )
  }

  stop(
    paste0(
      "No fue posible obtener un diseño aceptable para '",
      categoria_id,
      "' después de ",
      max_reintentos,
      " intento(s). Último motivo: ",
      ultimo_error
    )
  )
}


# --------------------------------------------------
# Validar el archivo de diseño experimental largo
# --------------------------------------------------

validar_diseno_dce <- function(
    diseno,
    categorias_ids,
    atributos,
    niveles,
    n_blocks_esperados = 4,
    tareas_por_bloque = 4,
    min_diferencias = 2
) {

  columnas_requeridas <- c(
    "categoria_id",
    "bloque",
    "tarea_diseno",
    "alternativa",
    "perfil_id",
    "atributo_id",
    "nivel_id"
  )

  faltantes <- setdiff(
    columnas_requeridas,
    names(diseno)
  )

  if (length(faltantes) > 0) {
    stop(
      paste0(
        "Faltan columnas en el diseño DCE: ",
        paste(faltantes, collapse = ", ")
      )
    )
  }

  categorias_ids <- as.character(
    categorias_ids
  )

  categorias_presentes <- sort(
    unique(
      as.character(diseno$categoria_id)
    )
  )

  if (!setequal(categorias_ids, categorias_presentes)) {
    stop(
      "Las categorías presentes en el diseño no coinciden con el catálogo."
    )
  }

  for (categoria_id in categorias_ids) {

    atributos_categoria <- obtener_atributos_categoria(
      categoria_id = categoria_id,
      atributos = atributos
    )

    ids_atributos <- as.character(
      atributos_categoria$atributo_id
    )

    dcat <- diseno[
      diseno$categoria_id == categoria_id,
      ,
      drop = FALSE
    ]

    bloques <- sort(
      unique(
        as.integer(dcat$bloque)
      )
    )

    if (
      length(bloques) != n_blocks_esperados ||
      !identical(
        bloques,
        seq_len(n_blocks_esperados)
      )
    ) {
      stop(
        paste0(
          "La categoría '",
          categoria_id,
          "' no contiene exactamente los bloques esperados 1..",
          n_blocks_esperados,
          "."
        )
      )
    }

    for (bloque in bloques) {

      db <- dcat[
        as.integer(dcat$bloque) == bloque,
        ,
        drop = FALSE
      ]

      tareas <- sort(
        unique(
          as.integer(db$tarea_diseno)
        )
      )

      if (length(tareas) != tareas_por_bloque) {
        stop(
          paste0(
            "La categoría '",
            categoria_id,
            "', bloque ",
            bloque,
            " no contiene ",
            tareas_por_bloque,
            " tareas."
          )
        )
      }

      for (tarea in tareas) {

        dt <- db[
          as.integer(db$tarea_diseno) == tarea,
          ,
          drop = FALSE
        ]

        alts <- sort(
          unique(
            as.character(dt$alternativa)
          )
        )

        if (!identical(alts, c("A", "B"))) {
          stop(
            paste0(
              "La tarea ",
              categoria_id,
              "/",
              bloque,
              "/",
              tarea,
              " no contiene exactamente A y B."
            )
          )
        }

        perfiles <- list()

        for (alt in c("A", "B")) {

          da <- dt[
            dt$alternativa == alt,
            ,
            drop = FALSE
          ]

          if (
            nrow(da) != length(ids_atributos) ||
            !setequal(
              as.character(da$atributo_id),
              ids_atributos
            )
          ) {
            stop(
              paste0(
                "El perfil ",
                alt,
                " de la tarea ",
                categoria_id,
                "/",
                bloque,
                "/",
                tarea,
                " no contiene todos los atributos una sola vez."
              )
            )
          }

          valores <- setNames(
            as.character(da$nivel_id),
            as.character(da$atributo_id)
          )

          perfil <- as.data.frame(
            as.list(
              valores[ids_atributos]
            ),
            stringsAsFactors = FALSE
          )

          validacion <- validar_perfil_dce(
            perfil = perfil,
            categoria_id = categoria_id,
            atributos = atributos,
            niveles = niveles,
            exigir_perfil_completo = TRUE
          )

          if (!isTRUE(validacion$valido)) {
            stop(
              paste0(
                "Perfil inválido en la tarea ",
                categoria_id,
                "/",
                bloque,
                "/",
                tarea,
                ", alternativa ",
                alt,
                "."
              )
            )
          }

          perfiles[[alt]] <- perfil
        }

        diferencias <- contar_diferencias_perfiles_dce(
          perfil_a = perfiles$A,
          perfil_b = perfiles$B,
          categoria_id = categoria_id,
          atributos = atributos
        )

        if (diferencias < min_diferencias) {
          stop(
            paste0(
              "La tarea ",
              categoria_id,
              "/",
              bloque,
              "/",
              tarea,
              " tiene menos de ",
              min_diferencias,
              " atributos diferentes."
            )
          )
        }
      }
    }
  }

  TRUE
}


# --------------------------------------------------
# Cargar y validar el diseño fijo de la encuesta
# --------------------------------------------------

cargar_diseno_dce <- function(
    ruta,
    categorias_ids,
    atributos,
    niveles,
    n_blocks_esperados = 4,
    tareas_por_bloque = 4
) {

  if (!file.exists(ruta)) {
    stop(
      paste0(
        "No se encontró el diseño experimental en '",
        ruta,
        "'. Ejecute primero source(\"scripts/generar_diseno_dce.R\")."
      )
    )
  }

  diseno <- utils::read.csv(
    ruta,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  validar_diseno_dce(
    diseno = diseno,
    categorias_ids = categorias_ids,
    atributos = atributos,
    niveles = niveles,
    n_blocks_esperados = n_blocks_esperados,
    tareas_por_bloque = tareas_por_bloque
  )

  diseno
}


# --------------------------------------------------
# Convertir un bloque fijo en las tareas usadas por Shiny
# --------------------------------------------------

construir_tareas_desde_diseno_dce <- function(
    diseno,
    bloque,
    categorias_ids,
    atributos,
    randomizar_lados = TRUE,
    randomizar_orden = TRUE
) {

  bloque <- as.integer(
    bloque
  )

  categorias_ids <- as.character(
    categorias_ids
  )

  resultado <- setNames(
    vector(
      "list",
      length(categorias_ids)
    ),
    categorias_ids
  )

  for (categoria_id in categorias_ids) {

    atributos_categoria <- obtener_atributos_categoria(
      categoria_id = categoria_id,
      atributos = atributos
    )

    ids_atributos <- as.character(
      atributos_categoria$atributo_id
    )

    dcat <- diseno[
      diseno$categoria_id == categoria_id &
        as.integer(diseno$bloque) == bloque,
      ,
      drop = FALSE
    ]

    if (nrow(dcat) == 0) {
      stop(
        paste0(
          "No existen tareas para '",
          categoria_id,
          "' en el bloque ",
          bloque,
          "."
        )
      )
    }

    tareas_ids <- sort(
      unique(
        as.integer(dcat$tarea_diseno)
      )
    )

    if (isTRUE(randomizar_orden)) {
      tareas_ids <- sample(
        tareas_ids
      )
    }

    tareas_categoria <- lapply(
      seq_along(tareas_ids),
      function(posicion) {

        tarea_original <- tareas_ids[[posicion]]

        dt <- dcat[
          as.integer(dcat$tarea_diseno) == tarea_original,
          ,
          drop = FALSE
        ]

        crear_perfil_alt <- function(alt) {

          da <- dt[
            dt$alternativa == alt,
            ,
            drop = FALSE
          ]

          valores <- setNames(
            as.character(da$nivel_id),
            as.character(da$atributo_id)
          )

          as.data.frame(
            as.list(
              valores[ids_atributos]
            ),
            stringsAsFactors = FALSE
          )
        }

        perfil_a <- crear_perfil_alt("A")
        perfil_b <- crear_perfil_alt("B")

        id_a <- unique(
          as.character(
            dt$perfil_id[dt$alternativa == "A"]
          )
        )

        id_b <- unique(
          as.character(
            dt$perfil_id[dt$alternativa == "B"]
          )
        )

        if (
          length(id_a) != 1 ||
          length(id_b) != 1
        ) {
          stop(
            "Cada alternativa del diseño debe tener un único perfil_id."
          )
        }

        lados_intercambiados <- FALSE

        if (
          isTRUE(randomizar_lados) &&
          sample(c(FALSE, TRUE), 1)
        ) {

          tmp_perfil <- perfil_a
          perfil_a <- perfil_b
          perfil_b <- tmp_perfil

          tmp_id <- id_a
          id_a <- id_b
          id_b <- tmp_id

          lados_intercambiados <- TRUE
        }

        list(
          tarea_id = paste0(
            categoria_id,
            "_bloque_",
            bloque,
            "_tarea_",
            tarea_original
          ),
          categoria_id = categoria_id,
          bloque = bloque,
          tarea_diseno = tarea_original,
          posicion_presentacion = posicion,
          lados_intercambiados = lados_intercambiados,
          perfil_A_id = id_a,
          perfil_B_id = id_b,
          perfil_A = perfil_a,
          perfil_B = perfil_b,
          diferencias = contar_diferencias_perfiles_dce(
            perfil_a = perfil_a,
            perfil_b = perfil_b,
            categoria_id = categoria_id,
            atributos = atributos
          )
        )
      }
    )

    resultado[[categoria_id]] <- tareas_categoria
  }

  resultado
}


# ==================================================
# GUARDADO Y EXPORTACIÓN DE RESPUESTAS
# ==================================================

# --------------------------------------------------
# Utilidades de escritura segura
# --------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}


formatear_fecha_hora_dce <- function(x) {

  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NA_character_)
  }

  format(
    x,
    "%Y-%m-%dT%H:%M:%S%z"
  )
}


normalizar_valor_exportacion_dce <- function(valor) {

  if (is.null(valor) || length(valor) == 0) {
    return(NA_character_)
  }

  if (length(valor) == 1) {
    return(valor)
  }

  paste(
    as.character(valor),
    collapse = "|"
  )
}


escribir_csv_atomico_dce <- function(datos, ruta) {

  dir.create(
    dirname(ruta),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temporal <- tempfile(
    pattern = "tmp_dce_",
    tmpdir = dirname(ruta),
    fileext = ".csv"
  )

  utils::write.csv(
    datos,
    temporal,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )

  ok <- file.copy(
    from = temporal,
    to = ruta,
    overwrite = TRUE
  )

  unlink(temporal)

  if (!isTRUE(ok)) {
    stop(
      paste0(
        "No fue posible escribir el archivo: ",
        ruta
      )
    )
  }

  invisible(ruta)
}


escribir_rds_atomico_dce <- function(objeto, ruta) {

  dir.create(
    dirname(ruta),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temporal <- tempfile(
    pattern = "tmp_dce_",
    tmpdir = dirname(ruta),
    fileext = ".rds"
  )

  saveRDS(
    objeto,
    temporal
  )

  ok <- file.copy(
    from = temporal,
    to = ruta,
    overwrite = TRUE
  )

  unlink(temporal)

  if (!isTRUE(ok)) {
    stop(
      paste0(
        "No fue posible escribir el archivo: ",
        ruta
      )
    )
  }

  invisible(ruta)
}


obtener_snapshot_estado_dce <- function(estado) {

  if (inherits(estado, "reactivevalues")) {
    return(
      shiny::isolate(
        shiny::reactiveValuesToList(
          estado,
          all.names = TRUE
        )
      )
    )
  }

  if (is.list(estado)) {
    return(estado)
  }

  stop(
    "El estado de la encuesta debe ser un reactiveValues o una lista."
  )
}


# --------------------------------------------------
# Borrador de sesión
# --------------------------------------------------

guardar_borrador_sesion_dce <- function(
    estado,
    ruta_base = "data/responses"
) {

  snapshot <- obtener_snapshot_estado_dce(
    estado
  )

  id_sesion <- as.character(
    snapshot$id_sesion
  )

  if (
    length(id_sesion) != 1 ||
    is.na(id_sesion) ||
    id_sesion == ""
  ) {
    stop(
      "No existe un id_sesion válido para guardar el borrador."
    )
  }

  ruta <- file.path(
    ruta_base,
    "borradores",
    paste0(
      id_sesion,
      ".rds"
    )
  )

  escribir_rds_atomico_dce(
    snapshot,
    ruta
  )

  invisible(ruta)
}


# --------------------------------------------------
# Fila general del participante
# --------------------------------------------------

crear_fila_participante_dce <- function(
    estado,
    preguntas,
    fin = NULL
) {

  snapshot <- obtener_snapshot_estado_dce(
    estado
  )

  if (is.null(fin)) {
    fin <- snapshot$fin
  }

  if (is.null(fin)) {
    fin <- Sys.time()
  }

  inicio <- snapshot$inicio

  duracion_segundos <-
    if (!is.null(inicio)) {
      as.numeric(
        difftime(
          fin,
          inicio,
          units = "secs"
        )
      )
    } else {
      NA_real_
    }

  orden_categorias <- as.character(
    snapshot$orden_categorias
  )

  elegibilidad <- snapshot$elegibilidad

  if (is.null(elegibilidad)) {
    elegibilidad <- list()
  }

  elecciones_no_vacias <- 0L
  if (!is.null(snapshot$elecciones_dce)) {
    elecciones_no_vacias <- sum(
      vapply(
        snapshot$elecciones_dce,
        function(x) !respuesta_vacia(x),
        logical(1)
      )
    )
  }

  base <- list(
    id_sesion = as.character(snapshot$id_sesion),
    consentimiento_aceptado = isTRUE(snapshot$consentimiento_aceptado),
    consentimiento_fecha = formatear_fecha_hora_dce(snapshot$consentimiento_fecha),
    inicio = formatear_fecha_hora_dce(inicio),
    fin = formatear_fecha_hora_dce(fin),
    duracion_segundos = duracion_segundos,
    bloque_dce = as.integer(snapshot$bloque_dce),
    orden_categorias = paste(orden_categorias, collapse = "|"),
    categoria_1 = if (length(orden_categorias) >= 1) orden_categorias[[1]] else NA_character_,
    categoria_2 = if (length(orden_categorias) >= 2) orden_categorias[[2]] else NA_character_,
    categoria_3 = if (length(orden_categorias) >= 3) orden_categorias[[3]] else NA_character_,
    cumple_actividad = elegibilidad$cumple_actividad %||% NA,
    cumple_ventas = elegibilidad$cumple_ventas %||% NA,
    cumple_comuna = elegibilidad$cumple_comuna %||% NA,
    cumple_cargo = elegibilidad$cumple_cargo %||% NA,
    elegible_muestra = elegibilidad$elegible_muestra %||% NA,
    n_elecciones_dce = elecciones_no_vacias,
    completada = TRUE
  )

  ids_preguntas <- as.character(
    preguntas$id[
      !is.na(preguntas$id) &
        preguntas$id != ""
    ]
  )

  for (id in ids_preguntas) {
    base[[id]] <- normalizar_valor_exportacion_dce(
      snapshot$respuestas[[id]]
    )
  }

  as.data.frame(
    base,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


# --------------------------------------------------
# Elecciones: una fila por choice set respondido
# --------------------------------------------------

crear_tabla_elecciones_dce <- function(
    estado,
    categorias = NULL,
    exigir_completo = TRUE
) {

  snapshot <- obtener_snapshot_estado_dce(
    estado
  )

  filas <- list()
  contador <- 1L

  orden_categorias <- as.character(
    snapshot$orden_categorias
  )

  for (pos_categoria in seq_along(orden_categorias)) {

    categoria_id <- orden_categorias[[pos_categoria]]
    tareas_categoria <- snapshot$tareas_dce[[categoria_id]]

    if (is.null(tareas_categoria)) {
      stop(
        paste0(
          "No existen tareas guardadas para la categoría '",
          categoria_id,
          "'."
        )
      )
    }

    categoria_nombre <- categoria_id

    if (
      !is.null(categorias) &&
      "categoria_id" %in% names(categorias) &&
      "nombre" %in% names(categorias)
    ) {
      pos_nombre <- match(
        categoria_id,
        as.character(categorias$categoria_id)
      )
      if (!is.na(pos_nombre)) {
        categoria_nombre <- as.character(
          categorias$nombre[[pos_nombre]]
        )
      }
    }

    for (pos_tarea in seq_along(tareas_categoria)) {

      tarea <- tareas_categoria[[pos_tarea]]
      clave <- paste0(
        categoria_id,
        "_tarea_",
        pos_tarea
      )

      eleccion <- snapshot$elecciones_dce[[clave]]

      if (respuesta_vacia(eleccion)) {
        if (isTRUE(exigir_completo)) {
          stop(
            paste0(
              "Falta la elección de ",
              categoria_id,
              ", tarea presentada ",
              pos_tarea,
              "."
            )
          )
        }
        eleccion <- NA_character_
      }

      eleccion <- as.character(eleccion)

      perfil_elegido_id <- NA_character_

      if (identical(eleccion, "A")) {
        perfil_elegido_id <- as.character(tarea$perfil_A_id)
      } else if (identical(eleccion, "B")) {
        perfil_elegido_id <- as.character(tarea$perfil_B_id)
      }

      alternativa_diseno_elegida <- NA_character_

      if (identical(eleccion, "ninguna")) {
        alternativa_diseno_elegida <- "ninguna"
      } else if (eleccion %in% c("A", "B")) {
        if (isTRUE(tarea$lados_intercambiados)) {
          alternativa_diseno_elegida <- if (eleccion == "A") "B" else "A"
        } else {
          alternativa_diseno_elegida <- eleccion
        }
      }

      filas[[contador]] <- data.frame(
        id_sesion = as.character(snapshot$id_sesion),
        bloque_dce = as.integer(snapshot$bloque_dce),
        categoria_posicion = as.integer(pos_categoria),
        categoria_id = categoria_id,
        categoria_nombre = categoria_nombre,
        tarea_posicion = as.integer(pos_tarea),
        tarea_diseno = as.integer(tarea$tarea_diseno),
        tarea_id = as.character(tarea$tarea_id),
        lados_intercambiados = isTRUE(tarea$lados_intercambiados),
        perfil_A_id = as.character(tarea$perfil_A_id),
        perfil_B_id = as.character(tarea$perfil_B_id),
        n_diferencias = as.integer(tarea$diferencias),
        eleccion = eleccion,
        perfil_elegido_id = perfil_elegido_id,
        alternativa_diseno_elegida = alternativa_diseno_elegida,
        eligio_A = identical(eleccion, "A"),
        eligio_B = identical(eleccion, "B"),
        eligio_ninguna = identical(eleccion, "ninguna"),
        stringsAsFactors = FALSE
      )

      contador <- contador + 1L
    }
  }

  do.call(
    rbind,
    filas
  )
}


# --------------------------------------------------
# Atributos mostrados: formato largo, dos alternativas
# por tarea. Permite reconstruir exactamente qué vio
# cada participante.
# --------------------------------------------------

crear_tabla_atributos_presentados_dce <- function(
    estado,
    atributos,
    niveles
) {

  snapshot <- obtener_snapshot_estado_dce(
    estado
  )

  filas <- list()
  contador <- 1L

  orden_categorias <- as.character(
    snapshot$orden_categorias
  )

  for (pos_categoria in seq_along(orden_categorias)) {

    categoria_id <- orden_categorias[[pos_categoria]]
    tareas_categoria <- snapshot$tareas_dce[[categoria_id]]

    atributos_categoria <- obtener_atributos_categoria(
      categoria_id = categoria_id,
      atributos = atributos
    )

    for (pos_tarea in seq_along(tareas_categoria)) {

      tarea <- tareas_categoria[[pos_tarea]]

      clave <- paste0(
        categoria_id,
        "_tarea_",
        pos_tarea
      )

      eleccion <- as.character(
        snapshot$elecciones_dce[[clave]] %||% NA_character_
      )

      for (alternativa in c("A", "B")) {

        perfil <- if (alternativa == "A") tarea$perfil_A else tarea$perfil_B
        perfil_id <- if (alternativa == "A") tarea$perfil_A_id else tarea$perfil_B_id

        for (i in seq_len(nrow(atributos_categoria))) {

          atributo_id <- as.character(
            atributos_categoria$atributo_id[[i]]
          )

          nivel_id <- as.character(
            perfil[[atributo_id]][[1]]
          )

          dn <- niveles[
            niveles$categoria_id == categoria_id &
              niveles$atributo_id == atributo_id &
              niveles$nivel_id == nivel_id,
            ,
            drop = FALSE
          ]

          nivel_etiqueta <-
            if (nrow(dn) == 1) as.character(dn$etiqueta[[1]]) else nivel_id

          nivel_orden <-
            if (nrow(dn) == 1) as.integer(dn$orden[[1]]) else NA_integer_

          precio_clp <- NA_real_

          if (identical(atributo_id, "precio_mensual")) {
            precio_clp <- precio_nivel_a_100k_dce(nivel_id) * 100000
          }

          filas[[contador]] <- data.frame(
            id_sesion = as.character(snapshot$id_sesion),
            bloque_dce = as.integer(snapshot$bloque_dce),
            categoria_posicion = as.integer(pos_categoria),
            categoria_id = categoria_id,
            tarea_posicion = as.integer(pos_tarea),
            tarea_diseno = as.integer(tarea$tarea_diseno),
            tarea_id = as.character(tarea$tarea_id),
            alternativa = alternativa,
            perfil_id = as.character(perfil_id),
            alternativa_elegida = identical(eleccion, alternativa),
            eleccion_tarea = eleccion,
            atributo_orden = as.integer(atributos_categoria$orden[[i]]),
            atributo_id = atributo_id,
            atributo_nombre = as.character(atributos_categoria$nombre[[i]]),
            nivel_id = nivel_id,
            nivel_etiqueta = nivel_etiqueta,
            nivel_orden = nivel_orden,
            precio_clp = precio_clp,
            stringsAsFactors = FALSE
          )

          contador <- contador + 1L
        }
      }
    }
  }

  do.call(
    rbind,
    filas
  )
}


# --------------------------------------------------
# Guardado final completo
# --------------------------------------------------

guardar_resultados_sesion_dce <- function(
    estado,
    preguntas,
    categorias,
    atributos,
    niveles,
    ruta_base = "data/responses"
) {

  snapshot <- obtener_snapshot_estado_dce(
    estado
  )

  id_sesion <- as.character(
    snapshot$id_sesion
  )

  participante <- crear_fila_participante_dce(
    estado = snapshot,
    preguntas = preguntas,
    fin = snapshot$fin
  )

  elecciones <- crear_tabla_elecciones_dce(
    estado = snapshot,
    categorias = categorias,
    exigir_completo = TRUE
  )

  atributos_mostrados <- crear_tabla_atributos_presentados_dce(
    estado = snapshot,
    atributos = atributos,
    niveles = niveles
  )

  n_tareas_esperadas <- sum(
    vapply(
      snapshot$tareas_dce,
      length,
      integer(1)
    )
  )

  if (nrow(elecciones) != n_tareas_esperadas) {
    stop(
      paste0(
        "Se esperaban ",
        n_tareas_esperadas,
        " elecciones y se obtuvieron ",
        nrow(elecciones),
        "."
      )
    )
  }

  ruta_sesion <- file.path(
    ruta_base,
    "sesiones_completas",
    id_sesion
  )

  dir.create(
    ruta_sesion,
    recursive = TRUE,
    showWarnings = FALSE
  )

  ruta_participante <- file.path(
    ruta_sesion,
    "participante.csv"
  )

  ruta_elecciones <- file.path(
    ruta_sesion,
    "elecciones_dce.csv"
  )

  ruta_atributos <- file.path(
    ruta_sesion,
    "atributos_presentados_dce.csv"
  )

  ruta_estado <- file.path(
    ruta_sesion,
    "estado_final.rds"
  )

  escribir_csv_atomico_dce(
    participante,
    ruta_participante
  )

  escribir_csv_atomico_dce(
    elecciones,
    ruta_elecciones
  )

  escribir_csv_atomico_dce(
    atributos_mostrados,
    ruta_atributos
  )

  escribir_rds_atomico_dce(
    snapshot,
    ruta_estado
  )

  ruta_borrador <- file.path(
    ruta_base,
    "borradores",
    paste0(
      id_sesion,
      ".rds"
    )
  )

  if (file.exists(ruta_borrador)) {
    unlink(ruta_borrador)
  }

  invisible(
    list(
      carpeta_sesion = ruta_sesion,
      participante = ruta_participante,
      elecciones = ruta_elecciones,
      atributos_presentados = ruta_atributos,
      estado = ruta_estado
    )
  )
}


# --------------------------------------------------
# Si se vuelve atrás desde la pantalla final, se quita
# el resultado final anterior para impedir que una versión
# desactualizada quede marcada como completada.
# --------------------------------------------------

invalidar_guardado_final_sesion_dce <- function(
    id_sesion,
    ruta_base = "data/responses"
) {

  ruta_sesion <- file.path(
    ruta_base,
    "sesiones_completas",
    as.character(id_sesion)
  )

  if (dir.exists(ruta_sesion)) {
    unlink(
      ruta_sesion,
      recursive = TRUE,
      force = TRUE
    )
  }

  invisible(TRUE)
}


# --------------------------------------------------
# Consolidación de todas las sesiones completas
# --------------------------------------------------

rbind_fill_dce <- function(lista) {

  if (length(lista) == 0) {
    return(data.frame())
  }

  columnas <- unique(
    unlist(
      lapply(lista, names),
      use.names = FALSE
    )
  )

  lista <- lapply(
    lista,
    function(x) {
      faltantes <- setdiff(
        columnas,
        names(x)
      )
      for (col in faltantes) {
        x[[col]] <- NA
      }
      x[, columnas, drop = FALSE]
    }
  )

  do.call(
    rbind,
    lista
  )
}


consolidar_respuestas_dce <- function(
    ruta_base = "data/responses"
) {

  raiz <- file.path(
    ruta_base,
    "sesiones_completas"
  )

  if (!dir.exists(raiz)) {
    stop(
      "Todavía no existe ninguna sesión completa para consolidar."
    )
  }

  carpetas <- list.dirs(
    raiz,
    full.names = TRUE,
    recursive = FALSE
  )

  if (length(carpetas) == 0) {
    stop(
      "Todavía no existe ninguna sesión completa para consolidar."
    )
  }

  leer_archivo <- function(nombre) {
    tablas <- lapply(
      carpetas,
      function(carpeta) {
        ruta <- file.path(
          carpeta,
          nombre
        )
        if (!file.exists(ruta)) {
          return(NULL)
        }
        utils::read.csv(
          ruta,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          fileEncoding = "UTF-8"
        )
      }
    )
    tablas <- Filter(
      Negate(is.null),
      tablas
    )
    rbind_fill_dce(tablas)
  }

  participantes <- leer_archivo(
    "participante.csv"
  )

  elecciones <- leer_archivo(
    "elecciones_dce.csv"
  )

  atributos_presentados <- leer_archivo(
    "atributos_presentados_dce.csv"
  )

  ruta_salida <- file.path(
    ruta_base,
    "consolidado"
  )

  dir.create(
    ruta_salida,
    recursive = TRUE,
    showWarnings = FALSE
  )

  rutas <- list(
    participantes = file.path(ruta_salida, "participantes.csv"),
    elecciones = file.path(ruta_salida, "elecciones_dce.csv"),
    atributos_presentados = file.path(ruta_salida, "atributos_presentados_dce.csv")
  )

  escribir_csv_atomico_dce(
    participantes,
    rutas$participantes
  )

  escribir_csv_atomico_dce(
    elecciones,
    rutas$elecciones
  )

  escribir_csv_atomico_dce(
    atributos_presentados,
    rutas$atributos_presentados
  )

  invisible(
    list(
      rutas = rutas,
      n_participantes = nrow(participantes),
      n_elecciones = nrow(elecciones),
      n_atributos_presentados = nrow(atributos_presentados)
    )
  )
}
