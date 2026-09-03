# =============================================================
# CONEXIÓN Y GUARDADO DCE EN SUPABASE / POSTGRESQL
# =============================================================

variables_db_dce <- function() {
  c(
    "DCE_DB_HOST",
    "DCE_DB_PORT",
    "DCE_DB_NAME",
    "DCE_DB_USER",
    "DCE_DB_PASSWORD"
  )
}

validar_variables_db_dce <- function() {
  vars <- variables_db_dce()
  valores <- Sys.getenv(vars, unset = "")
  faltantes <- vars[valores == ""]

  if (length(faltantes) > 0) {
    stop(
      paste0(
        "Faltan variables de entorno para la base de datos: ",
        paste(faltantes, collapse = ", "),
        ". Revise el archivo .Renviron."
      )
    )
  }

  invisible(TRUE)
}

conectar_supabase_dce <- function() {
  validar_variables_db_dce()

  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Falta el paquete DBI. Ejecute install.packages('DBI').")
  }

  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("Falta el paquete RPostgres. Ejecute install.packages('RPostgres').")
  }

  sslmode <- Sys.getenv("DCE_DB_SSLMODE", unset = "require")

  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DCE_DB_HOST"),
    port = as.integer(Sys.getenv("DCE_DB_PORT", unset = "5432")),
    dbname = Sys.getenv("DCE_DB_NAME"),
    user = Sys.getenv("DCE_DB_USER"),
    password = Sys.getenv("DCE_DB_PASSWORD"),
    sslmode = sslmode
  )
}

parsear_fecha_bd_dce <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x))
  }

  if (length(x) == 0 || is.na(x) || trimws(as.character(x)) == "") {
    return(as.POSIXct(NA))
  }

  tz <- Sys.getenv("DCE_TIMEZONE", unset = "America/Santiago")
  as.POSIXct(as.character(x), tz = tz)
}

asegurar_columnas_bd_dce <- function(df, columnas) {
  faltantes <- setdiff(columnas, names(df))
  for (col in faltantes) {
    df[[col]] <- NA
  }
  df[, columnas, drop = FALSE]
}

normalizar_participante_bd_dce <- function(df) {
  columnas <- c(
    "id_sesion",
    "consentimiento_aceptado",
    "consentimiento_fecha",
    "inicio",
    "fin",
    "duracion_segundos",
    "bloque_dce",
    "orden_categorias",
    "categoria_1",
    "categoria_2",
    "categoria_3",
    "cumple_actividad",
    "cumple_ventas",
    "cumple_comuna",
    "cumple_cargo",
    "elegible_muestra",
    "n_elecciones_dce",
    "completada",
    "cargo",
    "conocimiento_ia",
    "actividad_principal",
    "ventas_anuales",
    "trabajadores",
    "antiguedad",
    "comuna",
    "nivel_adopcion_ia",
    "sistemas_digitales",
    "n_establecimientos"
  )

  df <- asegurar_columnas_bd_dce(df, columnas)

  df$consentimiento_fecha <- parsear_fecha_bd_dce(df$consentimiento_fecha[[1]])
  df$inicio <- parsear_fecha_bd_dce(df$inicio[[1]])
  df$fin <- parsear_fecha_bd_dce(df$fin[[1]])

  df$bloque_dce <- as.integer(df$bloque_dce)
  df$n_elecciones_dce <- as.integer(df$n_elecciones_dce)
  df$trabajadores <- as.integer(df$trabajadores)
  df$antiguedad <- as.integer(df$antiguedad)
  df$n_establecimientos <- as.integer(df$n_establecimientos)
  df$duracion_segundos <- as.numeric(df$duracion_segundos)

  booleanas <- c(
    "consentimiento_aceptado",
    "cumple_actividad",
    "cumple_ventas",
    "cumple_comuna",
    "cumple_cargo",
    "elegible_muestra",
    "completada"
  )

  for (col in booleanas) {
    df[[col]] <- as.logical(df[[col]])
  }

  df
}

normalizar_elecciones_bd_dce <- function(df) {
  columnas <- c(
    "id_sesion",
    "bloque_dce",
    "categoria_posicion",
    "categoria_id",
    "categoria_nombre",
    "tarea_posicion",
    "tarea_diseno",
    "tarea_id",
    "lados_intercambiados",
    "perfil_A_id",
    "perfil_B_id",
    "n_diferencias",
    "eleccion",
    "perfil_elegido_id",
    "alternativa_diseno_elegida",
    "eligio_A",
    "eligio_B",
    "eligio_ninguna"
  )

  df <- asegurar_columnas_bd_dce(df, columnas)

  enteras <- c(
    "bloque_dce",
    "categoria_posicion",
    "tarea_posicion",
    "tarea_diseno",
    "n_diferencias"
  )

  for (col in enteras) {
    df[[col]] <- as.integer(df[[col]])
  }

  booleanas <- c(
    "lados_intercambiados",
    "eligio_A",
    "eligio_B",
    "eligio_ninguna"
  )

  for (col in booleanas) {
    df[[col]] <- as.logical(df[[col]])
  }

  df
}

# -------------------------------------------------------------
# ADAPTAR NOMBRES DE COLUMNAS DE R A POSTGRESQL
# -------------------------------------------------------------
# PostgreSQL convierte identificadores no entrecomillados a minúsculas.
# Los CSV de la app conservan nombres legibles como perfil_A_id / eligio_A,
# mientras que las columnas físicas de PostgreSQL son perfil_a_id / eligio_a.
# Esta función hace la traducción solo al momento de escribir en la BD.

adaptar_nombres_elecciones_postgres_dce <- function(df) {
  mapa <- c(
    perfil_A_id = "perfil_a_id",
    perfil_B_id = "perfil_b_id",
    eligio_A = "eligio_a",
    eligio_B = "eligio_b"
  )

  for (origen in names(mapa)) {
    if (origen %in% names(df)) {
      names(df)[names(df) == origen] <- unname(mapa[[origen]])
    }
  }

  df
}


normalizar_atributos_bd_dce <- function(df) {
  columnas <- c(
    "id_sesion",
    "bloque_dce",
    "categoria_posicion",
    "categoria_id",
    "tarea_posicion",
    "tarea_diseno",
    "tarea_id",
    "alternativa",
    "perfil_id",
    "alternativa_elegida",
    "eleccion_tarea",
    "atributo_orden",
    "atributo_id",
    "atributo_nombre",
    "nivel_id",
    "nivel_etiqueta",
    "nivel_orden",
    "precio_clp"
  )

  df <- asegurar_columnas_bd_dce(df, columnas)

  enteras <- c(
    "bloque_dce",
    "categoria_posicion",
    "tarea_posicion",
    "tarea_diseno",
    "atributo_orden",
    "nivel_orden"
  )

  for (col in enteras) {
    df[[col]] <- as.integer(df[[col]])
  }

  df$precio_clp <- as.numeric(df$precio_clp)
  df$alternativa_elegida <- as.logical(df$alternativa_elegida)

  df
}

# -------------------------------------------------------------
# INSERT SEGURO COMPATIBLE CON RLS
# -------------------------------------------------------------
# RPostgres::dbAppendTable()/dbWriteTable() utiliza COPY para acelerar
# cargas masivas. PostgreSQL no permite COPY FROM en tablas con RLS activo.
# Esta función usa INSERT parametrizados por lotes, que sí respetan RLS.

insertar_dataframe_postgres_dce <- function(
    con,
    schema,
    table,
    df,
    tamano_lote = 100L
) {
  if (!is.data.frame(df)) {
    stop("df debe ser un data.frame.")
  }

  if (nrow(df) == 0) {
    return(invisible(0L))
  }

  if (ncol(df) == 0) {
    stop("df no contiene columnas para insertar.")
  }

  tamano_lote <- max(1L, as.integer(tamano_lote))

  tabla_sql <- as.character(
    DBI::dbQuoteIdentifier(
      con,
      DBI::Id(schema = schema, table = table)
    )
  )

  columnas_sql <- paste(
    as.character(DBI::dbQuoteIdentifier(con, names(df))),
    collapse = ", "
  )

  total_insertadas <- 0L

  inicios <- seq.int(
    from = 1L,
    to = nrow(df),
    by = tamano_lote
  )

  for (inicio_lote in inicios) {
    fin_lote <- min(
      inicio_lote + tamano_lote - 1L,
      nrow(df)
    )

    lote <- df[
      inicio_lote:fin_lote,
      ,
      drop = FALSE
    ]

    n_columnas <- ncol(lote)
    n_filas <- nrow(lote)

    marcadores <- character(n_filas)
    parametros <- vector(
      "list",
      n_filas * n_columnas
    )

    indice_parametro <- 1L
    posicion_lista <- 1L

    for (i in seq_len(n_filas)) {
      indices_fila <- indice_parametro:(
        indice_parametro + n_columnas - 1L
      )

      marcadores[[i]] <- paste0(
        "(",
        paste0("$", indices_fila, collapse = ", "),
        ")"
      )

      for (j in seq_len(n_columnas)) {
        # Usar [i] conserva clases como POSIXct, Date, integer, etc.
        parametros[[posicion_lista]] <- lote[[j]][i]
        posicion_lista <- posicion_lista + 1L
      }

      indice_parametro <- indice_parametro + n_columnas
    }

    sql <- paste0(
      "insert into ",
      tabla_sql,
      " (",
      columnas_sql,
      ") values ",
      paste(marcadores, collapse = ", ")
    )

    insertadas <- DBI::dbExecute(
      con,
      sql,
      params = parametros
    )

    total_insertadas <- total_insertadas + as.integer(insertadas)
  }

  invisible(total_insertadas)
}


guardar_sesion_supabase_dce <- function(
    participante,
    elecciones,
    atributos_presentados,
    con = NULL
) {
  cerrar_al_salir <- is.null(con)

  if (cerrar_al_salir) {
    con <- conectar_supabase_dce()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  participante <- normalizar_participante_bd_dce(participante)
  elecciones <- normalizar_elecciones_bd_dce(elecciones)
  atributos_presentados <- normalizar_atributos_bd_dce(atributos_presentados)

  # Traducir únicamente los nombres mixtos usados por los CSV de la app
  # a los nombres reales en minúsculas de PostgreSQL.
  elecciones_bd <- adaptar_nombres_elecciones_postgres_dce(elecciones)

  if (nrow(participante) != 1) {
    stop("participante debe contener exactamente una fila.")
  }

  id_sesion <- as.character(participante$id_sesion[[1]])

  if (!all(as.character(elecciones$id_sesion) == id_sesion)) {
    stop("elecciones contiene un id_sesion distinto al participante.")
  }

  if (!all(as.character(atributos_presentados$id_sesion) == id_sesion)) {
    stop("atributos_presentados contiene un id_sesion distinto al participante.")
  }

  DBI::dbWithTransaction(
    con,
    {
      # El DELETE replica el comportamiento local: si la misma sesión
      # se vuelve a completar, su versión anterior se reemplaza completa.
      DBI::dbExecute(
        con,
        "delete from tesis_dce.dce_participantes where id_sesion = $1",
        params = list(id_sesion)
      )

      insertar_dataframe_postgres_dce(
        con = con,
        schema = "tesis_dce",
        table = "dce_participantes",
        df = participante
      )

      insertar_dataframe_postgres_dce(
        con = con,
        schema = "tesis_dce",
        table = "dce_elecciones",
        df = elecciones_bd
      )

      insertar_dataframe_postgres_dce(
        con = con,
        schema = "tesis_dce",
        table = "dce_atributos_presentados",
        df = atributos_presentados
      )
    }
  )

  invisible(TRUE)
}

invalidar_sesion_supabase_dce <- function(id_sesion, con = NULL) {
  cerrar_al_salir <- is.null(con)

  if (cerrar_al_salir) {
    con <- conectar_supabase_dce()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  DBI::dbExecute(
    con,
    "delete from tesis_dce.dce_participantes where id_sesion = $1",
    params = list(as.character(id_sesion))
  )

  invisible(TRUE)
}

contar_sesion_supabase_dce <- function(id_sesion, con = NULL) {
  cerrar_al_salir <- is.null(con)

  if (cerrar_al_salir) {
    con <- conectar_supabase_dce()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  id <- as.character(id_sesion)

  data.frame(
    participantes = DBI::dbGetQuery(
      con,
      "select count(*)::integer as n from tesis_dce.dce_participantes where id_sesion = $1",
      params = list(id)
    )$n[[1]],
    elecciones = DBI::dbGetQuery(
      con,
      "select count(*)::integer as n from tesis_dce.dce_elecciones where id_sesion = $1",
      params = list(id)
    )$n[[1]],
    atributos = DBI::dbGetQuery(
      con,
      "select count(*)::integer as n from tesis_dce.dce_atributos_presentados where id_sesion = $1",
      params = list(id)
    )$n[[1]]
  )
}


# =============================================================
# INTEGRACIÓN DUAL: GUARDADO LOCAL + SUPABASE
# =============================================================

supabase_configurado_dce <- function() {
  vars <- variables_db_dce()
  valores <- Sys.getenv(vars, unset = "")
  all(valores != "")
}

ruta_pendientes_supabase_dce <- function(ruta_base = "data/responses") {
  ruta <- file.path(ruta_base, "pendientes_supabase")
  dir.create(ruta, recursive = TRUE, showWarnings = FALSE)
  ruta
}

limpiar_pendientes_supabase_sesion_dce <- function(
    id_sesion,
    ruta_base = "data/responses"
) {
  carpeta <- ruta_pendientes_supabase_dce(ruta_base)
  patron <- paste0("^", gsub("([\\W])", "\\\\\\1", as.character(id_sesion)), "__")
  archivos <- list.files(carpeta, pattern = patron, full.names = TRUE)
  if (length(archivos) > 0) {
    unlink(archivos, force = TRUE)
  }
  invisible(TRUE)
}

guardar_pendiente_supabase_dce <- function(
    operacion,
    id_sesion,
    payload = NULL,
    error = NULL,
    ruta_base = "data/responses"
) {
  operacion <- match.arg(operacion, c("guardar", "eliminar"))
  carpeta <- ruta_pendientes_supabase_dce(ruta_base)

  # Una nueva operación para la misma sesión reemplaza pendientes anteriores.
  limpiar_pendientes_supabase_sesion_dce(
    id_sesion = id_sesion,
    ruta_base = ruta_base
  )

  ruta <- file.path(
    carpeta,
    paste0(as.character(id_sesion), "__", operacion, ".rds")
  )

  saveRDS(
    list(
      operacion = operacion,
      id_sesion = as.character(id_sesion),
      payload = payload,
      error = if (is.null(error)) NULL else conditionMessage(error),
      creado_en = Sys.time()
    ),
    ruta
  )

  invisible(ruta)
}

crear_datos_sesion_supabase_dce <- function(
    estado,
    preguntas,
    categorias,
    atributos,
    niveles
) {
  snapshot <- obtener_snapshot_estado_dce(estado)

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

  atributos_presentados <- crear_tabla_atributos_presentados_dce(
    estado = snapshot,
    atributos = atributos,
    niveles = niveles
  )

  list(
    participante = participante,
    elecciones = elecciones,
    atributos_presentados = atributos_presentados
  )
}

guardar_resultados_dual_dce <- function(
    estado,
    preguntas,
    categorias,
    atributos,
    niveles,
    ruta_base = "data/responses",
    usar_supabase = TRUE
) {
  # 1) El guardado local sigue siendo obligatorio y constituye el respaldo.
  local <- guardar_resultados_sesion_dce(
    estado = estado,
    preguntas = preguntas,
    categorias = categorias,
    atributos = atributos,
    niveles = niveles,
    ruta_base = ruta_base
  )

  resultado <- list(
    local = local,
    supabase_ok = FALSE,
    supabase_error = NULL,
    pendiente_supabase = NULL
  )

  if (!isTRUE(usar_supabase)) {
    resultado$supabase_error <- "Supabase no está configurado; se conservó el guardado local."
    return(invisible(resultado))
  }

  datos <- crear_datos_sesion_supabase_dce(
    estado = estado,
    preguntas = preguntas,
    categorias = categorias,
    atributos = atributos,
    niveles = niveles
  )

  id_sesion <- as.character(datos$participante$id_sesion[[1]])

  error_remoto <- NULL

  tryCatch(
    {
      guardar_sesion_supabase_dce(
        participante = datos$participante,
        elecciones = datos$elecciones,
        atributos_presentados = datos$atributos_presentados
      )
    },
    error = function(e) {
      error_remoto <<- e
    }
  )

  if (is.null(error_remoto)) {
    limpiar_pendientes_supabase_sesion_dce(
      id_sesion = id_sesion,
      ruta_base = ruta_base
    )
    resultado$supabase_ok <- TRUE
  } else {
    resultado$supabase_error <- conditionMessage(error_remoto)
    resultado$pendiente_supabase <- guardar_pendiente_supabase_dce(
      operacion = "guardar",
      id_sesion = id_sesion,
      payload = datos,
      error = error_remoto,
      ruta_base = ruta_base
    )
  }

  invisible(resultado)
}

invalidar_guardado_dual_dce <- function(
    id_sesion,
    ruta_base = "data/responses",
    usar_supabase = TRUE
) {
  # Invalidar siempre la versión local primero.
  invalidar_guardado_final_sesion_dce(
    id_sesion = id_sesion,
    ruta_base = ruta_base
  )

  resultado <- list(
    supabase_ok = FALSE,
    supabase_error = NULL,
    pendiente_supabase = NULL
  )

  if (!isTRUE(usar_supabase)) {
    resultado$supabase_error <- "Supabase no está configurado."
    return(invisible(resultado))
  }

  error_remoto <- NULL

  tryCatch(
    {
      invalidar_sesion_supabase_dce(
        id_sesion = id_sesion
      )
    },
    error = function(e) {
      error_remoto <<- e
    }
  )

  if (is.null(error_remoto)) {
    limpiar_pendientes_supabase_sesion_dce(
      id_sesion = id_sesion,
      ruta_base = ruta_base
    )
    resultado$supabase_ok <- TRUE
  } else {
    resultado$supabase_error <- conditionMessage(error_remoto)
    resultado$pendiente_supabase <- guardar_pendiente_supabase_dce(
      operacion = "eliminar",
      id_sesion = id_sesion,
      payload = NULL,
      error = error_remoto,
      ruta_base = ruta_base
    )
  }

  invisible(resultado)
}

sincronizar_pendientes_supabase_dce <- function(
    ruta_base = "data/responses",
    verbose = TRUE
) {
  carpeta <- ruta_pendientes_supabase_dce(ruta_base)
  archivos <- list.files(carpeta, pattern = "\\.rds$", full.names = TRUE)

  if (length(archivos) == 0) {
    if (isTRUE(verbose)) cat("No hay operaciones pendientes de Supabase.\\n")
    return(invisible(list(procesados = 0L, exitosos = 0L, fallidos = 0L)))
  }

  exitosos <- 0L
  fallidos <- 0L

  for (ruta in archivos) {
    item <- readRDS(ruta)
    ok <- FALSE
    mensaje <- NULL

    tryCatch(
      {
        if (identical(item$operacion, "guardar")) {
          guardar_sesion_supabase_dce(
            participante = item$payload$participante,
            elecciones = item$payload$elecciones,
            atributos_presentados = item$payload$atributos_presentados
          )
        } else if (identical(item$operacion, "eliminar")) {
          invalidar_sesion_supabase_dce(item$id_sesion)
        } else {
          stop("Operación pendiente desconocida: ", item$operacion)
        }
        ok <- TRUE
      },
      error = function(e) {
        mensaje <<- conditionMessage(e)
      }
    )

    if (isTRUE(ok)) {
      unlink(ruta, force = TRUE)
      exitosos <- exitosos + 1L
      if (isTRUE(verbose)) {
        cat("[OK] ", item$operacion, " -> ", item$id_sesion, "\\n", sep = "")
      }
    } else {
      fallidos <- fallidos + 1L
      if (isTRUE(verbose)) {
        cat("[FALLO] ", item$operacion, " -> ", item$id_sesion,
            ": ", mensaje, "\\n", sep = "")
      }
    }
  }

  invisible(
    list(
      procesados = length(archivos),
      exitosos = exitosos,
      fallidos = fallidos
    )
  )
}
