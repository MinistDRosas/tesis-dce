# ==================================================
# GENERAR DISEÑO DCE EFICIENTE + COBERTURA DE 3 PRECIOS
# ==================================================
#
# Versión actualizada para el instrumento operativo:
# - 16 choice sets por categoría.
# - 2 alternativas de contratación + opción fija de no contratación.
# - 4 bloques de 4 tareas por categoría.
# - Precio tratado como variable continua para la estimación de DAP.
# - Los 3 niveles de precio deben estar presentes en cada bloque.
# - En cada tarea A y B deben tener precios distintos.
# - Balance global objetivo por categoría: 11 / 10 / 11 perfiles
#   (bajo / medio / alto).
# - Comparaciones objetivo: 5 bajo-medio, 6 bajo-alto,
#   5 medio-alto.
#
# SEGURIDAD:
# Por defecto NO sobrescribe data/design/diseno_dce.csv. Genera archivos
# "*_candidato.csv" para poder compararlos y validarlos primero.
# Para reemplazar deliberadamente el diseño operativo, cambie
# SOBRESCRIBIR_DISENO_OPERATIVO a TRUE y ejecute el script nuevamente.
# ==================================================

source("R/helpers.R")

if (!requireNamespace("idefix", quietly = TRUE)) {
  stop(
    paste0(
      "Falta el paquete 'idefix'. Instálelo con:\n",
      "renv::install(\"idefix\")\n",
      "y vuelva a ejecutar este script."
    )
  )
}

# --------------------------------------------------
# CONFIGURACIÓN
# --------------------------------------------------

SOBRESCRIBIR_DISENO_OPERATIVO <- FALSE

ruta_instrumento <- "data/content/instrumento_DCE.xlsx"

if (isTRUE(SOBRESCRIBIR_DISENO_OPERATIVO)) {
  ruta_salida <- "data/design/diseno_dce.csv"
  ruta_resumen <- "data/design/resumen_diseno_dce.csv"
} else {
  ruta_salida <- "data/design/diseno_dce_candidato.csv"
  ruta_resumen <- "data/design/resumen_diseno_dce_candidato.csv"
}

n_sets <- 16L
n_blocks <- 4L
n_start <- 8L
max_iter <- 30L
blocking_iter <- 100L
min_diferencias <- 2L

# Número de asignaciones de precio candidatas evaluadas por categoría.
# La búsqueda es rápida porque reutiliza la codificación de los perfiles.
n_busquedas_precio <- 6000L

# Detener la generación si imponer los 3 precios deteriora demasiado
# la eficiencia D respecto del diseño base de idefix.
min_eficiencia_relativa_pct <- 90

# Semillas separadas por categoría para reproducibilidad.
# Mantienen la misma convención usada en el diseño de 16 sets.
semillas <- c(
  gestion_marketing = 20261830L,
  atencion_cliente = 20262830L,
  inventario_logistica = 20263830L
)

# Semilla adicional, separada, para optimizar la ubicación de precios.
semillas_precio <- semillas + 50000L

# --------------------------------------------------
# FUNCIONES AUXILIARES: CODIFICACIÓN Y DB-ERROR
# --------------------------------------------------

obtener_niveles_precio_3_dce <- function(categoria_id, niveles) {
  x <- obtener_niveles_atributo(
    categoria_id = categoria_id,
    atributo_id = "precio_mensual",
    niveles = niveles
  )

  if (nrow(x) != 3L) {
    stop(
      paste0(
        "La categoría '", categoria_id,
        "' debe tener exactamente 3 niveles de precio; se encontraron ",
        nrow(x), "."
      )
    )
  }

  as.character(x$nivel_id)
}

codificar_perfil_db_dce <- function(
    perfil,
    categoria_id,
    atributos,
    niveles
) {
  atributos_categoria <- obtener_atributos_categoria(
    categoria_id = categoria_id,
    atributos = atributos
  )

  salida <- numeric(0)
  indice_precio <- NA_integer_

  for (i in seq_len(nrow(atributos_categoria))) {
    atributo_id <- as.character(atributos_categoria$atributo_id[[i]])
    nivel_id <- as.character(perfil[[atributo_id]][[1]])

    niveles_atributo <- obtener_niveles_atributo(
      categoria_id = categoria_id,
      atributo_id = atributo_id,
      niveles = niveles
    )

    ids_niveles <- as.character(niveles_atributo$nivel_id)

    if (identical(atributo_id, "precio_mensual")) {
      indice_precio <- length(salida) + 1L
      salida <- c(
        salida,
        precio_nivel_a_100k_dce(nivel_id)
      )
      next
    }

    k <- length(ids_niveles)
    posicion <- match(nivel_id, ids_niveles)

    if (is.na(posicion)) {
      stop(
        paste0(
          "Nivel desconocido '", nivel_id,
          "' en ", categoria_id, "/", atributo_id, "."
        )
      )
    }

    # Effect coding: los primeros k-1 niveles forman la identidad;
    # el último nivel se representa con -1 en las k-1 columnas.
    if (k <= 1L) {
      next
    }

    if (posicion < k) {
      z <- rep(0, k - 1L)
      z[[posicion]] <- 1
    } else {
      z <- rep(-1, k - 1L)
    }

    salida <- c(salida, z)
  }

  if (is.na(indice_precio)) {
    stop(
      paste0(
        "No se encontró el atributo precio_mensual en '",
        categoria_id, "'."
      )
    )
  }

  list(
    vector = salida,
    indice_precio = indice_precio
  )
}

preparar_db_error_categoria_dce <- function(
    diseno_categoria,
    categoria_id,
    atributos,
    niveles
) {
  tareas <- unique(
    diseno_categoria[, c("bloque", "tarea_diseno"), drop = FALSE]
  )

  tareas <- tareas[
    order(as.integer(tareas$bloque), as.integer(tareas$tarea_diseno)),
    ,
    drop = FALSE
  ]

  salida <- vector("list", nrow(tareas))
  indice_precio_global <- NULL

  for (i in seq_len(nrow(tareas))) {
    bloque <- as.integer(tareas$bloque[[i]])
    tarea <- as.integer(tareas$tarea_diseno[[i]])

    dt <- diseno_categoria[
      as.integer(diseno_categoria$bloque) == bloque &
        as.integer(diseno_categoria$tarea_diseno) == tarea,
      ,
      drop = FALSE
    ]

    vectores <- list()

    for (alt in c("A", "B")) {
      da <- dt[dt$alternativa == alt, , drop = FALSE]
      valores <- setNames(
        as.character(da$nivel_id),
        as.character(da$atributo_id)
      )

      perfil <- as.data.frame(
        as.list(valores),
        stringsAsFactors = FALSE
      )

      cod <- codificar_perfil_db_dce(
        perfil = perfil,
        categoria_id = categoria_id,
        atributos = atributos,
        niveles = niveles
      )

      vectores[[alt]] <- cod$vector

      if (is.null(indice_precio_global)) {
        indice_precio_global <- cod$indice_precio
      } else if (!identical(indice_precio_global, cod$indice_precio)) {
        stop("La posición codificada del precio no es estable dentro de la categoría.")
      }
    }

    salida[[i]] <- list(
      bloque = bloque,
      tarea_diseno = tarea,
      A = vectores$A,
      B = vectores$B
    )
  }

  list(
    tareas = salida,
    indice_precio = indice_precio_global,
    n_parametros_atributos = length(salida[[1]]$A)
  )
}

calcular_db_error_asignacion_dce <- function(
    preparado,
    precios_A,
    precios_B
) {
  n_tareas <- length(preparado$tareas)

  if (
    length(precios_A) != n_tareas ||
    length(precios_B) != n_tareas
  ) {
    stop("La asignación de precios no coincide con el número de tareas.")
  }

  p_attr <- preparado$n_parametros_atributos
  p_total <- p_attr + 1L
  M <- matrix(0, nrow = p_total, ncol = p_total)

  prob <- rep(1 / 3, 3)
  W <- diag(prob) - tcrossprod(prob)

  for (i in seq_len(n_tareas)) {
    a <- preparado$tareas[[i]]$A
    b <- preparado$tareas[[i]]$B

    a[[preparado$indice_precio]] <- precios_A[[i]]
    b[[preparado$indice_precio]] <- precios_B[[i]]

    X <- rbind(
      c(0, a),
      c(0, b),
      c(1, rep(0, p_attr))
    )

    M <- M + t(X) %*% W %*% X
  }

  det_info <- determinant(M, logarithm = TRUE)

  if (det_info$sign <= 0) {
    return(Inf)
  }

  exp(-as.numeric(det_info$modulus) / p_total)
}

calcular_db_error_diseno_dce <- function(
    diseno_categoria,
    categoria_id,
    atributos,
    niveles
) {
  preparado <- preparar_db_error_categoria_dce(
    diseno_categoria = diseno_categoria,
    categoria_id = categoria_id,
    atributos = atributos,
    niveles = niveles
  )

  precios_A <- numeric(length(preparado$tareas))
  precios_B <- numeric(length(preparado$tareas))

  for (i in seq_along(preparado$tareas)) {
    tarea <- preparado$tareas[[i]]

    dt <- diseno_categoria[
      as.integer(diseno_categoria$bloque) == tarea$bloque &
        as.integer(diseno_categoria$tarea_diseno) == tarea$tarea_diseno &
        diseno_categoria$atributo_id == "precio_mensual",
      ,
      drop = FALSE
    ]

    pa <- as.character(dt$nivel_id[dt$alternativa == "A"][[1]])
    pb <- as.character(dt$nivel_id[dt$alternativa == "B"][[1]])

    precios_A[[i]] <- precio_nivel_a_100k_dce(pa)
    precios_B[[i]] <- precio_nivel_a_100k_dce(pb)
  }

  calcular_db_error_asignacion_dce(
    preparado = preparado,
    precios_A = precios_A,
    precios_B = precios_B
  )
}

# --------------------------------------------------
# FUNCIONES AUXILIARES: ASIGNACIÓN DE 3 PRECIOS
# --------------------------------------------------

generar_asignacion_precios_dce <- function(
    tareas,
    niveles_precio
) {
  bajo <- niveles_precio[[1]]
  medio <- niveles_precio[[2]]
  alto <- niveles_precio[[3]]

  # En cada bloque hay 4 tareas. Cada bloque recibe los tres tipos
  # de comparación y uno de ellos se repite. Globalmente el tipo
  # bajo-alto se repite en dos bloques; los otros, en uno cada uno.
  tipos_doblados <- sample(
    c("BM", "BA", "BA", "MA"),
    size = 4L,
    replace = FALSE
  )

  asignacion <- vector("list", nrow(tareas))
  contador <- 1L

  for (bloque in 1:4) {
    idx <- which(as.integer(tareas$bloque) == bloque)

    if (length(idx) != 4L) {
      stop(
        paste0(
          "El bloque ", bloque,
          " no contiene exactamente 4 tareas."
        )
      )
    }

    tipos <- sample(
      c("BM", "BA", "MA", tipos_doblados[[bloque]]),
      size = 4L,
      replace = FALSE
    )

    for (j in seq_along(idx)) {
      tipo <- tipos[[j]]

      par <- switch(
        tipo,
        BM = c(bajo, medio),
        BA = c(bajo, alto),
        MA = c(medio, alto),
        stop("Tipo de comparación de precio desconocido.")
      )

      if (stats::runif(1) < 0.5) {
        par <- rev(par)
      }

      asignacion[[contador]] <- data.frame(
        bloque = as.integer(tareas$bloque[idx[[j]]]),
        tarea_diseno = as.integer(tareas$tarea_diseno[idx[[j]]]),
        precio_A = par[[1]],
        precio_B = par[[2]],
        tipo = tipo,
        stringsAsFactors = FALSE
      )

      contador <- contador + 1L
    }
  }

  out <- do.call(rbind, asignacion)
  out[
    order(as.integer(out$bloque), as.integer(out$tarea_diseno)),
    ,
    drop = FALSE
  ]
}

actualizar_precio_perfil_id_dce <- function(perfil_id, nuevo_precio) {
  partes <- strsplit(as.character(perfil_id), "|", fixed = TRUE)[[1]]
  idx <- grepl("^precio_mensual=", partes)

  if (sum(idx) != 1L) {
    stop(
      paste0(
        "No fue posible localizar una única entrada precio_mensual en perfil_id: ",
        perfil_id
      )
    )
  }

  partes[idx] <- paste0("precio_mensual=", nuevo_precio)
  paste(partes, collapse = "|")
}

aplicar_asignacion_precios_dce <- function(
    diseno_categoria,
    asignacion
) {
  out <- diseno_categoria

  for (i in seq_len(nrow(asignacion))) {
    bloque <- as.integer(asignacion$bloque[[i]])
    tarea <- as.integer(asignacion$tarea_diseno[[i]])

    for (alt in c("A", "B")) {
      nuevo <- if (alt == "A") {
        as.character(asignacion$precio_A[[i]])
      } else {
        as.character(asignacion$precio_B[[i]])
      }

      idx_tarea_alt <-
        as.integer(out$bloque) == bloque &
        as.integer(out$tarea_diseno) == tarea &
        out$alternativa == alt

      idx_precio <- idx_tarea_alt & out$atributo_id == "precio_mensual"

      if (sum(idx_precio) != 1L) {
        stop(
          paste0(
            "Se esperaba una fila de precio en ",
            bloque, "/", tarea, "/", alt, "."
          )
        )
      }

      out$nivel_id[idx_precio] <- nuevo

      perfiles <- unique(as.character(out$perfil_id[idx_tarea_alt]))

      if (length(perfiles) != 1L) {
        stop("El perfil_id no es único dentro de una alternativa.")
      }

      nuevo_id <- actualizar_precio_perfil_id_dce(
        perfil_id = perfiles[[1]],
        nuevo_precio = nuevo
      )

      out$perfil_id[idx_tarea_alt] <- nuevo_id
    }
  }

  out
}

validar_cobertura_precios_dce <- function(
    diseno_categoria,
    categoria_id,
    niveles_precio
) {
  dp <- diseno_categoria[
    diseno_categoria$atributo_id == "precio_mensual",
    ,
    drop = FALSE
  ]

  conteos <- table(
    factor(
      as.character(dp$nivel_id),
      levels = niveles_precio
    )
  )

  if (!identical(as.integer(conteos), c(11L, 10L, 11L))) {
    stop(
      paste0(
        "El balance global de precios de '", categoria_id,
        "' no es 11/10/11. Conteos: ",
        paste(as.integer(conteos), collapse = "/"), "."
      )
    )
  }

  pares <- c(BM = 0L, BA = 0L, MA = 0L)

  for (bloque in 1:4) {
    db <- dp[as.integer(dp$bloque) == bloque, , drop = FALSE]
    conteo_bloque <- table(
      factor(
        as.character(db$nivel_id),
        levels = niveles_precio
      )
    )

    if (any(as.integer(conteo_bloque) < 2L) ||
        any(as.integer(conteo_bloque) > 3L)) {
      stop(
        paste0(
          "En '", categoria_id, "', bloque ", bloque,
          ", cada precio debe aparecer 2 o 3 veces."
        )
      )
    }

    for (tarea in 1:4) {
      dt <- db[as.integer(db$tarea_diseno) == tarea, , drop = FALSE]
      z <- as.character(dt$nivel_id)

      if (length(z) != 2L || length(unique(z)) != 2L) {
        stop(
          paste0(
            "Precios iguales o incompletos en '", categoria_id,
            "', bloque ", bloque, ", tarea ", tarea, "."
          )
        )
      }

      posiciones <- sort(match(z, niveles_precio))
      clave <- paste(posiciones, collapse = "-")

      tipo <- switch(
        clave,
        "1-2" = "BM",
        "1-3" = "BA",
        "2-3" = "MA",
        stop("Par de precios no reconocido.")
      )

      pares[[tipo]] <- pares[[tipo]] + 1L
    }
  }

  if (!identical(as.integer(pares[c("BM", "BA", "MA")]), c(5L, 6L, 5L))) {
    stop(
      paste0(
        "Las comparaciones de precio de '", categoria_id,
        "' no siguen 5/6/5. Conteos: ",
        paste(as.integer(pares), collapse = "/"), "."
      )
    )
  }

  list(
    conteos = as.integer(conteos),
    pares = as.integer(pares[c("BM", "BA", "MA")])
  )
}

optimizar_precios_3niveles_dce <- function(
    diseno_categoria,
    categoria_id,
    atributos,
    niveles,
    seed,
    n_busquedas = 6000L
) {
  niveles_precio <- obtener_niveles_precio_3_dce(
    categoria_id = categoria_id,
    niveles = niveles
  )

  tareas <- unique(
    diseno_categoria[, c("bloque", "tarea_diseno"), drop = FALSE]
  )

  tareas <- tareas[
    order(as.integer(tareas$bloque), as.integer(tareas$tarea_diseno)),
    ,
    drop = FALSE
  ]

  preparado <- preparar_db_error_categoria_dce(
    diseno_categoria = diseno_categoria,
    categoria_id = categoria_id,
    atributos = atributos,
    niveles = niveles
  )

  set.seed(seed)

  mejor_db <- Inf
  mejor_asignacion <- NULL

  for (iter in seq_len(n_busquedas)) {
    a <- generar_asignacion_precios_dce(
      tareas = tareas,
      niveles_precio = niveles_precio
    )

    precios_A <- vapply(
      a$precio_A,
      precio_nivel_a_100k_dce,
      numeric(1)
    )

    precios_B <- vapply(
      a$precio_B,
      precio_nivel_a_100k_dce,
      numeric(1)
    )

    db <- calcular_db_error_asignacion_dce(
      preparado = preparado,
      precios_A = precios_A,
      precios_B = precios_B
    )

    if (is.finite(db) && db < mejor_db) {
      mejor_db <- db
      mejor_asignacion <- a
    }
  }

  if (is.null(mejor_asignacion)) {
    stop(
      paste0(
        "No fue posible encontrar una asignación válida de 3 precios para '",
        categoria_id, "'."
      )
    )
  }

  final <- aplicar_asignacion_precios_dce(
    diseno_categoria = diseno_categoria,
    asignacion = mejor_asignacion
  )

  cobertura <- validar_cobertura_precios_dce(
    diseno_categoria = final,
    categoria_id = categoria_id,
    niveles_precio = niveles_precio
  )

  list(
    diseno = final,
    DB_error = mejor_db,
    asignacion = mejor_asignacion,
    niveles_precio = niveles_precio,
    conteos_precio = cobertura$conteos,
    conteos_pares = cobertura$pares
  )
}

# --------------------------------------------------
# CARGAR Y VALIDAR INSTRUMENTO
# --------------------------------------------------

instrumento <- cargar_instrumento_xlsx(ruta_instrumento)

categorias <- instrumento$categorias
atributos <- instrumento$atributos
niveles <- instrumento$niveles
preguntas <- instrumento$preguntas
opciones <- instrumento$opciones
textos <- instrumento$textos

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

categorias_ordenadas <- categorias[
  !is.na(categorias$categoria_id) & categorias$categoria_id != "",
  ,
  drop = FALSE
]

categorias_ordenadas <- categorias_ordenadas[
  order(categorias_ordenadas$orden),
  ,
  drop = FALSE
]

categorias_dce <- as.character(categorias_ordenadas$categoria_id)

if (!all(categorias_dce %in% names(semillas))) {
  stop("Faltan semillas configuradas para una o más categorías DCE.")
}

cat("\n==============================================\n")
cat("GENERACIÓN DEL DISEÑO DCE - 3 PRECIOS\n")
cat("==============================================\n")
cat("Choice sets por categoría:", n_sets, "\n")
cat("Bloques:", n_blocks, "\n")
cat("Tareas por bloque/categoría:", n_sets / n_blocks, "\n")
cat("Tareas por encuestado:", (n_sets / n_blocks) * length(categorias_dce), "\n")
cat("Asignaciones de precio evaluadas/categoría:", n_busquedas_precio, "\n")
cat("Salida:", ruta_salida, "\n\n")

if (!isTRUE(SOBRESCRIBIR_DISENO_OPERATIVO)) {
  cat("MODO SEGURO: el diseño operativo NO será sobrescrito.\n\n")
}

resultados <- setNames(vector("list", length(categorias_dce)), categorias_dce)

for (categoria_id in categorias_dce) {
  cat("----------------------------------------------\n")
  cat("Generando:", categoria_id, "\n")

  inicio <- Sys.time()

  base <- generar_diseno_eficiente_categoria_dce(
    categoria_id = categoria_id,
    atributos = atributos,
    niveles = niveles,
    n_sets = n_sets,
    n_blocks = n_blocks,
    seed = semillas[[categoria_id]],
    n_start = n_start,
    max_iter = max_iter,
    blocking_iter = blocking_iter,
    min_diferencias = min_diferencias,
    max_reintentos = 3
  )

  # Recalcular el DB-error desde el archivo largo sirve como control
  # independiente del valor reportado por idefix.
  db_base_recalculado <- calcular_db_error_diseno_dce(
    diseno_categoria = base$diseno,
    categoria_id = categoria_id,
    atributos = atributos,
    niveles = niveles
  )

  precios <- optimizar_precios_3niveles_dce(
    diseno_categoria = base$diseno,
    categoria_id = categoria_id,
    atributos = atributos,
    niveles = niveles,
    seed = semillas_precio[[categoria_id]],
    n_busquedas = n_busquedas_precio
  )

  eficiencia_relativa <- 100 * db_base_recalculado / precios$DB_error

  if (
    !is.finite(eficiencia_relativa) ||
    eficiencia_relativa < min_eficiencia_relativa_pct
  ) {
    stop(
      paste0(
        "La eficiencia D relativa de '", categoria_id,
        "' cayó a ", round(eficiencia_relativa, 2),
        "%, bajo el mínimo configurado de ",
        min_eficiencia_relativa_pct, "%."
      )
    )
  }

  resultados[[categoria_id]] <- list(
    diseno = precios$diseno,
    n_perfiles_validos = base$n_perfiles_validos,
    n_parametros_atributos = base$n_parametros_atributos,
    n_parametros_total = base$n_parametros_total,
    DB_error_base = db_base_recalculado,
    DB_error_3precios = precios$DB_error,
    eficiencia_D_relativa_pct = eficiencia_relativa,
    ortogonalidad_base = base$ortogonalidad,
    seed_base = base$seed_usada,
    seed_precios = semillas_precio[[categoria_id]],
    conteos_precio = precios$conteos_precio,
    conteos_pares = precios$conteos_pares
  )

  fin <- Sys.time()

  cat("DB-error base:", db_base_recalculado, "\n")
  cat("DB-error 3 precios:", precios$DB_error, "\n")
  cat("Eficiencia D relativa:", round(eficiencia_relativa, 2), "%\n")
  cat("Precios bajo/medio/alto:", paste(precios$conteos_precio, collapse = "/"), "\n")
  cat("Pares BM/BA/MA:", paste(precios$conteos_pares, collapse = "/"), "\n")
  cat(
    "Tiempo:",
    round(as.numeric(difftime(fin, inicio, units = "secs")), 1),
    "segundos\n\n"
  )
}

# --------------------------------------------------
# CONSOLIDAR Y VALIDAR
# --------------------------------------------------

diseno <- do.call(
  rbind,
  lapply(resultados, function(x) x$diseno)
)
rownames(diseno) <- NULL

validar_diseno_dce(
  diseno = diseno,
  categorias_ids = categorias_dce,
  atributos = atributos,
  niveles = niveles,
  n_blocks_esperados = n_blocks,
  tareas_por_bloque = n_sets / n_blocks,
  min_diferencias = min_diferencias
)

# Validación explícita de cobertura de precio una vez consolidado.
for (categoria_id in categorias_dce) {
  validar_cobertura_precios_dce(
    diseno_categoria = diseno[diseno$categoria_id == categoria_id, , drop = FALSE],
    categoria_id = categoria_id,
    niveles_precio = obtener_niveles_precio_3_dce(categoria_id, niveles)
  )
}

resumen <- do.call(
  rbind,
  lapply(
    categorias_dce,
    function(categoria_id) {
      x <- resultados[[categoria_id]]

      data.frame(
        categoria_id = categoria_id,
        n_sets = n_sets,
        n_blocks = n_blocks,
        tareas_por_bloque = n_sets / n_blocks,
        n_perfiles_validos = x$n_perfiles_validos,
        n_parametros_atributos = x$n_parametros_atributos,
        n_parametros_total = x$n_parametros_total,
        DB_error_base = x$DB_error_base,
        DB_error_3precios = x$DB_error_3precios,
        eficiencia_D_relativa_pct = x$eficiencia_D_relativa_pct,
        ortogonalidad_base = x$ortogonalidad_base,
        seed_base = x$seed_base,
        seed_precios = x$seed_precios,
        perfiles_precio_bajo = x$conteos_precio[[1]],
        perfiles_precio_medio = x$conteos_precio[[2]],
        perfiles_precio_alto = x$conteos_precio[[3]],
        tareas_bajo_medio = x$conteos_pares[[1]],
        tareas_bajo_alto = x$conteos_pares[[2]],
        tareas_medio_alto = x$conteos_pares[[3]],
        stringsAsFactors = FALSE
      )
    }
  )
)

# --------------------------------------------------
# GUARDAR
# --------------------------------------------------

dir.create("data/design", recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  diseno,
  ruta_salida,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

utils::write.csv(
  resumen,
  ruta_resumen,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("==============================================\n")
cat("DISEÑO GENERADO Y VALIDADO ✅\n")
cat("==============================================\n")
cat("Diseño:", ruta_salida, "\n")
cat("Resumen:", ruta_resumen, "\n")
cat("Filas del archivo largo:", nrow(diseno), "\n\n")
print(resumen)

if (!isTRUE(SOBRESCRIBIR_DISENO_OPERATIVO)) {
  cat(
    "\nEl diseño operativo no fue modificado. Compare el candidato, ejecute\n",
    "source(\"scripts/qa_final_instrumento.R\") sobre el diseño que vaya a publicar\n",
    "y solo después promuévalo deliberadamente.\n",
    sep = ""
  )
}
