# ==================================================
# QA FINAL LOCAL DEL INSTRUMENTO DCE - 3 PRECIOS
# ==================================================
# Este script NO modifica el instrumento, el diseño ni las respuestas.
# Comprueba la versión local antes del despliegue.
#
# Controles adicionales de esta versión:
# - 16 choice sets por categoría, 4 bloques x 4 tareas.
# - Los 3 precios están presentes en todas las categorías y bloques.
# - Cada precio aparece 2 o 3 veces por bloque (8 perfiles).
# - A y B nunca tienen el mismo precio.
# - Balance global de precio 11/10/11.
# - Pares bajo-medio / bajo-alto / medio-alto = 5/6/5.
# - No hay tareas duplicadas dentro de una categoría.
# - Los perfiles respetan las restricciones DCE.
# - La normalización para Supabase convierte precio_clp a BIGINT seguro.
# ==================================================

cat("\n==============================================\n")
cat("QA FINAL LOCAL DEL INSTRUMENTO DCE\n")
cat("==============================================\n\n")

fallos <- character(0)
advertencias <- character(0)
exitos <- character(0)

ok <- function(mensaje) {
  exitos <<- c(exitos, mensaje)
  cat("[OK] ", mensaje, "\n", sep = "")
}

warn <- function(mensaje) {
  advertencias <<- c(advertencias, mensaje)
  cat("[ADVERTENCIA] ", mensaje, "\n", sep = "")
}

fail <- function(mensaje) {
  fallos <<- c(fallos, mensaje)
  cat("[FALLO] ", mensaje, "\n", sep = "")
}

# --------------------------------------------------
# 1. Archivos esenciales
# --------------------------------------------------

cat("\n--- 1. Archivos esenciales ---\n")

archivos_esenciales <- c(
  "app.R",
  "R/helpers.R",
  "R/db_supabase.R",
  "data/content/instrumento_DCE.xlsx",
  "data/design/diseno_dce.csv",
  "scripts/generar_diseno_dce.R",
  "renv.lock"
)

for (ruta in archivos_esenciales) {
  if (file.exists(ruta)) {
    ok(paste("Existe", ruta))
  } else {
    fail(paste("Falta", ruta))
  }
}

if (length(fallos) > 0) {
  stop("QA detenida: faltan archivos esenciales.")
}

# --------------------------------------------------
# 2. Sintaxis de R
# --------------------------------------------------

cat("\n--- 2. Sintaxis de R ---\n")

archivos_r <- c(
  "R/helpers.R",
  "R/db_supabase.R",
  "app.R",
  "scripts/generar_diseno_dce.R"
)

for (ruta in archivos_r) {
  resultado <- tryCatch(
    {
      parse(file = ruta)
      TRUE
    },
    error = function(e) {
      fail(paste("Error de sintaxis en", ruta, "->", conditionMessage(e)))
      FALSE
    }
  )

  if (isTRUE(resultado)) {
    ok(paste("Sintaxis válida en", ruta))
  }
}

if (length(fallos) > 0) {
  stop("QA detenida: hay errores de sintaxis.")
}

# --------------------------------------------------
# 3. Cargar helpers, BD e instrumento
# --------------------------------------------------

cat("\n--- 3. Instrumento XLSX y funciones ---\n")

source("R/helpers.R")
source("R/db_supabase.R")

instrumento <- cargar_instrumento_xlsx(
  "data/content/instrumento_DCE.xlsx"
)

preguntas <- instrumento$preguntas
opciones <- instrumento$opciones
textos <- instrumento$textos
categorias <- instrumento$categorias
atributos <- instrumento$atributos
niveles <- instrumento$niveles

tryCatch(
  {
    validar_archivos_contenido(
      preguntas = preguntas,
      opciones = opciones,
      textos = textos,
      categorias = categorias,
      atributos = atributos,
      niveles = niveles
    )
    ok("Estructura básica del XLSX válida")
  },
  error = function(e) {
    fail(paste("Estructura XLSX inválida ->", conditionMessage(e)))
  }
)

tryCatch(
  {
    validar_catalogo_dce(
      categorias = categorias,
      atributos = atributos,
      niveles = niveles
    )
    ok("Catálogo DCE válido")
  },
  error = function(e) {
    fail(paste("Catálogo DCE inválido ->", conditionMessage(e)))
  }
)

tryCatch(
  {
    validar_configuracion_restricciones_dce(
      categorias = categorias,
      atributos = atributos,
      niveles = niveles
    )
    ok("Configuración de restricciones DCE válida")
  },
  error = function(e) {
    fail(paste("Restricciones DCE inválidas ->", conditionMessage(e)))
  }
)

# --------------------------------------------------
# 4. Textos congelados
# --------------------------------------------------

cat("\n--- 4. Textos finales ---\n")

texto_total_xlsx <- paste(
  c(
    as.character(textos$texto),
    as.character(preguntas$texto),
    as.character(preguntas$ayuda),
    as.character(opciones$etiqueta),
    if (!is.null(categorias$descripcion)) as.character(categorias$descripcion) else character(0),
    if (!is.null(atributos$descripcion)) as.character(atributos$descripcion) else character(0),
    if (!is.null(niveles$descripcion)) as.character(niveles$descripcion) else character(0)
  ),
  collapse = "\n"
)

texto_app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
texto_total <- paste(texto_total_xlsx, texto_app, sep = "\n")

comprobar_frase <- function(frase, nombre) {
  if (grepl(frase, texto_total, fixed = TRUE)) {
    ok(nombre)
  } else {
    fail(paste(nombre, "(no se encontró el texto esperado)"))
  }
}

comprobar_frase(
  "agregada y anónima",
  "Consentimiento menciona tratamiento agregado y anónimo"
)
comprobar_frase(
  "Acepto participar",
  "Consentimiento explícito presente"
)
comprobar_frase(
  "No contrataría ninguna",
  "Nombre final de la alternativa de no elección presente"
)
comprobar_frase(
  "Puede cerrar esta ventana",
  "Cierre informa que la ventana puede cerrarse"
)
comprobar_frase(
  "Código de respuesta",
  "Pantalla final usa 'Código de respuesta'"
)

frases_obsoletas <- c(
  "Ninguna opción",
  "Identificador de sesión",
  "Modo de desarrollo",
  "tareas siguen siendo simuladas",
  "todavía no corresponden al diseño experimental definitivo"
)

for (frase in frases_obsoletas) {
  if (grepl(frase, texto_total, fixed = TRUE)) {
    fail(paste("Quedó un texto obsoleto:", shQuote(frase)))
  } else {
    ok(paste("No aparece texto obsoleto:", shQuote(frase)))
  }
}

# --------------------------------------------------
# 5. Configuración experimental
# --------------------------------------------------

cat("\n--- 5. Configuración experimental ---\n")

lineas_app <- readLines("app.R", warn = FALSE)

if (any(grepl("n_bloques_dce\\s*<-\\s*4", lineas_app))) {
  ok("app.R está configurado para 4 bloques")
} else {
  fail("app.R no está claramente configurado con n_bloques_dce <- 4")
}

categorias_ids <- as.character(
  categorias$categoria_id[
    !is.na(categorias$categoria_id) & categorias$categoria_id != ""
  ]
)

tryCatch(
  {
    diseno <- cargar_diseno_dce(
      ruta = "data/design/diseno_dce.csv",
      categorias_ids = categorias_ids,
      atributos = atributos,
      niveles = niveles,
      n_blocks_esperados = 4,
      tareas_por_bloque = 4
    )
    ok("Diseño DCE cargado y validado: 4 bloques x 4 tareas")
  },
  error = function(e) {
    fail(paste("Diseño DCE inválido ->", conditionMessage(e)))
  }
)

if (exists("diseno")) {
  for (categoria_id in categorias_ids) {
    dcat <- diseno[diseno$categoria_id == categoria_id, , drop = FALSE]

    ids_sets <- unique(
      paste(
        as.integer(dcat$bloque),
        as.integer(dcat$tarea_diseno),
        sep = "_"
      )
    )

    bloques <- sort(unique(as.integer(dcat$bloque)))

    if (length(ids_sets) == 16L) {
      ok(paste(categoria_id, "contiene 16 choice sets"))
    } else {
      fail(
        paste(
          categoria_id,
          "contiene", length(ids_sets),
          "choice sets; se esperaban 16"
        )
      )
    }

    if (identical(bloques, 1:4)) {
      ok(paste(categoria_id, "contiene bloques 1..4"))
    } else {
      fail(paste(categoria_id, "no contiene exactamente los bloques 1..4"))
    }

    for (bloque in 1:4) {
      tareas_bloque <- unique(
        as.integer(
          dcat$tarea_diseno[as.integer(dcat$bloque) == bloque]
        )
      )

      if (length(tareas_bloque) == 4L) {
        ok(paste(categoria_id, "- bloque", bloque, "contiene 4 tareas"))
      } else {
        fail(
          paste(
            categoria_id,
            "- bloque", bloque,
            "contiene", length(tareas_bloque),
            "tareas; se esperaban 4"
          )
        )
      }
    }
  }
}

# --------------------------------------------------
# 6. QA específico de los 3 precios
# --------------------------------------------------

cat("\n--- 6. Cobertura y balance de precios ---\n")

if (exists("diseno")) {
  for (categoria_id in categorias_ids) {
    niveles_precio_df <- obtener_niveles_atributo(
      categoria_id = categoria_id,
      atributo_id = "precio_mensual",
      niveles = niveles
    )

    if (nrow(niveles_precio_df) != 3L) {
      fail(
        paste(
          categoria_id,
          "no tiene exactamente 3 niveles de precio en el catálogo"
        )
      )
      next
    }

    niveles_precio <- as.character(niveles_precio_df$nivel_id)

    dcat <- diseno[
      diseno$categoria_id == categoria_id,
      ,
      drop = FALSE
    ]

    dp <- dcat[dcat$atributo_id == "precio_mensual", , drop = FALSE]

    conteos <- table(
      factor(
        as.character(dp$nivel_id),
        levels = niveles_precio
      )
    )

    if (identical(as.integer(conteos), c(11L, 10L, 11L))) {
      ok(
        paste0(
          categoria_id,
          " balance global de precio = 11/10/11"
        )
      )
    } else {
      fail(
        paste0(
          categoria_id,
          " balance global de precio distinto de 11/10/11: ",
          paste(as.integer(conteos), collapse = "/")
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

      if (
        length(unique(as.character(db$nivel_id))) == 3L &&
        all(as.integer(conteo_bloque) >= 2L) &&
        all(as.integer(conteo_bloque) <= 3L)
      ) {
        ok(
          paste0(
            categoria_id,
            " - bloque ", bloque,
            " contiene los 3 precios (cada uno 2-3 veces)"
          )
        )
      } else {
        fail(
          paste0(
            categoria_id,
            " - bloque ", bloque,
            " no presenta correctamente los 3 precios: ",
            paste(as.integer(conteo_bloque), collapse = "/")
          )
        )
      }

      for (tarea in 1:4) {
        dt <- db[as.integer(db$tarea_diseno) == tarea, , drop = FALSE]
        z <- as.character(dt$nivel_id)

        if (length(z) != 2L || length(unique(z)) != 2L) {
          fail(
            paste0(
              categoria_id,
              " - bloque ", bloque,
              " tarea ", tarea,
              ": A y B no tienen precios distintos"
            )
          )
          next
        }

        pos <- sort(match(z, niveles_precio))
        clave <- paste(pos, collapse = "-")

        tipo <- switch(
          clave,
          "1-2" = "BM",
          "1-3" = "BA",
          "2-3" = "MA",
          NA_character_
        )

        if (is.na(tipo)) {
          fail(
            paste0(
              categoria_id,
              " - bloque ", bloque,
              " tarea ", tarea,
              ": par de precios no reconocido"
            )
          )
        } else {
          pares[[tipo]] <- pares[[tipo]] + 1L
        }
      }
    }

    if (identical(as.integer(pares[c("BM", "BA", "MA")]), c(5L, 6L, 5L))) {
      ok(paste(categoria_id, "comparaciones BM/BA/MA = 5/6/5"))
    } else {
      fail(
        paste0(
          categoria_id,
          " comparaciones BM/BA/MA distintas de 5/6/5: ",
          paste(as.integer(pares), collapse = "/")
        )
      )
    }
  }
}

# --------------------------------------------------
# 7. Duplicados y coherencia de perfiles
# --------------------------------------------------

cat("\n--- 7. Duplicados y perfiles ---\n")

if (exists("diseno")) {
  for (categoria_id in categorias_ids) {
    dcat <- diseno[diseno$categoria_id == categoria_id, , drop = FALSE]

    claves_tarea <- character(0)

    for (bloque in 1:4) {
      for (tarea in 1:4) {
        dt <- dcat[
          as.integer(dcat$bloque) == bloque &
            as.integer(dcat$tarea_diseno) == tarea,
          ,
          drop = FALSE
        ]

        ids_A <- unique(as.character(dt$perfil_id[dt$alternativa == "A"]))
        ids_B <- unique(as.character(dt$perfil_id[dt$alternativa == "B"]))

        if (length(ids_A) != 1L || length(ids_B) != 1L) {
          fail(
            paste0(
              categoria_id,
              " - ", bloque, "/", tarea,
              ": perfil_id no único en A o B"
            )
          )
          next
        }

        if (identical(ids_A[[1]], ids_B[[1]])) {
          fail(
            paste0(
              categoria_id,
              " - ", bloque, "/", tarea,
              ": A y B son el mismo perfil"
            )
          )
        }

        claves_tarea <- c(
          claves_tarea,
          paste(sort(c(ids_A[[1]], ids_B[[1]])), collapse = " || ")
        )
      }
    }

    if (anyDuplicated(claves_tarea) == 0L) {
      ok(paste(categoria_id, "no contiene tareas A/B duplicadas"))
    } else {
      fail(paste(categoria_id, "contiene tareas A/B duplicadas"))
    }
  }
}

# --------------------------------------------------
# 8. Normalización de precio para Supabase BIGINT
# --------------------------------------------------

cat("\n--- 8. Compatibilidad precio_clp con Supabase ---\n")

if (exists("normalizar_atributos_bd_dce", mode = "function")) {
  prueba <- data.frame(
    precio_clp = 110000.00000000001,
    stringsAsFactors = FALSE
  )

  normalizada <- tryCatch(
    normalizar_atributos_bd_dce(prueba),
    error = function(e) e
  )

  if (inherits(normalizada, "error")) {
    fail(
      paste(
        "normalizar_atributos_bd_dce falló ->",
        conditionMessage(normalizada)
      )
    )
  } else {
    valor <- normalizada$precio_clp[[1]]

    if (
      is.integer(valor) &&
      identical(valor, 110000L)
    ) {
      ok("precio_clp se normaliza a entero exacto para BIGINT")
    } else {
      fail(
        paste0(
          "precio_clp no quedó como entero exacto. Valor/clase: ",
          paste(valor, collapse = ","),
          " / ", paste(class(valor), collapse = ",")
        )
      )
    }
  }
} else {
  fail("No existe normalizar_atributos_bd_dce()")
}

# --------------------------------------------------
# 9. Tests automáticos existentes
# --------------------------------------------------

cat("\n--- 9. Tests automáticos ---\n")

scripts_test <- c(
  "scripts/test_restricciones_dce.R",
  "scripts/test_diseno_dce.R",
  "scripts/test_guardado_respuestas_dce.R"
)

for (ruta in scripts_test) {
  if (!file.exists(ruta)) {
    warn(paste("No se encontró", ruta, "y no pudo ejecutarse"))
    next
  }

  cat("\nEjecutando: ", ruta, "\n", sep = "")

  resultado <- tryCatch(
    {
      sys.source(ruta, envir = new.env(parent = globalenv()))
      TRUE
    },
    error = function(e) {
      fail(paste("Falló", ruta, "->", conditionMessage(e)))
      FALSE
    }
  )

  if (isTRUE(resultado)) {
    ok(paste("Pasó", ruta))
  }
}

# --------------------------------------------------
# 10. Escritura de respuestas
# --------------------------------------------------

cat("\n--- 10. Almacenamiento local ---\n")

ruta_respuestas <- "data/responses"
dir.create(ruta_respuestas, recursive = TRUE, showWarnings = FALSE)

ruta_prueba <- file.path(
  ruta_respuestas,
  paste0(".qa_escritura_", Sys.getpid(), ".tmp")
)

escritura_ok <- tryCatch(
  {
    writeLines("QA", ruta_prueba)
    file.exists(ruta_prueba)
  },
  error = function(e) FALSE
)

if (isTRUE(escritura_ok)) {
  unlink(ruta_prueba, force = TRUE)
  ok("data/responses tiene permisos de escritura")
} else {
  fail("No fue posible escribir en data/responses")
}

ruta_completas <- file.path(ruta_respuestas, "sesiones_completas")
ruta_borradores <- file.path(ruta_respuestas, "borradores")
ruta_pendientes <- file.path(ruta_respuestas, "pendientes_supabase")

n_completas <- if (dir.exists(ruta_completas)) {
  length(list.dirs(ruta_completas, recursive = FALSE, full.names = TRUE))
} else {
  0L
}

n_borradores <- if (dir.exists(ruta_borradores)) {
  length(list.files(ruta_borradores, pattern = "\\.rds$", full.names = TRUE))
} else {
  0L
}

n_pendientes <- if (dir.exists(ruta_pendientes)) {
  length(list.files(ruta_pendientes, pattern = "\\.rds$", full.names = TRUE))
} else {
  0L
}

if (n_completas > 0) {
  warn(
    paste0(
      "Hay ", n_completas,
      " sesión(es) completa(s) locales. No se eliminan automáticamente; revisar si corresponden a pruebas."
    )
  )
} else {
  ok("No hay sesiones completas locales acumuladas")
}

if (n_borradores > 0) {
  warn(
    paste0(
      "Hay ", n_borradores,
      " borrador(es) locales. Revisar antes del despliegue."
    )
  )
} else {
  ok("No hay borradores locales acumulados")
}

if (n_pendientes > 0) {
  fail(
    paste0(
      "Hay ", n_pendientes,
      " sincronización(es) pendiente(s) de Supabase. Sincronizar antes del despliegue."
    )
  )
} else {
  ok("No hay sincronizaciones pendientes de Supabase")
}

# --------------------------------------------------
# 11. Reproducibilidad
# --------------------------------------------------

cat("\n--- 11. Reproducibilidad ---\n")

if (file.exists("renv.lock")) {
  ok("renv.lock presente")

  lock_txt <- paste(readLines("renv.lock", warn = FALSE), collapse = "\n")

  for (pkg in c("idefix", "DBI", "RPostgres")) {
    patron <- paste0('"', pkg, '"')

    if (grepl(patron, lock_txt, fixed = TRUE)) {
      ok(paste(pkg, "aparece registrado en renv.lock"))
    } else {
      warn(
        paste0(
          pkg,
          " no aparece en renv.lock. Ejecutar renv::snapshot() antes de desplegar."
        )
      )
    }
  }
}

if (file.exists(".RData")) {
  warn(
    paste0(
      "Existe .RData en la raíz. Para una versión reproducible de campo, ",
      "se recomienda no restaurar/guardar workspaces automáticamente."
    )
  )
} else {
  ok("No existe .RData en la raíz del proyecto")
}

# --------------------------------------------------
# RESUMEN
# --------------------------------------------------

cat("\n==============================================\n")
cat("RESUMEN QA\n")
cat("==============================================\n")
cat("OK: ", length(exitos), "\n", sep = "")
cat("Advertencias: ", length(advertencias), "\n", sep = "")
cat("Fallos: ", length(fallos), "\n", sep = "")

if (length(advertencias) > 0) {
  cat("\nAdvertencias a revisar antes de publicar:\n")
  for (x in advertencias) cat(" - ", x, "\n", sep = "")
}

if (length(fallos) > 0) {
  cat("\nFallos:\n")
  for (x in fallos) cat(" - ", x, "\n", sep = "")
  stop("QA FINAL FALLÓ ❌")
}

cat("\nQA FINAL LOCAL PASÓ ✅\n")
cat("El diseño cumple la estructura experimental y la cobertura de 3 precios.\n")
