# ==================================================
# QA FINAL LOCAL DEL INSTRUMENTO DCE
# ==================================================
# Este script NO modifica el instrumento, el diseño ni las respuestas.
# Solo comprueba que la versión local esté coherente antes del despliegue.

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
  "data/content/instrumento_DCE.xlsx",
  "data/design/diseno_dce.csv",
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

for (ruta in c("R/helpers.R", "app.R")) {
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
# 3. Cargar helpers e instrumento
# --------------------------------------------------

cat("\n--- 3. Instrumento XLSX ---\n")

source("R/helpers.R")

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

if (exists("validar_catalogo_dce", mode = "function")) {
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
}

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
# 5. Configuración experimental congelada
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

    # tarea_diseno se numera 1..4 DENTRO de cada bloque.
    # Por eso el choice set se identifica por la combinación bloque + tarea_diseno,
    # no solo por tarea_diseno.
    ids_sets <- unique(
      paste(
        as.integer(dcat$bloque),
        as.integer(dcat$tarea_diseno),
        sep = "_"
      )
    )

    bloques <- sort(unique(as.integer(dcat$bloque)))

    if (length(ids_sets) == 16) {
      ok(paste(categoria_id, "contiene 16 choice sets (4 bloques x 4 tareas)"))
    } else {
      fail(
        paste(
          categoria_id,
          "contiene",
          length(ids_sets),
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

      if (length(tareas_bloque) == 4) {
        ok(paste(categoria_id, "- bloque", bloque, "contiene 4 tareas"))
      } else {
        fail(
          paste(
            categoria_id,
            "- bloque",
            bloque,
            "contiene",
            length(tareas_bloque),
            "tareas; se esperaban 4"
          )
        )
      }
    }
  }
}

# --------------------------------------------------
# 6. Tests automáticos existentes
# --------------------------------------------------

cat("\n--- 6. Tests automáticos ---\n")

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
# 7. Escritura de respuestas
# --------------------------------------------------

cat("\n--- 7. Almacenamiento local ---\n")

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

if (n_completas > 0) {
  warn(
    paste0(
      "Hay ", n_completas,
      " sesión(es) completa(s) en data/responses. Si son pruebas, deben limpiarse antes del levantamiento real."
    )
  )
} else {
  ok("No hay sesiones completas acumuladas")
}

if (n_borradores > 0) {
  warn(
    paste0(
      "Hay ", n_borradores,
      " borrador(es) en data/responses. Revisar/limpiar antes del levantamiento real."
    )
  )
} else {
  ok("No hay borradores acumulados")
}

# --------------------------------------------------
# 8. Reproducibilidad del proyecto
# --------------------------------------------------

cat("\n--- 8. Reproducibilidad ---\n")

if (file.exists("renv.lock")) {
  ok("renv.lock presente")

  lock_txt <- paste(readLines("renv.lock", warn = FALSE), collapse = "\n")

  if (grepl('"idefix"', lock_txt, fixed = TRUE)) {
    ok("idefix aparece registrado en renv.lock")
  } else {
    warn("idefix no aparece en renv.lock. Ejecutar renv::snapshot() para congelar la dependencia usada al generar el diseño.")
  }
}

if (file.exists(".RData")) {
  warn(
    paste0(
      "Existe .RData en la raíz del proyecto. Para una versión reproducible de campo, se recomienda no restaurar/guardar workspaces automáticamente y retirar este archivo antes del despliegue."
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
cat("Las advertencias no impiden ejecutar la app, pero deben resolverse antes del levantamiento real.\n")
