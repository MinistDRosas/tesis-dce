# ==================================================
# GENERAR DISEÑO DCE EFICIENTE INICIAL
# ==================================================
#
# Este script genera un diseño local D-eficiente con priors cero
# mediante el algoritmo Modified Fedorov de idefix.
#
# Estructura:
# - 12 choice sets por categoría.
# - 2 alternativas de contratación + opción fija de no contratación.
# - 3 bloques equilibrados de 4 tareas por categoría.
# - Cada encuestado verá 1 bloque: 4 tareas x 3 categorías = 12 tareas.
#
# La opción "No contrataría ninguna" se incorpora en la optimización
# mediante una constante específica de alternativa (ASC).
#
# Si posteriormente se obtienen priors a partir de un piloto,
# este diseño puede regenerarse como diseño DB-eficiente.
# ==================================================

source("R/helpers.R")

if (!requireNamespace("idefix", quietly = TRUE)) {
  stop(
    paste0(
      "Falta el paquete 'idefix'. Instálelo una vez con:\n",
      "install.packages(\"idefix\")\n",
      "Después ejecute renv::snapshot() y vuelva a correr este script."
    )
  )
}

ruta_instrumento <- "data/content/instrumento_DCE.xlsx"
ruta_salida <- "data/design/diseno_dce.csv"
ruta_resumen <- "data/design/resumen_diseno_dce.csv"

instrumento <- cargar_instrumento_xlsx(
  ruta_instrumento
)

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
  !is.na(categorias$categoria_id) &
    categorias$categoria_id != "",
  ,
  drop = FALSE
]

categorias_ordenadas <- categorias_ordenadas[
  order(categorias_ordenadas$orden),
  ,
  drop = FALSE
]

categorias_dce <- as.character(
  categorias_ordenadas$categoria_id
)

# --------------------------------------------------
# CONFIGURACIÓN DEL DISEÑO
# --------------------------------------------------

n_sets <- 12
n_blocks <- 3
n_start <- 8
max_iter <- 30
blocking_iter <- 100
min_diferencias <- 2

# Semillas separadas por categoría para reproducibilidad.
semillas <- setNames(
  20260830 +
    seq_along(categorias_dce) * 1000,
  categorias_dce
)

cat("\n==============================================\n")
cat("GENERACIÓN DEL DISEÑO DCE\n")
cat("==============================================\n")
cat("Choice sets por categoría:", n_sets, "\n")
cat("Bloques:", n_blocks, "\n")
cat("Tareas por bloque/categoría:", n_sets / n_blocks, "\n")
cat("Tareas por encuestado:", (n_sets / n_blocks) * length(categorias_dce), "\n\n")

resultados <- vector(
  "list",
  length(categorias_dce)
)

names(resultados) <- categorias_dce

for (categoria_id in categorias_dce) {

  cat("----------------------------------------------\n")
  cat("Generando:", categoria_id, "\n")
  cat("Semilla base:", semillas[[categoria_id]], "\n")

  inicio <- Sys.time()

  resultado <- generar_diseno_eficiente_categoria_dce(
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

  fin <- Sys.time()

  resultados[[categoria_id]] <- resultado

  cat("Perfiles válidos:", resultado$n_perfiles_validos, "\n")
  cat("Parámetros de atributos:", resultado$n_parametros_atributos, "\n")
  cat("Parámetros totales con ASC:", resultado$n_parametros_total, "\n")
  cat("DB-error:", resultado$DB_error, "\n")
  cat("Ortogonalidad:", resultado$ortogonalidad, "\n")
  cat("Semilla utilizada:", resultado$seed_usada, "\n")
  cat(
    "Tiempo:",
    round(
      as.numeric(
        difftime(fin, inicio, units = "secs")
      ),
      1
    ),
    "segundos\n\n"
  )
}

# --------------------------------------------------
# CONSOLIDAR Y VALIDAR
# --------------------------------------------------

diseno <- do.call(
  rbind,
  lapply(
    resultados,
    function(x) x$diseno
  )
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
        DB_error = x$DB_error,
        ortogonalidad = x$ortogonalidad,
        seed = x$seed_usada,
        stringsAsFactors = FALSE
      )
    }
  )
)

# --------------------------------------------------
# GUARDAR
# --------------------------------------------------

dir.create(
  "data/design",
  recursive = TRUE,
  showWarnings = FALSE
)

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
cat("Filas del archivo largo:", nrow(diseno), "\n")
cat("\nResumen por categoría:\n")
print(resumen)
