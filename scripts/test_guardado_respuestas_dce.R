# ==================================================
# TEST DEL GUARDADO Y EXPORTACIÓN DE RESPUESTAS DCE
# ==================================================

source("R/helpers.R")

ruta_instrumento <- "data/content/instrumento_DCE.xlsx"
ruta_diseno <- "data/design/diseno_dce.csv"

instrumento <- cargar_instrumento_xlsx(ruta_instrumento)

preguntas <- instrumento$preguntas
categorias <- instrumento$categorias
atributos <- instrumento$atributos
niveles <- instrumento$niveles

categorias_ids <- as.character(
  categorias$categoria_id[
    !is.na(categorias$categoria_id) &
      categorias$categoria_id != ""
  ]
)

diseno <- cargar_diseno_dce(
  ruta = ruta_diseno,
  categorias_ids = categorias_ids,
  atributos = atributos,
  niveles = niveles,
  n_blocks_esperados = 4,
  tareas_por_bloque = 4
)

set.seed(12345)

tareas <- construir_tareas_desde_diseno_dce(
  diseno = diseno,
  bloque = 1,
  categorias_ids = categorias_ids,
  atributos = atributos,
  randomizar_lados = TRUE,
  randomizar_orden = TRUE
)

orden_categorias <- categorias_ids

respuestas <- list(
  cargo = "gerencia",
  conocimiento_ia = "intermedio",
  actividad_principal = "comercio_minorista",
  ventas_anuales = "2400_25000_uf",
  trabajadores = 12,
  antiguedad = 7,
  comuna = "vina_del_mar",
  nivel_adopcion_ia = "puntual",
  sistemas_digitales = c("pos", "erp"),
  n_establecimientos = 2
)

elegibilidad <- evaluar_elegibilidad(respuestas)

elecciones <- list()
patron <- c("A", "B", "ninguna", "A")

for (categoria_id in orden_categorias) {
  for (i in 1:4) {
    elecciones[[paste0(categoria_id, "_tarea_", i)]] <- patron[[i]]
  }
}

estado <- list(
  pagina = 6,
  id_sesion = "TEST_GUARDADO_001",
  inicio = Sys.time() - 180,
  fin = Sys.time(),
  bloque_dce = 1,
  respuestas = respuestas,
  elegibilidad = elegibilidad,
  orden_categorias = orden_categorias,
  indice_categoria = 3,
  categoria_actual = orden_categorias[[3]],
  tarea_actual = 4,
  tareas_dce = tareas,
  elecciones_dce = elecciones,
  guardado_final = FALSE,
  rutas_guardado = NULL
)

ruta_test <- file.path(
  tempdir(),
  "test_guardado_respuestas_dce"
)

if (dir.exists(ruta_test)) {
  unlink(ruta_test, recursive = TRUE, force = TRUE)
}

dir.create(ruta_test, recursive = TRUE)

cat("\n==============================================\n")
cat("TEST DEL GUARDADO DE RESPUESTAS DCE\n")
cat("==============================================\n\n")

rutas <- guardar_resultados_sesion_dce(
  estado = estado,
  preguntas = preguntas,
  categorias = categorias,
  atributos = atributos,
  niveles = niveles,
  ruta_base = ruta_test
)

archivos_requeridos <- unlist(
  rutas[c(
    "participante",
    "elecciones",
    "atributos_presentados",
    "estado"
  )]
)

if (!all(file.exists(archivos_requeridos))) {
  stop("No se crearon todos los archivos esperados.")
}
cat("[OK] Se crearon los cuatro archivos de la sesión.\n")

participante <- utils::read.csv(
  rutas$participante,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

elec <- utils::read.csv(
  rutas$elecciones,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

atr <- utils::read.csv(
  rutas$atributos_presentados,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (nrow(participante) != 1) {
  stop("participante.csv debe contener exactamente una fila.")
}

if (nrow(elec) != 12) {
  stop("elecciones_dce.csv debe contener exactamente 12 filas.")
}

if (any(is.na(elec$eleccion)) || any(elec$eleccion == "")) {
  stop("Existen elecciones DCE vacías en la exportación final.")
}

cat("[OK] participante.csv contiene 1 fila y elecciones_dce.csv contiene 12 tareas.\n")

n_atributos_por_bloque <- sum(
  vapply(
    categorias_ids,
    function(categoria_id) {
      nrow(
        obtener_atributos_categoria(
          categoria_id = categoria_id,
          atributos = atributos
        )
      )
    },
    integer(1)
  )
)

n_atributos_esperados <- n_atributos_por_bloque * 4 * 2

if (nrow(atr) != n_atributos_esperados) {
  stop(
    paste0(
      "atributos_presentados_dce.csv debía contener ",
      n_atributos_esperados,
      " filas y contiene ",
      nrow(atr),
      "."
    )
  )
}

cat(
  "[OK] Se registraron ",
  nrow(atr),
  " filas de atributos presentados.\n",
  sep = ""
)

if (!identical(as.character(participante$sistemas_digitales), "pos|erp")) {
  stop("La respuesta múltiple de sistemas_digitales no se exportó correctamente.")
}
cat("[OK] Las respuestas múltiples se exportan separadas por '|'.\n")

if (!isTRUE(participante$elegible_muestra[[1]])) {
  stop("La elegibilidad del participante de prueba debería ser TRUE.")
}
cat("[OK] La elegibilidad se exportó correctamente.\n")

# Comprobar sobrescritura segura de la misma sesión.
primera_clave <- paste0(orden_categorias[[1]], "_tarea_1")
estado$elecciones_dce[[primera_clave]] <- "B"
estado$fin <- Sys.time()

guardar_resultados_sesion_dce(
  estado = estado,
  preguntas = preguntas,
  categorias = categorias,
  atributos = atributos,
  niveles = niveles,
  ruta_base = ruta_test
)

elec_2 <- utils::read.csv(
  rutas$elecciones,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

fila_primera <- elec_2[
  elec_2$categoria_id == orden_categorias[[1]] &
    elec_2$tarea_posicion == 1,
  ,
  drop = FALSE
]

if (nrow(fila_primera) != 1 || fila_primera$eleccion[[1]] != "B") {
  stop("La sobrescritura de una sesión existente no funcionó correctamente.")
}
cat("[OK] Una sesión reeditada sobrescribe su versión anterior.\n")

# Crear una segunda sesión para probar consolidación.
estado_2 <- estado
estado_2$id_sesion <- "TEST_GUARDADO_002"
estado_2$inicio <- Sys.time() - 120
estado_2$fin <- Sys.time()

guardar_resultados_sesion_dce(
  estado = estado_2,
  preguntas = preguntas,
  categorias = categorias,
  atributos = atributos,
  niveles = niveles,
  ruta_base = ruta_test
)

consolidado <- consolidar_respuestas_dce(
  ruta_base = ruta_test
)

if (consolidado$n_participantes != 2) {
  stop("La consolidación debería contener 2 participantes.")
}

if (consolidado$n_elecciones != 24) {
  stop("La consolidación debería contener 24 elecciones DCE.")
}

cat("[OK] Consolidación: 2 participantes y 24 elecciones DCE.\n")

cat("\n==============================================\n")
cat("TODAS LAS PRUEBAS DE GUARDADO PASARON ✅\n")
cat("==============================================\n")
