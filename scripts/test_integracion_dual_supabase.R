ejecutar_test_integracion_dual_dce <- function() {
  # =============================================================
  # TEST DE INTEGRACIÓN DUAL: LOCAL + SUPABASE
  # =============================================================

  source("R/helpers.R")
  source("R/db_supabase.R")

  if (!supabase_configurado_dce()) {
    stop("Supabase no está configurado en las variables de entorno.")
  }

  ruta_instrumento <- "data/content/instrumento_DCE.xlsx"
  ruta_diseno <- "data/design/diseno_dce.csv"

  instrumento <- cargar_instrumento_xlsx(ruta_instrumento)
  preguntas <- instrumento$preguntas
  categorias <- instrumento$categorias
  atributos <- instrumento$atributos
  niveles <- instrumento$niveles

  categorias_ids <- as.character(
    categorias$categoria_id[
      !is.na(categorias$categoria_id) & categorias$categoria_id != ""
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

  set.seed(260830)

  tareas <- construir_tareas_desde_diseno_dce(
    diseno = diseno,
    bloque = 1,
    categorias_ids = categorias_ids,
    atributos = atributos,
    randomizar_lados = TRUE,
    randomizar_orden = TRUE
  )

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

  elecciones <- list()
  patron <- c("A", "B", "ninguna", "A")
  for (categoria_id in categorias_ids) {
    for (i in 1:4) {
      elecciones[[paste0(categoria_id, "_tarea_", i)]] <- patron[[i]]
    }
  }

  id_test <- paste0("TEST_DUAL_", format(Sys.time(), "%Y%m%d%H%M%S"))

  estado <- list(
    pagina = 6,
    id_sesion = id_test,
    inicio = Sys.time() - 180,
    fin = Sys.time(),
    bloque_dce = 1,
    respuestas = respuestas,
    elegibilidad = evaluar_elegibilidad(respuestas),
    consentimiento_aceptado = TRUE,
    consentimiento_fecha = Sys.time() - 200,
    orden_categorias = categorias_ids,
    indice_categoria = 3,
    categoria_actual = categorias_ids[[3]],
    tarea_actual = 4,
    tareas_dce = tareas,
    elecciones_dce = elecciones,
    guardado_final = FALSE,
    rutas_guardado = NULL
  )

  ruta_test <- file.path(tempdir(), "test_integracion_dual_dce")
  if (dir.exists(ruta_test)) unlink(ruta_test, recursive = TRUE, force = TRUE)
  dir.create(ruta_test, recursive = TRUE)

  cat("\n==============================================\n")
  cat("TEST DE INTEGRACIÓN LOCAL + SUPABASE\n")
  cat("==============================================\n\n")

  on.exit(
    {
      try(invalidar_sesion_supabase_dce(id_test), silent = TRUE)
      if (dir.exists(ruta_test)) unlink(ruta_test, recursive = TRUE, force = TRUE)
    },
    add = TRUE
  )

  resultado <- guardar_resultados_dual_dce(
    estado = estado,
    preguntas = preguntas,
    categorias = categorias,
    atributos = atributos,
    niveles = niveles,
    ruta_base = ruta_test,
    usar_supabase = TRUE
  )

  if (!isTRUE(resultado$supabase_ok)) {
    stop("El guardado remoto falló: ", resultado$supabase_error)
  }

  archivos_locales <- unlist(
    resultado$local[c("participante", "elecciones", "atributos_presentados", "estado")]
  )
  if (!all(file.exists(archivos_locales))) {
    stop("No se generaron todos los archivos locales.")
  }
  cat("[OK] Guardado local completo.\n")

  conteo <- contar_sesion_supabase_dce(id_test)
  if (conteo$participantes[[1]] != 1 || conteo$elecciones[[1]] != 12) {
    stop("El conteo remoto no coincide con 1 participante y 12 elecciones.")
  }
  cat("[OK] Guardado Supabase: 1 participante y 12 elecciones.\n")

  # Reedición: cambiar la primera elección y volver a guardar.
  primera_clave <- paste0(categorias_ids[[1]], "_tarea_1")
  estado$elecciones_dce[[primera_clave]] <- "B"
  estado$fin <- Sys.time()

  resultado_2 <- guardar_resultados_dual_dce(
    estado = estado,
    preguntas = preguntas,
    categorias = categorias,
    atributos = atributos,
    niveles = niveles,
    ruta_base = ruta_test,
    usar_supabase = TRUE
  )

  if (!isTRUE(resultado_2$supabase_ok)) {
    stop("La reedición remota falló: ", resultado_2$supabase_error)
  }

  conteo_2 <- contar_sesion_supabase_dce(id_test)
  if (conteo_2$participantes[[1]] != 1 || conteo_2$elecciones[[1]] != 12) {
    stop("La reedición produjo duplicados en Supabase.")
  }
  cat("[OK] Reedición sincronizada sin duplicados.\n")

  # Invalidar como si el participante volviera atrás desde la pantalla final.
  inv <- invalidar_guardado_dual_dce(
    id_sesion = id_test,
    ruta_base = ruta_test,
    usar_supabase = TRUE
  )

  if (!isTRUE(inv$supabase_ok)) {
    stop("La invalidación remota falló: ", inv$supabase_error)
  }

  conteo_3 <- contar_sesion_supabase_dce(id_test)
  if (any(unlist(conteo_3) != 0)) {
    stop("La invalidación no eliminó completamente la sesión remota.")
  }

  ruta_local_sesion <- file.path(ruta_test, "sesiones_completas", id_test)
  if (dir.exists(ruta_local_sesion)) {
    stop("La invalidación no eliminó la sesión local.")
  }
  cat("[OK] Volver atrás invalida tanto local como Supabase.\n")

  cat("\n==============================================\n")
  cat("TODAS LAS PRUEBAS DE INTEGRACIÓN PASARON ✅\n")
  cat("==============================================\n")
}

ejecutar_test_integracion_dual_dce()
