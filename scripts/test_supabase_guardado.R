source("R/db_supabase.R")

cat("\n==============================================\n")
cat("TEST DE ESCRITURA / REEDICIÓN EN SUPABASE\n")
cat("==============================================\n\n")


probar_guardado_supabase_dce <- function() {
  id_test <- paste0(
    "TEST_SUPABASE_",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "_",
    sample(100000:999999, 1)
  )

  ahora <- Sys.time()

  participante <- data.frame(
    id_sesion = id_test,
    consentimiento_aceptado = TRUE,
    consentimiento_fecha = ahora - 240,
    inicio = ahora - 240,
    fin = ahora,
    duracion_segundos = 240,
    bloque_dce = 1L,
    orden_categorias = "gestion_marketing|atencion_cliente|inventario_logistica",
    categoria_1 = "gestion_marketing",
    categoria_2 = "atencion_cliente",
    categoria_3 = "inventario_logistica",
    cumple_actividad = TRUE,
    cumple_ventas = TRUE,
    cumple_comuna = TRUE,
    cumple_cargo = TRUE,
    elegible_muestra = TRUE,
    n_elecciones_dce = 12L,
    completada = TRUE,
    cargo = "gerencia",
    conocimiento_ia = "intermedio",
    actividad_principal = "comercio_minorista",
    ventas_anuales = "2400_25000_uf",
    trabajadores = 15L,
    antiguedad = 8L,
    comuna = "vina_del_mar",
    nivel_adopcion_ia = "puntual",
    sistemas_digitales = "pos|erp",
    n_establecimientos = 2L,
    stringsAsFactors = FALSE
  )

  categorias <- c(
    "gestion_marketing",
    "atencion_cliente",
    "inventario_logistica"
  )

  nombres <- c(
    "IA para Gestión Comercial y Marketing",
    "IA para Atención y Asistencia al Cliente",
    "IA para Inventario y Logística"
  )

  elecciones <- do.call(
    rbind,
    lapply(seq_along(categorias), function(j) {
      data.frame(
        id_sesion = id_test,
        bloque_dce = 1L,
        categoria_posicion = j,
        categoria_id = categorias[[j]],
        categoria_nombre = nombres[[j]],
        tarea_posicion = 1:4,
        tarea_diseno = 1:4,
        tarea_id = paste0(categorias[[j]], "_B1_T", 1:4),
        lados_intercambiados = c(FALSE, TRUE, FALSE, TRUE),
        perfil_A_id = paste0("A_", j, "_", 1:4),
        perfil_B_id = paste0("B_", j, "_", 1:4),
        n_diferencias = c(3L, 4L, 2L, 5L),
        eleccion = c("A", "B", "ninguna", "A"),
        perfil_elegido_id = c(
          paste0("A_", j, "_1"),
          paste0("B_", j, "_2"),
          NA,
          paste0("A_", j, "_4")
        ),
        alternativa_diseno_elegida = c("A", "A", "ninguna", "B"),
        eligio_A = c(TRUE, FALSE, FALSE, TRUE),
        eligio_B = c(FALSE, TRUE, FALSE, FALSE),
        eligio_ninguna = c(FALSE, FALSE, TRUE, FALSE),
        stringsAsFactors = FALSE
      )
    })
  )

  # Un conjunto reducido de atributos basta para comprobar transacción,
  # claves foráneas y reemplazo de una sesión. La app real insertará todos.
  atributos_presentados <- do.call(
    rbind,
    lapply(seq_len(nrow(elecciones)), function(i) {
      elec <- elecciones[i, ]
      do.call(
        rbind,
        lapply(c("A", "B"), function(alt) {
          data.frame(
            id_sesion = id_test,
            bloque_dce = 1L,
            categoria_posicion = elec$categoria_posicion,
            categoria_id = elec$categoria_id,
            tarea_posicion = elec$tarea_posicion,
            tarea_diseno = elec$tarea_diseno,
            tarea_id = elec$tarea_id,
            alternativa = alt,
            perfil_id = if (alt == "A") elec$perfil_A_id else elec$perfil_B_id,
            alternativa_elegida = identical(as.character(elec$eleccion), alt),
            eleccion_tarea = elec$eleccion,
            atributo_orden = 1L,
            atributo_id = "atributo_test",
            atributo_nombre = "Atributo de prueba",
            nivel_id = paste0("nivel_", alt),
            nivel_etiqueta = paste("Nivel", alt),
            nivel_orden = if (alt == "A") 1L else 2L,
            precio_clp = NA_real_,
            stringsAsFactors = FALSE
          )
        })
      )
    })
  )

  con <- conectar_supabase_dce()
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)

  if (!DBI::dbIsValid(con)) {
    stop("La conexión fue creada, pero RPostgres la reporta como inválida.")
  }

  # Asegurar limpieza si un test anterior quedó interrumpido.
  invalidar_sesion_supabase_dce(id_test, con = con)

  guardar_sesion_supabase_dce(
    participante = participante,
    elecciones = elecciones,
    atributos_presentados = atributos_presentados,
    con = con
  )

  conteo_1 <- contar_sesion_supabase_dce(id_test, con = con)
  print(conteo_1)

  if (
    conteo_1$participantes[[1]] != 1 ||
    conteo_1$elecciones[[1]] != 12 ||
    conteo_1$atributos[[1]] != nrow(atributos_presentados)
  ) {
    stop("El primer guardado no produjo los conteos esperados.")
  }

  cat("[OK] Primera escritura transaccional.\n")

  # Probar re-edición/sobrescritura de la misma sesión.
  elecciones$eleccion[nrow(elecciones)] <- "B"
  elecciones$perfil_elegido_id[nrow(elecciones)] <- elecciones$perfil_B_id[nrow(elecciones)]
  elecciones$alternativa_diseno_elegida[nrow(elecciones)] <- "A"
  elecciones$eligio_A[nrow(elecciones)] <- FALSE
  elecciones$eligio_B[nrow(elecciones)] <- TRUE
  elecciones$eligio_ninguna[nrow(elecciones)] <- FALSE

  participante$fin <- Sys.time()
  participante$duracion_segundos <- 300

  guardar_sesion_supabase_dce(
    participante = participante,
    elecciones = elecciones,
    atributos_presentados = atributos_presentados,
    con = con
  )

  conteo_2 <- contar_sesion_supabase_dce(id_test, con = con)

  if (
    conteo_2$participantes[[1]] != 1 ||
    conteo_2$elecciones[[1]] != 12 ||
    conteo_2$atributos[[1]] != nrow(atributos_presentados)
  ) {
    stop("La re-edición duplicó o perdió registros.")
  }

  ultima <- DBI::dbGetQuery(
    con,
    paste0(
      "select eleccion from tesis_dce.dce_elecciones ",
      "where id_sesion = $1 order by categoria_posicion desc, tarea_posicion desc limit 1"
    ),
    params = list(id_test)
  )$eleccion[[1]]

  if (!identical(as.character(ultima), "B")) {
    stop("La re-edición no actualizó la última elección a B.")
  }

  cat("[OK] Reedición reemplaza la versión previa sin duplicar filas.\n")

  # Limpiar: el test no debe dejar datos ficticios.
  invalidar_sesion_supabase_dce(id_test, con = con)

  conteo_3 <- contar_sesion_supabase_dce(id_test, con = con)

  if (sum(unlist(conteo_3)) != 0) {
    stop("La eliminación por cascada no limpió completamente la sesión de prueba.")
  }

  cat("[OK] Eliminación por cascada limpia participante, elecciones y atributos.\n")
  cat("\nTODAS LAS PRUEBAS DE SUPABASE PASARON ✅\n")

  invisible(TRUE)
}

probar_guardado_supabase_dce()
