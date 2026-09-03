source("R/db_supabase.R")

cat("\n==============================================\n")
cat("TEST DE CONEXIÓN A SUPABASE\n")
cat("==============================================\n\n")

probar_conexion_supabase_dce <- function() {
  con <- conectar_supabase_dce()

  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)

  if (!DBI::dbIsValid(con)) {
    stop("La conexión fue creada, pero RPostgres la reporta como inválida.")
  }

  cat("[OK] Conexión creada y válida según DBI.\n")

  info <- DBI::dbGetQuery(
    con,
    "select current_database() as database, current_user as usuario, now() as hora_servidor"
  )

  print(info)

  requeridas <- c(
    "dce_participantes",
    "dce_elecciones",
    "dce_atributos_presentados"
  )

  existen <- vapply(
    requeridas,
    function(tabla) {
      DBI::dbExistsTable(
        con,
        DBI::Id(schema = "tesis_dce", table = tabla)
      )
    },
    logical(1)
  )

  faltantes <- requeridas[!existen]

  if (length(faltantes) > 0) {
    stop(
      paste0(
        "Faltan tablas en Supabase: ",
        paste(faltantes, collapse = ", "),
        "."
      )
    )
  }

  cat("\n[OK] Conexión establecida.\n")
  cat("[OK] Las tres tablas DCE existen.\n")
  cat("\nTEST DE CONEXIÓN PASÓ ✅\n")

  invisible(TRUE)
}

probar_conexion_supabase_dce()
