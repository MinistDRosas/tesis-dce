# ============================================================
# TEST DE SEGURIDAD - USUARIO RESTRINGIDO SUPABASE
# ============================================================

source("R/db_supabase.R")

probar_seguridad_supabase <- function() {

  cat("\n==============================================\n")
  cat("TEST DE SEGURIDAD SUPABASE\n")
  cat("==============================================\n\n")

  con <- conectar_supabase_dce()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (!DBI::dbIsValid(con)) {
    stop("La conexión no es válida.")
  }

  identidad <- DBI::dbGetQuery(
    con,
    "select current_user, session_user, current_database() as database"
  )

  print(identidad)

  if (!identical(as.character(identidad$current_user[[1]]), "tesis_dce_app")) {
    stop(
      paste0(
        "La conexión no está usando el usuario restringido tesis_dce_app. ",
        "Usuario actual: ",
        identidad$current_user[[1]]
      )
    )
  }

  cat("[OK] La conexión usa tesis_dce_app.\n")

  rol <- DBI::dbGetQuery(
    con,
    paste0(
      "select rolsuper, rolcreatedb, rolcreaterole, rolreplication, rolbypassrls ",
      "from pg_roles where rolname = current_user"
    )
  )

  if (nrow(rol) != 1) {
    stop("No fue posible comprobar los atributos del rol.")
  }

  privilegios_peligrosos <- c(
    rol$rolsuper[[1]],
    rol$rolcreatedb[[1]],
    rol$rolcreaterole[[1]],
    rol$rolreplication[[1]],
    rol$rolbypassrls[[1]]
  )

  if (any(privilegios_peligrosos)) {
    stop("tesis_dce_app posee privilegios administrativos que no debería tener.")
  }

  cat("[OK] El usuario no es superusuario y no puede saltarse RLS.\n")

  tablas <- c(
    "tesis_dce.dce_participantes",
    "tesis_dce.dce_elecciones",
    "tesis_dce.dce_atributos_presentados"
  )

  for (tabla in tablas) {
    for (priv in c("SELECT", "INSERT", "UPDATE", "DELETE")) {
      q <- sprintf(
        "select has_table_privilege(current_user, '%s', '%s') as permitido",
        tabla,
        priv
      )

      permitido <- DBI::dbGetQuery(con, q)$permitido[[1]]

      if (!isTRUE(permitido)) {
        stop(
          paste0(
            "Falta el privilegio ", priv,
            " sobre ", tabla, "."
          )
        )
      }
    }
  }

  cat("[OK] Tiene únicamente los permisos operativos necesarios sobre las tres tablas DCE.\n")

  puede_crear_db <- DBI::dbGetQuery(
    con,
    "select has_database_privilege(current_user, current_database(), 'CREATE') as permitido"
  )$permitido[[1]]

  if (isTRUE(puede_crear_db)) {
    stop("tesis_dce_app puede crear objetos a nivel de base de datos; no debería.")
  }

  cat("[OK] No puede crear objetos a nivel de base de datos.\n")

  rls <- DBI::dbGetQuery(
    con,
    paste0(
      "select c.relname as tabla, c.relrowsecurity as rls_activo ",
      "from pg_class c ",
      "join pg_namespace n on n.oid = c.relnamespace ",
      "where n.nspname = 'tesis_dce' ",
      "and c.relname in ('dce_participantes','dce_elecciones','dce_atributos_presentados') ",
      "order by c.relname"
    )
  )

  if (nrow(rls) != 3 || !all(rls$rls_activo)) {
    stop("RLS no está activo en las tres tablas DCE.")
  }

  cat("[OK] RLS está activo en las tres tablas.\n")

  cat("\n==============================================\n")
  cat("TEST DE SEGURIDAD PASÓ ✅\n")
  cat("==============================================\n")

  invisible(TRUE)
}

probar_seguridad_supabase()
