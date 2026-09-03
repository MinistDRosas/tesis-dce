# =============================================================
# REINTENTAR OPERACIONES SUPABASE PENDIENTES
# =============================================================

source("R/helpers.R")
source("R/db_supabase.R")

cat("\n==============================================\n")
cat("SINCRONIZACIÓN DE PENDIENTES SUPABASE\n")
cat("==============================================\n\n")

if (!supabase_configurado_dce()) {
  stop("Supabase no está configurado en las variables de entorno.")
}

resultado <- sincronizar_pendientes_supabase_dce(
  ruta_base = "data/responses",
  verbose = TRUE
)

cat("\nProcesados:", resultado$procesados, "\n")
cat("Exitosos:", resultado$exitosos, "\n")
cat("Fallidos:", resultado$fallidos, "\n")
