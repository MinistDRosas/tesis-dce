# ==================================================
# CONSOLIDAR RESPUESTAS COMPLETAS DE LA ENCUESTA
# ==================================================

source("R/helpers.R")

resultado <- consolidar_respuestas_dce(
  ruta_base = "data/responses"
)

cat("\n==============================================\n")
cat("RESPUESTAS CONSOLIDADAS ✅\n")
cat("==============================================\n")
cat("Participantes:", resultado$n_participantes, "\n")
cat("Elecciones DCE:", resultado$n_elecciones, "\n")
cat("Filas de atributos presentados:", resultado$n_atributos_presentados, "\n\n")
cat("Archivos generados:\n")
cat(" -", resultado$rutas$participantes, "\n")
cat(" -", resultado$rutas$elecciones, "\n")
cat(" -", resultado$rutas$atributos_presentados, "\n")
