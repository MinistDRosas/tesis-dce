# ==================================================
# PRUEBAS AUTOMÁTICAS DE RESTRICCIONES DCE
# ==================================================
# Ejecutar desde la raíz del proyecto con:
# source("scripts/test_restricciones_dce.R")
# ==================================================

source("R/helpers.R")

ruta_instrumento <- "data/content/instrumento_DCE.xlsx"

instrumento <- cargar_instrumento_xlsx(
  ruta_instrumento
)

categorias <- instrumento$categorias
atributos <- instrumento$atributos
niveles <- instrumento$niveles

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

cat("\n==============================================\n")
cat("PRUEBAS DE RESTRICCIONES DCE\n")
cat("==============================================\n\n")


# --------------------------------------------------
# Funciones auxiliares de prueba
# --------------------------------------------------

esperar_invalido <- function(
    nombre_prueba,
    perfil,
    categoria_id,
    restriccion_esperada
) {
  
  resultado <- validar_perfil_dce(
    perfil = perfil,
    categoria_id = categoria_id,
    atributos = atributos,
    niveles = niveles
  )
  
  if (
    isTRUE(resultado$valido) ||
    !(restriccion_esperada %in%
        resultado$restricciones_incumplidas)
  ) {
    stop(
      paste0(
        "FALLÓ: ",
        nombre_prueba,
        ". Se esperaba incumplir ",
        restriccion_esperada,
        "."
      )
    )
  }
  
  cat(
    "[OK] ",
    nombre_prueba,
    " -> ",
    paste(
      resultado$restricciones_incumplidas,
      collapse = ", "
    ),
    "\n",
    sep = ""
  )
}


esperar_valido <- function(
    nombre_prueba,
    perfil,
    categoria_id
) {
  
  resultado <- validar_perfil_dce(
    perfil = perfil,
    categoria_id = categoria_id,
    atributos = atributos,
    niveles = niveles
  )
  
  if (!isTRUE(resultado$valido)) {
    stop(
      paste0(
        "FALLÓ: ",
        nombre_prueba,
        ". El perfil debía ser válido, pero incumplió: ",
        paste(
          resultado$restricciones_incumplidas,
          collapse = ", "
        ),
        "."
      )
    )
  }
  
  cat(
    "[OK] ",
    nombre_prueba,
    " -> válido\n",
    sep = ""
  )
}


# ==================================================
# GESTIÓN COMERCIAL Y MARKETING
# ==================================================

perfil_gm_invalido <- list(
  bi_analisis = "no_incluido",
  prediccion_demanda = "no_incluida",
  gestion_precios = "no_incluida",
  generacion_contenido = "no_incluida",
  automatizacion = "a_solicitud",
  precio_mensual = "precio_40000"
)

perfil_gm_valido <- list(
  bi_analisis = "descriptivo",
  prediccion_demanda = "no_incluida",
  gestion_precios = "no_incluida",
  generacion_contenido = "no_incluida",
  automatizacion = "a_solicitud",
  precio_mensual = "precio_40000"
)

esperar_invalido(
  "GM-R1: servicio sin funciones principales",
  perfil_gm_invalido,
  "gestion_marketing",
  "GM-R1"
)

esperar_valido(
  "GM: una función principal basta para constituir el servicio",
  perfil_gm_valido,
  "gestion_marketing"
)


# ==================================================
# ATENCIÓN Y ASISTENCIA AL CLIENTE
# ==================================================

perfil_ac_r1 <- list(
  autonomia = "responde_consultas",
  canales_digitales = "no_incluidos",
  atencion_telefonica = "no_incluida",
  integracion = "consulta",
  continuidad_humana = "transferencia_contexto",
  precio_mensual = "precio_50000"
)

perfil_ac_r2 <- list(
  autonomia = "gestiona_solicitud",
  canales_digitales = "chat_web",
  atencion_telefonica = "no_incluida",
  integracion = "sin_integracion",
  continuidad_humana = "sin_transferencia",
  precio_mensual = "precio_50000"
)

perfil_ac_valido_transferencia <- list(
  autonomia = "gestiona_solicitud",
  canales_digitales = "chat_web",
  atencion_telefonica = "no_incluida",
  integracion = "sin_integracion",
  continuidad_humana = "transferencia_contexto",
  precio_mensual = "precio_50000"
)

perfil_ac_valido_integracion <- list(
  autonomia = "gestiona_solicitud",
  canales_digitales = "chat_web",
  atencion_telefonica = "no_incluida",
  integracion = "consulta",
  continuidad_humana = "sin_transferencia",
  precio_mensual = "precio_50000"
)

esperar_invalido(
  "AC-R1: servicio sin canal digital ni telefónico",
  perfil_ac_r1,
  "atencion_cliente",
  "AC-R1"
)

esperar_invalido(
  "AC-R2: gestiona solicitud sin integración ni transferencia",
  perfil_ac_r2,
  "atencion_cliente",
  "AC-R2"
)

esperar_valido(
  "AC: gestión sin integración es válida si transfiere con contexto",
  perfil_ac_valido_transferencia,
  "atencion_cliente"
)

esperar_valido(
  "AC: gestión sin transferencia es válida si consulta sistemas",
  perfil_ac_valido_integracion,
  "atencion_cliente"
)


# ==================================================
# INVENTARIO Y LOGÍSTICA
# ==================================================

perfil_il_r1 <- list(
  control_inventario = "no_incluido",
  reposicion = "no_incluida",
  distribucion = "no_incluida",
  integracion = "integracion_automatica",
  precio_mensual = "precio_35000"
)

perfil_il_r2 <- list(
  control_inventario = "situaciones_conocidas",
  reposicion = "reposicion_automatica",
  distribucion = "no_incluida",
  integracion = "carga_manual",
  precio_mensual = "precio_35000"
)

perfil_il_r3 <- list(
  control_inventario = "situaciones_conocidas",
  reposicion = "no_incluida",
  distribucion = "optimizacion_dinamica",
  integracion = "carga_manual",
  precio_mensual = "precio_35000"
)

perfil_il_valido_reposicion <- list(
  control_inventario = "no_incluido",
  reposicion = "reposicion_automatica",
  distribucion = "no_incluida",
  integracion = "integracion_automatica",
  precio_mensual = "precio_90000"
)

perfil_il_valido_distribucion <- list(
  control_inventario = "no_incluido",
  reposicion = "no_incluida",
  distribucion = "optimizacion_dinamica",
  integracion = "integracion_automatica",
  precio_mensual = "precio_90000"
)

esperar_invalido(
  "IL-R1: servicio sin control, reposición ni distribución",
  perfil_il_r1,
  "inventario_logistica",
  "IL-R1"
)

esperar_invalido(
  "IL-R2: reposición automática con carga manual",
  perfil_il_r2,
  "inventario_logistica",
  "IL-R2"
)

esperar_invalido(
  "IL-R3: optimización dinámica con carga manual",
  perfil_il_r3,
  "inventario_logistica",
  "IL-R3"
)

esperar_valido(
  "IL: reposición automática con integración automática",
  perfil_il_valido_reposicion,
  "inventario_logistica"
)

esperar_valido(
  "IL: optimización dinámica con integración automática",
  perfil_il_valido_distribucion,
  "inventario_logistica"
)


# ==================================================
# COMPROBACIÓN EXHAUSTIVA DEL UNIVERSO DE PERFILES
# ==================================================

cat("\n----------------------------------------------\n")
cat("COMPROBACIÓN EXHAUSTIVA\n")
cat("----------------------------------------------\n")

conteos_esperados <- list(
  gestion_marketing = c(
    total = 2304,
    validos = 2295,
    invalidos = 9
  ),
  atencion_cliente = c(
    total = 432,
    validos = 357,
    invalidos = 75
  ),
  inventario_logistica = c(
    total = 216,
    validos = 156,
    invalidos = 60
  )
)

for (categoria_id in names(conteos_esperados)) {
  
  universo <- evaluar_universo_perfiles_dce(
    categoria_id = categoria_id,
    atributos = atributos,
    niveles = niveles
  )
  
  total <- nrow(universo)
  validos <- sum(universo$valido)
  invalidos <- total - validos
  
  esperado <- conteos_esperados[[categoria_id]]
  
  if (
    total != esperado[["total"]] ||
    validos != esperado[["validos"]] ||
    invalidos != esperado[["invalidos"]]
  ) {
    stop(
      paste0(
        "FALLÓ el conteo exhaustivo de '",
        categoria_id,
        "'. Obtenido: ",
        total,
        " total / ",
        validos,
        " válidos / ",
        invalidos,
        " inválidos."
      )
    )
  }
  
  cat(
    "[OK] ",
    categoria_id,
    ": ",
    total,
    " perfiles totales | ",
    validos,
    " válidos | ",
    invalidos,
    " inválidos\n",
    sep = ""
  )
}

cat("\n==============================================\n")
cat("TODAS LAS PRUEBAS DE RESTRICCIONES PASARON ✅\n")
cat("==============================================\n\n")
