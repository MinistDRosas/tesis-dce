# ==================================================
# PRUEBAS DEL BLOQUE 6
# PERFILES VÁLIDOS Y TAREAS A/B DE PRUEBA
# ==================================================

library(readxl)

source("R/helpers.R")


# --------------------------------------------------
# Cargar instrumento
# --------------------------------------------------

ruta_instrumento <- "data/content/instrumento_DCE.xlsx"

instrumento <- cargar_instrumento_xlsx(
  ruta_instrumento
)

categorias <- instrumento$categorias
atributos <- instrumento$atributos
niveles <- instrumento$niveles


# --------------------------------------------------
# Validaciones previas
# --------------------------------------------------

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


categorias_ids <- as.character(
  categorias$categoria_id[
    !is.na(categorias$categoria_id) &
      categorias$categoria_id != ""
  ]
)


# --------------------------------------------------
# Construir pools válidos
# --------------------------------------------------

perfiles_validos <- setNames(
  lapply(
    categorias_ids,
    function(categoria_id) {

      obtener_perfiles_validos_categoria_dce(
        categoria_id = categoria_id,
        atributos = atributos,
        niveles = niveles
      )
    }
  ),
  categorias_ids
)


esperados <- c(
  gestion_marketing = 2295,
  atencion_cliente = 357,
  inventario_logistica = 156
)


cat(
  "\n============================================\n"
)

cat(
  "COMPROBACIÓN DE POOLS VÁLIDOS\n"
)

cat(
  "============================================\n"
)


for (categoria_id in names(esperados)) {

  cantidad <- nrow(
    perfiles_validos[[categoria_id]]
  )

  if (!identical(
    as.integer(cantidad),
    as.integer(esperados[[categoria_id]])
  )) {

    stop(
      paste0(
        "[FALLÓ] ",
        categoria_id,
        ": se esperaban ",
        esperados[[categoria_id]],
        " perfiles válidos y se obtuvieron ",
        cantidad,
        "."
      )
    )
  }

  cat(
    "[OK] ",
    categoria_id,
    ": ",
    cantidad,
    " perfiles válidos\n",
    sep = ""
  )
}


# --------------------------------------------------
# Comprobar generación repetida de tareas
# --------------------------------------------------

cat(
  "\n============================================\n"
)

cat(
  "COMPROBACIÓN DE TAREAS A/B\n"
)

cat(
  "============================================\n"
)


set.seed(
  20260830
)


for (categoria_id in categorias_ids) {

  pool <- perfiles_validos[[categoria_id]]

  for (repeticion in seq_len(30)) {

    tareas <- generar_tareas_prueba_categoria_dce(
      categoria_id = categoria_id,
      atributos = atributos,
      niveles = niveles,
      n_tareas = 3,
      min_diferencias = 2,
      perfiles_validos = pool
    )

    if (length(tareas) != 3) {

      stop(
        paste0(
          "[FALLÓ] ",
          categoria_id,
          ": no se generaron exactamente 3 tareas."
        )
      )
    }

    ids_perfiles <- character(0)

    for (i in seq_along(tareas)) {

      tarea <- tareas[[i]]

      validacion_a <- validar_perfil_dce(
        perfil = tarea$perfil_A,
        categoria_id = categoria_id,
        atributos = atributos,
        niveles = niveles,
        exigir_perfil_completo = TRUE
      )

      validacion_b <- validar_perfil_dce(
        perfil = tarea$perfil_B,
        categoria_id = categoria_id,
        atributos = atributos,
        niveles = niveles,
        exigir_perfil_completo = TRUE
      )

      if (!isTRUE(validacion_a$valido)) {

        stop(
          paste0(
            "[FALLÓ] ",
            categoria_id,
            " tarea ",
            i,
            ": Alternativa A inválida."
          )
        )
      }

      if (!isTRUE(validacion_b$valido)) {

        stop(
          paste0(
            "[FALLÓ] ",
            categoria_id,
            " tarea ",
            i,
            ": Alternativa B inválida."
          )
        )
      }

      diferencias <- contar_diferencias_perfiles_dce(
        perfil_a = tarea$perfil_A,
        perfil_b = tarea$perfil_B,
        categoria_id = categoria_id,
        atributos = atributos
      )

      if (diferencias < 2) {

        stop(
          paste0(
            "[FALLÓ] ",
            categoria_id,
            " tarea ",
            i,
            ": A y B difieren en menos de 2 atributos."
          )
        )
      }

      if (identical(
        tarea$perfil_A_id,
        tarea$perfil_B_id
      )) {

        stop(
          paste0(
            "[FALLÓ] ",
            categoria_id,
            " tarea ",
            i,
            ": A y B son el mismo perfil."
          )
        )
      }

      ids_perfiles <- c(
        ids_perfiles,
        tarea$perfil_A_id,
        tarea$perfil_B_id
      )
    }

    if (anyDuplicated(ids_perfiles) > 0) {

      stop(
        paste0(
          "[FALLÓ] ",
          categoria_id,
          ": se repitió un perfil dentro del mismo bloque de 3 tareas."
        )
      )
    }
  }

  cat(
    "[OK] ",
    categoria_id,
    ": 30 generaciones de 3 tareas superadas\n",
    sep = ""
  )
}


# --------------------------------------------------
# Generar una sesión completa de ejemplo
# --------------------------------------------------

tareas_sesion <- generar_tareas_prueba_dce(
  categorias_ids = categorias_ids,
  perfiles_validos_por_categoria =
    perfiles_validos,
  atributos = atributos,
  niveles = niveles,
  n_tareas = 3,
  min_diferencias = 2
)


if (
  !all(
    categorias_ids %in%
      names(tareas_sesion)
  )
) {

  stop(
    "[FALLÓ] La sesión completa no contiene todas las categorías."
  )
}


cat(
  "\n============================================\n"
)

cat(
  "RESUMEN DE UNA SESIÓN DE EJEMPLO\n"
)

cat(
  "============================================\n"
)


for (categoria_id in categorias_ids) {

  cat(
    "[OK] ",
    categoria_id,
    ": ",
    length(
      tareas_sesion[[categoria_id]]
    ),
    " tareas generadas\n",
    sep = ""
  )
}


cat(
  "\n============================================\n"
)

cat(
  "TODAS LAS PRUEBAS DE PERFILES Y TAREAS PASARON ✅\n"
)

cat(
  "============================================\n"
)
