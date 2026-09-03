# ==================================================
# TEST DEL DISEÑO EXPERIMENTAL DCE
# ==================================================

source("R/helpers.R")

ruta_instrumento <- "data/content/instrumento_DCE.xlsx"
ruta_diseno <- "data/design/diseno_dce.csv"

instrumento <- cargar_instrumento_xlsx(
  ruta_instrumento
)

categorias <- instrumento$categorias
atributos <- instrumento$atributos
niveles <- instrumento$niveles

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

diseno <- cargar_diseno_dce(
  ruta = ruta_diseno,
  categorias_ids = categorias_dce,
  atributos = atributos,
  niveles = niveles,
  n_blocks_esperados = 4,
  tareas_por_bloque = 4
)

cat("\n==============================================\n")
cat("TEST DEL DISEÑO DCE\n")
cat("==============================================\n")

for (categoria_id in categorias_dce) {

  cat("\n", categoria_id, "\n", sep = "")

  dcat <- diseno[
    diseno$categoria_id == categoria_id,
    ,
    drop = FALSE
  ]

  for (bloque in 1:4) {

    db <- dcat[
      as.integer(dcat$bloque) == bloque,
      ,
      drop = FALSE
    ]

    tareas <- sort(
      unique(
        as.integer(db$tarea_diseno)
      )
    )

    cat(
      "  Bloque ",
      bloque,
      ": ",
      length(tareas),
      " tareas",
      "\n",
      sep = ""
    )
  }
}

# Probar también la transformación a tareas de Shiny
set.seed(123)

tareas_sesion <- construir_tareas_desde_diseno_dce(
  diseno = diseno,
  bloque = 1,
  categorias_ids = categorias_dce,
  atributos = atributos,
  randomizar_lados = TRUE,
  randomizar_orden = TRUE
)

for (categoria_id in categorias_dce) {

  stopifnot(
    length(
      tareas_sesion[[categoria_id]]
    ) == 4
  )

  for (tarea in tareas_sesion[[categoria_id]]) {

    stopifnot(
      tarea$diferencias >= 2
    )

    stopifnot(
      !identical(
        tarea$perfil_A_id,
        tarea$perfil_B_id
      )
    )
  }
}

cat("\n==============================================\n")
cat("TODAS LAS PRUEBAS DEL DISEÑO PASARON ✅\n")
cat("==============================================\n")
