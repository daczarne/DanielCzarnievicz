#=============#
#### ENGIH ####
#=============#

library(tidyverse)

# Codigueras --------------------------------------------------------------
deptos <- tibble::tribble(
   ~depto_id, ~depto_name,
   01L, "Montevideo",
   02L, "Artigas",
   03L, "Canelones",
   04L, "Cerro Largo",
   05L, "Colonia",
   06L, "Durazno",
   07L, "Flores",
   08L, "Florida",
   09L, "Lavalleja",
   10L, "Maldonado",
   11L, "Paysandú",
   12L, "Río Negro",
   13L, "Rivera",
   14L, "Rocha",
   15L, "Salto",
   16L, "San José",
   17L, "Soriano",
   18L, "Tacuarembó",
   19L, "Treinta y Tres"
)

fuentes_de_energia_para_cocinar <- tibble::tribble(
   ~fuente_id, ~fuente_name,
   1L, "Energía eléctrica",
   2L, "Gas por cañería",
   3L, "Supergás",
   4L, "Queroseno",
   5L, "Leña",
   6L, "Ninguna"
)

dim_articulos <- readxl::read_xlsx(
   path = "DataSets/ENGIH/ENGIH 2016 Codificador CCIF.xlsx"
) %>%
   dplyr::rename_all(
      .funs = tolower
   ) %>%
   dplyr::transmute(
      articulo = base::as.integer(ccif),
      descripcion
   )

dim_categoria_gasto <- tibble::tribble(
   ~categoria_id, ~categoria_nombre,
   01L, "Panes, galletas, galletitas y otros productos derivados de la harina",
   02L, "Arroz, granos, y sus derivados",
   03L, "Pastas y pizzas",
   04L, "Carne bovina",
   05L, "Carne ovina",
   06L, "Carne porcina",
   07L, "Carne de aves",
   08L, "Chacinados y embutidos",
   09L, "Otras carnes",
   10L, "Pescados",
   11L, "Leche, quesos, yogurt, y otros lácteos",
   12L, "Huevos",
   13L, "Aceites y grasas",
   14L, "Frutas",
   15L, "Verduras",
   16L, "Azucar",
   17L, "Café, Té y Yerba mate",
   18L, "Dulces varios",
   19L, "Bebidas (no alcohólicas)",
   20L, "Otros gastos en alimentos y bebidas no alcohólicas",
   21L, "Bebidas alcohólicas, tabaco, y estupefacientes",
   22L, "Prendas de vestir y Calzados",
   23L, "Vivienda",
   24L, "Muebles y artículos para el hogar",
   25L, "Salud",
   26L, "Transporte",
   27L, "Comunicaciones",
   28L, "Recreación y cultura",
   29L, "Educación",
   30L, "Restaurantes y Hoteles",
   31L, "Bienes y servicios diversos",
   32L, "Gastos no de consumo",
   99L, "Imputados"
)

dim_divisiones <- readxl::read_xlsx(
   path = "DataSets/ENGIH/ENGIH 2016 Codificador CCIF.xlsx"
) %>%
   dplyr::rename_all(
      .funs = tolower
   ) %>%
   dplyr::transmute(
      division_id = base::as.integer(`división`),
      division_nombre = stringr::str_to_sentence(`división - nombre`)
   ) %>%
   dplyr::distinct()

dim_gastos <- readxl::read_xlsx(
   path = "DataSets/ENGIH/ENGIH 2016 Codificador CCIF.xlsx"
) %>%
   dplyr::rename_all(
      .funs = tolower
   ) %>%
   dplyr::transmute(
      articulo = base::as.integer(ccif),
      categoria_id = base::as.integer(categoria_id),
      division_id = base::as.integer(`división`),
      factor = `factor para mensualizar`
   )

# Gastos ------------------------------------------------------------------
haven::read_sav(
   file = "DataSets/ENGIH/ENGIH 2016 Base de Datos Gastos.sav"
) %>%
   dplyr::rename_all(
      .funs = tolower
   ) %>%
   dplyr::mutate(
      fecha = stringr::str_c(domanio, "-", dommes, "-01"),
      fecha = lubridate::ymd(fecha)
   ) %>%
   dplyr::transmute(
      numero,
      fecha,
      depto = base::factor(
         x = base::as.integer(domdepartamento),
         levels = deptos$depto_id,
         labels = deptos$depto_name
      ),
      articulo = base::as.integer(articulocodigo),
      cantidad = cantidadconvertida,
      calorias,
      destino = base::as.character(forcats::as_factor(destinocodigo)),
      destino = stringr::str_remove_all(destino, "‘|’"),
      destino = dplyr::if_else(destino %in% base::c("Hogar", "Extraordinario hogar"), "Hogar", "Otro"),
      destino = forcats::as_factor(destino),
      peso
   ) %>%
   dplyr::group_by(
      articulo,
      depto,
      destino
   ) %>%
   dplyr::summarise(
      cantidad_promedio_por_hogar = stats::weighted.mean(x = cantidad, w = peso, na.rm = TRUE),
      calorias_promedio_por_hogar = stats::weighted.mean(x = calorias, w = peso, na.rm = TRUE)
   )


#



#===============#
#### THE END ####
#===============#