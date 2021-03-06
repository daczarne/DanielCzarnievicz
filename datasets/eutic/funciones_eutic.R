#=======================#
#### FUNCIONES EUTIC ####
#=======================#

### Genera plot para las TIC que los hogares tienen

plotly_hogares_tienen <- function(.data, group_var_1, group_var_2) {

   xaxis_title <- dplyr::case_when(
      group_var_1 == "localidad" ~ "Localidad",
      group_var_1 == "ingresos_total" ~ "Nivel de ingresos"
   )

   .data %>%
      dplyr::transmute(
         group_var_1 = !!rlang::sym(group_var_1),
         group_var_2 = !!rlang::sym(group_var_2),
         peso_hogar
      ) %>%
      dplyr::group_by(
         group_var_1,
         group_var_2
      ) %>%
      dplyr::summarise(
         n = base::sum(peso_hogar, na.rm = TRUE),
         .groups = "drop_last"
      ) %>%
      dplyr::mutate(
         proporcion = n / base::sum(n, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      plotly::plot_ly() %>%
      plotly::add_trace(
         x = ~group_var_1,
         y = ~proporcion,
         color = ~group_var_2,
         colors = "Paired",
         type = "bar",
         hovertemplate = ~base::paste0(
            "%{y:0.2%}"
         )
      ) %>%
      plotly::layout(
         xaxis = base::list(
            title = base::paste("<b>", xaxis_title, "</b>")
         ),
         yaxis = base::list(
            title = "<b>Porcentaje de los hogares</b>",
            tickformat = "%"
         ),
         legend = base::list(
            bgcolor = "#E2E2E2",
            orientation = "h",
            yanchor = "bottom",
            xanchor = "left",
            y = -.30
         ),
         hovermode = "x"
      ) %>%
      plotly::config(
         locale = "es",
         displayModeBar = FALSE
      )

}

### Genera plot para las TIC que las personas usan

plotly_personas_uso_tic <- function(.data, group_var_1, group_var_2, plotly_legend_y = -0.45) {

   colors <- dplyr::if_else(group_var_2 %in% base::c("frecuencia_uso_internet", "frecuencia_uso_internet_celular"), "Accent", "Paired")

   xaxis_title <- dplyr::case_when(
      group_var_1 == "localidad" ~ "Localidad",
      group_var_1 == "ingresos_total" ~ "Nivel del ingresos (del hogar)",
      group_var_1 == "sexo" ~ "Sexo",
      group_var_1 == "nivel_educ" ~ "Nivel educativo"
   )

   .data %>%
      dplyr::mutate(
         group_var_1 = !!rlang::sym(group_var_1),
         group_var_2 = !!rlang::sym(group_var_2)
      ) %>%
      dplyr::group_by(
         group_var_1,
         group_var_2
      ) %>%
      dplyr::summarise(
         n = base::sum(peso_persona, na.rm = TRUE),
         .groups = "drop_last"
      ) %>%
      dplyr::mutate(
         prop = n / base::sum(n, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      plotly::plot_ly() %>%
      plotly::add_trace(
         x = ~group_var_1,
         y = ~prop,
         color = ~group_var_2,
         colors = colors,
         type = "bar",
         hovertemplate = ~base::paste0(
            "%{y:0.2%}"
         )
      ) %>%
      plotly::layout(
         xaxis = base::list(
            title = base::paste("<b>", xaxis_title, "</b>")
         ),
         yaxis = base::list(
            title = "<b>Porcentaje de las personas</b>",
            tickformat = "%"
         ),
         legend = base::list(
            bgcolor = "#E2E2E2",
            orientation = "h",
            yanchor = "bottom",
            xanchor = "left",
            y = plotly_legend_y
         ),
         hovermode = "x"
      ) %>%
      plotly::config(
         locale = "es",
         displayModeBar = FALSE
      )

}

### Genera plot para los usos que las personas hacen de las TICs

plotly_personas_usos_tics <- function(.data, group_by_var, plotly_legend_y = -0.35) {

   xaxis_title <- dplyr::case_when(
      group_by_var == "localidad" ~ "Localidad",
      group_by_var == "ingresos_total" ~ "Nivel de ingresos (del hogar)",
      group_by_var == "sexo" ~ "Sexo",
      group_by_var == "nivel_educ" ~ "Nivel educativo"
   )

   .data %>%
      plotly::plot_ly() %>%
      plotly::add_trace(
         x = ~group_by_var,
         y = ~proporcion,
         color = ~tipo_uso,
         colors = "Set3",
         type = "bar",
         hovertemplate = ~base::paste0(
            "%{y:0.2%}"
         )
      ) %>%
      plotly::layout(
         xaxis = base::list(
            title = base::paste("<b>", xaxis_title, "</b>")
         ),
         yaxis = base::list(
            title = "<b>Porcentaje de las personas</b>",
            tickformat = "%"
         ),
         legend = base::list(
            bgcolor = "#E2E2E2",
            orientation = "h",
            yanchor = "bottom",
            xanchor = "left",
            y = plotly_legend_y
         ),
         hovermode = "x"
      ) %>%
      plotly::config(
         locale = "es",
         displayModeBar = FALSE
      )

}

### Genera data set de usos de Internet por tipo de uso

generar_data_usos_internet_por_tipo_de_uso <- function(.data, group_by_var, var_pattern) {

   var_list <- stringr::str_subset(
      string = base::names(eutic),
      pattern = var_pattern
   )

   aux_data <- .data %>%
      dplyr::mutate(
         group_by_var = !!rlang::sym(group_by_var),
         pattern_var = !!rlang::sym(var_list[1])
      ) %>%
      dplyr::group_by(
         group_by_var,
         pattern_var
      ) %>%
      dplyr::summarise(
         n = base::sum(peso_hogar, na.rm = TRUE),
         .groups = "drop_last"
      ) %>%
      dplyr::mutate(
         proporcion = n / base::sum(n, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(
         pattern_var == "Sí"
      ) %>%
      dplyr::transmute(
         group_by_var,
         tipo_uso = stringr::str_replace(
            string = var_list[1],
            pattern = base::paste0("usos_internet", var_pattern),
            replacement = ""
         ),
         proporcion
      )

   for (i in 2:base::length(var_list)) {

      aux_data %<>%
         dplyr::bind_rows(
            .data %>%
               dplyr::mutate(
                  group_by_var = !!rlang::sym(group_by_var),
                  pattern_var = !!rlang::sym(var_list[i])
               ) %>%
               dplyr::group_by(
                  group_by_var,
                  pattern_var
               ) %>%
               dplyr::summarise(
                  n = base::sum(peso_hogar, na.rm = TRUE),
                  .groups = "drop_last"
               ) %>%
               dplyr::mutate(
                  proporcion = n / base::sum(n, na.rm = TRUE)
               ) %>%
               dplyr::ungroup() %>%
               dplyr::filter(
                  pattern_var == "Sí"
               ) %>%
               dplyr::transmute(
                  group_by_var,
                  tipo_uso = stringr::str_replace(
                     string = var_list[i],
                     pattern = base::paste0("usos_internet", var_pattern),
                     replacement = ""
                  ),
                  proporcion
               )
         )

   }

   aux_data %>%
      dplyr::mutate(
         tipo_uso = dplyr::case_when(
            tipo_uso == "bienes_y_servicios" ~ "Bienes y servicios",
            tipo_uso == "servicios_medicos" ~ "Servicios médicos",
            tipo_uso == "salud" ~ "Salud en general",
            tipo_uso == "estado" ~ "Estado o gobierno",
            tipo_uso == "wikis" ~ "Información en general",
            tipo_uso == "informacion" ~ "Buscar información",
            tipo_uso == "curso_a_distancia" ~ "Curso a distancia",
            tipo_uso == "interactuar_centro_de_estudio" ~ "Inscripciones",
            tipo_uso == "material_docente" ~ "Descargó material docente",
            tipo_uso == "buscar_trabajo" ~ "Buscó empleo",
            tipo_uso == "wfh" ~ "Teletrabajo",
            tipo_uso == "email_laboral" ~ "Responder correo fuera de horario",
            tipo_uso == "email_personal" ~ "Enviar/Recivir correos",
            tipo_uso == "redes" ~ "Utilizar redes sociales",
            tipo_uso == "chat" ~ "Chatear",
            tipo_uso == "llamadas" ~ "Llamadas",
            tipo_uso == "date_app" ~ "Apps para conocer gente",
            tipo_uso == "radio" ~ "Escuchar radio",
            tipo_uso == "tv" ~ "Ver TV",
            tipo_uso == "streaming" ~ "Streaming",
            tipo_uso == "gaming" ~ "Gaming",
            tipo_uso == "software" ~ "Descargar software",
            tipo_uso == "leer" ~ "Leer",
            tipo_uso == "blogging" ~ "Blogging",
            tipo_uso == "web" ~ "Página web propia",
            tipo_uso == "storage" ~ "Almacenar archivos online",
            tipo_uso == "compra" ~ "Compras online",
            tipo_uso == "venta" ~ "Ventas online",
            tipo_uso == "banking" ~ "Banca online",
            tipo_uso == "booking" ~ "Reservas online",
            tipo_uso == "youtube" ~ "YouTube",
            tipo_uso == "netflix" ~ "Netflix",
            tipo_uso == "veratv" ~ "VeraTV",
            tipo_uso == "aire" ~ "Canales de aire",
            tipo_uso == "cable" ~ "Canales de cable",
            tipo_uso == "otro" ~ "Otros",
            tipo_uso == "tarjeta_internacional" ~ "Tj. de crédito internacional",
            tipo_uso == "tarjeta_nacional" ~ "Tj. de crédito nacional",
            tipo_uso == "tarjeta_prepaga" ~ "Tj. prepaga internacional",
            tipo_uso == "tarjeta_debito" ~ "Tj. de débito",
            tipo_uso == "paypal" ~ "Paypal",
            tipo_uso == "antel" ~ "Antel Bits",
            tipo_uso == "otros" ~ "Otros",
            tipo_uso == "facebook" ~ "Facebook",
            tipo_uso == "twitter" ~ "Twitter",
            tipo_uso == "google" ~ "Google+",
            tipo_uso == "instagram" ~ "Instagram",
            tipo_uso == "linkedin" ~ "LinkedIn",
            tipo_uso == "otras" ~ "Otras redes",
            TRUE ~ tipo_uso
         ),
         tipo_uso = forcats::as_factor(tipo_uso)
      )

}

#===============#
#### THE END ####
#===============#