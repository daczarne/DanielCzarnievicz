#=======================#
#### FUNCIONES ENCOR ####
#=======================#

### Genera el plot de la pregunta superior

plotly_questions_one <- function(q, th) {

   titulo <- dplyr::case_when(
      q == "cantidad_ideal_hijos" ~ "<b>Cantidad ideal de hijos</b>",
      q == "edad_ideal_primer_hijo" ~ "<b>Edad ideal para primer hijo</b>"
   )

   encor %>%
      dplyr::mutate(
         variable := !!rlang::sym(q)
      ) %>%
      dplyr::filter(
         tuvo_hijos == th
      ) %>%
      base::droplevels() %>%
      dplyr::group_by(
         sexo,
         variable
      ) %>%
      dplyr::summarise(
         n = base::sum(peso, na.rm = TRUE),
         .groups = "drop_last"
      ) %>%
      dplyr::mutate(
         prop = n / base::sum(n)
      ) %>%
      plotly::plot_ly() %>%
      plotly::add_trace(
         x = ~variable,
         y = ~prop,
         color = ~sexo,
         colors = "Dark2",
         type = "bar",
         hovertemplate = ~base::paste0(
            "%{y:0.2%}"
         )
      ) %>%
      plotly::layout(
         xaxis = base::list(
            title = titulo
         ),
         yaxis = base::list(
            title = "<b>Porcentaje</b>",
            tickformat = "%"
         ),
         legend = base::list(
            title = base::list(
               text = "<b>Sexo de quien<br>responde<b>"
            ),
            bgcolor = "#E2E2E2",
            orientation = "h",
            yanchor = "bottom",
            xanchor = "left",
            y = -.40
         ),
         hovermode = "x"
      ) %>%
      plotly::config(
         locale = "es",
         displayModeBar = FALSE
      )

}

### Genera el plot de las preguntas de motherhood

plotly_question_motherhood <- function(q) {

   encor %>%
      dplyr::mutate(
         variable := !!rlang::sym(q)
      ) %>%
      dplyr::group_by(
         sexo,
         variable
      ) %>%
      dplyr::summarise(
         n = base::sum(peso, na.rm = TRUE),
         .groups = "drop_last"
      ) %>%
      dplyr::mutate(
         prop = n / sum(n)
      ) %>%
      plotly::plot_ly() %>%
      plotly::add_trace(
         x = ~variable,
         y = ~prop,
         color = ~sexo,
         colors = "Dark2",
         type = "bar",
         hovertemplate = ~base::paste0(
            "%{y:0.2%}"
         )
      ) %>%
      plotly::layout(
         xaxis = base::list(
            title = NA
         ),
         yaxis = base::list(
            title = "<b>Porcentaje</b>",
            tickformat = "%"
         ),
         legend = base::list(
            title = base::list(
               text = "<b>Sexo de quien<br>responde<b>"
            ),
            bgcolor = "#E2E2E2",
            orientation = "h",
            yanchor = "bottom",
            xanchor = "left",
            y = -.40
         ),
         hovermode = "x"
      ) %>%
      plotly::config(
         locale = "es",
         displayModeBar = FALSE
      )

}

### Genera el sankey plot

generar_sankey <- function(.data, var_1, var_2) {

   var_1 <- "metodo_primera_relacion"

   var_2 <- "metodo_ultima_relacion"

   aux_data <- .data %>%
      dplyr::group_by(
         var_1 := !!rlang::sym(var_1),
         var_2 := !!rlang::sym(var_2)
      ) %>%
      dplyr::summarise(
         n = base::sum(peso, na.rm = TRUE),
         .groups = "drop_last"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(
         stats::complete.cases(.)
      ) %>%
      dplyr::transmute(
         source = var_1,
         target = stringr::str_c(var_2, " "),
         value = n / base::sum(n)
      )

   # Define nodos
   nodes <- base::data.frame(name = base::c(base::as.character(aux_data$source), base::as.character(aux_data$target)) %>% base::unique())

   # Agrega IDs con 0 indexing (porque JS usa 0 indexing)
   aux_data$IDsource <- base::match(aux_data$source, nodes$name) - 1L
   aux_data$IDtarget <- base::match(aux_data$target, nodes$name) - 1L

   # Construye el Sankey
   networkD3::sankeyNetwork(
      Links = base::as.data.frame(aux_data),
      Nodes = nodes,
      Source = "IDsource",
      Target = "IDtarget",
      Value = "value",
      NodeID = "name",
      sinksRight = FALSE,
      nodeWidth = 40L,
      fontSize = 13L,
      nodePadding = 20L
   )

}

paste_kable <- function(.data) {
   knitr::kable(utils::head(.data))
}

#===============#
#### THE END ####
#===============#