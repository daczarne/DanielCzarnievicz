#==============================#
#### FUNCIONES CONSTRUCCIÓN ####
#==============================#

## Genera los frames para el plot

accumulate_by <- function(dat, var) {

   var <- lazyeval::f_eval(var, dat)
   lvls <- plotly:::getLevels(var)
   dats <- base::lapply(base::seq_along(lvls), function(x) {
      base::cbind(dat[var %in% lvls[base::seq(1, x)], ], frame = lvls[[x]])
   })
   dplyr::bind_rows(dats)

}

## Genera plot con doble eje y animación para mostrar evolución de permisos y superficie

plotly_construccion <- function(.data) {

   .data %>%
      dplyr::group_by(
         year
      ) %>%
      dplyr::summarise(
         permisos = base::sum(permisos, na.rm = TRUE),
         superficie = base::sum(superficie, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      accumulate_by(
         ~year
      ) %>%
      plotly::plot_ly(
         x = ~year,
         y = ~permisos,
         frame = ~frame,
         type = "bar",
         hovertemplate = ~base::paste0(
            "%{y:,.0f}"
         ),
         name = "Permisos"
      ) %>%
      plotly::add_trace(
         x = ~year,
         y = ~superficie / 1e3,
         frame = ~frame,
         type = "scatter",
         mode = "markers+lines",
         yaxis = "y2",
         line = base::list(
            width = 4
         ),
         marker = base::list(
            size = 8,
            line = base::list(
               color = 'black',
               width = 2
            )
         ),
         hovertemplate = ~base::paste0(
            "%{y:,.0f}"
         ),
         name = "Superficie"
      ) %>%
      plotly::layout(
         separators = ",.",
         xaxis = base::list(
            title = NA
         ),
         yaxis = base::list(
            title = "<b>Permisos emitidos</b>",
            tickformat = ",.0f"
         ),
         yaxis2 = base::list(
            title = base::paste0("<b>", "Superficie (en miles de m2)", "</b>"),
            overlaying = "y",
            side = "right",
            automargin = TRUE
         ),
         legend = base::list(
            orientation = "h"
         ),
         hovermode = "x"
      ) %>%
      plotly::animation_opts(
         frame = 300,
         easing = "linear",
         redraw = TRUE,
         mode = "immediate"
      ) %>%
      plotly::animation_slider(
         currentvalue = list(
            visible = TRUE,
            prefix = "Año: ",
            font = list(
               color = "navyblue"
            )
         )
      ) %>%
      plotly::config(
         locale = "en",
         displayModeBar = TRUE,
         displaylogo = FALSE,
         modeBarButtonsToRemove = base::c(
            "zoom2d",
            "zoomIn2d",
            "zoomOut2d",
            "select2d",
            "drawclosedpath",
            "lasso2d",
            "pan2d",
            "drawrect",
            "autoScale2d",
            "hoverClosestCartesian",
            "hoverCompareCartesian",
            "toggleSpikelines"
         )
      ) %>%
      htmlwidgets::onRender(
         "$(function() {
            $('.updatemenu-item-text').click();
         });"
      )
}

#===============#
#### THE END ####
#===============#