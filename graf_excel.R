#### Formato de grafica excel ####

grafica_excel <- function(ts_obj,
                          titulo = NULL,
                          subtitulo = NULL,
                          linea_unidad = NULL,
                          nota = NULL,
                          fuente = NULL,
                          colores = NULL,
                          nombres = NULL,   
                          y_break_by = NULL,
                          date_breaks = "1 year",
                          date_labels = "%Y",
                          etiqueta_mes = NULL,
                          mostrar_leyenda = TRUE,
                          posicion_leyenda = c(0.15, 0.85),
                          grosor_linea = 1,
                          exportar = FALSE,
                          nombre_archivo = "grafica_excel.png",
                          ancho = 10,
                          alto = 6,
                          dpi = 300) {
  
  library(ggplot2)
  library(ggthemes)
  library(zoo)
  library(reshape2)
  library(showtext)
  
  font_add("Arial", "C:/Windows/Fonts/arial.ttf")
  showtext_auto()
  
  if (!is.ts(ts_obj)) {
    stop("El objeto debe ser de clase 'ts'")
  }
  
  # Convertir ts a data.frame con fechas reales
  fechas <- as.Date(as.yearmon(time(ts_obj)))
  df <- data.frame(fecha = fechas, coredata(ts_obj))
  
  if (ncol(df) == 2) {
    colnames(df)[2] <- "Serie_1"
  }
  
  if (is.null(colnames(df)[-1])) {
    colnames(df)[-1] <- paste0("Serie_", seq_len(ncol(df) - 1))
  }
  
  df_long <- melt(df,
                  id.vars = "fecha",
                  variable.name = "Serie",
                  value.name = "Valor")
  
  # ==================================================
  # Names
  # ==================================================
  
  if (!is.null(nombres)) {
    
    if (length(nombres) != length(unique(df_long$Serie))) {
      stop("El número de elementos en 'nombres' debe coincidir con el número de series.")
    }
    
    df_long$Serie <- factor(df_long$Serie,
                            levels = unique(df_long$Serie),
                            labels = nombres)
  }
  
  # ==================================================
  
  if (is.null(colores)) {
    colores <- ggthemes::excel_new_pal()(length(unique(df_long$Serie)))
  }
  
  # ==================================================
  # Eje Y
  # ==================================================
  
  rango_y <- range(df_long$Valor, na.rm = TRUE)
  
  if (!is.null(y_break_by)) {
    
    ymin <- floor(rango_y[1] / y_break_by) * y_break_by
    ymax <- ceiling(rango_y[2] / y_break_by) * y_break_by
    
    escala_y <- scale_y_continuous(
      limits = c(ymin, ymax),
      breaks = seq(ymin, ymax, by = y_break_by),
      expand = c(0, 0)
    )
    
  } else {
    
    escala_y <- scale_y_continuous(
      limits = rango_y,
      expand = c(0, 0)
    )
  }
  
  # ==================================================
  # Eje X
  # ==================================================
  
  rango_x <- range(df_long$fecha, na.rm = TRUE)
  
  escala_x <- scale_x_date(
    limits = rango_x,
    date_breaks = date_breaks,
    date_labels = date_labels,
    expand = c(0, 0)
  )
  
  # ==================================================
  # Grafica
  # ==================================================
  
  p <- ggplot(df_long,
              aes(x = fecha,
                  y = Valor,
                  color = Serie)) +
    
    geom_line(linewidth = grosor_linea) +
    
    scale_color_manual(values = colores) +
    escala_x +
    escala_y +
    
    labs(
      title = titulo,
      subtitle = if (!is.null(linea_unidad)) {
        paste0(subtitulo, "\n", linea_unidad)
      } else {
        subtitulo
      },
      x = NULL,
      y = NULL,
      color = NULL,
      caption = if (!is.null(nota) | !is.null(fuente)) {
        paste0(nota, "\n", fuente)
      } else {
        NULL
      }
    ) +
    
    theme_excel_new(base_family = "Arial") +
    
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.line.x = element_line(color = "black", linewidth = 0.6),
      axis.line.y = element_line(color = "black", linewidth = 0.6),
      
      axis.ticks.length = grid::unit(0.15, "cm"),
      axis.ticks = element_line(color = "black", linewidth = 0.4),
      
      axis.text = element_text(color = "black",
                               family = "Arial",
                               size = 8),
      
      axis.text.x = element_text(
        margin = margin(t = 4),
        vjust = 1
      ),
      
      axis.text.y = element_text(
        margin = margin(r = 6)
      ),
      
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 11,
        family = "Arial",
        colour = "black"
      ),
      
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 10,
        family = "Arial",
        colour = "black"
      ),
      
      plot.caption = element_text(
        hjust = 0,
        size = 8,
        family = "Arial",
        colour = "black"
      ),
      
      legend.position = if (mostrar_leyenda) posicion_leyenda else "none",
      
      legend.background = element_rect(
        fill = "transparent",
        colour = NA),
      
      legend.key = element_rect(
        fill = "transparent",
        colour = NA),
      
      legend.text = element_text(
        family = "Arial",
        color = "black",
        size = 8
      ),
      
      plot.margin = margin(15, 20, 40, 20)
    )
  
  if (!is.null(etiqueta_mes)) {
    p <- p +
      annotate("text",
               x = max(df$fecha),
               y = min(df_long$Valor),
               label = etiqueta_mes,
               hjust = 1,
               vjust = -1,
               family = "Arial",
               size = 5)
  }
  
  if (exportar) {
    ggsave(nombre_archivo,
           plot = p,
           width = ancho,
           height = alto,
           dpi = dpi)
  }
  
  return(p)
}
