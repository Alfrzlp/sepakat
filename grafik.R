# Grafik Kapal ------------------------------------------------------------
grafik_kapal <- function(data) {
  plot_ly(arrange(data, date), type = "scatter", mode = "lines+markers") %>%
    add_trace(
      x = ~date, y = ~Kapal, connectgaps = TRUE,
      hovertemplate = paste("%{x}    :", "%{y} Kapal<extra></extra>"),
      line = list(color = "rgba(49,130,189, 1)", width = 3),
      marker = list(color = "rgba(49,130,189, 1)", size = 9),
      
      fill = "tozeroy",
      fillcolor = "rgba(49,130,189, 0.2)"
    ) %>%
    plotly::layout(
      showlegend = FALSE,
      margin = list(l = 0, r = 20, b = 0, t = 40, pad = 10),
      xaxis = list(
        zerolinewidth = 12,
        tickformat = "%b %y", title = "",
        # tickcolor = 'rgb(204, 204, 204)',
        tickwidth = 3,
        ticklen = 3,
        tickfont = list(
          family = "Poppins",
          size = 15,
          color = "rgb(82, 82, 82)"
        ),
        linewidth = 0
      ),
      yaxis = list(
        title = "",
        tickfont = list(
          family = "Poppins",
          size = 15,
          color = "rgb(82, 82, 82)"
        )
      ),
      hoverlabel = list(
        font = list(
          family = "Poppins",
          size = 15
        )
      ),
      # paper_bgcolor='rgb(255,255,255)',
      # plot_bgcolor='rgb(229,229,229)'
      plot_bgcolor = "#e5ecf6",
      annotations = list(
        text = "Jumlah Kunjungan Kapal", xref = "paper", x = 0,
        yref = "paper", y = 1, yshift = 40, xshift = 0, showarrow = FALSE,
        font = list(
          family = "Poppins",
          size = 17,
          color = "rgb(82, 82, 82)"
        )
      )
    ) %>%
    plotly::config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"))
}


# Grafik Lalu Lintas ------------------------------------------------------
grafik_lalulintas <- function(data, jenis) {
  satuan <- 'Unit'
    
  if (jenis == 'Penumpang') {
    satuan <- 'Orang'
    y1 <- 'Penumpang_muat'
    y2 <- 'Penumpang_bongkar'
    data <- data[c('date', y1, y2)]
    
  }else if (jenis == 'Mobil') {
    y1 <- 'Mobil_muat'
    y2 <- 'Mobil_bongkar'
    data <- data[c('date', y1, y2)]
    
    if (sum(data[2:3]) == 0) {
      return(empty_plot("Data tidak ditemukan"))
    }
    
  }else if (jenis == 'Motor') {
    y1 <- 'Motor_muat'
    y2 <- 'Motor_bongkar'
    data <- data[c('date', y1, y2)]
  
    if (sum(data[2:3]) == 0) {
      return(empty_plot("Data tidak ditemukan"))
    }
  }
  
  y1 <- as.formula(paste0('~ ', y1))
  y2 <- as.formula(paste0('~ ', y2))
  
  plot_ly(data = data, type = "scatter", mode = "lines+markers") %>%
    add_trace(
      name = 'Turun',
      x = ~date, y = y2, connectgaps = TRUE,
      hovertemplate = paste("%{x}    :", "%{y} ", satuan),
      line = list(color = "rgba(49,130,189, 1)", width = 3),
      marker = list(color = "rgba(49,130,189, 1)", size = 9),
      # biru 49,130,189,
      # orange gba(228,140,28, 1)
      fill = "tozeroy",
      fillcolor = "rgba(49,130,189, 0.2)"
    ) %>%
    add_trace(
      name = 'Naik',
      x = ~date, y = y1, connectgaps = TRUE,
      hovertemplate = paste("%{x}    :", "%{y} ", satuan),
      line = list(color = "rgba(228,140,28, 1)", width = 3),
      marker = list(color = "rgba(228,140,28, 1)", size = 9),
      
      fill = "tozeroy",
      fillcolor = "rgba(228,140,28, 0.2)"
    ) %>%
    plotly::layout(
      showlegend = FALSE,
      margin = list(l = 0, r = 20, b = 0, t = 40, pad = 10),
      xaxis = list(
        zerolinewidth = 12,
        tickformat = "%b %y", title = "",
        # tickcolor = 'rgb(204, 204, 204)',
        tickwidth = 3,
        ticklen = 3,
        tickfont = list(
          family = "Poppins",
          size = 15,
          color = "rgb(82, 82, 82)"
        ),
        linewidth = 0
      ),
      yaxis = list(
        title = "",
        tickfont = list(
          family = "Poppins",
          size = 15,
          color = "rgb(82, 82, 82)"
        )
      ),
      hoverlabel = list(
        font = list(
          family = "Poppins",
          size = 15
        )
      ),
      # paper_bgcolor='rgb(255,255,255)',
      # plot_bgcolor='rgb(229,229,229)'
      plot_bgcolor = "#e5ecf6",
      annotations = list(
        text = paste0("Jumlah Kunjungan ", jenis), xref = "paper", x = 0,
        yref = "paper", y = 1, yshift = 40, xshift = 0, showarrow = FALSE,
        font = list(
          family = "Poppins",
          size = 17,
          color = "rgb(82, 82, 82)"
        )
      )
    ) %>%
    plotly::config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"))
}



# Grafix Bongkar Muat -----------------------------------------------------
grafik_bm <- function(data, jenis, bar_color = 'rgba(0,100,80,0.7)'){
  plot_ly(
    data = data,
    x = ~nilai,
    y = ~ reorder(name, nilai),
    type = "bar",
    text = ~nilai, textposition = 'auto',
    hovertemplate = paste("%{y} :", "%{x} Ton<extra></extra>"),
    marker = list(
      color = bar_color
    )
  ) %>% 
    plotly::layout(
      margin = list(l = 0, r = 20, b = 0, t = 40, pad = 10),
      plot_bgcolor = "#e5ecf6",
      xaxis = list(
        zerolinewidth = 3,
        title = "",
        # tickcolor = 'rgb(204, 204, 204)',
        tickwidth = 3,
        ticklen = 3,
        tickfont = list(
          family = "Poppins",
          size = 13,
          color = "rgb(82, 82, 82)"
        ),
        linewidth = 0
      ),
      yaxis = list(
        title = "",
        tickfont = list(
          family = "Poppins",
          size = 13,
          color = "rgb(82, 82, 82)"
        )
      ),
      hoverlabel = list(
        font = list(
          family = "Poppins",
          size = 15
        )
      ),
      annotations = list(
        text = jenis, xref = "paper", x = 0,
        yref = "paper", y = 1, yshift = 40, xshift = 0, showarrow = FALSE,
        font = list(
          family = "Poppins",
          size = 17,
          color = "rgb(82, 82, 82)"
        )
      )
    ) %>% 
    plotly::config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d"))
}

