# df_kapal <- readxl::read_xlsx("data/df_kapal.xlsx")
# df_kapal <- df_kapal[df_kapal$pelabuhan == "Jampea", ]


# Grafik Kapal ------------------------------------------------------------
grafik_kapal <- function(data) {
  plot_ly(arrange(data, date), type = "scatter", mode = "lines+markers") %>%
    add_trace(
      x = ~date, y = ~kapal, fill = "tozeroy", connectgaps = TRUE,
      hovertemplate = paste("%{x}    :", "%{y} Kapal<extra></extra>"),
      line = list(color = "rgba(49,130,189, 1)", width = 3),
      marker = list(color = "rgba(49,130,189, 1)", size = 9),
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

# grafik_kapal(df_kapal)



# Grafik Lalu Lintas ------------------------------------------------------
# df_lalulintas <- readxl::read_xlsx('data/df_lalulintas.xlsx')
# df_sub <- df_lalulintas[df_lalulintas$pelabuhan == 'Jampea', ]


grafik_lalulintas <- function(data, jenis) {
  df_sub <- data[data$nama == jenis, ]
  df_sub <- arrange(df_sub, date)
  
  df_naik <- df_sub[df_sub$jenis == 'muat', ]
  df_turun <- df_sub[df_sub$jenis == 'bongkar', ]
  
  if (jenis == 'Penumpang') {
    satuan <- 'Orang'
  }else{
    satuan <- 'Unit'
  }
  
  plot_ly(type = "scatter", mode = "lines+markers") %>%
    add_trace(
      name = 'Naik',
      data = df_naik,
      x = ~date, y = ~nilai, connectgaps = TRUE,
      hovertemplate = paste("%{x}    :", "%{y} ", satuan),
      line = list(width = 3),
      marker = list(size = 9)
    ) %>%
    add_trace(
      name = 'Turun',
      data = df_turun,
      x = ~date, y = ~nilai, connectgaps = TRUE,
      hovertemplate = paste("%{x}    :", "%{y} Orang"),
      line = list(color = "rgba(49,130,189, 1)", width = 3),
      marker = list(color = "rgba(49,130,189, 1)", size = 9)
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


# grafik_lalulintas(df_sub, 'Mobil')
# grafik_lalulintas(df_sub, 'Motor')
# grafik_lalulintas(df_sub, 'Penumpang')


# Grafix Bongkar Muat -----------------------------------------------------
# df_muat <- readxl::read_xlsx('data/RekapMuat.xlsx', sheet = 'Jampea')
# 
# p_muat <- df_muat %>% 
#   dplyr::select(-Motor, -Mobil, -Penumpang) %>%
#   pivot_longer(-c(1:2)) %>% 
#   group_by(tahun, name) %>% 
#   summarise(
#     nilai = sum(value)
#   ) %>% 
#   filter(tahun == 2023, nilai != 0) %>% 
#   top_n(10, nilai) 


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


# grafik_bm(p_muat, 'Muat')
# grafik_bm(p_muat, 'Muat', bar_color = 'rgba(49,130,189, 0.9)')
