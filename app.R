library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(shinyjs)
# library(bslib)
library(waiter)
library(plotly)
library(xlsx)
library(DT)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
# library(leaflet)

source('syntaxOlahData.R')
source('grafik.R')

# Global Variable ---------------------------------------------------------
# NAMA_PELABUHAN <- c('Jampea', "Pamatata", "Benteng/Selayar") 
NAMA_PELABUHAN <- c('Jampea', "Pamatata") 
NAMA_BULAN <- c(
  "Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", 
  "Agustus", "September", "Oktober", "November", "Desember"
)
HEWAN <- c('Rusa', 'Sapi', 'Kambing', 'Kerbau', 'Kuda')
COLNAMES_DATA <- c('Tahun', 'Bulan', 'Laporan', 'Kunjungan', 'Nama Kapal', 'Pelabuhan', 'Pelayaran', 'Bendera', 'Pemilik', 'Tiba Tanggal', 'Tiba Jam', 'Pelabuhan Asal', 'Jenis Kapal', 'Berangkat Tanggal', 'Berangkat Jam', 'Pelabuhan Tujuan', 'Komoditas', 'Nilai', 'Jenis', 'Panjang', 'GRT')


# header -------------------------------------------------------------------------
header <- shinydashboard::dashboardHeader(
  title = ''
) 

# header$children[[2]]$children <-tags$a(
#   tags$img(src = 'logo.png', height = '30', width = '150')
# )

# Footer ------------------------------------------------------------------
footer <- shinydashboardPlus::dashboardFooter(
  left = 'BPS Kab Kepulauan Selayar',
  right = 'Kepulauan Selayar, 2023'
)


# © 2022 Posit Software, PBC | All Rights Reserved | Terms Of Use

# Sidebar -----------------------------------------------------------------
sidebar <- shinydashboardPlus::dashboardSidebar(minified = TRUE,
  shinydashboard::sidebarMenu(
    # menuItem(HTML("&nbsp;Pelabuhan"), tabName = "pelabuhan", icon = icon("ship", 'fa-solid')),
    menuItem(HTML("&nbsp;Olah Data"), tabName = "olah", icon = icon("magnifying-glass-chart", 'fa-solid'), badgeLabel = "admin", badgeColor = "green"),
    menuItem(HTML("&nbsp;Database"), tabName = "database", icon = icon("database", 'fa-solid')),
    menuItem(HTML("&nbsp;Rekap Muat"), tabName = "rekapMuat", icon = icon("chevron-up", 'fa-solid')),
    menuItem(HTML("&nbsp;Rekap Bongkar"), tabName = "rekapBongkar", icon = icon("chevron-down", 'fa-solid')),
    menuItem(HTML("&nbsp;Visualisasi Data"), tabName = "viz", icon = icon("chart-line", 'fa-solid'))
  )
)

# Body --------------------------------------------------------------------
body <- shinydashboard::dashboardBody(
  tabItems(
    # Tab depan -------------------
    # tabItem(tabName = 'pelabuhan',
    #         fillRow(
    #           bootstrapPage(
    #             div(class = "outer",
    #                 tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
    #                 leafletOutput("map_pelabuhan", width = "100%", height = "100%")
    #           ))
    #         )
    # ),
    
    # Tab Pengolahan --------------
    tabItem(tabName = 'olah',
            fluidRow(
              useWaitress(),
              column(
                width = 3, 
                box(
                  width = NULL, headerBorder = FALSE, 
                  selectInput(inputId = "laporanInput", choices = NAMA_PELABUHAN, label = 'Laporan'),
                  selectInput(inputId = "bulanInput", choices = NAMA_BULAN, label = 'Bulan'),
                  selectInput(inputId = "tahunInput", choices = 2023:2050, label = 'Tahun', selected = format(Sys.Date(), "%Y")),
                  fileInput(inputId = "uploadData", label = "Data", buttonLabel = "Upload", multiple = FALSE, accept = ".xlsx"),      
                  actionButton(inputId = 'processButton', label = 'Process', style = "color: white; background-color: #04AA6D; width:100%;")  
                )
              ),
              column(
                width = 9,
                box(
                  width = NULL, headerBorder = FALSE, 
                  downloadButton(outputId = 'excelButton', icon = NULL, label = 'Excel', style = "color: white; background-color: #04AA6D; width:100px;"),     
                  # downloadButton(outputId = 'csvButton', icon = NULL, label = 'CSV', style = "color: white; background-color: #fa4811; width:9%;"),     
                  actionButton(inputId = 'copyButton', label = 'Copy', style = "color: white; background-color: #4682b4; width:100px"),
                  actionButton(inputId = 'uploadButton', label = HTML('&nbsp;Upload'), style = "color: white; background-color: #4682b4; width:100px;", icon = icon('circle-arrow-up', 'fa-solid')),  
                  DT::dataTableOutput("dataHasil") 
                )
              )
            )
    ),
    # Tab database -----------
    tabItem(tabName = 'database',
            fluidRow(
              column(
                width = 12,
                box(
                  width = NULL, headerBorder = FALSE, 
                  downloadButton(outputId = 'exceldbButton', icon = NULL, label = 'Excel', style = "color: white; background-color: #04AA6D; width:100px;"), 
                  actionButton(inputId = 'copydbButton', label = 'Copy', style = "color: white; background-color: #4682b4; width:100px;"),
                  actionButton(inputId = 'refreshdbButton', label = 'Refresh', style = "color: white; background-color: #4682b4; width:100px;"),
                  DT::dataTableOutput("dataFull")
                )
              )
            )
    ),
    
    tabItem(tabName = 'rekapBongkar',
            fluidRow(
              column(
                width = 12,
                box(
                  width = NULL, headerBorder = FALSE, height = '55px',
                  downloadButton(outputId = 'excelbongkarButton', icon = NULL, label = 'Excel', style = "color: white; background-color: #04AA6D; width:100px;"),  
                  actionButton(inputId = 'copybongkarButton', label = 'Copy', style = "color: white; background-color: #4682b4; width:100px;"),
                  actionButton(inputId = 'refreshbongkarButton', label = 'Refresh', style = "color: white; background-color: #4682b4; width:100px;"),
                  # selectInput(inputId = "viewBongkarInput", choices = c('Semua Komoditas', 'Hanya Komoditas yang Ada'), label = NULL, width = '200px'),
                  div(
                    selectInput(inputId = "tahunBongkarInput", choices = 2023:2024, label = NULL, selected = format(Sys.Date(), "%Y"), width = '100px'),
                    style = "float:right"
                  )
                ),
                
                tabBox(
                  width = NULL, id = 'tabBoxDataBongkar', 
                  tabPanel("Jampea", DT::dataTableOutput("bongkarJampea")),
                  tabPanel("Ujung", DT::dataTableOutput("bongkarUjung")),
                  tabPanel("Kayuadi", DT::dataTableOutput("bongkarKayuadi")),
                  tabPanel("Jinato", DT::dataTableOutput("bongkarJinato")),
                  tabPanel("Bonerate", DT::dataTableOutput("bongkarBonerate")),
                  tabPanel("Kalaotoa", DT::dataTableOutput("bongkarKalaotoa")),
                  tabPanel("Pamatata", DT::dataTableOutput("bongkarPamatata")),
                  tabPanel("Pattumbukan", DT::dataTableOutput("bongkarPattumbukan"))
                  # tabPanel("Benteng/Selayar", DT::dataTableOutput("bongkarBenteng"))
                )
                
              )
            )       
    ),
    
    # Tab visualisasi -----------
    tabItem(tabName = 'rekapMuat',
            fluidRow(
              column(
                width = 12,
                box(
                  width = NULL, headerBorder = FALSE, height = '55px',
                  downloadButton(outputId = 'excelmuatButton', icon = NULL, label = 'Excel', style = "color: white; background-color: #04AA6D; width:100px;"),  
                  actionButton(inputId = 'copymuatButton', label = 'Copy', style = "color: white; background-color: #4682b4; width:100px;"),
                  actionButton(inputId = 'refreshmuatButton', label = 'Refresh', style = "color: white; background-color: #4682b4; width:100px;"),
                  div(
                    selectInput(inputId = "tahunMuatInput", choices = 2023:2024, label = NULL, selected = format(Sys.Date(), "%Y"), width = "100px"),
                    style = "float:right"
                  )
                ),
                
                tabBox(
                  width = NULL, id = 'tabBoxDataMuat', 
                  tabPanel("Jampea", DT::dataTableOutput("muatJampea")),
                  tabPanel("Ujung", DT::dataTableOutput("muatUjung")),
                  tabPanel("Kayuadi", DT::dataTableOutput("muatKayuadi")),
                  tabPanel("Jinato", DT::dataTableOutput("muatJinato")),
                  tabPanel("Bonerate", DT::dataTableOutput("muatBonerate")),
                  tabPanel("Kalaotoa", DT::dataTableOutput("muatKalaotoa")),
                  tabPanel("Pamatata", DT::dataTableOutput("muatPamatata")),
                  tabPanel("Pattumbukan", DT::dataTableOutput("muatPattumbukan"))
                  # tabPanel("Benteng/Selayar", DT::dataTableOutput("muatBenteng"))
                )
                
              )
            )         
    ),
    # Tab visualisasi -----------
    tabItem(tabName = 'viz',
            # fluidRow(
            #   column(3, valueBoxOutput("kapalBox", width = NULL)),
            #   column(3, valueBoxOutput("penumpangBox", width = NULL)),
            #   column(3, valueBoxOutput("motorBox", width = NULL)),
            #   column(3, valueBoxOutput("mobilBox", width = NULL))
            # ), 
            
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL, 
                       title = span(icon("right-left", 'fa-solid'), HTML("&nbsp;&nbsp;Mobilitas Penduduk dan Kendaraan")),
                       footer = 'Sumber: Kantor Pelabuhan Jampea dan Pamatata', 
                       solidHeader = TRUE,
                       fluidRow(
                         column(2,
                                selectInput(inputId = "pelabuhanVizInput", choices = c('Jampea', 'Ujung', 'Jinato', 'Kayuadi', 'Bonerate', 'Kalaotoa', 'Pamatata', 'Pattumbukan'), label = NULL, width = '200px')
                         ),
                         column(2,
                                selectInput(inputId = "jenisVizInput", choices = c('Kapal','Penumpang', 'Motor', 'Mobil'), label = NULL, width = '200px')
                         )
                       ),
                       plotlyOutput("grafikLine", height = 350)
                     ),
                     box(
                       width = NULL, 
                       title = span(icon("table-cells-large", 'fa-solid'), HTML("&nbsp;&nbsp;Bongkar Muat")),
                       footer = 'Sumber: Kantor Pelabuhan Jampea dan Pamatata', 
                       solidHeader = TRUE, 
                       fluidRow(
                         column(2,
                                selectInput(inputId = "pelVizBMInput", choices = c('Jampea', 'Ujung', 'Jinato', 'Kayuadi', 'Bonerate', 'Kalaotoa'), label = NULL, width = '200px')
                         ),
                         column(2,
                                selectInput(inputId = "tahunVizBMInput", choices = 2023:2024, label = NULL, width = '200px')
                         ),
                         column(2,
                                selectInput(inputId = "topnVizBMInput", choices = c(5:15), label = NULL, width = '200px')
                         )
                       ),
                       column(width = 6, plotlyOutput("topBongkar", height = 350)),
                       column(width = 6, plotlyOutput("topMuat", height = 350))
                     )
              )
            )
            
    )
  ),
  shinyjs::useShinyjs(),
  tags$script(HTML("$('body').addClass('fixed');")),
  tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/rect23.png"),
    tags$title("Sepakat"),
    tags$style(
      '.content-wrapper {
            background-color: #e6e6e5;
          }
          
          .box {
            border-top: none;
            border-radius: 3px;
            padding: 1px;
            box-shadow: 0px 3px 3px 0px rgb(0, 0, 0, 0.2);
          }
          
          #tabBoxDataMuat {
            border-top: none;
            border-radius: 3px;
            box-shadow: 0px 3px 3px 0px rgb(0, 0, 0, 0.2);
          }
          
          .shiny-notification-default {color: white; background-color:#112446; opacity:0.8}
          .shiny-notification-message {color: white; background-color:#04AA6D; opacity:0.8}
          .shiny-notification-warning {color: white; background-color:#112446; opacity:0.8}
          .shiny-notification-error {color: white; background-color:#FF5757; opacity:0.8}
          
          #DataTables_Table_0 {
            color: #000000
          }
          
          #dataHasil {
            margin-top: 10px;
            font-weight: 1.5;
          }
          
          #dataFull {
            margin-top: 10px;
            font-weight: 1.5;
          }
          
          table.dataTable thead tr {
            background-color: #4682b4;
            color: white;
          }
        
          
      '
    ))
)


# Ui  -----------------------------------------------------------
ui <- shinydashboardPlus::dashboardPage(
  options = list(sidebarExpandOnHover = TRUE),
  skin = "blue", 
  header = header, 
  sidebar = sidebar,
  body = body,
  footer = footer
  # md = TRUE
) 

# data peta 
# df_pelabuhan <- sf::st_read('data/pelabuhan_indo.geojson') 

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  ALL_TAHUN <- 2023:as.numeric(format(Sys.Date(), "%Y"))
  DF_HASIL <- NULL
  ADA_DATA <- reactiveVal(FALSE)
  waitress <- Waitress$new(selector = '#processButton', theme = "overlay-opacity", infinite = TRUE)
  Wtupload <- Waitress$new(selector = '#uploadButton', theme = "overlay-opacity", infinite = TRUE)
  WtrefreshMuat <- Waitress$new(selector = '#refreshmuatButton', theme = "overlay-opacity", infinite = TRUE)
  WtrefreshBongkar <- Waitress$new(selector = '#refreshbongkarButton', theme = "overlay-opacity", infinite = TRUE)
  
  # Tab Pelabuhan -----------------------------------------------------------
  # output$map_pelabuhan <- renderLeaflet({
  #   leaflet(
  #       options = leafletOptions(zoomControl = FALSE)
  #   ) %>% 
  #     fitBounds(120.093992100707, -7.63591806489362, 122.147841557813, -5.62228337910473) %>%
  #     addProviderTiles(provider = providers$OpenStreetMap, group = 'OSM') %>%
  #     addProviderTiles(provider = providers$CartoDB.DarkMatterNoLabels, group = 'Carto DB') %>%
  #     # setView(lng = 120.883669, lat = -6.793342, zoom = 5) %>% 
  #     addCircleMarkers(
  #       data = df_pelabuhan,
  #       # popup = ~ label,
  #       label = ~ namaobj,
  #       color = ~ fill_circle,
  #       opacity = 1,
  #       radius = 5
  #     ) %>% 
  #     addLayersControl(
  #       baseGroups = c("Carto DB", "OSM"),
  #       position = "bottomright"
  #     )
  # })
  
  
  # Tab Olah Data -----------------------------------------------------------
  
  # Tombol process
  observeEvent(input$processButton, {
    if (is.null(input$uploadData)) {
      shiny::showNotification("Silahkan upload data terlebih dahulu", type = 'error')
    } else{
      waitress$start()
      Sys.sleep(.3)
      
      # proses pengolahan --------------
      DF_HASIL <<- olahDataPelabuhan(
        loc = input$uploadData$datapath, pelabuhan = input$laporanInput
      )
      
      # menampilkan output -------------
      output$dataHasil <- DT::renderDataTable(
        DF_HASIL, options = list(scrollX = TRUE),
        colnames = stringr::str_to_title(colnames(DF_HASIL)),
        rownames = FALSE
      ) 
      
      if (!is.null(DF_HASIL)) {
        ADA_DATA(TRUE)
      }else{
        ADA_DATA(FALSE)
      }
      
      waitress$close()
    }
    
  })
  
  # Tombol download EXCEL
  output$excelButton <- downloadHandler(
    filename = function(){
      paste0(input$laporanInput, "_", input$bulanInput, "_", input$tahunInput, ".xlsx")
    },
    content = function(fname){
      writexl::write_xlsx(x = DF_HASIL, path = fname)
    }
  )
  
  # Tombol Copy
  observeEvent(input$copyButton, {
    clipr::write_clip(DF_HASIL)
    shiny::showNotification("Data Copied", type = 'warning')
  })
  
  # Tombol upload
  observeEvent(input$uploadButton, {
    LAPORAN <- input$laporanInput
    BULAN <- input$bulanInput
    TAHUN <- input$tahunInput
    DF_FULL <- readxl::read_xlsx('data/dataFull.xlsx')
    
    if (nrow(DF_FULL) == 0) {
      sudah_ada <- FALSE
    }else{
      all_data <- unique(paste0(DF_FULL$laporan, DF_FULL$bulan, DF_FULL$tahun))
      if (paste0(LAPORAN, BULAN, TAHUN) %in% all_data) {
        sudah_ada <- TRUE
      } else {
        sudah_ada <- FALSE
      }
    }
    
    # konfirmasi
    if (sudah_ada) {
      showModal(modalDialog(
        tagList(str_glue("{LAPORAN} {BULAN} {TAHUN} data already exist. Do you want replace it?")),
        title = "Confirm Upload Data",
        footer = tagList(actionButton(inputId = "confirmUpload", "Yes"), modalButton("No")),
        size = 's'
      ))
    }else {
      showModal(modalDialog(
        tagList("Are you sure you want to upload these data?"),
        title = "Confirm Upload Data",
        footer = tagList(actionButton(inputId = "confirmUpload", "Yes"), modalButton("No")),
        size = 's'
      ))
    }
    
  })
  
  # Tombol konfirmasi upload
  observeEvent(input$confirmUpload, {
    removeModal()
    Wtupload$start()
    Sys.sleep(.3)
    
    sudah_ada <- FALSE
    
    tryCatch(
      {
        df_full <- readxl::read_xlsx('data/dataFull.xlsx')
        LAPORAN <- input$laporanInput
        BULAN <- input$bulanInput
        TAHUN <- as.numeric(input$tahunInput)
        DF_UPLOAD <- NULL
        
        if (nrow(df_full) == 0) {
          DF_UPLOAD <- DF_HASIL %>%
            dplyr::mutate(
              tahun = TAHUN,
              bulan = BULAN,
              laporan = LAPORAN,
              .before = 1
            )
        }else{
          all_data <- unique(paste0(df_full$laporan, df_full$bulan, df_full$tahun))
          if (paste0(LAPORAN, BULAN, TAHUN) %in% all_data) {
            sudah_ada <- TRUE
          } else {
            sudah_ada <- FALSE
          }
          
          
          if (!sudah_ada) {
            DF_UPLOAD <- DF_HASIL %>%
              dplyr::mutate(
                tahun = TAHUN,
                bulan = BULAN,
                laporan = LAPORAN,
                .before = 1
              ) %>%
              dplyr::bind_rows(df_full)
          }else{
            df_full <- df_full %>% 
              dplyr::filter(
                !(bulan == BULAN & tahun == TAHUN & laporan == LAPORAN)
              )
            
            DF_UPLOAD <- DF_HASIL %>%
              dplyr::mutate(
                tahun = TAHUN,
                bulan = BULAN,
                laporan = LAPORAN,
                .before = 1
              ) %>%
              dplyr::bind_rows(df_full)
          }
        }
        
        writexl::write_xlsx(x = DF_UPLOAD, path = 'data/dataFull.xlsx')
        shiny::showNotification("Upload Data Sukses", type = 'message')
      },
      error = function(cond){
        shiny::showNotification("Error ketika upload data", type = 'error')
        shiny::showNotification(cond, type = 'error')
      },
      finally = {
        LAPORAN <- input$laporanInput
        BULAN <- input$bulanInput
        TAHUN <- as.numeric(input$tahunInput)
        
        addDataLaluLintas(
          DF_HASIL, tahun = TAHUN, bulan = BULAN, laporan = LAPORAN, exist = sudah_ada
        )
        addDataRekap(
          DF_HASIL, tahun = TAHUN, bulan = BULAN, laporan = LAPORAN, exist = sudah_ada
        )
        
        if (!TAHUN %in% ALL_TAHUN) {
          ALL_TAHUN <- c(ALL_TAHUN, TAHUN)
          updateSelectInput(session, "tahunMuatInput", choices = ALL_TAHUN)
          updateSelectInput(session, "tahunBongkarInput", choices = ALL_TAHUN)
          updateSelectInput(session, "tahunVizBMInput", choices = ALL_TAHUN)
        }
        shiny::showNotification("Rekap Data Mobilitas dan Bongkar Muat Sukses", type = 'message')
      }
    )
    
    Wtupload$close()
  })
  
  # Nonaktifkan tombol
  shiny::observe({
    shinyjs::toggleState("excelButton", isTRUE(ADA_DATA()))
    shinyjs::toggleState("csvButton", isTRUE(ADA_DATA()))
    shinyjs::toggleState("copyButton", isTRUE(ADA_DATA()))
    shinyjs::toggleState("uploadButton", isTRUE(ADA_DATA()))
  })
  
  
  
  # Tab Database ------------------------------------------------------------
  # Database
  df_full <- reactive({
    readxl::read_xlsx('data/dataFull.xlsx')
  })
  
  output$dataFull <- DT::renderDataTable(
    df_full(), options = list(scrollX = TRUE),
    colnames = COLNAMES_DATA,
    rownames = FALSE
  ) 
  
  # Tombol download EXCEL db
  output$exceldbButton <- downloadHandler(
    filename = function(){
      paste0("Data_BongkarMuat_", format(Sys.time(), "%H:%M:%S_%d%b%Y"), ".xlsx")
    },
    content = function(fname){
      writexl::write_xlsx(x = df_full(), path = fname)
    }
  )
  
  # Tombol Copy db
  observeEvent(input$copydbButton, {
    clipr::write_clip(df_full())
    shiny::showNotification("Data Copied", type = 'warning')
  })
  
  # Tombol Refresh db
  observeEvent(input$refreshdbButton, {
    df_full <- readxl::read_xlsx('data/dataFull.xlsx')
    output$dataFull <- DT::renderDataTable(
      df_full, options = list(scrollX = TRUE),
      rownames = FALSE
    ) 
  })
  

  # Tab Rekap V2 ------------------------------------------------------------
  df_bm <- reactive(
    readxl::read_xlsx('data/df_rekap.xlsx')
  )
  
  refreshMuat <- function(){
    df_rekap <- df_bm()
    df_rekap <- df_rekap[df_rekap$tahun == as.numeric(input$tahunMuatInput) & df_rekap$jenis == 'muat', -c(1, 3, 4, 6)]
    
    # Data rekap ----------------------
    output$muatJampea <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Jampea', -2], 
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't' ),
      rownames = FALSE
    )
    
    output$muatUjung <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Ujung', -2], 
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
    
    output$muatBonerate <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Bonerate', -2], 
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
    
    output$muatKalaotoa <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Kalaotoa', -2], 
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
    
    output$muatKayuadi <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Kayuadi', -2], 
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
    
    output$muatJinato <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Jinato', -2],
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
    
    output$muatPamatata <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Pamatata', -2],
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
    
    output$muatPattumbukan <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Pattumbukan', -2],
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
  }
  
  observeEvent(input$tahunMuatInput, {
    refreshMuat()
  })
  
  # Tab rekap ---------------------------------------------------------------
  # Tombol refresh rekap
  observeEvent(input$refreshmuatButton, {
    WtrefreshMuat$start()
    Sys.sleep(.3)
    refreshMuat()
    WtrefreshMuat$close()
  })
  
  observeEvent(input$refreshbongkarButton, {
    WtrefreshBongkar$start()
    Sys.sleep(.3)
    refreshBongkar()
    WtrefreshBongkar$close()
  })

  
  # Tombol download EXCEL Muat
  output$excelmuatButton <- downloadHandler(
    filename = function(){
      paste0("Rekap_Muat_", format(Sys.time(), "%H:%M:%S_%d%b%Y"), ".xlsx")
    },
    content = function(fname){
      file.copy(from = 'data/df_rekap.xlsx', to = fname)
    }
  )
  
  # Tombol Copy Muat
  observeEvent(input$copymuatButton, {
    dat <- readxl::read_xlsx('data/df_rekap.xlsx')
    dat <- dat[dat$pelabuhan == input$tabBoxDataMuat & dat$jenis == 'muat' & dat$tahun == as.numeric(input$tahunMuatInput), -4]
    
    clipr::write_clip(dat)
    shiny::showNotification("Data Copied", type = 'warning')
  })
  
  
  refreshBongkar <- function(){
    df_rekap <- df_bm()
    df_rekap <- df_rekap[df_rekap$tahun == as.numeric(input$tahunBongkarInput) & df_rekap$jenis == 'bongkar', -c(1, 3, 4, 6)]
    
    
    # Data rekap ----------------------
    output$bongkarJampea <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Jampea', -2],
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't' ),
      rownames = FALSE
    )
    
    output$bongkarUjung <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Ujung', -2],
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't' ),
      rownames = FALSE
    )
    
    output$bongkarBonerate <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Bonerate', -2],
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
    
    output$bongkarKalaotoa <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Kalaotoa', -2],
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
    
    output$bongkarKayuadi <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Kayuadi', -2],
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
    
    output$bongkarJinato <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Jinato', -2],
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
    
    output$bongkarPamatata <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Pamatata', -2],
      options = list(style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
    
    output$bongkarPattumbukan <- DT::renderDataTable(
      df_rekap[df_rekap$pelabuhan == 'Pattumbukan', -2],
      options = list(scrollX = TRUE, style = 'bootstrap', pageLength = 13, dom = 't'),
      rownames = FALSE
    )
  }
  
  observeEvent(input$tahunBongkarInput, {
    refreshBongkar()
  })
  
  
  
  # Tombol download EXCEL Bongkar
  output$excelbongkarButton <- downloadHandler(
    filename = function(){
      paste0("Rekap_Bongkar_", format(Sys.time(), "%H:%M:%S_%d%b%Y"), ".xlsx")
    },
    content = function(fname){
      file.copy(from = 'data/df_rekap.xlsx', to = fname)
    }
  )
  
  # Tombol Copy Bongkar
  observeEvent(input$copybongkarButton, {
    dat <- readxl::read_xlsx('data/df_rekap.xlsx')
    dat <- dat[dat$pelabuhan == input$tabBoxDataBongkar & dat$jenis == 'bongkar' & dat$tahun == as.numeric(input$tahunBongkarInput), -4]
    
    clipr::write_clip(dat)
    shiny::showNotification("Data Copied", type = 'warning')
  })
  
  
  
  # Tab Visualisasi -------------------------------------------------------
  # Grafik line
  output$grafikLine <- renderPlotly({
    df_mobilitas <- readxl::read_xlsx('data/df_mobilitas.xlsx')
    df_mobilitas <- df_mobilitas[df_mobilitas$pelabuhan == input$pelabuhanVizInput, ]
    
    if (nrow(df_mobilitas) == 0) {
      return(empty_plot("Data tidak ditemukan"))
    } 
    
    if (input$jenisVizInput == 'Kapal') {
      return(grafik_kapal(df_mobilitas))
    }else{
      return(grafik_lalulintas(df_mobilitas, input$jenisVizInput))
    }
  })
  

  # Grafik bongkar muat 
  DF_REKAP <- reactive({
    topn <- as.numeric(input$topnVizBMInput)
    
    df_rekap <- readxl::read_xlsx('data/df_rekap.xlsx')
    df_rekap <- df_rekap[
      df_rekap$tahun == as.numeric(input$tahunVizBMInput) & df_rekap$pelabuhan == input$pelVizBMInput, 
    ]
    
    HEWAN <- c('Rusa', 'Sapi', 'Kambing', 'Kerbau', 'Kuda')
    ALL_NAMES <- setdiff(colnames(df_rekap), c('Mobil', 'Motor', 'Penumpang', HEWAN))
    df_rekap <- df_rekap[, ALL_NAMES]
    
    df_rekap <- df_rekap %>% tidyr::pivot_longer(-c(1:6), values_to = 'nilai') 
    df_rekap <- df_rekap[df_rekap$nilai != 0, ]
    
    df_rekap %>%
      group_by(jenis, name) %>% 
      summarise(
        nilai = sum(nilai)
      ) 
  })
  
  
  toListen <- reactive({
    list(input$tahunVizBMInput, input$pelVizBMInput, input$topnVizBMInput)
  })
  observeEvent(toListen(), {
    topn <- as.numeric(input$topnVizBMInput)
    
    df_rekap <- DF_REKAP() %>% 
      group_by(jenis) %>% 
      top_n(topn)
    
    df_bongkar <- df_rekap[df_rekap$jenis == 'bongkar', ]
    if (sum(df_bongkar$nilai) == 0) {
      output$topBongkar <- renderPlotly({
        empty_plot(str_glue("Tidak ada Data Bongkar di Pelabuhan {input$pelVizBMInput}"))
      })
    } else {
      output$topBongkar <- renderPlotly({
        grafik_bm(df_bongkar, jenis = 'Bongkar', bar_color = 'rgba(49,130,189, 0.9)')
      })
    }
    
    
    df_muat <- df_rekap[df_rekap$jenis == 'muat', ]
    if (sum(df_muat$nilai) == 0) {
      output$topMuat <- renderPlotly({
        empty_plot(str_glue("Tidak ada Data Muat di Pelabuhan {input$pelVizBMInput}"))
      })
    } else {
      output$topMuat <- renderPlotly({
        grafik_bm(df_muat, jenis = 'Muat')
      })
    }
    
  })
}


# Run ---------------------------------------------------------------------
shinyApp(ui, server)


