library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(shinyjs)
# library(bslib)
library(waiter)
library(xlsx)
library(DT)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

source('syntax.R')

# Global Variable ---------------------------------------------------------
NAMA_PELABUHAN <- c('Jampea', "Pamatata", "Benteng/Selayar")
NAMA_BULAN <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli", 
                "Agustus", "September", "Oktober", "November", "Desember")


# header -------------------------------------------------------------------------
header <- shinydashboardPlus::dashboardHeader(
    title = ""
) 

# Footer ------------------------------------------------------------------
footer <- shinydashboardPlus::dashboardFooter(
    left = strong('BPS Kab Kepulauan Selayar'),
    right = 'Kepulauan Selayar, 2023'
)


# Sidebar -----------------------------------------------------------------
sidebar <- shinydashboardPlus::dashboardSidebar(
    shinydashboard::sidebarMenu(
        menuItem("Pelabuhan", tabName = "pelabuhan", icon = icon("ship", 'fa-solid')),
        menuItem("Olah Data", tabName = "olah", icon = icon("magnifying-glass-chart", 'fa-solid')),
        menuItem("Database", tabName = "database", icon = icon("database", 'fa-solid')),
        menuItem("Rekap Muat", tabName = "rekapMuat", icon = icon("chevron-up", 'fa-solid')),
        menuItem("Rekap Bongkar", tabName = "rekapBongkar", icon = icon("chevron-down", 'fa-solid')),
        menuItem("Visualisasi Data", tabName = "viz", icon = icon("chart-line", 'fa-solid'))
    )
)

# Body --------------------------------------------------------------------
body <- shinydashboard::dashboardBody(
    tabItems(
        # Tab depan -------------------
        tabItem(tabName = 'pelabuhan',
        ),
        
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
                            actionButton(inputId = 'processButton', label = 'Process', style = "color: white; background-color: #04AA6D; width:100%;"),    
                            actionButton(inputId = 'uploadButton', label = 'Upload', style = "color: white; background-color: #4682b4; width:100%;")    
                        )
                    ),
                    column(
                        width = 9,
                        box(
                            width = NULL, headerBorder = FALSE, 
                            downloadButton(outputId = 'excelButton', icon = NULL, label = 'Excel', style = "color: white; background-color: #04AA6D; width:9%;"),     
                            downloadButton(outputId = 'csvButton', icon = NULL, label = 'CSV', style = "color: white; background-color: #fa4811; width:9%;"),     
                            actionButton(inputId = 'copyButton', label = 'Copy', style = "color: white; background-color: #4682b4; width:9%;"),
                            DT::dataTableOutput("dataHasil") 
                        )
                    )
                )
        ),
        # Tab visualisasi -----------
        tabItem(tabName = 'database',
          fluidRow(
            column(
              width = 12,
              box(
                width = NULL, headerBorder = FALSE, 
                downloadButton(outputId = 'exceldbButton', icon = NULL, label = 'Excel', style = "color: white; background-color: #04AA6D; width:9%;"),     
                downloadButton(outputId = 'csvdbButton', icon = NULL, label = 'CSV', style = "color: white; background-color: #fa4811; width:9%;"),     
                actionButton(inputId = 'copydbButton', label = 'Copy', style = "color: white; background-color: #4682b4; width:9%;"),
                actionButton(inputId = 'refreshdbButton', label = 'Refresh', style = "color: white; background-color: #4682b4; width:9%;"),
                DT::dataTableOutput("dataFull")
              )
            )
          )
        ),
        
        tabItem(tabName = 'rekapBongkar'
        ),
        
        # Tab visualisasi -----------
        tabItem(tabName = 'rekapMuat',
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = NULL, headerBorder = FALSE, 
                      downloadButton(outputId = 'excelrekapButton', icon = NULL, label = 'Excel', style = "color: white; background-color: #04AA6D; width:9%;"),     
                      downloadButton(outputId = 'csvrekapButton', icon = NULL, label = 'CSV', style = "color: white; background-color: #fa4811; width:9%;"),     
                      actionButton(inputId = 'copyrekapButton', label = 'Copy', style = "color: white; background-color: #4682b4; width:9%;"),
                      actionButton(inputId = 'refreshrekapButton', label = 'Refresh', style = "color: white; background-color: #4682b4; width:9%;"),
                    ),
                    
                    tabBox(
                      width = NULL, id = 'tabBoxData', 
                      tabPanel("Jampea", DT::dataTableOutput("dataJampea")),
                      tabPanel("Ujung", DT::dataTableOutput("dataUjung")),
                      tabPanel("Kayuadi", DT::dataTableOutput("dataKayuadi")),
                      tabPanel("Jinato", DT::dataTableOutput("dataJinato")),
                      tabPanel("Bonerate", DT::dataTableOutput("dataBonerate")),
                      tabPanel("Kalaotoa", DT::dataTableOutput("dataKalaotoa")),
                      tabPanel("Pamatata", DT::dataTableOutput("dataPamatata")),
                      tabPanel("Patumbukkan", DT::dataTableOutput("dataPatumbukkan")),
                      tabPanel("Benteng/Selayar", DT::dataTableOutput("dataBenteng"))
                    )
      
                  )
                )         
        ),
        # Tab visualisasi -----------
        tabItem(tabName = 'viz'
        )
    ),
    shinyjs::useShinyjs(),
    
    tags$head(
      tags$style(
         '.content-wrapper {
            background-color: #e6e6e5;
          }
          
          .box {
            border-top: none;
            border-radius: 3px;
            box-shadow: 0px 3px 3px 0px rgb(0, 0, 0, 0.2);
          }
          
          #tabBoxData {
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



#
# Server ------------------------------------------------------------------
server <- function(input, output) {
    DF_HASIL <- NULL
    ADA_DATA <- reactiveVal(FALSE)
    waitress <- Waitress$new(selector = '#processButton', theme = "overlay-opacity", infinite = TRUE)
    Wtupload <- Waitress$new(selector = '#uploadButton', theme = "overlay-opacity", infinite = TRUE)
    
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
                DF_HASIL, options = list(scrollX = TRUE, initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#4682b4', 'color': 'white'});",
                  "}")
                ),
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
    
    # Tombol download CSV
    output$csvButton <- downloadHandler(
        filename = function(){
            paste0(input$laporanInput, "_", input$bulanInput, "_", input$tahunInput, ".csv")
        },
        content = function(fname){
            vroom::vroom_write(x = DF_HASIL, file = fname, delim = ',')
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
      
      df_full <- readxl::read_xlsx('data/dataFull.xlsx')
      LAPORAN <- input$laporanInput
      BULAN <- input$bulanInput
      TAHUN <- as.numeric(input$tahunInput)
      
      
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
          df_full <- df_full %>% dplyr::filter(
            !(bulan == BULAN & tahun == TAHUN & laporan == LAPORAN)
          )
        }
        
        DF_UPLOAD <- DF_HASIL %>%
          dplyr::mutate(
            tahun = TAHUN,
            bulan = BULAN,
            laporan = LAPORAN,
            .before = 1
          ) %>%
          dplyr::bind_rows(df_full)
      }

      writexl::write_xlsx(x = DF_UPLOAD, path = 'data/dataFull.xlsx')
      shiny::showNotification("Upload Data Success", type = 'message')
      
      Wtupload$close()
    })
    
    
    # Nonaktifkan tombol
    shiny::observe({
        shinyjs::toggleState("excelButton", isTRUE(ADA_DATA()))
        shinyjs::toggleState("csvButton", isTRUE(ADA_DATA()))
        shinyjs::toggleState("copyButton", isTRUE(ADA_DATA()))
        shinyjs::toggleState("uploadButton", isTRUE(ADA_DATA()))
    })

    # Database
    df_full <- reactive({
      readxl::read_xlsx('data/dataFull.xlsx')
    })
    
    output$dataFull <- DT::renderDataTable(
      df_full(), options = list(scrollX = TRUE, initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#4682b4', 'color': 'white'});",
        "}")
      ),
      # colnames = stringr::str_to_title(colnames(DF_HASIL)),
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
    
    # Tombol download CSV db
    output$csvdbButton <- downloadHandler(
      filename = function(){
        paste0("Data_BongkarMuat_", format(Sys.time(), "%H:%M:%S_%d%b%Y"), ".csv")
      },
      content = function(fname){
        vroom::vroom_write(x = df_full(), file = fname, delim = ',')
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
        df_full, options = list(scrollX = TRUE, initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#4682b4', 'color': 'white'});",
          "}")
        ),
        # colnames = stringr::str_to_title(colnames(DF_HASIL)),
        rownames = FALSE
      ) 
    })
    
    
    # Tombol refresh rekap
    # observeEvent(refreshrekapButton, {
    #   df_rekap <- df_full() %>% 
    #     dplyr::filter(!is.na(jenis)) %>% 
    #     # filter(jenis == "bongkar") %>% 
    #     group_by(bulan, pelabuhan, nama, jenis) %>% 
    #     summarise(nilai = sum(nilai)) %>% 
    #     pivot_wider(names_from = nama, values_from = nilai) %>% 
    #     group_by(pelabuhan)
    #   
    #   df_rekap[is.na(df_rekap)] <- 0
    #   df_rekap
    #   
    #   
    #   wbbongkar = createWorkbook()
    #   wbmuat = createWorkbook()
    #   
    #   df_rekap %>% 
    #     group_split() %>%
    #     sapply(function(dfx){
    #       dfx <- dfx %>% 
    #         mutate(pelabuhan = str_remove_all(pelabuhan, 'Wilker|Pelabuhan|Posker'))
    #       
    #       sheet_muat = createSheet(wbmuat, dfx$pelabuhan[1])
    #       sheet_bongkar = createSheet(wbbongkar, dfx$pelabuhan[1])
    #       
    #       df_muat <- as.data.frame(dplyr::filter(dfx, jenis == 'muat')) %>% 
    #         dplyr::select(-pelabuhan, -jenis)
    #       df_bongkar <- as.data.frame(dplyr::filter(dfx, jenis == 'bongkar')) %>% 
    #         dplyr::select(-pelabuhan, -jenis)
    #       
    #       addDataFrame(
    #         df_muat, sheet = sheet_muat, row.names = FALSE
    #       )
    #       addDataFrame(
    #         df_bongkar, sheet = sheet_bongkar, row.names = FALSE
    #       )
    #     })
    #   saveWorkbook(wbmuat, "RekapMuat2.xlsx")
    #   saveWorkbook(wbbongkar, "RekapBongkar2.xlsx")
    # 
    # })
                  
    # Membaca semua sheet
    df_jampea <- reactive({
      readxl::read_xlsx('data/RekapMuat.xlsx', sheet = 'Jampea')
    })
    df_ujung <- reactive({
      readxl::read_xlsx('data/RekapMuat.xlsx', sheet = 'Ujung')
    })
    df_bonerate <- reactive({
      readxl::read_xlsx('data/RekapMuat.xlsx', sheet = 'Bonerate')
    })
    df_kalaotoa <- reactive({
      readxl::read_xlsx('data/RekapMuat.xlsx', sheet = 'Kalaotoa')
    })
    df_kayuadi <- reactive({
      readxl::read_xlsx('data/RekapMuat.xlsx', sheet = 'Kayuadi')
    })
    df_jinato <- reactive({
      readxl::read_xlsx('data/RekapMuat.xlsx', sheet = 'Jinato')
    })
    
    
    # Data rekap ----------------------
    output$dataJampea <- DT::renderDataTable(
      df_jampea(), options = list(scrollX = TRUE, initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#4682b4', 'color': 'white'});",
        "}"), style = 'bootstrap', pageLength = 13, dom = 't'
      ),
      # colnames = stringr::str_to_title(colnames(DF_HASIL)),
      rownames = FALSE
    )
    
    output$dataUjung <- DT::renderDataTable(
      df_ujung(), options = list(scrollX = TRUE, initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#4682b4', 'color': 'white'});",
        "}"), style = 'bootstrap', pageLength = 13, dom = 't'
      ),
      # colnames = stringr::str_to_title(colnames(DF_HASIL)),
      rownames = FALSE
    )
    
    output$dataBonerate <- DT::renderDataTable(
      df_bonerate(), options = list(scrollX = TRUE, initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#4682b4', 'color': 'white'});",
        "}"), style = 'bootstrap', pageLength = 13, dom = 't'
      ),
      # colnames = stringr::str_to_title(colnames(DF_HASIL)),
      rownames = FALSE
    )
    
    output$dataKalaotoa <- DT::renderDataTable(
      df_kalaotoa(), options = list(scrollX = TRUE, initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#4682b4', 'color': 'white'});",
        "}"), style = 'bootstrap', pageLength = 13, dom = 't'
      ),
      # colnames = stringr::str_to_title(colnames(DF_HASIL)),
      rownames = FALSE
    )
    
    output$dataKayuadi <- DT::renderDataTable(
      df_kayuadi(), options = list(scrollX = TRUE, initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#4682b4', 'color': 'white'});",
        "}"), style = 'bootstrap', pageLength = 13, dom = 't'
      ),
      # colnames = stringr::str_to_title(colnames(DF_HASIL)),
      rownames = FALSE
    )
    
    output$dataJinato <- DT::renderDataTable(
      df_jinato(), options = list(scrollX = TRUE, initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#4682b4', 'color': 'white'});",
        "}"), style = 'bootstrap', pageLength = 13, dom = 't'
      ),
      # colnames = stringr::str_to_title(colnames(DF_HASIL)),
      rownames = FALSE
    )
    
    
    
    
    
    
    
    
    
    
    # Tombol download EXCEL rekap
    output$excelrekapButton <- downloadHandler(
      filename = function(){
        paste0("Rekap_BongkarMuat_", format(Sys.time(), "%H:%M:%S_%d%b%Y"), ".xlsx")
      },
      content = function(fname){
        writexl::write_xlsx(x = df_full(), path = fname)
      }
    )
    
    # Tombol download CSV rekap
    output$csvrekapButton <- downloadHandler(
      filename = function(){
        paste0("Rekap_BongkarMuat_", format(Sys.time(), "%H:%M:%S_%d%b%Y"), ".csv")
      },
      content = function(fname){
        vroom::vroom_write(x = df_full(), file = fname, delim = ',')
      }
    )
    
    # Tombol Copy rekap
    observeEvent(input$copyrekapButton, {
      print(input$tabBoxData)
      
      clipr::write_clip(df_full())
      shiny::showNotification("Data Copied", type = 'warning')
    })
}


# Run ---------------------------------------------------------------------
shinyApp(ui, server)


