# Daftar masalah
# masih 7.30 terus di jampea
# error di pamatata


# Fungsi olah data ------------------------------------------------------
olahDataPelabuhan <- function(loc, pelabuhan){
  tryCatch(
    {
      if (pelabuhan == "Jampea") {
        COLNAMES <- c('no', 'nmkapal', 'bendera', 'pemilik', 'panjang', 'grt', 'dwt', 'tiba_tgl', 'tiba_jam', 'pel_asal', 'tambat_tgl', 'tambat_jam', 'jenis_kapal', 'berangkat_tgl', 'berangkat_jam', 'pel_tujuan', 'bongkar_barang', 'bongkar_vol', 'bongkar_hewan', 'bongkar_ekor', 'muat_barang', 'muat_vol', 'muat_hewan', 'muat_ekor', 'mobil_turun', 'mobil_naik', 'motor_turun', 'motor_naik', 'pnp_turun', 'pnp_naik', 'muatan_jenis', 'muatan_vol', 'muatan_mobil', 'muatan_motor', 'muatan_pnp')
        COLTYPES <- c('text', 'text', 'text', 'text', 'numeric', 'numeric', 'numeric', 'date', 'numeric', 'text', 'date', 'numeric', 'text', 'date', 'numeric', 'text', 'text', 'numeric', 'text', 'numeric', 'text', 'numeric', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'text', 'numeric', 'numeric', 'numeric', 'numeric')
        
        # baca data per sheet
        df_sheet1 <- readxl::read_xlsx(
          loc, sheet = 1, skip = 9, col_names = COLNAMES, col_types = COLTYPES
        )
        df_sheet2 <- readxl::read_xlsx(
          loc, sheet = 2, skip = 1, col_names = COLNAMES, col_types = COLTYPES
        )
        
        # gabung data
        df_raw <- dplyr::bind_rows(
          df_sheet1[, -c(11, 12)],
          df_sheet2[, -c(11, 12)]
        )
        
        # hapus baris terakhir
        idx <- which(df_raw$no == "JUMLAH TOTAL")
        
        # hapus baris bawah bagian tanda tangan
        df_raw <- df_raw[-c(idx:nrow(df_raw)), ]
        
        
        df_hasil <- df_raw %>%
          # buat kolom baru yaitu nama pelabuhan dan jenis pelayaran
          # ubah kolom no menjadi angka saja, selainnya menjadi NA
          dplyr::mutate(
            pelabuhan = ifelse(stringr::str_detect(no, 'PELABUHAN|WILKER|POSKER'), no, NA),
            pelayaran = ifelse(stringr::str_detect(nmkapal, 'PELAYARAN|PERINTIS|FERRY'), nmkapal, NA),
            no = as.numeric(no),
            .before = 'bendera'
          ) %>%
          dplyr::filter(!(is.na(bongkar_barang) & !is.na(bongkar_vol))) %>%
          # isi nama pelabuhan, pelayaran dan no dengan nilai sebelumnya
          tidyr::fill(c(pelabuhan, pelayaran, no)) %>%
          # Minimal salah satu dari kolom2 diatas memiliki nilai (Tidak NA)
          dplyr::filter_at(
            c('pemilik', "bongkar_barang", "bongkar_hewan", "muat_barang", "muat_hewan", 'mobil_turun', 'mobil_naik', 'motor_turun', 'motor_naik', 'pnp_turun', 'pnp_naik', 'muatan_jenis', 'muatan_mobil', 'muatan_motor', 'muatan_pnp'),
            dplyr::any_vars(!is.na(.))
          ) %>%
          tidyr::fill(c(nmkapal, bendera, tiba_tgl, tiba_jam, pel_asal, jenis_kapal, berangkat_tgl, berangkat_jam, pel_tujuan, panjang, grt)) %>%
          dplyr::group_by(no, nmkapal, pelabuhan, pelayaran) %>%
          dplyr::mutate(
            pemilik = ifelse(is.na(pemilik), '', pemilik),
            pemilik = paste0(pemilik, collapse = ' '),
            pemilik = stringr::str_trim(pemilik)
          ) %>%
          dplyr::ungroup(no, nmkapal, pelayaran) %>% 
          dplyr::filter(pemilik != "")  %>% 
          dplyr::mutate(
            kunjungan = dplyr::case_when(
              no == 1 & dplyr::lag(no, default = 0) == 1 & nmkapal != dplyr::lag(nmkapal, default = "") ~ 1,
              no != dplyr::lag(no, default = 0) ~ 1,
              TRUE ~ 0
            ),
            kunjungan = cumsum(kunjungan),
            .after = no
          ) %>% 
          dplyr::select(-no) %>% 
          dplyr::group_by(pelabuhan, kunjungan) %>% 
          mutate(
            id = dplyr::cur_group_id(),
            .before = kunjungan
          ) %>% 
          dplyr::ungroup() 
        
        
        # df kapal ----------------------------------------------------------------
        df_kapal <- df_hasil %>% 
          dplyr::select(id:pel_tujuan) %>% 
          dplyr::distinct(id, .keep_all = TRUE)
        
        # df bongkar --------------------------------------------------------------
        df_bongkar <- data.frame(
          id = rep(df_hasil$id, 5),
          nama = c(df_hasil$bongkar_barang, df_hasil$bongkar_hewan, rep(c('penumpang', 'mobil', 'motor'), each = nrow(df_hasil))),
          nilai = c(df_hasil$bongkar_vol, df_hasil$bongkar_ekor, df_hasil$pnp_turun, df_hasil$mobil_turun, df_hasil$motor_turun)
        ) %>% 
          dplyr::filter(nama != "-") %>% 
          # Mengatasi nama barang yang di enter
          dplyr::filter(nilai != "-") %>%  
          dplyr::group_by(id) %>% 
          dplyr::mutate(
            jenis = 'bongkar',
            nama = ifelse(
              !nama %in% c('penumpang', 'mobil', 'motor') & is.na(dplyr::lead(nilai)) & !dplyr::lead(nama) %in% c('penumpang', 'mobil', 'motor'),
              paste(nama, dplyr::lead(nama)), nama
            )
          ) %>% 
          dplyr::filter(!is.na(nilai)) %>% 
          dplyr::arrange(id) 
        
        # df muat -----------------------------------------------------------------
        df_muat <- data.frame(
          id = rep(df_hasil$id, 5),
          nama = c(df_hasil$muat_barang, df_hasil$muat_hewan, rep(c('penumpang', 'mobil', 'motor'), each = nrow(df_hasil))),
          nilai = c(df_hasil$muat_vol, df_hasil$muat_ekor, df_hasil$pnp_naik, df_hasil$mobil_naik, df_hasil$motor_naik)
        ) %>% 
          dplyr::filter(nama != "-") %>% 
          dplyr::filter(nilai != "-") %>% 
          dplyr::mutate(
            jenis = 'muat',
            nama = ifelse(
              !nama %in% c('penumpang', 'mobil', 'motor') & is.na(dplyr::lead(nilai)) & !dplyr::lead(nama) %in% c('penumpang', 'mobil', 'motor'),
              paste(nama, dplyr::lead(nama)), nama
            )
          ) %>% 
          dplyr::filter(!is.na(nilai)) %>% 
          dplyr::arrange(id)
        
        
        df_bm <- bind_rows(
          df_bongkar, 
          df_muat
        ) %>% 
          arrange(id)
        
        
        df_final <- df_kapal %>% 
          left_join(df_bm) %>% 
          select(-id) %>% 
          mutate(
            berangkat_jam = format(as.POSIXct(berangkat_jam, origin = "1960-01-01"), format = "%H:%M"),
            tiba_jam = format(as.POSIXct(tiba_jam, origin = "1960-01-01"), format = "%H:%M")
          )
        
        shiny::showNotification("Success", type = 'message')
        return(olahKomoditas(df_final))
        
      # Pelabuhan Pamatata ------------------------------------------------------
      }else if (pelabuhan == "Pamatata") {
        # Bongkar
        df_bongkar <- readxl::read_xlsx(
          loc, sheet = 'KEDATANGAN', skip = 9,
          col_names = c('tiba_tgl', 'nmkapal', 'tiba_jam', 'jml_trip', 'pnp_dewasa', 'pnp_anak', 'pnp_turun', 
                        'g1', 'g2', 'g3', 'g4_pnp', 'g4_brg', 'g5_pnp', 'g5_brg', 'g6_pnp', 'g6_brg', 'g7', 'g8', 'g9',
                        'motor_turun', 'r4_turun', 'bus_turun', 'truck_turun', 'alatberat_turun',
                        'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7'),
          col_types = c('date', 'text', 'date', rep('numeric', 28))
        ) %>% 
          dplyr::slice(-1) %>% 
          dplyr::filter(!is.na(nmkapal)) %>% 
          tibble::rowid_to_column('kunjungan') %>% 
          tidyr::fill(tiba_tgl) %>% 
          dplyr::mutate(
            # berangkat_jam = tiba_jam - 2*60*60,
            # berangkat_jam = format(berangkat_jam, format = "%H:%M"),
            berangkat_jam = NA,
            tiba_jam = format(tiba_jam, format = "%H:%M"),
            
            pel_asal = 'Bira', pel_tujuan = 'Pamatata', pemilik = 'PT. ASDP',
            pelabuhan = 'PELABUHAN PAMATATA', pelayaran = "FERRY", bendera = "RI",
            jenis_kapal = "KMP", 
            panjang = NA, dwt = NA, grt = case_when(
              nmkapal == "KMP. KORMOMOLIN" ~ 884,
              nmkapal == "KMP. SANGKE PALANGGA" ~ 884,
              nmkapal == "KMP. BALIBO" ~ 540,
              nmkapal == "KMP. TAKABONERATE" ~ 842,
              nmkapal == "KMP. BONTOHARU" ~ 1124,
              TRUE ~ NA
            ),
            barang_turun = g4_brg + g5_brg + g6_brg,
            mobil_turun = r4_turun + bus_turun + truck_turun + alatberat_turun,
            
            berangkat_tgl = tiba_tgl,
            tiba_tgl = ifelse(
              is.na(tiba_jam), tiba_tgl, 
              lubridate::ymd_hm(paste(tiba_tgl, tiba_jam))
            )
          ) %>% 
          dplyr::select(
            kunjungan, nmkapal, pelabuhan, pelayaran, bendera, pemilik, tiba_tgl, tiba_jam, pel_asal, jenis_kapal, berangkat_tgl, berangkat_jam, pel_tujuan,
            "B. CAMP" = barang_turun, penumpang = pnp_turun, motor = motor_turun, mobil = mobil_turun
          ) %>% 
          tidyr::pivot_longer(14:17, names_to = 'nama', values_to = 'nilai') %>% 
          dplyr::mutate(jenis = 'bongkar')
        
        
        # Muat
        df_muat <- readxl::read_xlsx(
          loc, sheet = 'KEBERANGKATAN', skip = 9,
          col_names = c('berangkat_tgl', 'nmkapal', 'berangkat_jam', 'jml_trip', 'pnp_dewasa', 'pnp_anak', 'pnp_naik', 
                        'g1', 'g2', 'g3', 'g4_pnp', 'g4_brg', 'g5_pnp', 'g5_brg', 'g6_pnp', 'g6_brg', 'g7', 'g8', 'g9',
                        'motor_naik', 'r4_naik', 'bus_naik', 'truck_naik', 'alatberat_naik',
                        'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7'),
          col_types = c('date', 'text', 'date', rep('numeric', 28))
        ) %>% 
          dplyr::slice(-1) %>% 
          dplyr::filter(!is.na(nmkapal)) %>% 
          tibble::rowid_to_column('kunjungan') %>% 
          tidyr::fill(berangkat_tgl) %>% 
          dplyr::mutate(
            # tiba_jam = berangkat_jam - 2*60*60,
            # berangkat_jam = format(berangkat_jam, format = "%H:%M"),
            berangkat_jam = NA,
            tiba_jam = format(tiba_jam, format = "%H:%M"),
            
            pel_asal = 'Bira', pel_tujuan = 'Pamatata', pemilik = 'PT. ASDP',
            pelabuhan = 'PELABUHAN PAMATATA', pelayaran = "FERRY", bendera = "RI",
            jenis_kapal = "KMP", 
            panjang = NA, dwt = NA, grt = case_when(
              nmkapal == "KMP. KORMOMOLIN" ~ 884,
              nmkapal == "KMP. SANGKE PALANGGA" ~ 884,
              nmkapal == "KMP. BALIBO" ~ 540,
              nmkapal == "KMP. TAKABONERATE" ~ 842,
              nmkapal == "KMP. BONTOHARU" ~ 1124,
              TRUE ~ NA
            ),
            barang_naik = g4_brg + g5_brg + g6_brg,
            mobil_naik = r4_naik + bus_naik + truck_naik + alatberat_naik,
            
            # tiba_tgl = ifelse(
            #   is.na(tiba_jam), berangkat_tgl,
            #   lubridate::ymd_hm(paste(berangkat_tgl, tiba_jam))
            # ),
            # berangkat_tgl = ifelse(
            #   is.na(tiba_jam), tiba_tgl,
            #   tiba_tgl + 2*60*60
            # )
            berangkat_tgl = tiba_tgl,
            tiba_tgl = ifelse(
              is.na(tiba_jam), tiba_tgl, 
              lubridate::ymd_hm(paste(tiba_tgl, tiba_jam))
            )
          ) %>% 
          dplyr::select(
            kunjungan, nmkapal, pelabuhan, pelayaran, bendera, pemilik, tiba_tgl, tiba_jam, pel_asal, jenis_kapal, berangkat_tgl, berangkat_jam, pel_tujuan,
            "B. CAMP" = barang_naik, penumpang = pnp_naik, motor = motor_naik, mobil = mobil_naik
          ) %>% 
          tidyr::pivot_longer(14:17, names_to = 'nama', values_to = 'nilai') %>% 
          dplyr::mutate(jenis = 'muat')
        
        
        df_final <- dplyr::bind_rows(
          df_bongkar, df_muat
        ) %>% 
          dplyr::arrange(kunjungan) %>% 
          mutate(
            berangkat_tgl = as.POSIXct(berangkat_tgl, origin = "1970-01-01"),
            tiba_tgl = as.POSIXct(tiba_tgl, origin = "1970-01-01")
          )
        
        shiny::showNotification("Success", type = 'message')
        return(olahKomoditas(df_final))
      }
    },
    error = function(cond) {
      shiny::showNotification("Error: Port name does not match", type = 'error')
      shiny::showNotification(cond, type = 'error')
      # Choose a return value in case of error
      NULL
    }
    # warning = function(cond) {
    #   # shiny::showNotification("Data caused a warning", type = 'warning')
    #   # shiny::showNotification(cond, type = 'warning')
    #   # Choose a return value in case of warning
    # 
    # },
    # finally = {
    #   NULL
    # }
  )
}





# Fungsi untuk merapihkan komoditas ---------------------------------------
olahKomoditas <- function(df_hasil){
  df_hasil %>% 
    dplyr::mutate(
      pelabuhan = str_to_title(pelabuhan),
      pelayaran = str_to_title(pelayaran),
      nama = case_when(
        nama %in% c('BARANG.CAMP', 'B CAMP', 'B. CAMP', 'B.  CAMP', 'B.CAMP') ~ 'Barang Campuran',
        nama %in% c('B.BANGUNAN') ~ 'Bahan Bangunan',
        nama %in% c('LPG KSG', 'LPG KSNG', 'ELPIJI KOSONG', 'LPG KOSONG') ~ 'LPG Kosong',
        nama %in% c('GABUS (IKAN)', 'GABUS(IKAN)') ~ 'Gabus Ikan',
        nama %in% c('IKANKERING', 'IKAN KERING') ~ 'Ikan Kering',
        nama %in% c('P. BLOCK', 'PAVING BLOK') ~ 'Paving Blok',
        nama %in% c('DRUM KSNG', 'DRUM KOSONG') ~ 'Drum Kosong',
        nama %in% c('CIPPING', 'CHIPPING') ~ 'Chipping',
        nama %in% c('MENTE', 'JAMBU MENTE') ~ 'Jambu Mente',
        nama %in% c('B.SEMBAKO') ~ 'Sembako',
        nama %in% c('LPG') ~ 'LPG',
        nama %in% c('GC') ~ 'GC',
        TRUE ~ str_to_title(nama)
      )
    )
}



# Fungsi merekap bongkar --------------------------------------------------
# dat <- readxl::read_xlsx('dataFull.xlsx')
# dat %>% 
#   filter(is.na(jenis)) %>% 
#   clipr::write_clip()
# 
# df_rekap <- dat %>% 
#   dplyr::filter(!is.na(jenis)) %>% 
#   # filter(jenis == "bongkar") %>% 
#   group_by(bulan, pelabuhan, nama, jenis) %>% 
#   summarise(nilai = sum(nilai)) %>% 
#   pivot_wider(names_from = nama, values_from = nilai) %>% 
#   group_by(pelabuhan)
# 
# df_rekap[is.na(df_rekap)] <- 0
# df_rekap
# 
# 
# wbbongkar = createWorkbook()
# wbmuat = createWorkbook()
# 
# df_rekap %>% 
#   group_split() %>%
#   sapply(function(dfx){
#     dfx <- dfx %>% 
#       mutate(pelabuhan = str_remove_all(pelabuhan, 'Wilker|Pelabuhan|Posker'))
#     
#     sheet_muat = createSheet(wbmuat, dfx$pelabuhan[1])
#     sheet_bongkar = createSheet(wbbongkar, dfx$pelabuhan[1])
#     
#     df_muat <- as.data.frame(dplyr::filter(dfx, jenis == 'muat')) %>% 
#       dplyr::select(-pelabuhan, -jenis)
#     df_bongkar <- as.data.frame(dplyr::filter(dfx, jenis == 'bongkar')) %>% 
#       dplyr::select(-pelabuhan, -jenis)
#     
#     addDataFrame(
#       df_muat, sheet = sheet_muat, row.names = FALSE
#     )
#     addDataFrame(
#       df_bongkar, sheet = sheet_bongkar, row.names = FALSE
#     )
#   })
# saveWorkbook(wbmuat, "RekapMuat2.xlsx")
# saveWorkbook(wbbongkar, "RekapBongkar2.xlsx")
# 
# 
# wb = createWorkbook()
# df_rekap %>% 
#   filter(jenis == 'muat') %>% 
#   group_split() %>%
#   sapply(function(dfx){
#     dfx <- dfx %>% 
#       mutate(pelabuhan = str_remove_all(pelabuhan, 'Wilker|Pelabuhan|Posker'))
#     sheet = createSheet(wb, dfx$pelabuhan[1])
#     addDataFrame(as.data.frame(dfx), sheet = sheet, row.names = FALSE)
#   })
# saveWorkbook(wb, "RekapMuat.xlsx")


# Fungsi membaca semua sheet ----------------------------------------------
# read_excel_allsheets <- function(filename, tibble = FALSE) {
#   # I prefer straight data.frames
#   # but if you like tidyverse tibbles (the default with read_excel)
#   # then just pass tibble = TRUE
#   sheets <- readxl::excel_sheets(filename)
#   x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
#   if(!tibble) x <- lapply(x, as.data.frame)
#   names(x) <- sheets
#   x
# }



# Fungsi PDF ke Excel -----------------------------------------------------
# library(pdftools)
# 
# tx <- pdf_text('F:/__distribusi/Simopel/2023/Jampea/LAP. OPS. UPP JAMPEA DESEMBER 2023.pdf')
# tx2 <- unlist(str_split(tx, "[\\r\\n]+"))
# tx3 <- str_split_fixed(str_trim(tx2), "\\s{2,}", 5)





