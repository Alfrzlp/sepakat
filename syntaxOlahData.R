# Fungsi olah data ------------------------------------------------------
olahDataPelabuhan <- function(loc, pelabuhan, in_shiny = TRUE){
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
          dplyr::mutate(
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
          dplyr::group_by(id) %>% 
          dplyr::mutate(
            jenis = 'bongkar'
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
          dplyr::mutate(
            jenis = 'muat'
          ) %>% 
          dplyr::filter(!is.na(nilai)) %>% 
          dplyr::arrange(id)
        
        
        df_bm <- dplyr::bind_rows(
          df_bongkar, 
          df_muat
        ) %>% 
          dplyr::arrange(id)
        
        
        df_final <- df_kapal %>% 
          dplyr::left_join(df_bm) %>% 
          dplyr::select(-id, -dwt) %>% 
          dplyr::mutate(
            tiba_jam = as.POSIXct(format(tiba_jam, nsmall = 2), format = "%H.%M", tz = 'Asia/Makassar'),
            tiba_jam = format(tiba_jam, format = "%H:%M"),
            berangkat_jam = as.POSIXct(format(berangkat_jam, nsmall = 2), format = "%H.%M", tz = 'Asia/Makassar'),
            berangkat_jam = format(berangkat_jam, format = "%H:%M")
          )
        
        # Patumbbukan
        df_final <- olahKomoditas(df_final)
        
        # barang yang akan di bongkar di pattumbukan
        df_bongkarPtk <- df_final %>% 
          dplyr::filter(str_detect(pel_tujuan, 'PATTUMBUKAN')) %>% 
          mutate(
            pel_asal = pelabuhan,
            pel_tujuan = NA,
            pelabuhan = 'Pelabuhan Pattumbukan',
            tiba_jam = NA,
            berangkat_jam = NA
          ) %>% 
          group_by(kunjungan) %>% 
          mutate(kunjungan = cur_group_id()) 
        
        # barang berasal dari pattumbukan
        df_muatPtk <- df_final %>% 
          dplyr::filter(str_detect(pel_asal, 'PATTUMBUKAN')) %>% 
          mutate(
            pel_tujuan = pelabuhan,
            pel_asal = NA,
            pelabuhan = 'Pelabuhan Pattumbukan',
            tiba_jam = NA,
            berangkat_jam = NA
          ) %>% 
          group_by(kunjungan) %>% 
          mutate(kunjungan = cur_group_id()) 
        
        df_final <- df_final %>% 
          bind_rows(df_muatPtk) %>% 
          bind_rows(df_bongkarPtk) 
        
        if(in_shiny){
          shiny::showNotification("Success", type = 'message')
        }
        
        
        return(df_final)
        
        # Pelabuhan Pamatata ------------------------------------------------------
      }else if (pelabuhan == "Pamatata") {
        # Bongkar ---------------------------------------------------------
        df_bongkar <- tryCatch(
          {
            df_bongkar <- readxl::read_xlsx(
              loc, sheet = 'KEDATANGAN', skip = 9,
              col_names = c('tiba_tgl', 'nmkapal', 'tiba_jam', 'jml_trip', 'pnp_dewasa', 'pnp_anak', 'pnp_turun',
                            'g1', 'g2', 'g3', 'g4_pnp', 'g4_brg', 'g5_pnp', 'g5_brg', 'g6_pnp', 'g6_brg', 'g7', 'g8', 'g9',
                            'motor_turun', 'r4_turun', 'bus_turun', 'truck_turun', 'alatberat_turun',
                            'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7'),
              col_types = c('date', 'text', 'date', rep('numeric', 28))
            )
          },
          error = function(cond){
            df_bongkar <- readxl::read_xlsx(
              loc, sheet = 'KEDATANGAN', skip = 9,
              col_names = c('tiba_tgl', 'nmkapal', 'tiba_jam', 'jml_trip', 'pnp_dewasa', 'pnp_anak', 'pnp_turun',
                            'g1', 'g2', 'g3', 'g4_pnp', 'g4_brg', 'g5_pnp', 'g5_brg', 'g6_pnp', 'g6_brg', 'g7', 'g8', 'g9',
                            'motor_turun', 'r4_turun', 'bus_turun', 'truck_turun', 'alatberat_turun'),
              col_types = c('date', 'text', 'date', rep('numeric', 21))
            )
          }
        )
        
        df_bongkar <- df_bongkar %>%
          dplyr::slice(-1) %>%
          dplyr::filter(!is.na(nmkapal)) %>%
          tibble::rowid_to_column('kunjungan') %>%
          tidyr::fill(tiba_tgl) %>%
          dplyr::mutate(
            berangkat_jam = NA,
            tiba_jam = format(tiba_jam, format = "%H:%M"),
            
            pel_asal = 'Bira', pel_tujuan = 'Bira', pemilik = 'PT. ASDP',
            pelabuhan = 'PELABUHAN PAMATATA', pelayaran = "FERRY", bendera = "RI",
            jenis_kapal = "KMP",
            panjang = NA, grt = case_when(
              nmkapal == "KMP. KORMOMOLIN" ~ 884,
              nmkapal == "KMP. SANGKE PALANGGA" ~ 884,
              nmkapal == "KMP. BALIBO" ~ 540,
              nmkapal == "KMP. TAKABONERATE" ~ 842,
              nmkapal == "KMP. BONTOHARU" ~ 1124,
              TRUE ~ NA
            ),
            barang_turun = g4_brg + g5_brg + g6_brg,
            mobil_turun = r4_turun + bus_turun + truck_turun + alatberat_turun,
            
            tiba_tgl = as.POSIXct(tiba_tgl, origin = "1970-01-01", tz = 'Asia/Makassar'),
            berangkat_tgl = tiba_tgl
          ) %>%
          dplyr::select(
            kunjungan, nmkapal, pelabuhan, pelayaran, bendera, pemilik, tiba_tgl, tiba_jam, pel_asal, jenis_kapal, berangkat_tgl, berangkat_jam, pel_tujuan,
            "B. CAMP" = barang_turun, penumpang = pnp_turun, motor = motor_turun, mobil = mobil_turun
          ) %>%
          tidyr::pivot_longer(14:17, names_to = 'nama', values_to = 'nilai') %>%
          dplyr::mutate(jenis = 'bongkar')
        
        
        # Muat -------------------------------------------------------------------
        df_muat <- tryCatch({
          df_muat <- readxl::read_xlsx(
            loc, sheet = 'KEBERANGKATAN', skip = 9,
            col_names = c('berangkat_tgl', 'nmkapal', 'berangkat_jam', 'jml_trip', 'pnp_dewasa', 'pnp_anak', 'pnp_naik', 
                          'g1', 'g2', 'g3', 'g4_pnp', 'g4_brg', 'g5_pnp', 'g5_brg', 'g6_pnp', 'g6_brg', 'g7', 'g8', 'g9',
                          'motor_naik', 'r4_naik', 'bus_naik', 'truck_naik', 'alatberat_naik',
                          'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7'),
            col_types = c('date', 'text', 'date', rep('numeric', 28))
          )
        }, 
        error = function(cond){
          df_muat <- readxl::read_xlsx(
            loc, sheet = 'KEBERANGKATAN', skip = 9,
            col_names = c('berangkat_tgl', 'nmkapal', 'berangkat_jam', 'jml_trip', 'pnp_dewasa', 'pnp_anak', 'pnp_naik', 
                          'g1', 'g2', 'g3', 'g4_pnp', 'g4_brg', 'g5_pnp', 'g5_brg', 'g6_pnp', 'g6_brg', 'g7', 'g8', 'g9',
                          'motor_naik', 'r4_naik', 'bus_naik', 'truck_naik', 'alatberat_naik'),
            col_types = c('date', 'text', 'date', rep('numeric', 21))
          )
        }
        )
        
        df_muat <- df_muat %>%
          dplyr::slice(-1) %>%
          dplyr::filter(!is.na(nmkapal)) %>%
          tibble::rowid_to_column('kunjungan') %>%
          tidyr::fill(berangkat_tgl) %>%
          dplyr::mutate(
            tiba_jam = NA,
            berangkat_jam = format(berangkat_jam, format = "%H:%M"),
            
            pel_asal = 'Bira', pel_tujuan = 'Bira', pemilik = 'PT. ASDP',
            pelabuhan = 'PELABUHAN PAMATATA', pelayaran = "FERRY", bendera = "RI",
            jenis_kapal = "KMP",
            panjang = NA,
            grt = case_when(
              nmkapal == "KMP. KORMOMOLIN" ~ 884,
              nmkapal == "KMP. SANGKE PALANGGA" ~ 884,
              nmkapal == "KMP. BALIBO" ~ 540,
              nmkapal == "KMP. TAKABONERATE" ~ 842,
              nmkapal == "KMP. BONTOHARU" ~ 1124,
              TRUE ~ NA
            ),
            barang_naik = g4_brg + g5_brg + g6_brg,
            mobil_naik = r4_naik + bus_naik + truck_naik + alatberat_naik,
            
            berangkat_tgl =  as.POSIXct(berangkat_tgl, origin = "1970-01-01", tz = 'Asia/Makassar'),
            tiba_tgl = berangkat_tgl,
          ) %>%
          dplyr::select(
            kunjungan, nmkapal, pelabuhan, pelayaran, bendera, pemilik, tiba_tgl, tiba_jam, pel_asal, jenis_kapal, berangkat_tgl, berangkat_jam, pel_tujuan,
            "B. CAMP" = barang_naik, penumpang = pnp_naik, motor = motor_naik, mobil = mobil_naik
          ) %>%
          tidyr::pivot_longer(14:17, names_to = 'nama', values_to = 'nilai') %>%
          dplyr::mutate(jenis = 'muat')
        
        
        df_final <- dplyr::bind_rows(df_bongkar, df_muat) %>%
          dplyr::arrange(kunjungan)
        
        
        if(in_shiny){
          shiny::showNotification("Success", type = 'message')
        }
        
        return(olahKomoditas(df_final))
      }
    },
    error = function(cond) {
      if(in_shiny){
        shiny::showNotification("Error: Data Laporan Tidak Sesuai", type = 'error')
        shiny::showNotification(cond, type = 'error')
      }
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
        nama %in% c('BARANG.CAMP', 'B CAMP', 'B. CAMP', 'B.  CAMP', 'B.CAMP', 'B..CAMP', 'B.CAMPUR', "B.. CAMP") ~ 'Barang Campuran',
        nama %in% c('B.BANGUNAN', 'B. BANGUNAN') ~ 'Bahan Bangunan',
        nama %in% c('LPG KSG', 'LPG KSNG', 'ELPIJI KOSONG', 'LPG KOSONG', "T. LPG", "TABUNG LPG") ~ 'LPG Kosong',
        nama %in% c('GABUS (IKAN)', 'GABUS(IKAN)', "GABUS ISI IKAN") ~ 'Gabus Ikan',
        nama %in% c('IKANKERING', 'IKAN KERING') ~ 'Ikan Kering',
        nama %in% c('P. BLOCK', 'PAVING BLOK') ~ 'Paving Blok',
        nama %in% c('DRUM KSNG', 'DRUM KOSONG') ~ 'Drum Kosong',
        nama %in% c('CIPPING', 'CHIPPING') ~ 'Chipping',
        nama %in% c('MENTE', 'JAMBU MENTE') ~ 'Jambu Mente',
        nama %in% c('B.SEMBAKO') ~ 'Sembako',
        nama %in% c('LPG', 'ELPIJI', 'LPG 3 KG') ~ 'LPG',
        nama %in% c('GC') ~ 'GC',
        nama %in% c('BBM') ~ 'BBM',
        nama %in% c('K.DURI') ~ 'Kawat Duri',
        nama %in% c('GC') ~ 'GC',
        nama %in% c('R.LAUT', 'R. LAUT', 'RUMPUT.L') ~ 'Rumput Laut',
        nama %in% c('T.PANCANG', 'TIANG') ~ 'Tiang Pancang',
        nama %in% c('TIANG BESI') ~ 'Tiang Besi Panel',
        nama %in% c('MOBILREDY MIX') ~ 'Mobil Mix',
        nama %in% c("BERAS,CARG", "CARGOBERA", "BERASCARGO") ~ 'Beras',
        nama %in% c("SERBUK PADI") ~ 'Dedak',
        TRUE ~ stringr::str_to_title(nama)
      )
    )
}



# rekap Lalu Lintas -------------------------------------------------------------
addDataLaluLintas <- function(df_hasil, tahun, bulan, laporan, exist){
  df_mobilitas <- readxl::read_xlsx('data/df_mobilitas.xlsx')
  bulan_angka <- which(NAMA_BULAN == bulan)
  
  if (laporan == 'Jampea') {
    df_kapal <- df_hasil %>% 
      group_by(pelabuhan) %>% 
      summarise(
        Kapal = max(kunjungan)
      ) 
    
    df_lalulintas <- df_hasil %>% 
      filter(nama %in% c("Penumpang", 'Mobil', 'Motor')) %>% 
      group_by(pelabuhan, jenis, nama) %>% 
      summarise(
        nilai = sum(nilai, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      mutate(nama = paste0(nama, '_', jenis)) %>%
      dplyr::select(-jenis) %>% 
      pivot_wider(names_from = nama, values_from = nilai) %>% 
      left_join(df_kapal, by = 'pelabuhan') %>% 
      mutate(
        tahun = tahun,
        bulan = bulan,
        laporan,
        date = as.Date(paste0('1/', bulan_angka, '/', tahun), format = '%d/%m/%Y'),
        pelabuhan = str_remove_all(pelabuhan, 'Wilker|Pelabuhan|Posker'),
        pelabuhan = str_trim(pelabuhan),
        .before = pelabuhan
      )
    
  }else if (laporan == 'Pamatata') {
    df_lalulintas <- df_hasil %>% 
      filter(nama %in% c("Penumpang", 'Mobil', 'Motor')) %>% 
      group_by(pelabuhan, jenis, nama) %>% 
      summarise(
        nilai = sum(nilai, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      mutate(nama = paste0(nama, '_', jenis)) %>%
      dplyr::select(-jenis) %>% 
      pivot_wider(names_from = nama, values_from = nilai) %>% 
      mutate(
        tahun = tahun,
        bulan = bulan,
        laporan,
        date = as.Date(paste0('01/', bulan_angka, '/', tahun), format = '%d/%m/%Y'),
        pelabuhan = str_remove_all(pelabuhan, 'Pelabuhan\\s'),
        .before = pelabuhan
      ) 
    
    df_lalulintas$Kapal <- max(df_hasil$kunjungan)
  }
  
  if (nrow(df_mobilitas) == 0) {
    df_lalulintas <- mutate_at(df_lalulintas, -c(1:5), ~ replace(.x, is.na(.x), 0))
    writexl::write_xlsx(df_lalulintas, 'data/df_mobilitas.xlsx')
  }else{
    if (exist) {
      LAPORAN <- laporan
      BULAN <- bulan
      TAHUN <- tahun
      
      df_mobilitas <- dplyr::filter(
        .data = df_mobilitas, !(laporan == LAPORAN & tahun == TAHUN & bulan == BULAN)
      )
    }
    
    df_final <- bind_rows(df_mobilitas, df_lalulintas) %>% 
      arrange(tahun, match(bulan, NAMA_BULAN))
    
    df_final <- mutate_at(df_final, -c(1:5), ~ replace(.x, is.na(.x), 0))
    
    writexl::write_xlsx(df_final, 'data/df_mobilitas.xlsx')
  }
}


# addDataLaluLintas(
#   df_hasil, tahun = 2023, bulan = 'Januari', laporan = 'Jampea', FALSE
# )



# Rekap Data Bongkar Muat -------------------------------------------------

addDataRekap <- function(df_hasil, tahun, bulan, laporan, exist){
  df_rekap <- readxl::read_xlsx('data/df_rekap.xlsx')
  bulan_angka <- which(NAMA_BULAN == bulan)
  
  
  df_rekap_baru <- df_hasil %>%
    dplyr::filter(!is.na(jenis)) %>%
    # filter(jenis == "bongkar") %>%
    group_by(pelabuhan, nama, jenis) %>%
    summarise(nilai = sum(nilai)) %>%
    pivot_wider(names_from = nama, values_from = nilai) %>%
    mutate(
      tahun = tahun,
      bulan = bulan,
      laporan,
      date = as.Date(paste0('01/', bulan_angka, '/', tahun), format = '%d/%m/%Y'),
      pelabuhan = str_remove_all(pelabuhan, 'Wilker|Pelabuhan|Posker'),
      pelabuhan = str_trim(pelabuhan),
      .before = pelabuhan
    )
    
  
  if (nrow(df_rekap) == 0) {
    df_rekap_baru <- mutate_at(df_rekap_baru, -c(1:6), ~ replace(.x, is.na(.x), 0))
    writexl::write_xlsx(df_rekap_baru, 'data/df_rekap.xlsx')
  }else{
    if (exist) {
      LAPORAN <- laporan
      BULAN <- bulan
      TAHUN <- tahun
      
      df_rekap <- dplyr::filter(
        .data = df_rekap, !(laporan == LAPORAN & tahun == TAHUN & bulan == BULAN)
      )
    }
    
    df_rekap_final <- bind_rows(df_rekap, df_rekap_baru) %>% 
      arrange(tahun, match(bulan, NAMA_BULAN))
    
    df_rekap_final <- mutate_at(df_rekap_final, -c(1:6), ~ replace(.x, is.na(.x), 0))
    
    writexl::write_xlsx(df_rekap_final, 'data/df_rekap.xlsx')
  }
}



# addDataRekap(
#   df_hasil, tahun = 2023, bulan = 'Januari', laporan = 'Jampea', TRUE
# )














# Fungsi Merekap ----------------------------------------------------------
rekapData <- function(){
  df_full <- readxl::read_xlsx('data/dataFull.xlsx')
  # df_full <- olahKomoditas(df_full)
  
  df_bongkar <- df_full %>% 
    dplyr::filter(str_detect(pel_tujuan, 'PATTUMBUKAN')) %>% 
    mutate(
      pel_asal = pel_tujuan,
      pel_tujuan = NA,
      pelabuhan = 'Pattumbukan'
    )
  df_muat <- df_full %>% 
    dplyr::filter(str_detect(pel_asal, 'PATTUMBUKAN')) %>% 
    mutate(
      pel_tujuan = pel_asal,
      pel_asal = NA,
      pelabuhan = 'Pattumbukan'
    )
  
  df_rekap <- df_full %>%
    bind_rows(df_muat) %>% 
    bind_rows(df_bongkar) %>% 
    dplyr::filter(!is.na(jenis)) %>%
    # filter(jenis == "bongkar") %>%
    group_by(tahun, bulan, pelabuhan, nama, jenis) %>%
    summarise(nilai = sum(nilai)) %>%
    pivot_wider(names_from = nama, values_from = nilai) %>%
    group_by(pelabuhan)
  
  df_rekap[is.na(df_rekap)] <- 0
  
  wbbongkar = createWorkbook()
  wbmuat = createWorkbook()
  
  # jampea
  df_rekap %>%
    dplyr::group_split() %>%
    sapply(function(dfx){
      dfx <- dfx %>%
        mutate(pelabuhan = str_remove_all(pelabuhan, 'Wilker|Pelabuhan|Posker'))
      
      nmpelabuhan <- stringr::str_trim(dfx$pelabuhan[1])
      sheet_muat <- xlsx::createSheet(wbmuat, nmpelabuhan)
      sheet_bongkar <- xlsx::createSheet(wbbongkar, nmpelabuhan)
      
      df_muat <- as.data.frame(dplyr::filter(dfx, jenis == 'muat')) %>%
        dplyr::select(-pelabuhan, -jenis) %>% 
        dplyr::arrange(match(bulan, NAMA_BULAN))
      df_bongkar <- as.data.frame(dplyr::filter(dfx, jenis == 'bongkar')) %>%
        dplyr::select(-pelabuhan, -jenis) %>% 
        dplyr::arrange(match(bulan, NAMA_BULAN))
      
      if (nmpelabuhan == "Pamatata") {
        df_muat <- dplyr::select(df_muat, c('tahun', 'bulan', 'Barang Campuran', 'Penumpang', 'Motor', 'Mobil'))
        df_bongkar <- dplyr::select(df_bongkar, c('tahun', 'bulan', 'Barang Campuran', 'Penumpang', 'Motor', 'Mobil'))
      }
      
      xlsx::addDataFrame(
        df_muat, sheet = sheet_muat, row.names = FALSE
      )
      xlsx::addDataFrame(
        df_bongkar, sheet = sheet_bongkar, row.names = FALSE
      )
    })
  
  # pattumbukan
  
  saveWorkbook(wbmuat, "data/RekapMuat.xlsx")
  saveWorkbook(wbbongkar, "data/RekapBongkar.xlsx")
}



# empty plot --------------------------------------------------------------
empty_plot <- function(title = NULL){
  p <- plotly_empty(type = "scatter", mode = "markers") %>%
    config(
      displayModeBar = FALSE
    ) %>%
    layout(
      title = list(
        text = title,
        yref = "paper",
        y = 0.5
      )
    )
  return(p)
} 

empty_plot("Data tidak ditemukan")


# untuk viz ---------------------------------------------------------------
rekapViz <- function(){
  df_full <- readxl::read_xlsx('data/dataFull.xlsx')
  
  # data kunjungan kapal
  df_kapal <- df_full %>% 
    group_by(tahun, bulan, pelabuhan) %>% 
    summarise(
      kapal = max(kunjungan)
    ) %>% 
    mutate(
      pelabuhan = str_remove_all(pelabuhan, 'Wilker|Pelabuhan|Posker'),
      pelabuhan = str_trim(pelabuhan),
      bulan = factor(bulan, NAMA_BULAN, labels = 1:12),
      date = lubridate::my(paste(bulan, tahun))
    ) %>% 
    dplyr::select(date, pelabuhan, kapal)
  
  # data penumpang, mobil, motor
  df_lalulintas <- df_full %>% 
    filter(nama %in% c("Penumpang", 'Mobil', 'Motor')) %>% 
    group_by(tahun, bulan, pelabuhan, jenis, nama) %>% 
    summarise(
      nilai = sum(nilai, na.rm = TRUE)
    ) %>% 
    mutate(
      pelabuhan = str_remove_all(pelabuhan, 'Wilker|Pelabuhan|Posker'),
      pelabuhan = str_trim(pelabuhan),
      bulan = factor(bulan, NAMA_BULAN, labels = 1:12),
      date = lubridate::my(paste(bulan, tahun))
    )
  
  writexl::write_xlsx(df_kapal, 'data/df_kapal.xlsx')
  writexl::write_xlsx(df_lalulintas, 'data/df_lalulintas.xlsx')
}


