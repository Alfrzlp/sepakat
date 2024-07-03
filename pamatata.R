loc <- 'F:/__distribusi/Simopel/2023/Pamatata/DATA PRODUKSI AGUSTUS 2023 PP PAMATATA.xlsx'

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
    berangkat_jam = NA,
    tiba_jam = format(tiba_jam, format = "%H:%M"),
    
    pel_asal = 'Bira', pel_tujuan = 'Bira', pemilik = 'PT. ASDP',
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
    
    tiba_tgl = as.POSIXct(tiba_tgl, origin = "1970-01-01", tz = 'Asia/Makassar'),
    berangkat_tgl = tiba_tgl
    # tiba_tgl = ifelse(
    #   is.na(tiba_jam), tiba_tgl, 
    #   lubridate::ymd_hm(paste(tiba_tgl, tiba_jam))
    # )
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
    tiba_jam = NA,
    berangkat_jam = format(berangkat_jam, format = "%H:%M"),
    
    pel_asal = 'Bira', pel_tujuan = 'Bira', pemilik = 'PT. ASDP',
    pelabuhan = 'PELABUHAN PAMATATA', pelayaran = "FERRY", bendera = "RI",
    jenis_kapal = "KMP", 
    panjang = NA, dwt = NA, 
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


df_final <- dplyr::bind_rows(
  df_bongkar, df_muat
) %>% 
  dplyr::arrange(kunjungan) %>% 
  mutate(
    berangkat_tgl = as.POSIXct(berangkat_tgl, origin = "1970-01-01", tz = 'Asia/Makassar'),
    tiba_tgl = as.POSIXct(tiba_tgl, origin = "1970-01-01", tz = 'Asia/Makassar')
  )

df_pamatata <- olahKomoditas(df_final)


df_jampea %>% 
  bind_rows(df_pamatata)
