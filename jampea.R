loc <- 'F:/__distribusi/Simopel/2023/Jampea/LAP. OPS. UPP JAMPEA DESEMBER 2023.xlsx'

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
  dplyr::arrange(id)


df_final <- df_kapal %>% 
  left_join(df_bm) %>% 
  dplyr::select(-id) %>% 
  mutate(
    tiba_jam = as.POSIXct(format(tiba_jam, nsmall = 2), format = "%H.%M", tz = 'Asia/Makassar'),
    tiba_jam = format(tiba_jam, format = "%H:%M"),
    berangkat_jam = as.POSIXct(format(berangkat_jam, nsmall = 2), format = "%H.%M", tz = 'Asia/Makassar'),
    berangkat_jam = format(berangkat_jam, format = "%H:%M")
  )

glimpse(df_final)
df_jampea <- olahKomoditas(df_final)
