
# Grafik Kapal ------------------------------------------------------------
df_mobilitas <- readxl::read_xlsx('data/df_mobilitas.xlsx')
df_mobilitas <- df_mobilitas[df_mobilitas$pelabuhan == 'Jampea', ]
df_mobilitas

grafik_kapal(df_mobilitas)


# Grafik Lalu Lintas ------------------------------------------------------
grafik_lalulintas(df_mobilitas, jenis = 'Mobil')
grafik_lalulintas(df_mobilitas, jenis = 'Motor')
grafik_lalulintas(df_mobilitas, jenis = 'Penumpang')

bench_res <- microbenchmark::microbenchmark(
  base = df_mobilitas[, c('Mobil_bongkar', 'Mobil_muat')],
  base2 = df_mobilitas[c('Mobil_bongkar', 'Mobil_muat')],
  base3 = df_mobilitas[c('Mobil_bongkar')],
  formula = model.frame(~ Mobil_bongkar + Mobil_muat, df_mobilitas),
  dplyr = df_mobilitas %>% select(c('Mobil_bongkar', 'Mobil_muat')),
  dplyr2 =  dplyr::select(df_mobilitas, c('Mobil_bongkar', 'Mobil_muat')),
  base4 =  df_mobilitas[2:3],
  times = 300
)
bench_res

ggplot(bench_res, aes(x=expr, y=time/10^9)) +
  geom_boxplot() +
  xlab('Expression') +
  ylab('Elapsed Time (Seconds)') +
  scale_y_continuous(breaks = seq(0,7,1)) +
  coord_flip()

df_mobilitas[c('Mobil_bongkar', 'Mobil_muat')]
sum(df_mobilitas[c('Mobil_bongkar', 'Mobil_muat')])


# Grafik bongkar muat -----------------------------------------------------
df_rekap <- readxl::read_xlsx('data/df_rekap.xlsx')
df_rekap

df_rekap <- df_rekap[
  df_rekap$tahun == 2023 & df_rekap$pelabuhan == 'Jampea', 
]

HEWAN <- c('Rusa', 'Sapi', 'Kambing', 'Kerbau', 'Kuda')
ALL_NAMES <- setdiff(colnames(df_rekap), c('Mobil', 'Motor', 'Penumpang', HEWAN))
df_rekap <- df_rekap[, ALL_NAMES]

df_rekap <- df_rekap %>% tidyr::pivot_longer(-c(1:6), values_to = 'nilai') 
df_rekap <- df_rekap[df_rekap$nilai != 0, ]


# df_bongkar <- df_rekap[df_rekap$jenis == 'bongkar', ]
df_rekap <- df_rekap %>%
  group_by(jenis, name) %>% 
  summarise(
    nilai = sum(nilai)
  ) %>% 
  top_n(10)

df_bongkar <- df_rekap[df_rekap$jenis == 'bongkar', ]
grafik_bm(df_bongkar, jenis = 'Bongkar')



df_muat <- df_rekap[df_rekap$jenis == 'muat', ]
'Source Sans Pro'


df_bongkar %>% 
  dplyr::select(-Motor, -Mobil, -Penumpang) %>%
  pivot_longer(-c(1:2)) %>% 
  group_by(tahun, name) %>% 
  summarise(
    nilai = sum(value)
  ) %>% 
  filter(tahun == as.numeric(input$tahunVizBMInput), nilai != 0) 

grafik_bm(df_rekap, jenis = 'bongkar')
