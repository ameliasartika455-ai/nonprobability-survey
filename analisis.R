library(readxl)
install.packages("psych")
library(psych)

# IMPORT DATA
data <- read_excel("C:/Users/Lenovo T480s/Downloads/DATA KUSIONER ZOOM MEETING.xlsx")
View(data)
# Melihat nama variabel
names(data)

# Melihat data
View(data)


# TABEL FREKUENSI PRODI
table(data$`Program Studi`)


# PERSENTASE PRODI
prop.table(table(data$`Program Studi`))*100


# GRAFIK BATANG
barplot(table(data$`Program Studi`),
        main = "Distribusi Responden Berdasarkan Program Studi",
        xlab = "Program Studi",
        ylab = "Frekuensi")


# PIE CHART
pie(table(data$`Program Studi`),
    main = "Persentase Responden Berdasarkan Program Studi")


# MEMBUAT SKOR TOTAL
data$TOTAL <- data$P1 + data$P2 + data$P3 + data$P4 + data$P5 +
  data$P6 + data$P7 + data$P8 + data$P9 + data$P10


# STATISTIK DESKRIPTIF
summary(data$TOTAL)
mean(data$TOTAL)
sd(data$TOTAL)
min(data$TOTAL)
max(data$TOTAL)

# Membuat skor total
data$total <- rowSums(data[,c("P1","P2","P3","P4","P5",
                              "P6","P7","P8","P9","P10")])

# Korelasi item dengan total
cor(data$P1, data$total)
cor(data$P2, data$total)
cor(data$P3, data$total)
cor(data$P4, data$total)
cor(data$P5, data$total)
cor(data$P6, data$total)
cor(data$P7, data$total)
cor(data$P8, data$total)
cor(data$P9, data$total)
cor(data$P10, data$total)


alpha(data[,c("P1","P2","P3","P4","P5",
              "P6","P7","P8","P9","P10")])

# Naive estimation
x <- 28
n <- 30

p_hat <- x/n
p_hat

# Proporsi populasi
pop <- c(
  matematika = 365,
  kimia = 331,
  fisika = 311,
  biologi = 359,
  lingkungan = 348,
  statistika = 165
)

# Proporsi sampel
sampel <- c(
  matematika = 2,
  kimia = 5,
  fisika = 2,
  biologi = 4,
  lingkungan = 5,
  statistika = 12
)

# Proporsi
prop_pop <- pop/sum(pop)
prop_sampel <- sampel/sum(sampel)

# Weighting
weight <- prop_pop/prop_sampel

weight
