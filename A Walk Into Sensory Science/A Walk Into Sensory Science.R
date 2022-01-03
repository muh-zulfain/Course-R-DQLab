##A Walk Into Sensory Science

##Cokelat
##Mengenal Data - Part 1
library(readr)

chocolates <- 
  read_csv(
    "https://storage.googleapis.com/dqlab-dataset/chocolates.csv",
    col_types = cols(
      panelist = col_factor(),
      session = col_factor(),
      rank = col_factor(),
      product = col_factor(levels = paste0("choc", 1:6)),
      .default = col_integer()
    )
  )
chocolates

##Mengenal Data - Part 2: Eksplorasi Struktur Data
install.packages("skimr",repos = "http://cran.us.r-project.org")
library(skimr)
skim(chocolates)

##Mengenal Data - Part 3
library(dplyr)

chocolates %>% 
  summarise(
    sample = toString(levels(product)),
    n_sample = n_distinct(product),
    n_panelist = n_distinct(panelist)
  )

n_sample <- 6
n_panelist <- 29

##Mengenal Data - Part 4
ncol(chocolates) - 4
atribut_sensoris <- colnames(chocolates[-c(1, 2, 3, 4)])
atribut_sensoris

##Mengenal Data - Part 5
install.packages("skimr",repos = "http://cran.us.r-project.org")
library(skimr)
library(dplyr)

chocolates %>% 
  select(atribut_sensoris) %>% 
  skim_without_charts()

batas_bawah <- 0
batas_atas <- 10

##Dari Satu Sisi - Part 1
model_bitterness <- aov(bitterness ~ product + panelist + session + panelist:product + panelist:session + product:session + rank, data = chocolates)

model_bitterness

##Dari Satu Sisi - Part 2
anova(model_bitterness)

##Dari Satu Sisi - Part 3
summary.lm(model_bitterness)
-1.74 # dua digit di belakang koma/titik

##Dari Satu Sisi - Part 6
library(FactoMineR)

res_bitterness <- AovSum(model_bitterness)

anova(model_bitterness)

res_bitterness$Ftest

##Dari Satu Sisi - Part 7
res_bitterness$Ttest[1:7, 1:2]
c("choc1", "choc4", "choc2", "choc5", "choc6", "choc3")

##Dari Satu Sisi - Part 8
install.packages('agricolae', repos="http://cran.rstudio.com/")
library(agricolae)

posthoc_bitterness <- HSD.test(model_bitterness, trt = "product")
posthoc_bitterness$groups

##Dari Satu Sisi - Part 9
install.packages('agricolae', repos="http://cran.rstudio.com/")
library(agricolae)

posthoc_bitterness$groups

plot.group(posthoc_bitterness, variation = "SE")

##Tak Cukup Satu Sisi - Part 2
library(dplyr)
library(corrplot)

chocolates2 <- chocolates %>% 
  select(atribut_sensoris) %>% 
  cor() %>% 
  corrplot(
    type = "upper",
    method = "square",
    diag = FALSE,
    addgrid.col = FALSE,
    order = "FPC", 
    tl.col = "gray30", 
    tl.srt = 30
  )
chocolates2

##Tak Cukup Satu Sisi - Part 4
RDS_from_web <- function(url) {
  tempFile_location <- tempfile()
  download.file(url, tempFile_location)
  rds_file <- readRDS(tempFile_location)
  file.remove(tempFile_location)
  rds_file
}

chocolates_adjmean <- RDS_from_web("https://storage.googleapis.com/dqlab-dataset/chocolates_adjmean.rds")
chocolates_adjmean
dim(chocolates_adjmean)

##Tak Cukup Satu Sisi - Part 5
library(FactoMineR)
chocolates_pca <- PCA(chocolates_adjmean, graph = FALSE)
names(chocolates_pca)
chocolates_pca$eig

##Tak Cukup Satu Sisi - Part 6
library(factoextra)

fviz_eig(chocolates_pca, choice = "eigenvalue", addlabels = TRUE)
fviz_eig(chocolates_pca, choice = "variance",addlabels = TRUE)

##Tak Cukup Satu Sisi - Part 7
library(factoextra)

fviz_pca_ind(chocolates_pca, repel = TRUE)

##Tak Cukup Satu Sisi - Part 8
library(factoextra)
fviz_pca_ind(chocolates_pca, repel = TRUE)

choc2 <- FALSE
choc3 <- TRUE
choc4 <- FALSE
choc5 <- FALSE
choc6 <- TRUE

##Tak Cukup Satu Sisi - Part 9
library(factoextra)
fviz_pca_var(chocolates_pca, repel = TRUE)

##Tak Cukup Satu Sisi - Part 10
library(factoextra)
fviz_pca_var(chocolates_pca, repel = TRUE)
"sticky"
"crunchy"

##Tak Cukup Satu Sisi - Part 11
library(factoextra)
fviz_pca_biplot(chocolates_pca, repel = TRUE, title = "Peta Persepsi Produk Cokelat Komersial")

##Konsistensi
metode_riset <- TRUE
kolom_panelis <- TRUE
jumlah_panelis <- TRUE
kolom_sampel <- TRUE
nama_sampel <- FALSE
jumlah_sampel <- TRUE
kolom_atribut_sensoris <- TRUE
jumlah_atribut_sensoris <- TRUE
informasi_hedonik <- TRUE

##Konsitensi
library(devtools)
install_github("aswansyahputra/sensehubr")
library(sensehubr)

chocolates_qda <- 
  chocolates %>% 
  specify(
    sensory_method = "QDA",
    panelist = panelist,
    product = product,
    session = session,
    pres_order = rank,
    attribute = cocoa_a:granular
  )

chocolates_qda

##Dari Satu Sisi: ala SenseHub - Part 1
chocolates_qda_local <- 
  chocolates_qda %>% 
  analyse(choice = "local")

chocolates_qda_local

##Dari Satu Sisi: ala SenseHub - Part 2
visualise(chocolates_qda_local)

##Tak Cukup Satu Sisi: ala SenseHub - Part 1
chocolates_qda_global <- 
  chocolates_qda %>% 
  analyse(choice = "global")

chocolates_qda_global

##Tak Cukup Satu Sisi: ala SenseHub - Part 2
visualise(chocolates_qda_global, choice = "eigenvalue")
visualise(chocolates_qda_global, choice = "product", repel = TRUE)
visualise(chocolates_qda_global, choice = "attribute", repel = TRUE)
