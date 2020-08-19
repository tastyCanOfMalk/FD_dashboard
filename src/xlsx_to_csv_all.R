options(java.parameters = "-Xmx8g")
library(xlsx)
library(stringr)
library(readr)

# refresh latest only ---------------------------------------------------------
wd <- getwd()

defect_xlsx_dir <- paste0(wd, "/data/xlsx/Defect_Data")
defect_csv_dir  <- paste0(wd, "/data/csv/Defect_data/")

defect_files       <- list.files(defect_xlsx_dir, full.names=TRUE)
defect_latest_file <- defect_files[length(defect_files)]
latest_defect_xlsx <- read.xlsx(defect_latest_file, 
                                "Detail",
                                colIndex = seq(1:19))
latest_defect_xlsx <- latest_defect_xlsx[complete.cases(latest_defect_xlsx),]

defect_csv_name <- str_extract(defect_latest_file, "(?<=_Data/)(.*?)(?=\\.)")

write_csv(latest_defect_xlsx, paste0(defect_csv_dir, defect_csv_name, ".csv"))


yield_xlsx_dir  <- paste0(wd, "/data/xlsx/Yield_Data")
yield_csv_dir   <- paste0(wd, "/data/csv/Yield_data/")

yield_files       <- list.files(yield_xlsx_dir, full.names=TRUE)
yield_latest_file <- yield_files[length(yield_files)]
# latest_yield_xlsx <- read.xlsx(yield_latest_file, "Detail")
latest_yield_xlsx <- read.xlsx(yield_latest_file,
                               "Detail",
                               colIndex = seq(1:26))
latest_yield_xlsx <- latest_yield_xlsx[complete.cases(latest_yield_xlsx),]

yield_csv_name  <- str_extract(yield_latest_file, "(?<=_Data/)(.*?)(?=\\.)")

write_csv(latest_yield_xlsx,  paste0(yield_csv_dir, yield_csv_name, ".csv"))

# ALL FILES ---------------------------------------------------------------

library(xlsx)
library(stringr)
library(readr)

wd <- getwd()

defect_xlsx_dir <- paste0(wd, "/data/xlsx/Defect_Data")
yield_xlsx_dir  <- paste0(wd, "/data/xlsx/Yield_Data")

defect_csv_dir <- paste0(wd, "/data/csv/Defect_data/")
yield_csv_dir <- paste0(wd, "/data/csv/Yield_data/")

defect_files <- list.files(defect_xlsx_dir, full.names=TRUE)
yield_files <- list.files(yield_xlsx_dir, full.names=TRUE)

for(file in defect_files){
  print(paste("Reading XLSX from:", file))
  temp_xlsx <- read.xlsx(file,
                         "Detail",
                         colIndex = seq(1:19))
  # temp_xlsx <- temp_xlsx[complete.cases(temp_xlsx),]
  csv_name <- str_extract(file, "(?<=_Data/)(.*?)(?=\\.)")
  print(paste("Writing CSV:", csv_name))
  write_csv(temp_xlsx, paste0(defect_csv_dir, csv_name, ".csv"))
}

for(file in yield_files){
  print(paste("Reading XLSX from:", file))
  temp_xlsx <- read.xlsx(file,"Detail")
  # temp_xlsx <- read.xlsx(file,"Detail",colIndex = seq(1:26))
  # temp_xlsx <- temp_xlsx[complete.cases(temp_xlsx),]
  csv_name <- str_extract(file, "(?<=_Data/)(.*?)(?=\\.)")
  print(paste("Writing CSV:", csv_name))
  write_csv(temp_xlsx, paste0(yield_csv_dir, csv_name, ".csv"))
}
