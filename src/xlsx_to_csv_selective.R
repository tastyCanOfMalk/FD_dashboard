library(xlsx)
library(stringr)
library(readr)

# get xlsx directories ---------------------------------------------------------
wd <- getwd()
# defect_xlsx_dir <- paste0(wd, "/data/xlsx/Defect_Data")
yield_xlsx_dir  <- paste0(wd, "/data/xlsx/Yield_Data")

# get export directories --------------------------------------------------
defect_csv_dir <- paste0(wd, "/data/csv/Defect_data/")
yield_csv_dir  <- paste0(wd, "/data/csv/Yield_data/")

# defect files ------------------------------------------------------------
defect_files       <- list.files(defect_xlsx_dir, full.names=TRUE)
defect_latest_file <- defect_files[5]
# defect_latest_file <- defect_files[length(defect_files)]

latest_defect_xlsx <- read.xlsx(defect_latest_file, 
                                "Detail",
                                colIndex = seq(1:19))

# yield files -------------------------------------------------------------
yield_files       <- list.files(yield_xlsx_dir, full.names=TRUE)
yield_latest_file <- yield_files[5]
# yield_latest_file <- yield_files[length(yield_files)]

latest_yield_xlsx <- read.xlsx(yield_latest_file, "Detail",
                               colIndex = seq(1:26))

# get export names --------------------------------------------------------
# defect_csv_name <- str_extract(defect_latest_file, "(?<=_Data/)(.*?)(?=\\.)")
yield_csv_name  <- str_extract(yield_latest_file, "(?<=_Data/)(.*?)(?=\\.)")

# export to csv -----------------------------------------------------------
# write_csv(latest_defect_xlsx, paste0(defect_csv_dir, defect_csv_name, ".csv"))
write_csv(latest_yield_xlsx,  paste0(yield_csv_dir, yield_csv_name, ".csv"))


