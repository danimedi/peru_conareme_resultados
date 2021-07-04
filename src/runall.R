# Source the R files containing functions and store them into an environment
# to avoid names clashes
setwd(here::here())
files <- list.files("src", full.names = TRUE, pattern = "[.]R$")
files <- files[basename(files) != "runall.R"]
funs <- new.env()
for (file in files) source(file, local = funs)

# Download raw data -----------------
create_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}
dir_pdf <- "data_raw/pdf"
create_dir(dir_pdf)
funs$download_pdfs_from_drive(drive_folder = "conareme_pdfs", output_dir = dir_pdf)

# Transform PDF to CSV ----------------
dir_csv <- "data_raw/csv"
create_dir(dir_csv)
funs$read_pdf_tables(path_input_pdf = dir_pdf, path_output_csv = dir_csv)

# Join data sets ----------------------
data_set <- funs$unify_data_sets(dir_csv)

# Clean data -----------------
data_set <- funs$clean_names(data_set)

# Save data -----------------
readr::write_csv(data_set, "data/data_set.csv")

# De-identify and save ---------------------
deidentify <- function(df) {
  dplyr::select(df, -dplyr::starts_with(c("Nombres", "Apellido")))
}
data_set_deidentified <- deidentify(data_set)
readr::write_csv(data_set, "data/data_set_deidentified.csv")
