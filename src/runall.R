library(dplyr)
library(readr)

# Source the R files containing functions and store them into an environment
# to avoid names clashes
setwd(here::here())
files <- list.files("src", full.names = TRUE, pattern = "[.]R$")
files <- files[basename(files) != "runall.R"]
funs <- new.env()
for (file in files) source(file, local = funs)

# Download and transform raw data? (time consuming process)
raw_data <- FALSE
if (raw_data) {
  # Download
  create_dir <- function(path) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  }
  dir_pdf <- "data_raw/pdf"
  create_dir(dir_pdf)
  funs$download_pdfs_from_drive(drive_folder = "conareme_pdfs", output_dir = dir_pdf)
  # Transform PDF to CSV
  dir_csv <- "data_raw/csv"
  create_dir(dir_csv)
  funs$read_pdf_tables(path_input_pdf = dir_pdf, path_output_csv = dir_csv)
}

# Join data sets
dir_csv <- "data_raw/csv"
data_set <- funs$unify_data_sets(dir_csv)

# Clean data
data_set <- funs$clean_names(data_set)

# Add gender column
data_set <- mutate(data_set, gender = funs$get_gender(Nombres))

# Fix university names
data_set <- mutate(
  data_set,
  Universidad_postulantes = funs$fix_university_names(Universidad_postulantes),
  Universidad_ingresantes = funs$fix_university_names(Universidad_ingresantes)
)

# Get the departments
data_set <- mutate(
  data_set,
  department_postulantes = funs$get_department_from_university(Universidad_postulantes),
  department_ingresantes = funs$get_department_from_university(Universidad_ingresantes)
)




# Save data
write_csv(data_set, "data/data_set.csv")

# De-identify and save
deidentify <- function(df) {
  select(df, -starts_with(c("Nombres", "Apellido")))
}
data_set_deidentified <- deidentify(data_set)
write_csv(data_set_deidentified, "data/data_set_deidentified.csv")
