library(googledrive)
library(here)

#' Download the raw data stored as PDFs in a Google Drive
#'
#' @param drive_folder Name of the Google Drive folder containing the PDFs.
#' @param output_dir Path to the directory where the downloaded files will be
#'   saved
#'
#' @export
download_pdfs_from_drive <- function(drive_folder, output_dir) {
  files_drive <- drive_ls(drive_folder)$name
  files_downloaded <- list.files(output_dir)
  for (file in files_drive) {
    filename <- here(output_dir, file)
    if (!file %in% files_downloaded) {
      drive_download(file, filename)
    }
  }
}
