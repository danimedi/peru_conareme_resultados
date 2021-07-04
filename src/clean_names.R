clean_names <- function(df) {
  # Starting with...
  useful_columns <- c("year", "Nombres", "Apellido", "Especialidad", "Universidad")
  i <- lapply(useful_columns, function(col) which(startsWith(names(df), col)))
  i <- unlist(i)
  df[i]
}
