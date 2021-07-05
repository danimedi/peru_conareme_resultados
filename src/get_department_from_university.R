#' Get the department from university
#' 
#' Obtain the department from the university abbreviations.
#'
#' @param universities Character vector containing the abbreviations of the
#'   universities.
#'
#' @return A character vector of the same length as `universities` containing
#'   the departments.
#' @export
get_department_from_university <- function(universities) {
  dict <- dplyr::tribble(
    ~university, ~department,
    "UCS", "Lima",
    "UCSM", "Arequipa",
    "UNA", "Puno",
    "UPAO", "La Libertad",
    "UNFV", "Lima",
    "UNSA", "Arequipa",
    "UCSUR", "Lima",
    "USMP", "Lima",
    "UNMSM", "Lima",
    "UNAP", "Loreto",
    "UCV", "La Libertad",
    "UPSJB", "Lima",
    "UNC", "Cajamarca",
    "UNCP", "Junin",
    "UNSLGI", "Ica",
    "UPLA", "Junin",
    "URP", "Lima",
    "UPCH", "Lima",
    "UNP", "Piura",
    "UPT", "Tacna",
    "UNPRG", "Lambayeque",
    "UNT", "La Libertad",
    "UNSAAC", "Cusco",
    "USP", "Ancash",
    "UPC", "Lima",
    "Universidad", NA,
    "NORTE", NA,
    "CENTRO 1", NA,
    "CENTRO 2", NA,
    "SUR", NA,
    "CENTRO 3", NA,
    "CONAREME", NA,
    "UContinental", "Junin"
  )
  df <- dplyr::left_join(dplyr::tibble(university = universities), dict)
  df$department
}
