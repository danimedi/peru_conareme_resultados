#' Fix the wrong names of some universities
#' 
#' Some universities have wrong names due to interactions with adjacent columns,
#' this function corrects these errors.
#'
#' @param university_names Character vector with the names of the universities.
#'
#' @return A character vector with the corrected names of the universities. The
#'   names that do not need correction stay the same.
#' @export
fix_university_names <- function(university_names) {
  
  university_names <- stringr::str_remove(university_names, "^\\w+ - ")
  
  lookup_postulantes <- c(
    "AUSMP" = "USMP", "DITHURP" = "URP", "EMMAUNMSM" = "UNMSM", "ENASUPCH" = "UPCH",
    "GOUSMP" = "USMP", "HUNT" = "UNT", "HURP" = "URP", "IKUNSA" = "UNSA",
    "IUPCH" = "UPCH", "LESUNSA" = "UNSA", "LESUSMP" = "USMP", "LINDAUPAO" = "UPAO",
    "NUNCP" = "UNCP", "OSURP" = "URP", "OUNMSM" = "UNMSM", "OUNSA" = "UNSA",
    "RIOUPAO" = "UPAO", "ROSURP" = "URP", "RRESUNMSM" = "UNMSM", "RUPAO" = "UPAO",
    "SUNMSM" = "UNMSM", "SUPAO" = "UPAO", "SUPCH" = "UPCH", "AURP" = "URP",
    "EDESUNMSM" = "UNMSM", "HUPAO" = "UPAO", "IEUNSA" = "UNSA", "SURP" = "URP",
    "SUSMP" = "USMP", "SABINUANMSM" = "UNMSM", "ROSARUIORP" = "URP",
    "N FEURNMASNMDEZ" = "UNMSM"
  )
  lookup_ingresantes <- c(
    "OUSMP" = "USMP", "SURP" = "URP", "ESUNP" = "UNP"
  )
  lookup_universities <- c(lookup_postulantes , lookup_ingresantes)
  ifelse(
    university_names %in% names(lookup_universities),
    lookup_universities[university_names],
    university_names
  )
}
