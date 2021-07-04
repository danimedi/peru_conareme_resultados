#' Unify data sets for 'postulantes' and 'ingresantes'
#' 
#' Join the data sets saved within the directory specified containing the CSV
#' files for the results of the exam of different years, for 'postulantes' and
#' 'ingresantes'.
#'
#' @param dir Path to the directory containing the CSV files.
#'
#' @export
unify_data_sets <- function(dir) {
  
  filenames <- list.files(dir, pattern = "(^\\d{4})_peru.+([.]csv$)", full.names = TRUE)
  suffixes <- c("_postulantes", "_ingresantes")
  
  # FUNCTIONS -----------------------
  
  # Add `year` column to recognize the year when joining different years
  read_data <- function(file, year) {
    dplyr::mutate(
      readr::read_csv(
        file = file,
        col_types = readr::cols(.default = readr::col_character())
      ),
      year = year
    )
  }
  
  # Join postulantes and ingresantes data sets of the same year based on the
  # columns available
  join_postulantes_ingresantes <- function(postulantes, ingresantes) {
    
    # Join postulantes and ingresantes ---------------
    
    evaluate_presence <- function(col) {
      col %in% names(postulantes) & col %in% names(ingresantes)
    }
    
    join <- function(by) {
      dplyr::full_join(
        postulantes, ingresantes,
        suffix = suffixes,
        by = c("year", by)
      )
    }
    
    # Select the right column to join
    if (evaluate_presence("Codigo")) {
      res <- join(by = "Codigo")
    } else if (evaluate_presence("No Doc.")) {
      res <- join(by = "No Doc.")
    } else {
      res <- join(by = c("Apellido Paterno", "Apellido Materno"))
    }
    
    
    # Deal with "phantom" data ---------------------
    
    deal_phantom_data <- function(df) {
      # Deal with those rows without last name
      if (!any(is.na(df[["Apellido Paterno_postulantes"]]))) {
        return(df)
      }
      get_col_names <- function(suffix) {
        paste0(c("Apellido Paterno", "Apellido Materno", "Nombres"), suffix)
      }
      paste_names_cols <- function(df, suffix) {
        cols <- get_col_names(suffix)
        purrr::pmap_chr(df[cols], paste0)
      }
      # Create a data set with this phantom data to check if it the data is
      # present in other parts of the data set and join it at the end (if there
      # is new data)
      
      phantom <- df[is.na(df[["Apellido Paterno_postulantes"]]), ]
      i <- match(
        paste_names_cols(phantom, suffixes[2]),
        paste_names_cols(df, suffixes[1])
      )
      phantom[!is.na(i), endsWith(names(phantom), suffixes[1])] <- df[na.omit(i), endsWith(names(df), "_postulantes")]
      if (length(na.omit(i)) == 0) {
        df
      } else {
        df <- df[-na.omit(i), ]
        df <- df[!is.na(df[["Apellido Paterno_postulantes"]]), ]
        df <- dplyr::bind_rows(df, phantom)
        df
      }
    }
    
    res <- deal_phantom_data(res)
    
    # Collapse first and last names ----------------
    collapse_names <- function(...) {
      purrr::pmap_chr(..., function(...) na.omit(unlist(list(...)))[1])
    }
    
    col_names <- c("Apellido Paterno", "Apellido Materno", "Nombres")
    for (x in col_names) {
      cols <- purrr::map_chr(
        c("", suffixes),
        function(suffix) paste0(x, suffix)
      )
      cols <- cols[cols %in% names(res)]
      res[[x]] <- collapse_names(res[cols])
      # You just need one column for each name, so remove the other columns
      res <- res[!(names(res) %in% cols) | (names(res) %in% x)]
    }
    
    # Remove unnecessary columns --------------
    # Those for identification or order numbers: `No`, `No Doc.`, `Codigo`
    remove_unnecessary_columns(res)
  }
  
  remove_unnecessary_columns <- function(df) {
    cols <- c("No", "No Doc.", "Codigo")
    cols <- unlist(
      lapply(cols, function(x) {
        c(x, paste0(x, suffixes[1]), paste0(x, suffixes[2]))
      })
    )
    i <- names(df) %in% cols
    df[!i]
  }
  
  str_extract <- function(string, pattern) {
    reg_exp <- regexpr(pattern, string)
    result <- regmatches(string, reg_exp)
    unlist(result)
  }
  
  
  # 1. UNIFY POSTULANTES AND INGRESANTES FOR EACH YEAR ---------------
  
  years <- unique(str_extract(basename(filenames), "\\d{4}"))
  res <- lapply(years, function(year) {
    i <- grep(x = basename(filenames), pattern = year)
    filenames_year <- filenames[i]
    # Check the information available to decide if merging the data is necessary
    # or if there is only one data set available for the year
    i_postulantes <- grep(filenames_year, pattern = "postulantes")
    i_ingresantes <- grep(filenames_year, pattern = "ingresantes")
    if (length(i_postulantes) == 1 & length(i_ingresantes) == 1) {
      postulantes <- read_data(filenames_year[i_postulantes], year = year)
      ingresantes <- read_data(filenames_year[i_ingresantes], year = year)
      join_postulantes_ingresantes(postulantes, ingresantes)
    } else if(length(i_postulantes) == 1 & length(i_ingresantes) == 0) {
      remove_unnecessary_columns(
        read_data(filenames_year[i_postulantes], year = year)
      )
    } else {
      warning(
        "There must be 1 or 2 files for a given year ",
        "and at least one for 'postulantes'"
      )
    }
  })
  
  # 2. UNIFY DIFFERENT YEARS --------------------
  
  res <- do.call(dplyr::bind_rows, res)
  
  # Some columns of the data sets of the years without 'ingresantes' do not have
  # the suffix `"_ingresantes"`
  cols <- gsub(glue::glue("({suffixes[1]}$|{suffixes[2]}$)"), "", names(res))
  freq_cols <- table(cols)
  problem_cols <- names(freq_cols)[freq_cols == 3]
  
  collapse_column <- function(df, col) {
    destiny_column_name <- paste0(col, suffixes[1])
    destiny_column <- df[[destiny_column_name]]
    joined_col <- ifelse(is.na(destiny_column), col, destiny_column)
    df <- df[names(df) != col]
    df[[destiny_column_name]] <- joined_col
    df
  }
  
  for (col in problem_cols) {
    res <- collapse_column(res, col)
  }
  
  res
  
}
