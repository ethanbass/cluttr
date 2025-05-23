#' @noRd
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' Check for suggested package
#'
#' This function checks for a suggested package and returns an error if the
#' package is not installed (if \code{return_boolean} is FALSE. Otherwise, it
#' returns a boolean value.
#'
#' @noRd
check_for_pkg <- function(pkg, return_boolean = FALSE){
  pkg_exists <- requireNamespace(pkg, quietly = TRUE)
  if (return_boolean){
    return(pkg_exists)
  } else if (!pkg_exists) {
    stop(paste(
      "Package", sQuote(pkg), "must be installed to perform this action:
          try", paste0("`install.packages('", pkg, "')`.")),
      call. = FALSE
    )
  }
  invisible(pkg_exists)
}

#' @noRd
check_packages <- function(pkg){
  invisible(sapply(pkg, check_for_pkg))
}


#' @importFrom scales pvalue_format
#' @noRd
p_val_format <- function(x){
  z <- scales::pvalue_format()(x)
  z[!is.finite(x)] <- ""
  z
}

#' @noRd
rnd <- function(x,digits = 2){
  format(round(x, digits = digits), nsmall = digits)
}
