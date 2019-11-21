#' summary.feedback
#'
#' Prints a summary of a feedback object
#'
#' @param object feedback object
#' @param ... ignored
#'
#' @return nothing
#' @export
#'
#' @examples
#' x  <- generateTestData(n=100, 
#'                        IQ1=round(rnorm(100, 100, 15))%/%5-10,
#'                        IQ2=round(rnorm(100, 100, 15))%/%5-10,
#'                        course=sample(c("BWL", "VWL", "Statistik"), size=100, replace=TRUE)
#'                       )
#' fb <- feedback(x, 'ID'=id('code'), 
#'               'IQ Test Ergebnis 1'=bar('IQ1', min=0, max=20),
#'               'IQ Test Ergebnis 2'=bar('IQ2', min=0, max=20),
#'               'Geschlecht'=grp('sex'),
#'               'Studiengang'=grp('course'))
#' summary(fb)
summary.feedback <- function(object, ...) {
  cat("ID-Code(s):", length(object$.ID), "\n")
  dup <- duplicated(object$.ID)
  if (any(dup)) {
    tab <- names(table(object$.ID)>1)
    cat("  Duplicates:", paste0(tab, collapse = ", "))
  }
  vnames <- object$.VARS
  for (vn in vnames) {
    cat(vn, '-', object[[vn]]$.CHART, ':', paste0(object[[vn]]$.RANGE, collapse=','), "\n")
    tab     <- '.'
    for (cn in object$.GROUP) {
      if (cn!='.') tab <- c(tab, paste(cn, row.names(object[[vn]][[cn]]), sep='.'))
    }
    cat(" ", paste0(tab, collapse=", "), "\n")
  }
}
