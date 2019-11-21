#' feedback
#'
#' Creates a feedback object
#'
#' @param x data frame: 
#' @param ... unused
#'
#' @return a feedback object
#' @export
feedback <- function(x, ...) { UseMethod("feedback") }

#' @param n numeric: minimal subgroup size (default: \code{n=30})
#' @param ignore.case logical: should case differences in id variables be ignored  (default: \code{TRUE})
#'
#' @rdname feedback
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
#' str(fb)
feedback.default <-  function (x, n=30, ignore.case=TRUE, ...) {
  if (!is.data.frame(x)) stop("Expected data frame in 'x'")
  ret   <- list(.VERSION='0.3')
  args  <- list(...)
  nargs <- names(args)
  # collect IDs
  for(arg in nargs) {
    argi <- args[[arg]]
    if (is.null(argi$name)) stop(sprintf("'%s' does not exist", argi$name))
    if (argi$type=="id") {
      if (!is.null(ret$.ID)) warning("'id' is not unique")
      ret$.ID   <- if (ignore.case) toupper(x[[argi$name]]) else x[[argi$name]]
      ret$.CODE <- sort(unique(ret$.ID)) 
    }
  }
  if (any(duplicated(ret$.ID))) {
    tab <- names(table(ret$.ID)>1)
    warning("Duplicate entries in 'id': ", paste0(tab, collapse = ", "))
  }
  # collect variables
  vars  <- c()
  #browser()
  for(arg in nargs) {
    argi <- args[[arg]]
    if (argi$type=="barchart") {
      if (is.null(x[[argi$name]])) stop(sprintf("Variable '%s' does not exist", argi$name))
      if (!is.factor(x[[argi$name]])) {
        range <- argi$range
        if (is.na(range[1])) range[1] <- min(x[[argi$name]], na.rm=TRUE)
        if (is.na(range[2])) range[2] <- max(x[[argi$name]], na.rm=TRUE)
        score <- factor(x[[argi$name]], levels=range[1]:range[2])
      } else {
        score <- x[[argi$name]]
      }
      ret[[arg]] <- list(.SCORE=score, .RANGE=range, .CHART='barchart',
                         .     =prop.table(table(factor(x[[argi$name]], levels=range[1]:range[2]))))
      vars <- c(vars, arg)
    }
  }
  ret$.VARS <- vars
  # collect subgroups
  #browser()
  grp <- '.'
  for(arg in nargs) {
    argi <- args[[arg]]
    if (argi$type=="subgroup") {
      if (is.null(x[[argi$name]])) stop(sprintf("Variable '%s' does not exist", argi$name))
      if (startsWith(arg, '.')) stop("names can not start with a dot")
      vari <- x[[argi$name]]
      tabi <- table(vari)
      if (any(tabi<n)) {
        ntai <- names(tabi[tabi<n])
        warning("Less than ", n, " observations per group: ", paste0(ntai, collapse = ", "))
        for (val in ntai) vari[vari==val] <- NA
      } 
      #browser()
      for (var in vars) {
        ret[[var]][[arg]] <- prop.table(table(vari, factor(ret[[var]]$.SCORE, levels=ret[[var]]$.RANGE[1]:ret[[var]]$.RANGE[2])), 1) 
      }
      grp <- c(grp, arg)
    }
  }
  ret$.GROUP <- grp
  class(ret) <- c('feedback', class(ret))
  ret
}