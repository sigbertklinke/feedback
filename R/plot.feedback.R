#' plot
#'
#' Stores your feedback data in a file \code{feedback.RDS} in the current directoy and starts a Shiny app 
#' with your feedback data. 
#' This function normally does not return; interrupt R to stop the application (usually by pressing Ctrl+C or Esc).
#' NOTE: the app will be copied from the library directory of \code{feedback} into the current directory. 
#' You may upload the \code{app.R} and \code{feedback.RDS} to your shiny server.
#'
#' @param x a feedback object
#' @param y ignored
#' @param ... ignored
#'
#' @return the result from \code{\link[shiny]{runApp}}
#' @method plot feedback
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
#' \dontrun{
#' plot(fb)
#' }
plot.feedback <- function(x, y=NULL, ...) {
  saveRDS(x, "feedback.RDS")
  app <- system.file("app", "app.R", package="feedback")
  file.copy(app, '.')
  shiny::runApp('.') 
}
