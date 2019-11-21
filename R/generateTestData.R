#' generateTestData
#'
#' Generates randomly test data set(s) of size \code{n}. For a detailed application see the vignette.
#'
#' @details The generated data set(s) contains at least eight variables:
#' \describe{
#' \item{\code{sex}}{sex of respondent (\code{M} male, \code{F} female)}
#' \item{\code{firstname}}{first name of respondent}
#' \item{\code{mothername}}{first name of respondents mother}
#' \item{\code{lastname}}{last name of respondent}
#' \item{\code{email}}{e-mail address of respondent}
#' \item{\code{birthplace}}{birthplace of respondent}
#' \item{\code{birthday}}{birthday of respondent}
#' \item{\code{code}}{code generated from respondents data: 2nd and 3rd letter of mothers first name, day of birth, 3rd and 4th letter of birth city and 1st letter of first name}
#' \item{\code{t}}{character at which time points the observations is included} 
#' }
#'
#' The basic data files for the generation can be found in the \code{extdata} directory of the package
#' \describe{
#' \item{\code{firstname_female.txt}}{taken from \href{https://de.wiktionary.org/wiki/Verzeichnis:International/Weibliche_Vornamen}{Wiktionary}}
#' \item{\code{firstname_male.txt}}{taken from \href{https://de.wiktionary.org/wiki/Verzeichnis:International/M\%C3\%A4nnliche_Vornamen}{Wiktionary}}
#' \item{\code{lastname.txt}}{taken from \href{http://wiki-de.genealogy.net/Die_1000_h\%C3\%A4ufigsten_Familiennamen_in_Deutschland}{genealogy.net Wiki}}
#' \item{\code{email_provider.txt}}{taken from \href{https://gist.github.com/tbrianjones/5992856}{T. Brian Jones \code{free_email_provider_domains.txt}}}
#' \item{\code{staedte.txt}}{taken from \href{https://de.wikipedia.org/wiki/Liste_der_St\%C3\%A4dte_in_Deutschland}{Wikipedia list of cities and towns in Germany}}
#' }
#'
#' The further named parameters are either functions of the form \code{FUN(n)} which generates \code{n} values or 
#' a vector of values from which is sampled by \code{sample(v, size=n, replace=TRUE)}. 
#' You may overwrite the internal functions (\code{sex}, ...) to generate your own values.  
#'
#' @param n number of observations for the test data (default: 500)
#' @param ... further \strong{named} (artificial) variables to add to the data set
#'
#' @return a data frame
#' @importFrom rio import
#' @export
#'
#' @examples
#' # create a single data set
#' d <- generateTestData(25)
#' str(d)
#' # create a single data set with an additional 'points' variable
#' d <- generateTestData(25, points=function(n) { sample(20, size=n, replace=TRUE)} )
#' str(d)
#' # generate panel data with two time points:
#' # 20 obs only in t1, 
#' # 10 in t1 and t2 and 
#' # 15 only in t2
#' n <- list(c(20, 1), c(10, 1, 2), c(15, 2))
#' d <- generateTestData(n)
#' str(d)
#' # generate panel data with three time points:
#' # 6 obs only in t1, 
#' # 5 in t1 and t2, 
#' # 8 in t1 and t3, 
#' # 7 in t1, t2 and t3
#' # 4 only in t2
#' # 3 in t2 and t3
#' # 2 only in t3
#' n <- list(c(6, 1), c(5, 1, 2), c(8, 1, 3), c(7, 1, 2, 3), c(4, 2), c(3, 2, 3), c(2, 3))
#' d <- generateTestData(n)
#' str(d)
generateTestData <- function(n=500, ...) {
  userfun <- function (f, n) {
    if (is.function(f)) return(f(n))
    f[sample(1:length(f), size=n, replace=TRUE)]
  }
  #
  nolist <- !is.list(n)
  if (nolist) {
    n <- as.list(n)
    for (i in 1:length(n)) n[[i]] <- c(n[[i]], i)
  }
  nd <- 1
  for (i in 1:length(n)) nd <- max(nd, n[[i]][-1])  
  data  <- vector('list', nd) 
  args  <- list(...)
  nargs <- setdiff(names(args), c("sex", "firstname", "lastname", "mothername", "email", "birthplace", "birthday", "code"))
  for (i in 1:length(n)) {
    if (length(n[[i]])>1) {
      nij <- n[[i]][1]
      if (nij<1) stop('Negative number of observations are impossible')
      dl <- list()
      if (is.null(args$sex)) {
        dl$sex <- sample(factor(c('M', 'F')), size=nij, replace=TRUE)
      } else {
        dl$sex <- userfun(args$sex, nij)
      }
      if (is.null(args$firstname)) {
        fnf  <- rio::import(system.file("extdata", "firstname_female.txt", package="findMatch2"))
        fnf  <- stringi::stri_trans_general(fnf[,1], "latin-ascii")
        fnm  <- rio::import(system.file("extdata", "firstname_male.txt", package="findMatch2"))
        fnm  <- stringi::stri_trans_general(fnm[,1], "latin-ascii")
        dl$firstname <- ifelse(as.numeric(dl$sex)==1, sample(fnf, size=nij, replace=TRUE),
                               sample(fnm, size=nij, replace=TRUE))
      } else {
        dl$firstname <- userfun(args$firstname, nij)
      }
      if (is.null(args$mothername)) {
        fnf  <- rio::import(system.file("extdata", "firstname_female.txt", package="findMatch2"))
        fnf  <- stringi::stri_trans_general(fnf[,1], "latin-ascii")
        dl$mothername <- sample(fnf, size=nij, replace=TRUE)
      } else {
        dl$mothername <- userfun(args$mothername, nij)
      }
      if (is.null(args$lastname)) {
        lns  <- rio::import(system.file("extdata", "lastname.txt", package="findMatch2"))
        lns  <- stringi::stri_trans_general(lns[,1], "latin-ascii")
        dl$lastname <- sample(lns, size=nij, replace=TRUE)
      } else {
        dl$lastname <- userfun(args$lastname, nij)
      }
      if (is.null(args$email)) {
        eml  <- rio::import(system.file("extdata", "email_provider.txt", package="findMatch2"))
        eml  <- stringi::stri_trans_general(eml[,1], "latin-ascii")
        dl$email <- paste0(dl$firstname, '.', dl$lastname, '@', sample(eml, size=nij, replace=TRUE))
      } else {
        dl$email <- userfun(args$email, nij)
      }
      if (is.null(args$birthplace)) {
        city <- trimws(readLines(system.file("extdata", "staedte.txt", package="findMatch2")))
        city <- stringi::stri_trans_general(city, "latin-ascii")
        dl$birthplace <- sample(city, size=nij, replace=TRUE)
      } else {
        dl$birthplace <- userfun(args$birthplace, nij)
      }
      if (is.null(args$birthday)) {
        dl$birthday <- sample(seq(as.Date('1995/01/01'), as.Date('2000/01/01'), by="day"), size=nij, replace=TRUE)
      } else {
        dl$birthday <- userfun(args$birthday, nij)
      }
      if (is.null(args$code)) {
        dl$code <- paste0(substring(dl$mothername, 2,3),
                          format(dl$birthday, "%0d"),
                          substring(dl$birthplace, 3,4),
                          substring(dl$firstname, 1, 1))
      } else {
        if (is.function(args$code)) dl$code <- args$code(nij)
      }
      if (length(nargs)) {
        for (k in 1:length(nargs)) {
          dl[[nargs[k]]] <- userfun(args[[nargs[k]]], nij) 
        }
      }

      ind  <- n[[i]][-1]
      dl$t <- rep(paste0(ind, collapse="-"), nij)
      df   <- data.frame(dl, stringsAsFactors = FALSE)
      for (k in ind) data[[k]] <- rbind(data[[k]], df)  
    }
  }
  if(nolist) return(data[[1]])
  data
}