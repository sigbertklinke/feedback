#' id
#'
#' Sets the name of ID variable in the data frame.
#'
#' @param name character: name of ID variable in data frame
#'
#' @return a list
#' @export
#'
#' @examples
#' id('test')
id  <- function(name) { 
  list(type="id", name=as.character(substitute(name))) 
}

#' grp 
#'
#' Sets a name of group variable in the data frame.
#'
#' @param name character: name of group variable in data frame
#'
#' @return a list
#' @export
#'
#' @examples
#' id('test')
grp <- function(name) { 
  list(type="subgroup", name=as.character(substitute(name)))  
}

#' bar
#'
#' Sets the name of a variable in the data frame for displaying in a barchart.
#'
#' @param name character: name of variable in the data frame to display in a barchart
#' @param min numeric: minimal value (default: \code{0})
#' @param max numeric: maximal value (default: \code{NA})
#'
#' @return a list
#' @export
#'
#' @examples
#' bar('test')
bar <- function(name, min=0, max=NA) { 
  list(type="barchart", name=as.character(substitute(name)), range=c(min,max)) 
}
