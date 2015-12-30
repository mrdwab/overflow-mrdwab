#' Turn an R expression into an SO answer
#' 
#' Parrot the input expression and append the output with knitr-style comments,
#' all as markdown.  \code{soanswer} changes the output for a single expression.
#' @param expr An R expression.
#' @return Markdown output is printed to the console, written to the clipboard 
#' and silently returned as a \code{noquote} character vector.
#' @author Richard Cotton
#' @seealso \code{\link[base]{addTaskCallback}}
#' @examples
#' # Output for various types, explicitly calling soanswer
#' soanswer(sin(2 * pi))
#' soanswer(sleep)
#' soanswer(message("A message!"))
#' soanswer(warning("A warning!"))
#' soanswer(stop("An error!"))
#' @importFrom utils capture.output
#' @export soanswer
soanswer <- function(expr)
{
  input_lines <- deparse(substitute(expr))
  output <- expr
  output_lines <- noquote(paste0("    ## ", output))
  lines <- c(input_lines, output_lines)
  cat(lines, sep = "\n")
  writeClip(lines)
  invisible(lines)
}
