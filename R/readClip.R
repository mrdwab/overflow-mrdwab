#' Read clipboard regardless of OS
#' 
#' Different operating systems have different ways of handling the clipboard. 
#' Given the frequency with which text is copied to the clipboard to place in
#' an answer on StackOverflow, this utility is provided.
#' 
#' @return character string containing text on the clipboard.
#' 

writeClip <- function(object){
  OS <- Sys.info()["sysname"]
  
  if(!(OS %in% c("Darwin", "Windows", "Linux"))) stop("Copying to clipboard not yet supported on your OS")
  
  switch(OS,
         "Darwin"={con <- pipe("pbcopy", "w")
         writeLines(object, con=con)
         close(con)},
         "Windows"=writeClipboard(object, format = 1),
         "Linux"={if(Sys.which("xclip") == "") warning("Clipboard on Linux requires 'xclip'. Try using:\nsudo apt-get install xclip")
           con <- pipe("xclip -selection clipboard -i", open="w")
           writeLines(object, con=con)
           close(con)})
}
