#' Converts a string to a number by character substitution
#'
#' Currently only supports a-zA-z and removes spaces
#'
#' @param init.string character string to be converted
#' @param chars.list list of characters to be replaced.  Default is c('a',...,'z','A',...,'Z')
#' @param num.list list of numbers to replace characters in chars.list.  Default is seq(from = 1, to = 52, by = 1)
#' @param mod.machine.int bool to return the full number (default = FALSE) or the full number modulus the integer range of the machine (set to TRUE).  %% only works for integers.  Writing a modulus for doubles.
#'
#' @return a string containing a number.  Need to use as.numeric()
#'
#' @examples
#' char2number('a')
#' char2number("Hello World")

char2number <- function (init.string, chars.list = 'NULL', num.list = 'NULL', mod.machine.int = FALSE) {
#takes a string and replaces each character with a number
#removes spaces

# default replaces a-z with 1 through 26, A-Z with 27 through 52 
if (num.list == 'NULL') {num.list <- seq(1,52,1)}
if (chars.list == 'NULL') {
	chars.list <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", 
		"l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
		"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
		"L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
}
if (!(length(num.list) == length(chars.list))) {stop("Lengths of num.list and chars.list do not match")}
#remove the space
curr.string <- gsub(pattern = " ", replacement = "", x = init.string)
#replace 
for (i in 1:length(chars.list)) {
	if (grepl(chars.list[i], curr.string)) {curr.string <- 
			gsub(pattern = chars.list[i], replacement = num.list[i], x = curr.string)
	}
}

if (mod.machine.int) 	{return(as.numeric(curr.string) %% .Machine$integer.max)}
else 					{return(as.numeric(curr.string))}

}
