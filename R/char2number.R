char2number <- function (init.string) {
#takes a string and replaces each character with a number
#removes spaces
# replaces a-z with 1 through 26
lower.case <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", 
"l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
lower.case.num <- seq(1,26,1)
# replaces A-Z with 27 through 52
upper.case <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
"L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
upper.case.num <- seq(27,52,1)

#remove the space
curr.string <- gsub(pattern = " ", replacement = "", x = init.string)
#replace 
for (i in 1:26) {
	if (grepl(lower.case[i], curr.string)) {curr.string <- 
			gsub(pattern = lower.case[i], replacement = lower.case.num[i], x = curr.string)
	}
	if (grepl(upper.case[i], curr.string)) {curr.string <- 
			gsub(pattern = upper.case[i], replacement = upper.case.num[i], x = curr.string)
	}
}

return(as.numeric(curr.string))
}
