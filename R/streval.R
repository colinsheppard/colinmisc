streval <- function(toeval){
	eval.parent(parse(text=toeval))
}
