#' Given two line segments, returns TRUE if they cross, FALSE if they don't
#'
#' from algorithm described at: http://stackoverflow.com/questions/7069420/check-if-two-line-segments-are-colliding-only-check-if-they-are-intersecting-n
#'
#' @param start.line.one list of coordinates c(x,y) for first point on first line
#' @param end.line.one  list of coordinates c(x,y) for second point on first line
#' @param start.line.two list of coordinates c(x,y) for first point on second line
#' @param end.line.two list of coordinates c(x,y) for second point on second line
#'
#' @return TRUE if lines cross, FALSE if lines do not
#'
#' @examples
#' do.lines.cross(start.line.one = c(0,0), end.line.one = c(1,1), 
#'		start.line.two = c(1,0), end.line.two = c(0,1))

do.lines.cross <- function (start.line.one, end.line.one, start.line.two, end.line.two) {
# Consider two lines segments [AB] and [CD]
# if points A and B are on different sides of segment [CD] AND
#	points C and D are on different sides of segment [AB]
# then line segments intersect
# Compute the cross produce for each case.  
# If they have the same sign, points on same side

# compare the first set of points to line from start.line.two to end.line.two
	temp1 <- 	(end.line.two[1] - start.line.two[1])*(start.line.one[2] - end.line.two[2]) -
				(end.line.two[2] - start.line.two[2])*(start.line.one[1] - end.line.two[1])
	temp2 <-	(end.line.two[1] - start.line.two[1])*(end.line.one[2] - end.line.two[2]) -
				(end.line.two[2] - start.line.two[2])*(end.line.one[1] - end.line.two[1])
	first.set.on.same.side <- ( ((temp1 < 0) && (temp2 < 0)) || ((temp1 > 0) && (temp2 > 0)) )

# compare the second set of points to line from start.line.one to end.line.one
	temp3 <-	(end.line.one[1] - start.line.one[1])*(start.line.two[2] - end.line.one[2]) -
				(end.line.one[2] - start.line.one[2])*(start.line.two[1] - end.line.one[1])
	temp4 <-	(end.line.one[1] - start.line.one[1])*(end.line.two[2] - end.line.one[2]) -
				(end.line.one[2] - start.line.one[2])*(end.line.two[1] - end.line.one[1])
	second.set.on.same.side <- ( ((temp3 < 0) && (temp4 < 0)) || ((temp3 > 0) && (temp4 > 0)) )
	
	if (!first.set.on.same.side && !second.set.on.same.side) 	{return(TRUE)}
	else 														{return(FALSE)}

}
