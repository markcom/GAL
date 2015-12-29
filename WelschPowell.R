#   Welsch - Powell

colorGraph.WP <- function(inputNeighbours) {
    sortedByLength <- colorGraph.sort(inputNeighbours)
    colorVector <- c(rep(0, length(neighbourList)))
    currentColor <- 1
    while (min(colorVector) == 0) {
        for(i in sortedByLength) {
            if (colorVector[i] == 0 && !(currentColor %in% colorVector[neighbourList[[i]]])){
                colorVector[i] <- currentColor
            }
        }
        currentColor <- currentColor + 1
    }
    colorVector
}
