#   Sort graph -> vector

colorGraph.sort <- function(inputNeighbours) {
    sortedByLength <- c(rep(0, length(neighbourList)))
    nOfNeighbours <- sapply(neighbourList, length)
    for (i in 1:length(inputNeighbours)) {
        iMax <- which.max(nOfNeighbours)
        sortedByLength[i] <- iMax
        nOfNeighbours[iMax] <- -1
    }
    sortedByLength
} 


#   Largest First

colorGraph.LF <- function(inputNeighbours) {
    sortedByLength <- colorGraph.sort(inputNeighbours)
    colorVector <- c(rep(0, length(neighbourList)))
    for(i in sortedByLength) {
        minColor <- 1
        while(minColor %in% colorVector[neighbourList[[i]]]){
            minColor <- minColor + 1
        }
        colorVector[i] <- minColor
    }
    colorVector
} 
