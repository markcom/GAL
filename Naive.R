#   Naive Algorithm

colorGraph.naive <- function(inputNeighbours) {
    colorVector <- c(rep(0, length(neighbourList)))
    for(i in 1:length(inputNeighbours)) {
        minColor <- 1
        while(minColor %in% colorVector[neighbourList[[i]]]){
            minColor <- minColor + 1
        }
        colorVector[i] <- minColor
    }
    colorVector
} 
