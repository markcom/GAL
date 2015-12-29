#Give an initial ordering of vertices as {v1, v2, …vi, ……vn};
#ColorCount = 1;
#V = uncolored vertices set;
#While (V != NULL){
#    V’ = V;
#    Choose v in V’ that has the maximum number of edges to other vertices in V’;
#    color(v) = colorCount;
#    move u in V’ that are adjacent to v from V’ to U;
#    while(V’ != NULL){
#        choose v in V’ that has a maximum number of edges to vertices in U;
#        color(v) = colorCount;
#        move u in V’ that are adjacent to U from V’ to U;
#   }
#    V = U;
#    ColorCount++;
#} 

#   Recursive Largest First 

colorGraph.RLF <- function(inputNeighbours) {
    colorVector <- c(rep(0, length(inputNeighbours)))
    currentColor <- 0
    nNeighbours <- sapply(inputNeighbours, length)
    
    while (min(colorVector) == 0) {
        currentColor <- currentColor + 1
        maxDegreeUncolored <- min(which(nNeighbours == max(nNeighbours[which(colorVector == 0)])))
        colorVector[maxDegreeUncolored] <- currentColor
        nNeighbours[maxDegreeUncolored] <- -1
        nonNeighbours <- c(1:length(inputNeighbours))[-inputNeighbours[[maxDegreeUncolored]]]
        allNonColored <- c(1:length(inputNeighbours))[which(colorVector == 0)]
        nonColoredNonNeighbours <- intersect(nonNeighbours, allNonColored)
        
        while (length(nonColoredNonNeighbours) > 0) {
            maxDegreeUncolored <- nonColoredNonNeighbours[which.max(sapply(inputNeighbours[nonColoredNonNeighbours],                                                function(x) length(x[colorVector[x] == 0])))]
            colorVector[maxDegreeUncolored] <- currentColor
            nNeighbours[maxDegreeUncolored] <- -1
            if (length(inputNeighbours[[maxDegreeUncolored]]) == 0) {
                nonNeighbours <- c(1:length(inputNeighbours))
            }
            else {
                nonNeighbours <- c(1:length(inputNeighbours))[-inputNeighbours[[maxDegreeUncolored]]]
            }
            
            allNonColored <- c(1:length(inputNeighbours))[which(colorVector == 0)]
            newNCNN <- intersect(nonNeighbours, allNonColored)
            nonColoredNonNeighbours <- intersect(nonColoredNonNeighbours, newNCNN)
        }
    }
    colorVector
} 
