#Give an initial ordering of vertices as V = {v1, v2, …vi, ……vn};
#Find a largest clique V’ of G, assign each vertex in V’ a distinct color class;
#V = V-V’;
#While(V != NULL){
#    Find a vertex v in V, which is adjacent to the largest number of distinctly
#    colored vertices, assign v to the lowest indexed color class that contains no
#    vertices adjacent to v;
#    If(no existing color class to assign to) create a new color class for v;
#    Move v out of V;
#}
#return color classes. 

#   DSATUR

colorGraph.DSATUR <- function(inputNeighbours) {
    remainingVertives <- c(1:length(inputNeighbours))
    colorVector <- c(rep(0, length(inputNeighbours)))
    currentColor <- 1
    nNeighbours <- sapply(inputNeighbours, length)
    selectedVertice <- min(which(nNeighbours == max(nNeighbours)))
    colorVector[selectedVertice] <- currentColor
    remainingVertives <- remainingVertives[-which(remainingVertives == selectedVertice)]
    
    while (min(colorVector) == 0) {
        maxNColors <- 0
        selectedVertice <- 0
        
        for (i in remainingVertives) {
            if (selectedVertice == 0) {
                selectedVertice <- i
                maxNColors <- length(unique(colorVector[inputNeighbours[[i]]]))
            }
            else if (maxNColors == length(unique(colorVector[inputNeighbours[[i]]]))){
                if (nNeighbours[i] > nNeighbours[selectedVertice])  {
                    selectedVertice <- i
                    maxNColors <- length(unique(colorVector[inputNeighbours[[i]]]))
                }
                else if ((nNeighbours[i] == nNeighbours[selectedVertice]) && sample(c(TRUE,FALSE), 1)) {
                    selectedVertice <- i
                    maxNColors <- length(unique(colorVector[inputNeighbours[[i]]]))
                }
            }
            else if (maxNColors < length(unique(colorVector[inputNeighbours[[i]]]))){
                selectedVertice <- i
                maxNColors <- length(unique(colorVector[inputNeighbours[[i]]]))
            }
        }
        
        i <-  0
        while (colorVector[selectedVertice] == 0){
            i <- i + 1
            if (!any(colorVector[inputNeighbours[[selectedVertice]]] == i)) {
                colorVector[selectedVertice] <- i
            }
        }
        remainingVertives <- remainingVertives[-which(remainingVertives == selectedVertice)]
    }
    colorVector
} 
