library(ggplot2)
library(igraph)


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
            maxDegreeUncolored <- nonColoredNonNeighbours[which.max(sapply(inputNeighbours[nonColoredNonNeighbours], 
                                                                           function(x) length(x[colorVector[x] == 0])))]
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


#   DSATUR  unique

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


options(digits.secs=6)
set.seed(12345)

n <- 100
runs <- 100

results <- data.frame(runN = factor(),
                      nVertices = integer(), 
                      nEdges = integer(), 
                      edgesCategory = factor(), 
                      algorithm = factor(),
                      nColors = integer(),
                      runningTime = numeric(),
                      components = integer())

for (runN in 1:runs){
    for (edgesCategory in 1:4){
        
        nVertices = sample((n-.1*n):(n+.1*n),1)
        maxEdges <- (nVertices * (nVertices-1) / 2)
        nEdges <- switch(edgesCategory, nVertices*2, nVertices*5, round(maxEdges * .5), round(maxEdges * .8))

#   Create Adjacency matrix
        adMatrix <- matrix(0, ncol = nVertices, nrow = nVertices)
        i <- 0
        while (i < nEdges){
            from <- sample(1:nVertices, 1)
            to <- sample(1:nVertices, 1)
            if (adMatrix[from,to] == 0 & from != to) {
                adMatrix[from, to] <- 1
                adMatrix[to, from] <- 1
                i <- i+1
            }
        }
#   Create Adjacency matrix

#   edges
        edges <- c()
        for (i in 1:nVertices) {
            for (j in i:nVertices){
                if (adMatrix[i,j] == 1) edges <- c(edges, i , j)
            }
        }

#   Create I-Graph
        g<-graph(edges, n=max(edges), directed=FALSE)
#   edges

#   Neighbourhood from A matrix
        neighbourList <- list()
        for (i in 1:nVertices) {
            tempRow <- vector(mode="numeric", length=0)
            for (j in 1:nVertices) {
                if (adMatrix[i,j] == 1) tempRow <- c(tempRow, j)
            }
            neighbourList[[i]] <- tempRow
        }


#   Number of components
        components <- components(g)$no

#   Run Naive
        now <- Sys.time()
        nColors <- max(colorGraph.naive(neighbourList))
        result <- data.frame(runN = runN,
                             nVertices = nVertices, 
                             nEdges = nEdges, 
                             edgesCategory = edgesCategory, 
                             algorithm = "NAIVE", 
                             nColors = nColors, 
                             runningTime = as.numeric(Sys.time()-now), 
                             components = components)
        results <- rbind(results, result)

#   Run Naive Largest First
        now <- Sys.time()
        nColors <- max(colorGraph.LF(neighbourList))
        result <- data.frame(runN = runN,
                             nVertices = nVertices, 
                             nEdges = nEdges, 
                             edgesCategory = edgesCategory, 
                             algorithm = "LF", 
                             nColors = nColors, 
                             runningTime = as.numeric(Sys.time()-now), 
                             components = components)
        results <- rbind(results, result)

#   Welsch - Powell
        now <- Sys.time()
        nColors <- max(colorGraph.WP(neighbourList))
        result <- data.frame(runN = runN,
                             nVertices = nVertices, 
                             nEdges = nEdges, 
                             edgesCategory = edgesCategory, 
                             algorithm = "WP", 
                             nColors = nColors, 
                             runningTime = as.numeric(Sys.time()-now), 
                             components = components)
        results <- rbind(results, result)


#   Recursive Largest First
        now <- Sys.time()
        nColors <- max(colorGraph.RLF(neighbourList))
        result <- data.frame(runN = runN,
                             nVertices = nVertices, 
                             nEdges = nEdges, 
                             edgesCategory = edgesCategory, 
                             algorithm = "RLF", 
                             nColors = nColors, 
                             runningTime = as.numeric(Sys.time()-now), 
                             components = components)
        results <- rbind(results, result)

#   Run DSATUR
        now <- Sys.time()
        nColors <- max(colorGraph.DSATUR(neighbourList))
        result <- data.frame(runN = runN,
                             nVertices = nVertices, 
                             nEdges = nEdges, 
                             edgesCategory = edgesCategory, 
                             algorithm = "DSATUR", 
                             nColors = nColors, 
                             runningTime = as.numeric(Sys.time()-now), 
                             components = components)
        results <- rbind(results, result)
    }
}

results$edgesCategory <- factor(results$edgesCategory)

results

#levelplot(adMatrix, main="stage 12-14 array correlation matrix", xlab="", ylab="", cuts=100, at=seq(0,1,0.5))


V(g)$color <- colorGraph.DSATUR(neighbourList)
plot(g)
colorGraph.DSATUR(neighbourList)

f <- function(x) c(Min = min(x), Max = max(x), Mean = mean(x), Median = median(x))

aggregate(runningTime~algorithm+edgesCategory, data=results, sum)
aggregate(nColors~edgesCategory+algorithm, data=results, f)

summary(results$nVertices)

aggregate(nEdges~edgesCategory, data=results, f)


timeAgg <- aggregate(runningTime~algorithm+edgesCategory, data=results, sum)
timeAgg

barchart(runningTime~edgesCategory|algorithm, data=timeAgg, layout = c(5,1), xlab= "Kategorie", main= "Porovnání jednotlivých algoritmů a kategorií")

temp <- results[!duplicated(results[c(1,4)]),]
aggregate(components ~ edgesCategory, results[!duplicated(results[c(1,4)]),], function(x) sum(x==1) / runs)


table(results[!duplicated(results[1]),]$nVertices)
hist(results[!duplicated(results[1]),]$nVertices, breaks = 100, col = "blue", main = "Přehled počtu vrcholů", xlab = "Počet vrchlů")


ggplot(data=results[!duplicated(results[c(1,3)]),], aes(nEdges)) + 
    geom_histogram(aes(fill = edgesCategory), binwidth = 15) + 
    ggtitle("Frekvence počtu hran pro jednotlivé kategorie")


