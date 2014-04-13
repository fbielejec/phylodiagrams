################
#---PACKAGES---#
################
require(arcdiagram)
# require(ape)
source("annotatedTreeReader.R")

tree = read.annotated.nexus(file = "data/noTimeMCC.tree")
colnames(tree$edge) <- c("ancestor", "node")

###################
#---ARC DIAGRAM---#
###################
states <- c()
for(i in 1 : length(tree$annotations)) {
  states[i] = tree$annotations[[i]]$location.states
}

edgeList = matrix(NA, nrow = dim(tree$edge)[1], ncol = 2, dimnames = list(NULL, c("node.state", "ancestor.state")))

k = 1
for(i in 1 : dim(tree$edge)[1]) {
  if(tree$edge[i, 1] != 0) {
    
    edgeList[k, 1] = strsplit(states[i], "\"")[[1]][2] # node.state
    edgeList[k, 2] = strsplit(states[tree$edge[i, 1]], "\"")[[1]][2] # ancestor.state
    k = k + 1
    
  }
}

head(edgeList)
# cbind(tree$edge, edgeList, tree$edge.length)

arcplot(edgeList, 
        #         labels = vlabels, 
        #         ordering = order,
        cex.labels = 1.2, 
        show.nodes = TRUE,
        #         col.nodes = vborders, 
        #         bg.nodes = vfill, 
        #         cex.nodes = log(degrees) + 0.5, 
        pch.nodes = 21, 
        lwd.nodes = 2, 
        line = -0.5, 
        col.arcs = hsv(0, 0, 0.2, 0.25),
        #         horizontal = FALSE,
        lwd.arcs = 50 * tree$edge.length
)

# make graph from edgelist
graph = graph.edgelist(edgeList, directed = TRUE)



