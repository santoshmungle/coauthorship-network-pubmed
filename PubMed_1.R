library(igraph)
library(rentrez)

r_search <- entrez_search(db="pubmed", term="greenlight laser prostate", retmax=1000)
PubMedSummary<- entrez_summary(db="pubmed", id=r_search$ids)
authorList<- data.frame(extract_from_esummary(PubMedSummary, "authors"))
Authors <- t(authorList)
input_data <- subset(Authors, select = -c(authtype, clusterid))
row.names(input_data) <- NULL


## function that makes all pairs
pairing <- function(x) {
  n = length(x)
  if (n<2) {
    res <- NULL
  } else {
    q <- combn(n,2)
    x2 <- x[q]
    #dim(x2) <- dim(q)
    res <- x2
  }
  return(res)
}

## for each paper create all author pairs
pairing_bypaper = lapply(input_data,pairing)

## remove papers that contribute no edges
pair_noedge = sapply(pairing_bypaper,is.null)
pair2_bypaper <- pairing_bypaper[!pair_noedge]

## combine all 'subgraphs'
pair_all <- do.call('c',pair2_bypaper)

## how many authors are there?
Num_authors <- length(unique(pair_all))

## make a graph
my_graph = graph(pair_all, directed = FALSE)


## size
V(my_graph)$vertex_degree <-  degree(my_graph)
scale_factor <- 0.1

mynew_graph <- delete.vertices(my_graph, V(my_graph)[ degree(my_graph) < 90] )
V(mynew_graph)$color <- sample(colours(), length(V(mynew_graph)))
edge.start <- ends(mynew_graph, es=E(mynew_graph), names=F)[,1]
edge.col <- V(mynew_graph)$color[edge.start]

plot(mynew_graph, 
     vertex.label.cex = 1.2, 
     edge.width = E(mynew_graph)$weight, 
     vertex.size = V(mynew_graph)$vertex_degree * scale_factor,
     vertex.color= V(mynew_graph)$color,
     edge.color=edge.col,
     asp = 0.5,
     margin = -0.1
)
