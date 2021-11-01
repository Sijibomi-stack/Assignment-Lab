dijkstra <-
function(wiki_graph, init_node){
    wiki_graph <-
      data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                 v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                 w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
    
    if(!(names(wiki_graph)[1] == "v1"&& names(wiki_graph)[2] == "v2" && names(wiki_graph)[3] =="w")){
      stop("column names of the input wiki_graph must be v1, v2 and w")
    }
    else if (!(init_node %in% unlist(wiki_graph[1], wiki_graph[2]))){
      stop("init_node must be in the input wiki_graph")
    }
    else{
      vertexSet_Q <- c()
      dist <- c()
      prev <- c()
      for (vertex in unique(unlist(list(wiki_graph$v1, wiki_graph$v2)))) {
        dist[vertex] <- Inf
        prev[vertex] <- 'NULL'
        vertexSet_Q <- append(vertexSet_Q, vertex)
      }
      names(prev) <- vertexSet_Q
      names(dist) <- vertexSet_Q
      dist[names(dist) == init_node] <- 0
      while(length(vertexSet_Q) != 0){
        vertex_u <- as.numeric(names(dist)[dist == min(unlist(dist[names(dist) %in% vertexSet_Q]))])
        vertexSet_Q <- vertexSet_Q[!(vertexSet_Q %in% vertex_u)]
        neighbors_of_u <- unique(unlist(list(wiki_graph$v2[wiki_graph$v1 %in% vertex_u], wiki_graph$v1[wiki_graph$v2 %in% vertex_u])))
        neighbors_of_u_in_Q <- neighbors_of_u[neighbors_of_u %in% vertexSet_Q]
        for (neighbor_v in neighbors_of_u_in_Q) {
          alt <- dist[names(dist) == vertex_u] + wiki_graph$w[(wiki_graph$v1 == vertex_u & wiki_graph$v2 == neighbor_v)]
          if(alt < dist[names(dist) == neighbor_v]){
            dist[names(dist) == neighbor_v] <- alt
            prev[names(dist) == neighbor_v] <- vertex_u
          }
        }
      }
      return(as.numeric(dist))
    }
  }
