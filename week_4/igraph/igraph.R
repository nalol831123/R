install.packages("igraph")
library(igraph)

g1 <- make_graph(c(1, 2, 2, 3, 3, 4,4,5))
plot(g1,layout=layout.circle(g1))

g2 <- graph.star(10, mode = "in")
plot(g2,layout=layout.fruchterman.reingold(g2))

