library(igraph)
library(dplyr)
#import Dataset
product <- read.csv(file.choose())
cpurchase <- read.csv(file.choose())

#Data Cleaning
product <- subset(product, group == 'Book' & salesrank != -1 & salesrank <= 150000)
df <- filter(cpurchase, cpurchase$Source %in% product$id & 
                          cpurchase$Target %in% product$id)

#Out-Degree Centrality for each Source (number of Source product people would buy "Target" product buy)
outDegree <- as.data.frame((table(df$Source)))

#In-Degree Centrality for each Target (number of Target product people would buy "Source" product buy)
inDegree <- as.data.frame((table(df$Target)))

#Total-Degree
Degree <- merge(inDegree, outDegree, by = 'Var1')
Degree$TotalDegree <- Degree$Freq.x + Degree$Freq.y
max(Degree$TotalDegree)

#Name of the Book for node 4429
subset(product, id == "4429")

#Find the subcomponents of the product with id == 33
net <- graph.data.frame(df, direct = T)
sub <- subcomponent(net, "4429",'ALL')

#Highest degree is Node 4429 
graph <- induced_subgraph(net, sub)
V(graph)$label <- V(graph)$name
V(graph)$degree <- degree(graph)

plot(graph,
     vertex.color = rainbow(51),
     vertex.size = V(graph)$degree*0.4,
     edge.arrow.size = 0.1,
     vertex.label.cex=0.001,
     layout = layout.graphopt)

#Closeness Centrality 
closeness.cent <- closeness(net, mode = 'all')

#Betweenness 
between <- betweenness(net, directed='T', weights=NA)

#Find the product with the highest betweenness: more control over the network, more information will pass through that node
which.max(between)
df[2501,]
subset(product, id == "28212")

#Degree Distribution
V(net)
V(net)$degree <- degree(net)

HisGraph <- hist(V(net)$degree,
     col='grey',
     main='Histogram of Degree',
     ylab='Frequency')

edge_density(net, loops = F)

