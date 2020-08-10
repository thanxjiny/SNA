###  1. statnet

# https://statkclee.github.io/network/ml-network-data.html

# 1.1 사회행렬(Sociomatrix) ------

library(network)

netmat1 <- rbind(c(0,1,1,0,0),
                 c(0,0,1,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,1,0,0))
rownames(netmat1) <- c("A","B","C","D","E")
colnames(netmat1) <- c("A","B","C","D","E")
net1 <- network(netmat1, matrix.type="adjacency")
class(net1)

summary(net1)

# 1.2 엣지리스트(Edgelist) -----

library(sna)

netmat2 <- rbind(c(1,2),
                 c(1,3),
                 c(2,3),
                 c(2,4),
                 c(3,2),
                 c(5,3))
net2 <- network(netmat2,matrix.type="edgelist")
network.vertex.names(net2) <- c("A","B","C","D","E")
class(net2)

summary(net2)

# 시각화

par(mfrow=c(1,2))
gplot(net1, vertex.col = 2, displaylabels = TRUE, main="사회행렬(SocioMatrix)")
gplot(net2, vertex.col = 2, displaylabels = TRUE, main="엣지리스트(Edgelist)")

# 1.3 노드 속성
set.vertex.attribute(net1, "gender", c("F", "F", "M", "F", "M"))
net1 %v% "alldeg" <- degree(net1)
list.vertex.attributes(net1)

get.vertex.attribute(net1, "gender")
get.vertex.attribute(net1, "alldeg")
get.vertex.attribute(net1, "na")



