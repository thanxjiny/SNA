###  1. statnet -----

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

# 1.3 노드 속성 ----
set.vertex.attribute(net1, "gender", c("F", "F", "M", "F", "M"))
net1 %v% "alldeg" <- degree(net1)
list.vertex.attributes(net1)

get.vertex.attribute(net1, "gender")
get.vertex.attribute(net1, "alldeg")
get.vertex.attribute(net1, "na")

# 1.4. 엣지 속성 -----
list.edge.attributes(net1)

set.edge.attribute(net1,"rndval", runif(network.size(net1),0,1)) 
 # runif: 0부터 1사이의 균일 분포
list.edge.attributes(net1)

summary(net1 %e% "rndval")

summary(get.edge.attribute(net1,"rndval"))

netval1 <- rbind(c(0,2,3,0,0),
                 c(0,0,3,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,2,0,0))
netval1 <- network(netval1,matrix.type="adjacency", ignore.eval=FALSE,names.eval="like")
network.vertex.names(netval1) <- c("A","B","C","D","E")
list.edge.attributes(netval1)

### 2. igraph ------

library(igraph)

# 2.1 네트워크 기초 ------

g1 <- graph(edges=c(1,2, 2,3, 3,1), n=3, directed=FALSE)

plot(g1)

class(g1)

g2 <- graph( edges=c(1,2, 2,3, 3,1), n=10, directed=FALSE)

plot(g2)

g3 <- graph( edges=c("John","Jim", "Jim","Jill", "Jill", "John"))
plot(g3)

g4 <- graph( c("John", "Jim", "Jim", "Jack", "Jim", "Jack", "John", "John"), 
             isolates=c("Jesse", "Janis", "Jennifer", "Justin") )  

plot(g4, edge.arrow.size=.5, vertex.color="gold", vertex.size=15, 
     vertex.frame.color="gray", vertex.label.color="black", 
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)


plot(graph_from_literal(a---b, b---c))

plot(graph_from_literal(a--+b, b+--c))

plot(graph_from_literal(a+-+b, b+-+c)) 

plot(graph_from_literal(a:b:c---c:d:e))

gl <- graph_from_literal(a-b-c-d-e-f, a-g-h-b, h-e:f:i, j)
plot(gl)

# 2.2 엣지, 노드 속성 -------

# detach(package:statnet)
suppressWarnings(suppressMessages(library(igraph)))

# 사회행렬 igraph 전환
inet1 <- graph.adjacency(netmat1)
class(inet1)

#str(inet1)

# 엣지리스트 igraph 전환
inet2 <- graph.edgelist(netmat2)
#summary(inet2)
#str(inet2)

# 노드와 엣지 속성 부여
V(inet2)$name <- c("A","B","C","D","E")
E(inet2)$val <- c(1:6)

summary(inet2)
str(inet2)

V(g4) 
 ### 노드 이름  

V(g4)$name

E(g4)
 ### 엣지 이름  

g4[]
 ### matrix

# 엣지, 노드 속성부여 방법 1
V(g4)$gender <- c("male", "male", "male", "male", "female", "female", "male")
E(g4)$type <- "email"
E(g4)$weight <- 10

vertex_attr(g4)

edge_attr(g4)

graph_attr(g4)

# 엣지, 노드 속성부여 방법 2
g4 <- set_graph_attr(g4, "name", "Email Network")
g4 <- set_graph_attr(g4, "something", "A thing")
graph_attr_names(g4)

graph_attr(g4, "name")

graph_attr(g4)

g4 <- delete_graph_attr(g4, "something")
graph_attr(g4)

# 시각화
plot(g4, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5,
     vertex.color=c( "pink", "skyblue")[1+(V(g4)$gender=="male")] )


# 간략화
g4s <- simplify( g4, remove.multiple = TRUE, remove.loops = TRUE, 
                 edge.attr.comb=c(weight="sum", type="ignore") )
plot(g4s, vertex.label.dist=1.5)

#### 3. network <-> igraph 네트워크 객체전환 -----

library(intergraph)
class(net1)
net1igraph <- asIgraph(net1)
class(net1igraph)
str(net1igraph)
