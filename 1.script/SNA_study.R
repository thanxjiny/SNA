#==========================================================================
# Topic : Social Network
#         
# Date : 2019. 05. 29
# Author : Junmo Nam
# URL : https://apple-rbox.tistory.com/12?category=1040726
#==========================================================================


#==========================================================================
# Load packages ----
#==========================================================================

pkg = c('dplyr','igraph','networkD3','visNetwork','ggplot2')

sapply(pkg,require,character.only = T)


#==========================================================================
# Make graph in igraph -----
#==========================================================================


df = data.frame(start = c('a','a','b','c','d','e','f','f','f','f'),
                end = c('a','b','c','d','e','e','a','b','c','e'))

#make network
G = graph_from_data_frame(df)
graph_from_data_frame(df,directed = F) #undirected

plot(G)
v = V(G)
attributes(v)

#shortest path 
shortest.paths(G)
  
shortest_paths(G,from = 'a',to = 'e')
 # shortest.paths : undirected
 # shortest_paths : directed

#degree : the number of nodes
degree(G) #all
degree(G,mode = 'in') #in
degree(G,mode = 'out') #out

#Power law fit or not
d.dist = degree_distribution(G, cumulative = T)
p.fit = power.law.fit(d.dist,impelementation = "plfit");p.fit

#eccentricity : 자신 이외의 노드와 거리 중 가장 큰 값 
eccentricity(G) #undirected
eccentricity(G,mode = 'in')
eccentricity(G,mode = 'out')

#diameter and radius
diameter(G) # diameter: 최대거리
radius(G) # radius : 최소거리

#centrality compare
central.df = data.frame(degree = degree(G) / (length(names(v))-1),
                        between = betweenness(G),
                        close = closeness(G,normalized = T));central.df

 # 1. degree : 노드가 다른 노드들과 얼마나 많은 관계 
 # 2. betweenness : 노드가 엣지와 엣지 사이에 많이 등장 
 # 3. closeness : 노드와 다른 노드 거리

#k core : 노드들을 그룹으로 묶었을때 최소 k개 만큼의 다른 노드와 연결관계 
coreness(G)

#LCC(local clustering coefficient) and Transitivity
trans = transitivity(G, type = "globalundirected") 
 
LCC = transitivity(G, type = "localundirected")  

#===========================
# 네트워크 이론 추가
#===========================

#Power Law
g <- static.power.law.game(500, 1000, exponent.out= 2.2, exponent.in = -1,
                           loops = FALSE, multiple = TRUE, finite.size.correction = TRUE)
V(g)

plot(g,vertex.size = 5,vertex.label = NA)


data.frame(degree = 0:max(degree(g)),p_k = degree.distribution(g)) %>% 
  ggplot(aes(degree,p_k))+
  geom_point()+
  theme_bw()+
  ylab('P(k)')+
  ggtitle('Power law fit of graph g')

small_g = data.frame(start = c('a','a','b','d'),end = c('c','b','c','a')) %>%
  graph_from_data_frame(directed = F)

plot(small_g)

#transitivity : 노드와 그 이웃간의 관계를 수치화 > 네트워크가 얼마나 조밀하게 묶여 있는지 > 노드가 네트워크 안에서 영향력


transitivity(small_g)

#transitivity = 3 * 삼각형 개수 / 3개의 노드가 연결된 경우

#clustering coefficient = 3개의 노드가 닫힌 형태로 연결됨 / 모든 연결된 경우 = transitivity

#LCC(local clustering coefficient) = 2 * 두 이웃이 연결된 경우  / 이웃 수*(이웃수-1)

V(small_g)$name

transitivity(small_g,type = 'local')
 # a : 2 *1 / 3 *(3-1)
 # b : 2 *1 / 2 *(2-1) 
 # c:  2 *1 / 2 *(2-1)
 # d : 2 *0 / 1 *(1-1)

#====================================
# network D3
#====================================

to_d3 = igraph_to_networkD3(G)

#add group
to_d3$nodes$group = 1

#d3 network
forceNetwork(Links = to_d3$links,Nodes = to_d3$nodes,
             Source = 'source', Target = 'target', 
             NodeID = 'name',Group = 'group')

#==========================================================================
# Visualize network by visNetwork
#==========================================================================

#build node and edge dataframe
nodes = data.frame(id = names(v),label = names(v))
edges = data.frame(from = df$start,to=df$end)

visNetwork(nodes = nodes, #visualize
           edges = edges,main = 'graph') 

#add arrow option
edges$arrows = 'to'
 #edges$arrows = NULL

visNetwork(nodes = nodes,edges = edges,main = 'graph with arrow') 

#set node color
nodes$color = c(rep('red',nrow(nodes)-1),'blue')

visNetwork(nodes = nodes,edges = edges,main = 'graph with arrow + color') 

#set node title
nodes$title = paste0(nodes$id,'<br/>','Node number :',1:nrow(nodes))

visNetwork(nodes = nodes,edges = edges,main = 'graph with arrow + color + title') 

#turn off physics 
visNetwork(nodes = nodes,edges = edges,
           main = 'graph with arrow + color + title + no physics') %>%
  visIgraphLayout(physics = F)

#highlight connected one
visNetwork(nodes = nodes,edges = edges,
           main = 'graph with arrow + color + title + no physics + highlight') %>%
  visIgraphLayout(physics = F) %>% 
  visOptions(highlightNearest = T)
