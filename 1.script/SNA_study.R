### SNA start 

## URL : https://kuduz.tistory.com/1087?category=834629

### 1. package ----

install.packages('tidygraph')
install.packages('ggraph')

library(tidygraph)
library(ggraph)
library(dplyr)
library(igraph)

### 2. topic:featureing  ----

## 2.1 load data

feat <- read.csv('./0.data/featuring.csv')

## 2.2 data preprocessing 

fg <- as_tbl_graph(feat)

## 2.3 graph

plot(fg)

ggraph(fg, layout = 'kk') +  geom_node_point()  +  geom_edge_link()

feat %>%
  as_tbl_graph() %>%
  ggraph(layout='kk') + 
  geom_node_text(aes(label=name)) +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)))

### 3. topic : subway

## 3.1 load data

subway <- read.csv('./0.data/subway.csv')

metro <- read.csv('./0.data/metro.csv')

## 3.2 graph

subway %>% as_tbl_graph() %>%
  ggraph(layout='kk') + 
  geom_edge_link(aes(color=line)) + 
  geom_node_point(color='gray25', size=1)

## 3.3 centrality

# • 매개 중심성: centrality_betweenness()
# • 근접 중심성: centrality_closeness()
# • 고유벡터 중심성: centrality_eigen()
# • 페이지랭크: centrality_pagerank()
# • 연결 중심성: centrality_degree()


# 고유벡터 중심성(eigenvector centrality) : 한 노드와 연결된 다른 노드의 중요성까지 따져서 중심성을 계산한 결과

subway %>% as_tbl_graph() %>%
  mutate(eig=centrality_eigen()) %>%
  as_tibble %>% arrange(desc(eig))


metro %>% as_tbl_graph() %>%
  mutate(eig=centrality_eigen()) %>%
  as_tibble %>% 
  arrange(desc(eig))

# 가중치(weights) 적용  

metro %>% as_tbl_graph() %>%
  mutate(eig=centrality_pagerank(weights=total)) %>%
  as_tibble %>% 
  arrange(desc(eig))

### 4. topic : school ### 사람과 단체 분리  

## 4.1 sample data

school <- data.frame(사람=c('가', '나', '다', '라', '마', '바'),
                       고교=c('1', '2', '3', '1', '2', '3'),
                       대학=c('a', 'b', 'a', 'b', 'a', 'b'))

school_고교 <- school[, c(1, 2)]

names(school_고교)[2] <- '학교'

school_대학 <- school[, c(1, 3)]

names(school_대학)[2] <- '학교'

school <- rbind(school_고교, school_대학)

## 4.2 graph

school %>% 
  as_tbl_graph() %>% 
  ggraph(layout='kk') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) +
  geom_node_text(aes(label=name))

## 4.3 igraph

sg <- graph_from_data_frame(school)

bipartite_mapping(sg) ### 사람 false / 학교 true  

V(sg) ### v : vertex 꼭지점 

V(sg)$type <- bipartite_mapping(sg)$type

as_incidence_matrix(sg)

t(as_incidence_matrix(sg))

as_incidence_matrix(sg) %*% t(as_incidence_matrix(sg))

sm <- as_incidence_matrix(sg) %*% t(as_incidence_matrix(sg))
diag(sm) <- 0
sm

sm %>% as_tbl_graph() %>%
  ggraph(layout='kk') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) +
  geom_node_text(aes(label=name))

### 5. topic : kovo ### 사람과 단체 분리  

## 5.1 load data

k <- read.csv('./0.data/kovo.csv')

## 5.2 data preprocessing

k_고교 <- k[, c(1, 2)]
k_대학 <- k[, c(1, 3)]
names(k_고교)[2] <- '학교'
names(k_대학)[2] <- '학교'
k <- rbind(k_고교, k_대학)

# 5.3 centrality

k %>% as_tbl_graph() %>%
  mutate(eig=centrality_eigen()) %>%
  arrange(desc(eig)) %>%
  as_tibble

k %>% as_tbl_graph() %>%
  mutate(pr=centrality_pagerank()) %>%
  arrange(desc(pr)) %>%
  as_tibble

kg <- graph_from_data_frame(k)
V(kg)$type <- bipartite_mapping(kg)$type
km <- as_incidence_matrix(kg)
km <- km %*% t(km)
diag(km) <- 0

km %>% as_tbl_graph()

km %>% as_tbl_graph() %>%
  mutate(pg=centrality_pagerank()) %>%
  arrange(desc(pg)) %>%
  as_tibble

km %>% as_tbl_graph() %>%
  mutate(cm=group_infomap()) %>%
  arrange(desc(cm)) %>%
  as_tibble

km %>% as_tbl_graph() %>%
  mutate(pg=centrality_pagerank(),
         cm=group_infomap()) %>%
  ggraph(layout='lgl') + 
  geom_edge_link(aes(width=weight), alpha=.8) +
  scale_edge_width(range=c(0.2, 2)) +
  geom_node_point(aes(size=pg, color=as.factor(cm)))

