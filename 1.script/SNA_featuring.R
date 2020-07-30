### SNA start (topic : featuring) 

## URL : https://kuduz.tistory.com/1087?category=834629

### 1. package ----

 #install.packages('tidygraph')
 #install.packages('ggraph')

library(tidygraph)
library(ggraph)

### 2. load data ----

feat <- read.csv('./0.data/featuring.csv')

### 3. data preprocessing ---- 

fg <- as_tbl_graph(feat)

class(fg)

### 4. graph ----

plot(fg)

ggraph(fg, layout = 'kk') +  geom_node_point()  +  geom_edge_link()

feat %>%
  as_tbl_graph() %>%
  ggraph(layout='kk') + 
  geom_node_text(aes(label=name)) +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)))

### 5. centrality

# 매개 중심성(betweenness centrality) : 매개 중심성은 노드 사이 최단 경로를 가지고 계산합니다. 만약 그래프에 존재하는 노드 두 개를 최단 거리로 연결한다고 할 때 유독 특정한 노드를 거쳐야 하는 일이 많다면 이 노드가 중요하다고 보는 겁니다.

feat %>% 
  as_tbl_graph() %>% 
  mutate(bet= centrality_betweenness()) %>%
  as_tibble %>%
  arrange(desc(bet))


# 근접(closeness) 중심성 : 근접 중심성도 거리 기준입니다. 근접 중심성은 이름 그대로 어떤 노드가 중요하다면 다른 노드에서 최단 거리로 접근할 수 있을 것이라는 가정에 뿌리를 두고 있습니다.

feat %>% 
  as_tbl_graph() %>% 
  mutate(bet=centrality_betweenness(),
         clo=centrality_closeness()) %>%
  as_tibble

