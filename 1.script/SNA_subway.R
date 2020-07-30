### SNA start (topic : featuring) 

## URL : https://kuduz.tistory.com/1087?category=834629

### 1. package ----

#install.packages('tidygraph')
#install.packages('ggraph')

library(tidygraph)
library(ggraph)

### 2. load data ----

subway <- read.csv('./0.data/subway.csv')

metro <- read.csv('./0.data/metro.csv')

### 3. data preprocessing ---- 

### 4. graph ----

subway %>% as_tbl_graph() %>%
  ggraph(layout='kk') + 
  geom_edge_link(aes(color=line)) + 
  geom_node_point(color='gray25', size=1)

### 5. centrality

# 고유벡터 중심성(eigenvector centrality) : 한 노드와 연결된 다른 노드의 중요성까지 따져서 중심성을 계산한 결과

subway %>% as_tbl_graph() %>%
  mutate(eig=centrality_eigen()) %>%
  as_tibble %>% arrange(desc(eig))


