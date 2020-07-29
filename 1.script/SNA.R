### SNA start

## URL : https://kuduz.tistory.com/1087?category=834629

### 1. package ----

 #install.packages('tidygraph')
 #install.packages('ggraph')

library(tidygraph)
library(ggraph)

### 2. load data

feat <- read.csv('./0.data/featuring.csv')

### 3. data preprocessing

fg <- as_tbl_graph(feat)

class(fg)

### 4. graph

plot(fg)

ggraph(fg, layout = 'kk') +  geom_node_point()  +  geom_edge_link()

feat %>%
  as_tbl_graph() %>%
  ggraph(layout='kk') + 
  geom_node_text(aes(label=name)) +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)))




