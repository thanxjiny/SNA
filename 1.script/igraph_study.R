

####  R과 네트워크 분석 (igraph)
# https://apple-rbox.tistory.com/11

library(igraph)

# 1 네트워크 정의 및 네트워크화

df = data.frame(start = c('a','a','b','c','d','e','f','f','f','f'),
                end = c('a','b','c','d','e','e','a','b','c','e'))

G = graph_from_data_frame(df)

# 1.1 네트워크 방향성

graph_from_data_frame(df,directed = F) # undirected

plot(G)

# 1.2 node name(unique)

v = V(G)

attributes(v)

# 2 네트워크 속성

# 

