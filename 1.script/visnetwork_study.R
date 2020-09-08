# 개인별 사고건수 : node size (value)
# 보험사기 기적발여부 : node color
# 개인 / 병원 / 정비업체 : node icon
# 동일 혐의자간 사고건수 : edge size (value)
# 탑승 구분 : edge color 
# 관련 사건 : edge name (label)


#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# Topic : visnetwork
#         
# Date : 2020. 09. 03
# Author : DJ KIM
# URL : http://datastorm-open.github.io/visNetwork/
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# Load packages ----
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

#install.packages("visNetwork")

rm(list = ls())
gc(reset = T)
getwd()


pkg = c('dplyr','igraph','networkD3','visNetwork','shiny')

sapply(pkg,require,character.only = T)



#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# introduction ----
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

require(visNetwork, quietly = TRUE)
# minimal example
nodes <- data.frame(id = 1:3)
edges <- data.frame(from = c(1,2), to = c(1,3))
visNetwork(nodes, edges, width = "100%")

visDocumentation()
vignette("Introduction-to-visNetwork") # with CRAN version
# shiny example
shiny::runApp(system.file("shiny", package = "visNetwork"))

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# nodes ------
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

nodes <- data.frame(id = 1:10,
                    
                    # add labels on nodes
                    label = paste("Node", 1:10),
                    
                    # add groups on nodes 
                    group = c("GrA", "GrB"),
                    
                    # size adding value
                    value = 1:10,          
                    
                    # control shape of nodes
                    shape = c("square", "triangle", "box", "circle", "dot", "star",
                              "ellipse", "database", "text", "diamond"),
                    
                    # tooltip (html or character), when the mouse is above
                    title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),
                    
                    # color
                    color = c("darkred", "grey", "orange", "darkblue", "purple"),
                    
                    # shadow
                    shadow = c(FALSE, TRUE, FALSE, TRUE, TRUE))             

# head(nodes)
# id  label group value    shape                     title    color shadow
#  1 Node 1   GrA     1   square <p><b>1</b><br>Node !</p>  darkred  FALSE
#  2 Node 2   GrB     2 triangle <p><b>2</b><br>Node !</p>     grey   TRUE

edges <- data.frame(from = c(1,2,5,7,8,10), to = c(9,3,1,6,4,7))

visNetwork(nodes, edges, height = "500px", width = "100%")

# global configuration 
nodes <- data.frame(id = 1:4)
edges <- data.frame(from = c(2,4,3,3), to = c(1,2,4,2))

visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shape = "square", 
           color = list(background = "lightblue", 
                        border = "darkblue",
                        highlight = "yellow"),
           shadow = list(enabled = TRUE, size = 10)) %>%
  visLayout(randomSeed = 12) # to have always the same network  

# global + individual configuration
nodes <- data.frame(id = 1:4, 
                    shape = c("circle", "square"), 
                    label = LETTERS[1:4])
edges <- data.frame(from = c(2,4,3,3), to = c(1,2,4,2))

visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(color = list(background = "lightblue", 
                        border = "darkblue",
                        highlight = "yellow"),
           shadow = list(enabled = TRUE, size = 10))  %>%
  visLayout(randomSeed = 12) # to have always the same network    

# a dot per level 
nodes <- data.frame(id = 1:3, 
                    color.background = c("red", "blue", "green"),
                    color.highlight.background = c("red", NA, "red"), 
                    shadow.size = c(5, 10, 15))

edges <- data.frame(from = c(1,2), to = c(1,3),
                    label = LETTERS[1:2], 
                    font.color =c ("red", "blue"), 
                    font.size = c(10,20))

visNetwork(nodes, edges)  

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# edges ------
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

edges <- data.frame(from = sample(1:10,8), to = sample(1:10, 8),
                    
                    # add labels on edges                  
                    label = paste("Edge", 1:8),
                    
                    # length
                    length = c(100,500),
                    
                    # width
                    width = c(4,1),
                    
                    # arrows
                    arrows = c("to", "from", "middle", "middle;to"),
                    
                    # dashes
                    dashes = c(TRUE, FALSE),
                    
                    # tooltip (html or character)
                    title = paste("Edge", 1:8),
                    
                    # smooth
                    smooth = c(FALSE, TRUE),
                    
                    # shadow
                    shadow = c(FALSE, TRUE, FALSE, TRUE)) 

# head(edges)
#  from to  label length    arrows dashes  title smooth shadow
#    10  7 Edge 1    100        to   TRUE Edge 1  FALSE  FALSE
#     4 10 Edge 2    500      from  FALSE Edge 2   TRUE   TRUE

nodes <- data.frame(id = 1:10, 
                    label = paste("Node", 1:10),
                    group = c("A", "B"))
visNetwork(nodes, edges, height = "500px", width = "100%")

# global configuration

nodes <- data.frame(id = 1:4)
edges <- data.frame(from = c(2,4,3,2), to = c(1,2,4,3))

visNetwork(nodes, edges, width = "100%") %>% 
  visEdges(shadow = TRUE,
           arrows =list(to = list(enabled = TRUE, scaleFactor = 2)),
           color = list(color = "lightblue", highlight = "red")) %>%
  visLayout(randomSeed = 12) # to have always the same network        

# global + individual configuration

nodes <- data.frame(id = 1:4, label = 1:4)
edges <- data.frame(from = c(2,4,3,2), 
                    to = c(1,2,4,3), 
                    dashes = c(TRUE, FALSE))

visNetwork(nodes, edges, width = "100%") %>% 
  visEdges(shadow = TRUE,
           arrows =list(to = list(enabled = TRUE, scaleFactor = 2)),
           color = list(color = "lightblue", highlight = "red")) %>%
  visLayout(randomSeed = 12) # to have always the same network   


nodes <- data.frame(id = 1:3, 
                    color.background = c("red", "blue", "green"),
                    color.highlight.background = c("red", NA, "red"), 
                    shadow.size = c(5, 10, 15))

edges <- data.frame(from = c(1,2), to = c(1,3),
                    label = LETTERS[1:2], 
                    font.color =c ("red", "blue"), 
                    font.size = c(10,20))

visNetwork(nodes, edges) 

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# group ------
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

nodes <- data.frame(id = 1:5, group = c(rep("A", 2), rep("B", 3)))
edges <- data.frame(from = c(2,5,3,3), to = c(1,2,4,2))

visNetwork(nodes, edges, width = "100%") %>% 
  # darkblue square with shadow for group "A"
  visGroups(groupname = "A", color = "darkblue", shape = "square", 
            shadow = list(enabled = TRUE)) %>% 
  # red triangle for group "B"
  visGroups(groupname = "B", color = "red", shape = "triangle")

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# title ------
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
edges <- data.frame(from = c(1,2), to = c(2,3))

# default, on group
visNetwork(nodes, edges, main = "A really simple example", width = "100%")

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# legend ------
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

# default, on group
visNetwork(nodes, edges, width = "100%") %>%
  visGroups(groupname = "A", color = "red") %>%
  visGroups(groupname = "B", color = "lightblue") %>%
  visLegend()

visNetwork(nodes, edges, width = "100%") %>%
  visGroups(groupname = "A", color = "red") %>%
  visGroups(groupname = "B", color = "lightblue") %>%
  visLegend(width = 0.1, position = "right", main = "Group")

# nodes data.frame for legend
lnodes <- data.frame(label = c("Group A", "Group B"),
                     shape = c( "ellipse"), color = c("red", "lightblue"),
                     title = "Informations", id = 1:2)

# edges data.frame for legend
ledges <- data.frame(color = c("lightblue", "red"),
                     label = c("reverse", "depends"), arrows =c("to", "from"))

visNetwork(nodes, edges, width = "100%") %>%
  visGroups(groupname = "A", color = "red") %>%
  visGroups(groupname = "B", color = "lightblue") %>%
  visLegend(addEdges = ledges, addNodes = lnodes, useGroups = FALSE)

edges <- data.frame(color = c("lightblue", "red"),
                     label = c("reverse", "depends"), arrows =c("to", "from"))

visNetwork(nodes, edges, width = "100%") %>%
  visGroups(groupname = "A", color = "red") %>%
  visGroups(groupname = "B", color = "lightblue") %>%
  visLegend(addEdges = ledges, useGroups = TRUE) 

# + group + icon + legend
nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
edges <- data.frame(from = c(1,2), to = c(2,3))

visNetwork(nodes, edges) %>%
  visGroups(groupname = "A", shape = "icon", 
            icon = list(code = "f0c0", size = 75)) %>%
  visGroups(groupname = "B", shape = "icon", 
            icon = list(code = "f007", color = "red")) %>%
  addFontAwesome() %>%
  visLegend(addNodes = list(
    list(label = "Group", shape = "icon", 
         icon = list(code = "f0c0", size = 25)),
    list(label = "User", shape = "icon", 
         icon = list(code = "f007", size = 50, color = "red"))), 
    useGroups = FALSE)

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# image / icons------
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

# image
path_to_images <- "https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/img/indonesia/"

nodes <- data.frame(id = 1:4, 
                    shape = c("image", "circularImage"),
                    image = paste0(path_to_images, 1:4, ".png"),
                    label = "I'm an image")

edges <- data.frame(from = c(2,4,3,3), to = c(1,2,4,2))

visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)


# fontAwesome icons

nodes <- data.frame(id = 1:3, group = c("B", "A", "B"))
edges <- data.frame(from = c(1,2), to = c(2,3))

visNetwork(nodes, edges, width = "100%") %>%
  visGroups(groupname = "A", shape = "icon", 
            icon = list(code = "f0c0", size = 75)) %>%
  visGroups(groupname = "B", shape = "icon", 
            icon = list(code = "f007", color = "red")) %>%
  addFontAwesome()

# Ionicons

nodes <- data.frame(id = 1:3, shape = "icon", icon.face = 'Ionicons',
                    icon.code = c("f101", "f100", "f101"))
edges <- data.frame(from = c(1,2), to = c(2,3))

visNetwork(nodes, edges) %>%
  addIonicons()

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# options------
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

# highlightNearest
nb <- 10
nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb),
                    group = sample(LETTERS[1:3], nb, replace = TRUE), value = 1:nb,
                    title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), stringsAsFactors = FALSE)
 # label : node name
 # value : node size
 
edges <- data.frame(from = c(8,2,7,6,1,8,9,4,6,2),
                    to = c(3,7,2,7,9,1,5,3,2,9),
                    value = rnorm(nb, 10), label = paste("Edge", 1:nb),
                    title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))


 # value : edge size

visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
  visOptions(highlightNearest = TRUE) %>%
  visLayout(randomSeed = 123)

# hover options
visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
  visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T)) %>%
  visLayout(randomSeed = 123)

# + image + icons
visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
  visGroups(groupname = "A", shape = "icon", 
            icon = list(code = "f0c0", size = 75)) %>%
  visGroups(groupname = "B", shape = "icon", 
            icon = list(code = "f007", color = "red")) %>%
  visGroups(groupname = "C", shape = "icon", 
            icon = list(code = "f1b9", color = "black")) %>%
  visOptions(highlightNearest = list(enabled =TRUE, degree = 2, hover = T)) %>%
  addFontAwesome() %>%
  visLayout(randomSeed = 123)

# Collapse / Uncollapse Nodes

nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
                    group = sample(LETTERS[1:3], 15, replace = TRUE))

edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
                    to = trunc(runif(15)*(15-1))+1)

# keeping all parent node attributes  
visNetwork(nodes, edges) %>% visEdges(arrows = "to") %>%
  visOptions(collapse = TRUE)

# setting some properties with clusterOptions
visNetwork(nodes, edges) %>% visEdges(arrows = "to") %>%
  visOptions(collapse = list(enabled = TRUE, clusterOptions = list(shape = "square"))) 

# enable / disable open cluster (proxy only) : 
# visEvents(type = "off", doubleClick = "networkOpenCluster")
# visEvents(type = "on", doubleClick = "networkOpenCluster")

# Data Manipulation

visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
  visOptions(manipulation = TRUE) %>%
  visLayout(randomSeed = 123)


#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# select by node id / group ------
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

# nodesIdSelection
visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 123)

# on "authorised" column
visNetwork(nodes, edges, height = "500px", width = "100%") %>%
  visOptions(selectedBy = "group") %>%
  visLayout(randomSeed = 123)

# or new column, with multiple groups
nodes$sample <- paste(sample(LETTERS[1:3], nrow(nodes), replace = TRUE),
                      sample(LETTERS[1:3], nrow(nodes), replace = TRUE), 
                      sep = ",")
nodes$label <- nodes$sample # for see groups

visNetwork(nodes, edges, height = "500px", width = "100%") %>%
  visOptions(selectedBy = list(variable = "sample", multiple = T)) %>%
  visLayout(randomSeed = 123)


# customized options
visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
  visOptions(highlightNearest = TRUE, 
             nodesIdSelection = list(enabled = TRUE,
                                     selected = "8",
                                     values = c(5:10),
                                     style = 'width: 200px; height: 26px;
                                 background: #f8f8f8;
                                 color: darkblue;
                                 border:none;
                                 outline:none;')) %>%
  visLayout(randomSeed = 123)

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# sample “Les miserables” ------
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

nodes <- jsonlite::fromJSON("https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/nodes_miserables.json")

edges <- jsonlite::fromJSON("https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/edges_miserables.json")


visNetwork(nodes, edges, height = "700px", width = "100%") %>%
  visOptions(selectedBy = "group", 
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visPhysics(stabilization = FALSE)

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# layout
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

# hierarchical 계측적인 

nodes <- data.frame(id = 1:7)
edges <- data.frame(from = c(1,2,2,2,3,3),
                    to = c(2,3,4,5,6,7))
visNetwork(nodes, edges, width = "100%") %>% 
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout() # same as   visLayout(hierarchical = TRUE) 


visNetwork(nodes, edges, width = "100%") %>% 
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout(direction = "LR", levelSeparation = 500)

nodes <- data.frame(id = 1:4, level = c(2, 1, 1, 1))
edges <- data.frame(from = c(1, 1, 1),
                    to = c(2,3,4))
### with level
visNetwork(nodes, edges, width = "100%") %>% 
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout() # same as   visLayout(hierarchical = TRUE) 

### without level (vis.js choice)
nodes$level <- NULL
visNetwork(nodes, edges, width = "100%") %>% 
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout() # same as   visLayout(hierarchical = TRUE) 

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# sample ----
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

# Collapse / Uncollapse Nodes

nb <- 10
nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb))
# label : node name
# value : node size

edges <- data.frame(from = c(8,2,7,6,1,8,9,4,6,2),
                    to = c(3,7,2,7,9,1,5,3,2,9),
                    label = paste(1:nb),
                    color = c("red", "red","blue","blue","blue"))
                    
# keeping all parent node attributes  
visNetwork(nodes, edges) %>% visEdges(arrows = "to") %>%
  visOptions(collapse = TRUE) %>% 
  visNodes(physics = F) %>%    
  visOptions(manipulation = TRUE)

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# layout ----
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

# set seed

nodes <- data.frame(id = 1:4)
edges <- data.frame(from = c(2,4,3,3), to = c(1,2,4,2))

visNetwork(nodes, edges, width = "100%") %>% 
  visLayout(randomSeed = 12) # to have always the same network 

# Hierarchical Layout

nodes <- data.frame(id = 1:7)
edges <- data.frame(from = c(1,2,2,2,3,3),
                    to = c(2,3,4,5,6,7))

visNetwork(nodes, edges, width = "100%") %>% 
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout() # same as visLayout(hierarchical = TRUE) 

visNetwork(nodes, edges, width = "100%") %>% 
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout(direction = "LR", levelSeparation = 500)

## levelSeparation, the distance between the different levels.
## direction, the direction of the hierarchical layout.
## sortMethod, the algorithm used to ascertain the levels of the nodes based on the data

# with level

nodes <- data.frame(id = 1:4, level = c(2, 1, 1, 1))
edges <- data.frame(from = c(1, 1, 1),
                    to = c(2,3,4))

visNetwork(nodes, edges, width = "100%") %>% 
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout() # same as   visLayout(hierarchical = TRUE) 

nodes$level <- NULL

visNetwork(nodes, edges, width = "100%") %>% 
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout() # same as   visLayout(hierarchical = TRUE) 

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# igraph ----
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

nnodes <- 100
nnedges <- 200

nodes <- data.frame(id = 1:nnodes)
edges <- data.frame(from = sample(1:nnodes, nnedges, replace = T),
                    to = sample(1:nnodes, nnedges, replace = T))

# with defaut layout
visNetwork(nodes, edges, height = "500px") %>%
  visIgraphLayout() %>%
  visNodes(size = 10)

visNetwork(nodes, edges, height = "500px") %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visNodes(size = 10) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T)

library("igraph", quietly = TRUE, warn.conflicts = FALSE, verbose = FALSE)
igraph_network <- graph.famous("Walther")

plot(igraph_network)

# get data and plot :
data <- toVisNetworkData(igraph_network)
visNetwork(nodes = data$nodes, edges = data$edges, height = "500px")

# or plot directly
visIgraph(igraph_network)

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# performance ----
#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

visNetwork(nodes, edges) %>%
  visPhysics(stabilization = FALSE)

visNetwork(nodes, edges) %>%
  visEdges(smooth = FALSE)

visNetwork(nodes, edges) %>%
  visIgraphLayout()
