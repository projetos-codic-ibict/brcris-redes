
x <- jsonlite::fromJSON(Search(conn = conection, index="pesqdf-publication",q="cannabis",size=200,raw=TRUE))

total_pesq <- x$hits$total$value

x <- x$hits$hits
#View(x)

names(x) <- gsub("_", "", names(x))

# Unnest the 'source' column
x_unnested <- x %>%
  unnest(source, names_sep = "_")

# Unnest other columns containing lists
x_unnested <- x_unnested %>%
  unnest(source_author, names_sep="_")

#View(x_unnested)

x_unnested <- x_unnested %>% rename(id_author = source_author_id, name = source_author_name, publicationDate = source_publicationDate,
                      id2 = id)


df_final <- select(x_unnested, "id_author","name","publicationDate","type","id2","source_type")

#View(df_final)

node_size <- df_final %>% group_by(id_author) %>% summarise(node_size=n())


node_type <- unique(select(df_final, id2, source_type))
#node_type$source_type <- unlist(node_type$source_type)

for (i in 1:nrow(node_type)) {
  
  node_type$source_type[i] <- node_type$source_type[i][[1]][[1]]
  
  
}

node_type$source_type <- unlist(node_type$source_type)


# Extract unique id_author and id2 values
unique_id_author <- unique(unlist(df_final$id_author))
unique_id2 <- unique(unlist(df_final$id2))

unique_name <- unique(select(df_final, name, id_author))
unique_name$name <- unlist(unique_name$name)





# Create nodes data frame
nodes <- data.frame(id = c(unique_id_author, unique_id2),
                    group = c(rep("id_author", length(unique_id_author)), rep("id2", length(unique_id2))),
                    stringsAsFactors = FALSE)

  #View(nodes)


nodes <- left_join(nodes, unique_name, by = c("id" = "id_author"))
nodes <- left_join(nodes, node_size, by = c("id" = "id_author"))
nodes <- left_join(nodes, node_type, by = c("id" = "id2"))

nodes <- nodes %>% replace_na(list(source_type='author'))
View(nodes)


edges <- data.frame(from = match(df_final$id_author, nodes$id) - 1,
                    to = match(df_final$id2, nodes$id) - 1,
                    stringsAsFactors = FALSE)

# Add relationships between multiple id_author and id2
multiple_edges <- table(df_final$id2) > 1
if (any(multiple_edges)) {
  multiple_id2 <- names(multiple_edges)[multiple_edges]
  for (id2 in multiple_id2) {
    id_author <- df_final$id_author[df_final$id2 == id2]
    for (i in 1:(length(id_author) - 1)) {
      edges <- rbind(edges, data.frame(from = match(id_author[i], nodes$id) - 1,
                                       to = match(id_author[i + 1], nodes$id) - 1),
                     stringsAsFactors = FALSE)
    }
  }
}



# Create the network
network <- forceNetwork(Links = edges, Nodes = nodes,
                        Source = "from", Target = "to",
                        NodeID = "name", Group = "source_type",
                        opacity = 0.8,zoom = T, Nodesize = "node_size", legend = TRUE)


network
