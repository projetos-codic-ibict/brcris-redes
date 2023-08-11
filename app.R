library("shiny")
library("shinyWidgets")
library("elastic")
library("networkD3")
library("dplyr")
library("tidyr")

conection <<- connect("http://172.16.16.90",port="9200",user="elastic",pwd = "codic2022")

ui <- fluidPage(


    mainPanel(column(12,
      textInput("queryInput", "Query:"),
      sliderInput("n_observacoes","N de observacoes",1,1000,value = "500"),
      actionButton("runButton", "Run"),
      networkD3::forceNetworkOutput("network"),
      wellPanel(textOutput("total_pub"),textOutput("total_pesq")),align='center')
    )

)

server <- function(input, output) {
  
  # Observa evento de clique do usuário para buscar
  observeEvent(input$runButton, {
    
    # Cria string de query reativa
    query_reactive <- reactive({
      
      query <- isolate(input$queryInput)
      return(query)
      
          })
    
    n_observacoes_reactive <- reactive({
      
      x <- isolate(input$n_observacoes)
      return(x)
      
    })
    
    # Cria e manipula dataframe de resultado da query
    df_final_reactive <- reactive({
    
    # Importa em formato JSON do elasticsearchr no index de 'pesqdf-publication'
    x <- jsonlite::fromJSON(Search(conn = conection, index="pesqdf-publication",q=query_reactive(),size=n_observacoes_reactive(),raw=TRUE))
    total_pub <- x$hits$total$value
    x <- x$hits$hits
    
    # Renomeia colunas com caractere especial '_'
    names(x) <- gsub("_", "", names(x))
    
    # Unnest na coluna 'source'
    x_unnested <- x %>%
      unnest(source, names_sep = "_")
    
    # Unnest em outras colunas contendo lista
    x_unnested <- x_unnested %>%
      unnest(source_author, names_sep="_")
    
    # Renomeia colunas para manipulacao
    x_unnested <- x_unnested %>% rename(id_author = source_author_id, name = source_author_name, publicationDate = source_publicationDate,
                                        id2 = id)
    
    x_unnested$total_pub <- total_pub
    
    # Seleciona apenas variaveis importantes
    df_final <- select(x_unnested, "id_author","name","publicationDate","type","id2","source_type","total_pub")
    
    return(df_final)
    
    })
    
    nodes_reactive <- reactive({
    
    # Extração de valores únicos para id_author e id2
    unique_id_author <- unique(unlist(df_final_reactive()$id_author))
    unique_id2 <- unique(unlist(df_final_reactive()$id2))
    
    unique_name <- unique(select(df_final_reactive(), name, id_author))
    unique_name$name <- unlist(unique_name$name)
    
    node_size <- df_final_reactive() %>% group_by(id_author) %>% summarise(node_size=n())
    
    node_type <- unique(select(df_final_reactive(), id2, source_type))
    #node_type$source_type <- unlist(node_type$source_type)
    
    for (i in 1:nrow(node_type)) {
      
      node_type$source_type[i] <- node_type$source_type[i][[1]][[1]]
      
      
    }
    
    node_type$source_type <- unlist(node_type$source_type)
    
    
    # Cria o dataframe de nodes
    nodes <- data.frame(id = c(unique_id_author, unique_id2),
                        group = c(rep("id_author", length(unique_id_author)), rep("id2", length(unique_id2))),
                        stringsAsFactors = FALSE)
    
    
    nodes <- left_join(nodes, unique_name, by = c("id" = "id_author"))
    nodes <- left_join(nodes, node_size, by = c("id" = "id_author"))
    nodes <- left_join(nodes, node_type, by = c("id" = "id2"))
    nodes <- nodes %>% replace_na(list(source_type='author'))
    
    
    
    return(nodes)
    
    })
    
    edges_reactive <- reactive({
    
    # Cria o dataframe de edges 'zero-indexed' (indexados por zero)
    edges <- data.frame(from = match(df_final_reactive()$id_author, nodes_reactive()$id) - 1,
                        to = match(df_final_reactive()$id2, nodes_reactive()$id) - 1,
                        stringsAsFactors = FALSE)
    
    # Adiciona múltiplas relações entre id_author e id2
    multiple_edges <- table(df_final_reactive()$id2) > 1
    if (any(multiple_edges)) {
      multiple_id2 <- names(multiple_edges)[multiple_edges]
      for (id2 in multiple_id2) {
        id_author <- df_final_reactive()$id_author[df_final_reactive()$id2 == id2]
        for (i in 1:(length(id_author) - 1)) {
          edges <- rbind(edges, data.frame(from = match(id_author[i], nodes_reactive()$id) - 1,
                                           to = match(id_author[i + 1], nodes_reactive()$id) - 1),
                         stringsAsFactors = FALSE)
        }
      }
    }
    
    return(edges)
    
    })
    
    

    
    
    ColourScale <- 'd3.scaleOrdinal()
            .domain(["master thesis", "doctoral thesis","journal article","conference proceedings","author"])
           .range(["#FF6900", "#694489","red","yellow","blue"]);'
    
    output$network <- renderForceNetwork({
      
      # Cria a rede
      network <- forceNetwork(Links = edges_reactive(), Nodes = nodes_reactive(),
                              Source = "from", Target = "to",
                              NodeID = "name", Group = "source_type", Nodesize = "node_size",
                              opacity = 0.8,zoom = T,legend = TRUE, colourScale = ColourScale )
      
      
      # Adiciona links ao dataframe a partir do campo 'id' do df NODES
      network$x$nodes$hyperlink <- paste0(
        'http://codich1.ibict.br:8081/vivo/individual?uri=https://brcris.ibict.br/individual/pers_',
        nodes_reactive()$id
      )
      

      #   Define uma click action para abrir o hyperlink para cada nó em uma nova janela
      network$x$options$clickAction = 'window.open(d.hyperlink)'
      
      network

    })
    
    

    
    output$total_pub <- renderText({
      
      paste(df_final_reactive()$total_pub[1],"publicações.")
      
    })
    
    output$total_pesq <- renderText({
      
      paste(length(unique(df_final_reactive()$id_author)),"pesquisadores.")
      
    })
    
    
  })
}

shinyApp(ui, server)
