# Necessary libraries
library(readxl)
library(igraph)
library(networkD3)

# Function to read data from an Excel file
read_pokemon_data <- function(file_path) {
  data <- read_xlsx(file_path)
  names(data) <- make.names(names(data))
  return(data)
}

# Function to create a graph from the data
create_graph <- function(data) {
  # Check that these columns exist
  required_columns <- c("Name", "HP", "Attack", "Defense", "Sp.Atk", "Sp.Def", "Speed", "Type.1")
  if (!all(required_columns %in% colnames(data))) {
    stop("Columns 'Name', 'HP', 'Attack', 'Defense', 'Sp.Atk', 'Sp.Def', 'Speed', 'Type.1' must be present in the data.")
  }
  
  # Calculate the sum of battle stats for each Pokémon
  data$Strength <- rowSums(data[, c("HP", "Attack", "Defense", "Sp.Atk", "Sp.Def", "Speed")])
  
  # Create a complete graph where each Pokémon is a node and the edges represent the difference in 'Strength'
  edges <- data.frame(
    from = rep(data$Name, each = nrow(data)),
    to = rep(data$Name, times = nrow(data)),
    weight = as.vector(outer(data$Strength, data$Strength, FUN = function(x, y) abs(x - y)))
  )
  
  # Remove edges that connect a node to itself
  edges <- edges[edges$from != edges$to, ]
  
  graph <- graph_from_data_frame(d = edges, vertices = data.frame(name = data$Name), directed = FALSE)
  
  return(graph)
}

# Function to select 6 Pokémon using the graph
select_pokemon_team <- function(data, graph, num_pokemon = 6) {
  # Use the selection algorithm to choose the Pokémon
  # In this case, select nodes with the highest degree of connection
  degrees <- degree(graph)
  data$degree <- degrees[V(graph)$name]
  
  # Sort Pokémon by degree of connection
  data <- data[order(-data$degree), ]
  
  # Select Pokémon ensuring 'Type.1' is diverse
  selected_pokemon <- c()
  selected_types <- c()
  
  for (i in 1:nrow(data)) {
    if (length(selected_pokemon) < num_pokemon && !(data$Type.1[i] %in% selected_types)) {
      selected_pokemon <- c(selected_pokemon, data$Name[i])
      selected_types <- c(selected_types, data$Type.1[i])
    }
  }
  
  return(selected_pokemon)
}

# Main function
main <- function(file_path) {
  # Read the data
  data <- read_pokemon_data(file_path)
  
  # Create the graph
  graph <- create_graph(data)
  
  # Select 6 Pokémon to form a team
  team <- select_pokemon_team(data, graph)
  
  # Print the selected team
  print(team)
  
  # Display the graph
  graph_d3 <- igraph_to_networkD3(graph)
  forceNetwork(Links = graph_d3$links, Nodes = graph_d3$nodes,
               Source = "source", Target = "target",
               NodeID = "name", Group = "name",
               opacity = 0.8)
}
# Execute the main function with the Excel file path
file_path <- ""
main(file_path)
