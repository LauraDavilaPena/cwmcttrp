# check_in_tabulist
check_in_tabulist<-function(tabulist, clients, id_route) {
  
  flag_exit <- 0
  for (j in 1:length(clients)) {
    client <- clients[j]
    if (length(tabulist$tabu_list_clients)) {
      for (i in 1:length(tabulist$tabu_list_counters)) {
        if ((tabulist$tabu_list_clients [[i]] == client) && (tabulist$tabu_list_routes [[i]] == id_route)) {
          flag_exit <- 1
        }
      }
    }
  }
  return(flag_exit)
}

# insert_in_tabu_list
insert_in_tabu_list<-function(clients, id_route, tau, tabulist) {

  if (length(tabulist$tabu_list_clients) > tabulist$max_size_tabu_list ) {
    index_to_delete <- return_low_counter(tabulist)
    tabulist <- delete_tabu_list_element(tabulist, index_to_delete)
  }
  
  end_position <- length(tabulist$tabu_list_clients) 
  for (i in 1:length(clients)) {
    client <- clients[i]
    tabulist$tabu_list_clients [[end_position+i]] <- client
    tabulist$tabu_list_routes  [[end_position+i]] <- id_route
    tabulist$tabu_list_counters[[end_position+i]] <- tau
  }
  
  return(tabulist)
}

# return_tau
return_tau<-function(n_clients, n_routes) {
  
  UB <- sqrt(n_clients*n_routes)
  LB <- 1
  
  return (floor(runif(1) *UB + LB))
  
}

# update_counters_tabu_list
update_counters_tabu_list<-function(tabulist) {
  
  if(length(tabulist$tabu_list_counters)) {
      for (i in 1:length(tabulist$tabu_list_counters)) {
        tabulist$tabu_list_counters[[i]] <- tabulist$tabu_list_counters[[i]] - 1
      }
    
      size <- length(tabulist$tabu_list_counters)
      counter <- 1
      while(counter < size) {
        if (tabulist$tabu_list_counters[[counter]] <= 0) {
          tabulist <- delete_tabu_list_element(tabulist, counter)
          
          size <- length(tabulist$tabu_list_counters)
        } else {
          counter <- counter + 1
        }
      }
  }
  
  return(tabulist)
}

# print tabu list
print_tabu_list<-function(tabulist){
  
  if(length(tabulist$tabu_list_counters)) {
    for (i in 1:length(tabulist$tabu_list_counters)) {
      
      print(paste0("client: ",  tabulist$tabu_list_clients[[i]],
                   " route: ",   tabulist$tabu_list_routes[[i]],
                   " counter: ", tabulist$tabu_list_counters[[i]]))
      
      
    }
  }
  
}


# create_tabu_list
create_tabu_list<-function(){
  tabulist <- list()
  tabulist$max_size_tabu_list <- 1000
  tabulist$tabu_list_clients<- list()
  tabulist$tabu_list_routes<- list()
  tabulist$tabu_list_counters<- list()
  
  return(tabulist)
}

# return_low_counter
return_low_counter<-function(tabulist){
  min<- Inf
  index <- -1
  for (i in 1:length(tabulist$tabu_list_counters)) {
      if (tabulist$tabu_list_counters[[i]] < min) {
        index <- i
      }
  }
  
  return (index)
}

# delete_tabu_list_element
delete_tabu_list_element<-function(tabulist, index_to_delete) {
  tabulist$tabu_list_clients  <- tabulist$tabu_list_clients[-index_to_delete]
  tabulist$tabu_list_routes   <- tabulist$tabu_list_routes[-index_to_delete]
  tabulist$tabu_list_counters <- tabulist$tabu_list_counters[-index_to_delete]
  
  return(tabulist)
}

# create_table_freq
create_table_freq<-function(size_clients, n_routes){
  table_frec <- list()
  for (i in 1:size_clients) {
    clients_v <- c(0)
    for (j in 2:n_routes) {
      clients_v <- c(clients_v, 0)
    }
    table_frec[[i]] <- clients_v
  }
  return(table_frec)
}

# update_table_freq
update_table_freq<-function(table_frec, client, id_route){
  table_frec[[as.numeric(client)]][as.numeric(id_route)] <- table_frec[[as.numeric(client)]][as.numeric(id_route)] + 1
  return(table_frec)
}

# return_table_freq
return_table_freq<-function(table_frec, client, id_route){
  return(table_frec[[as.numeric(client)]][as.numeric(id_route)] )
}
