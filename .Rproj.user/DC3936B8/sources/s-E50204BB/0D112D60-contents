# check_in_tabulist
check_in_tabulist<-function(tabulist, route) {
  flag_exit <- 0
  
  if (length(tabulist)) {
    for (i in 1:length(tabulist)) {
      if ((length(tabulist[[i]]) == length(route))&&(all(tabulist[[i]] == route))) {
        flag_exit <- 1
        break
      }
    }
  }
  
  
  return(flag_exit)
}

# insert_in_tabu_list
insert_in_tabu_list<-function(client, route, tau, tabu_list_data) {
  

  if (length(tabu_list_data$tabu_list_clients > tabu_list_data$max_size_tabu_list )) {
    index_to_delete <- return_low_counter(tabu_list_data)
    tabu_list_data <- delete_tabu_list_element(tabu_list_data, index_to_delete)
  }
  
  end_position <- length(tabu_list_data$tabu_list_clients) + 1
  
  tabu_list_data$tabu_list_clients [ end_position ] <- client
  tabu_list_data$tabu_list_routes  [ end_position ] <- route
  tabu_list_data$tabu_list_counters[ end_position ] <- tau
  
  return(tabu_list_data)
}

# return_tau
return_tau<-function(n_clients, n_routes) {
  
  UB <- sqrt(n_clients*n_routes)
  LB <- 1
  
  return (floor(runif(1) *UB + LB))
  
}

# update_counters_tabu_list
update_counters_tabu_list<-function(tabu_list_data) {
  
  for (i in 1:length(tabu_list_data$tabu_list_counters)) {
    
    tabu_list_data$tabu_list_counters[[i]] <- tabu_list_data$tabu_list_counters[[i]] - 1
    if (tabu_list_data$tabu_list_counters[[i]] == 0) {
      tabu_list_data <- delete_tabu_list_element(tabu_list_data, i)
    }
    
  }
  
  return(tabu_list_data)
}

# create_tabu_list
create_tabu_list<-function(){
  tabu_list_data <- list()
  tabu_list_data$max_size_tabu_list <- 1000
  tabu_list_data$tabu_list_clients<- list()
  tabu_list_data$tabu_list_routes<- list()
  tabu_list_data$tabu_list_counters<- list()
  
  return(tabu_list_data)
}

# return_low_counter
return_low_counter<-function(tabu_list_data){
  min<- Inf
  index <- -1
  for (i in 1:length(tabu_list_data$tabu_list_counters)) {
      if (tabu_list_data$tabu_list_counters[[i]] < min) {
        index <- i
      }
  }
  
  return (index)
}

# delete_tabu_list_element
delete_tabu_list_element<-function(tabu_list_data, index_to_delete) {
  tabu_list_data$tabu_list_clients  <- tabu_list_data$tabu_list_clients[-index_to_delete]
  tabu_list_data$tabu_list_routes   <- tabu_list_data$tabu_list_routes[-index_to_delete]
  tabu_list_data$tabu_list_counters <- tabu_list_data$tabu_list_counters[-index_to_delete]
  
  return(tabu_list_data)
}