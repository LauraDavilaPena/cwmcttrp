check_feasibility<-function(routes_res, route, input, type_root, type_problem) {
  all_vc <- 1
  subroutes <- 0

  for (i in 2:(length(route)-1)) {
    if (route[i] >  input$n1) all_vc <- 0
    if (sum(route==route[i])>1) subroutes <- 1
  }

  # determine new type
  if ((all_vc)&&(!subroutes)&&(type_root == "CVR")) type_root <- "PVR"
  if ((!all_vc)&&(!subroutes)&&(type_root == "CVR")) type_root <- "PTR"
  else if (subroutes) type_root <- "CVR"
  
    
  if (type_problem == "TTRP") {
    load1 <- calc_load2(route, input$vector.demandas)
    if (type_root == "PTR") total_capacity <- input$capacidad.truck
    else total_capacity <- input$capacidad.vehiculo
    subroute_total_capacity <- input$capacidad.truck
  }
  
  if (type_problem == "MCTTRP") {
    load1 <- calc_load2_MC(route, input$matriz.demandas)
    if (type_root == "PTR") total_capacity <- input$capacidad.truck[1]
    else total_capacity <- input$capacidad.vehiculo[1]
    subroute_total_capacity <- input$capacidad.truck[1]
  }

  # check total load
  if (load1 <= total_capacity) feasible <- 1
  else feasible <- 0
  
  # check subroute capacity
  if ((type_root == "CVR") && (feasible)) {
    #subroutes <- return_subroutes_string(route, input$n1)
    #print("XXXXXroute")
    #print(route)
    subroutes <- return_subroutes(route, input$n1)
    unfeasible <- 0
    for (i in 1:length(subroutes)) {
      subroute_i <- subroutes[[i]]$tour[2:(length(subroutes[[i]]$tour)-1)]
      #print("xxxx")
      #print(subroutes[[i]]$tour)
      if (type_problem == "TTRP") load_subroute <- calc_load2(subroute_i, input$vector.demandas)
      if (type_problem == "MCTTRP") load_subroute <- calc_load2_MC(subroute_i, input$matriz.demandas)
      if (load_subroute > subroute_total_capacity) {
        unfeasible <- 1
        break
      }
    }

    if (unfeasible) feasible <- 0
  }
  
  # check hoppers
  if ((type_problem == "MCTTRP") && (feasible)){
    if (type_root == "CVR") feasible <- check_capacity_hoppers_MCTTRP_CVR(route, routes_res, input) 
    else feasible <- check_capacity_hoppers_MCTTRP_PR (route, routes_res, input, total_capacity ) 
  } 
  
  return(feasible)
}

check_capacity_TTRP_total_routes_res<-function(routes_res_i, routes_res_j, input, total_capacity) {
  
  route_i <- routes_res_i$route
  route_j <- routes_res_j$route
  
  return(check_capacity_TTRP_total_routes(route_i, route_j, input, total_capacity))
}

check_capacity_total_routes<-function(route_i, route_j, input, total_capacity, type_problem) {
  
  if (type_problem == "TTRP") {
    load1 <- calc_load2(route_i, input$vector.demandas)
    load2 <- calc_load2(route_j, input$vector.demandas)
  }
  
  if (type_problem == "MCTTRP") {
    load1 <- calc_load2_MC(route_i, input$matriz.demandas)
    load2 <- calc_load2_MC(route_j, input$matriz.demandas)  
    total_capacity <- total_capacity[1]
  }
    
  new_load <- load1 + load2
  
  if (new_load <= total_capacity) {
    
    return(1)
    
  } else {
    
    return(0)
    
  }
}

check_capacity_TTRP_subroute_routes_res<-function(routes_res_i, routes_res_j, subtours, subroute_index, input) {
  
  route_i <- routes_res_i$route
  route_j <- routes_res_j$route
  
  return(check_capacity_TTRP_subroute_routes(route_i, route_j, subtours, subroute_index, input))
}

check_capacity_subroute_routes<-function(route_i, route_j, subtours, subroute_index, input, type_problem) {
  
  subroute_i <- subtours[[subroute_index]]$tour[2:(length(subtours[[subroute_index]]$tour)-1)]
  
  if (type_problem == "TTRP") {
    load1 <- calc_load2(route_i, input$vector.demandas)
    load2 <- calc_load2(route_j, input$vector.demandas)
    limit_truck <- input$capacidad.truck
    limit_total <- input$capacidad.vehiculo 
    load2_subroute_i <- calc_load2(subroute_i, input$vector.demandas)
    
  }
  
  if (type_problem == "MCTTRP") {
    load1 <- calc_load2_MC(route_i, input$matriz.demandas)
    load2 <- calc_load2_MC(route_j, input$matriz.demandas)   
    limit_truck <- input$capacidad.truck[1]
    limit_total <- input$capacidad.vehiculo[1]
    load2_subroute_i <- calc_load2_MC(subroute_i, input$matriz.demandas)
  }

  new_load <- load1 + load2

  if (new_load <= limit_total) {
    new_load <- load1 + load2_subroute_i
    if (new_load <= limit_truck) {
      return(1)
    } else {
      return(0)
    }
    
  } else {
    return(0)
  }
}

check_capacity_TTRP_merge_subroute_routes_res<-function(routes_res_i, routes_res_j, input) {
  
  route_i <- routes_res_i$route
  route_j <- routes_res_j$route
  
  return(check_capacity_TTRP_merge_subroute_routes(route_i, route_j, input))
}

check_capacity_merge_subroute_routes<-function(route_i, route_j, input, type_problem) {
  
  
  if (type_problem == "TTRP") {
    load1 <- calc_load2(route_i, input$vector.demandas)
    load2 <- calc_load2(route_j, input$vector.demandas)
    limit_truck <- input$capacidad.truck
    limit_total <- input$capacidad.vehiculo 

  }
  
  if (type_problem == "MCTTRP") {
    load1 <- calc_load2_MC(route_i, input$matriz.demandas)
    load2 <- calc_load2_MC(route_j, input$matriz.demandas)   
    limit_truck <- input$capacidad.truck[1]
    limit_total <- input$capacidad.vehiculo[1]
  }
  
  new_load <- load1 + load2

  if ((new_load <= limit_total)&&(load2 <= limit_truck)) {
    return(1)
  } 
  else {
    return(0)
  }
}

check_capacity_hoppers_MCTTRP_CVR<-function(route_i, routes_res, input) {
  
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  subroute <- return_subroutes_string(route_i, input$n1)
  main_route <- return_main_route(route_i)
  
  counter_hoppers_truck <- 0
  for (i in 2:(length(subroute)-1)) {
    i_client <- subroute[i]
    for (j in 1:length(input$matriz.demandas[i_client+1,])){
      demand <- input$matriz.demandas[i_client+1,j]
      while (demand > 0) {
        demand <- demand - cap_hoppers_truck
        counter_hoppers_truck <- counter_hoppers_truck + 1
      }
    }
  }
  
  counter_hoppers_trailer <- 0
  for (i in 2:(length(main_route)-1)) {
    i_client <- main_route[i]
    for (j in 1:length(input$matriz.demandas[i_client+1,])){
      demand <- input$matriz.demandas[i_client+1,j]
      #trailers
      while ((demand > 0) && (counter_hoppers_trailer < n_hoppers_trailer)){
        demand <- demand - cap_hoppers_trailer
        counter_hoppers_trailer <- counter_hoppers_trailer + 1
      }
      # trucks
      while ((demand > 0)) {
        demand <- demand - cap_hoppers_truck
        counter_hoppers_truck <- counter_hoppers_truck + 1
      }
    }
  }

  if ((counter_hoppers_trailer <= n_hoppers_trailer)&&(counter_hoppers_truck <= n_hoppers_truck)) {
    return (1)
  } else {
    return (0)
  }
  
}


check_capacity_hoppers_MCTTRP_PR<-function(route_i, routes_res, input, capacity) {
  
  if (capacity[1] == input$capacidad.vehiculo[1] ) {
    flag_exit <- check_capacity_hoppers_MCTTRP_PVR(route_i, routes_res, input, capacity)
  } else {
    flag_exit <- check_capacity_hoppers_MCTTRP_generic(route_i, input, 0) 
  }
  
  
  return(flag_exit)
}




check_capacity_hoppers_MCTTRP_generic<-function(route_i, input, check_trailer) {
  
  n_hoppers_truck <- length(input$H.camion[1,])
  n_hoppers_trailer <- length(input$H.trailer[1,])
  
  cap_hoppers_truck <- input$H.camion[1,1]
  cap_hoppers_trailer <- input$H.trailer[1,1]
  
  route <- route_i
  
  counter_hoppers_truck <- 0
  counter_hoppers_trailer <- 0
  for (i in 2:(length(route)-1)) {
    i_client <- route[i]
    for (j in 1:length(input$matriz.demandas[i_client+1,])){
      demand <- input$matriz.demandas[i_client+1,j]
      #trailers
      while ((demand > 0) && (check_trailer) && (counter_hoppers_trailer < n_hoppers_trailer)){
        demand <- demand - cap_hoppers_trailer
        counter_hoppers_trailer <- counter_hoppers_trailer + 1
      }
      # trucks
      while ((demand > 0)) {
        demand <- demand - cap_hoppers_truck
        counter_hoppers_truck <- counter_hoppers_truck + 1
      }
    }
  }
  
  if (check_trailer) {
    if ((counter_hoppers_trailer <= n_hoppers_trailer)&&(counter_hoppers_truck <= n_hoppers_truck)) {
      #print("return 1")
      return (1)
    } else {
      #print("return 0")
      return (0)
    }
  }
  else {
    if (counter_hoppers_truck <= n_hoppers_truck) {
      #print("return 1")
      return (1)
    } else {
      #print("return 0")
      return (0)
    }
  }
  
}


check_capacity_hoppers_MCTTRP_PVR<-function(route_i, routes_res, input, capacity){

  for (i in 1:(length(routes_res))) {
    if ( sum(routes_res[[i]]$route==route_i[2]) > 0 ) {
      index <- i
      break
    }
  }
  

  new_order <- c(0)
  for (i in 2:(length(route_i)-1)) {
    res <- return_size_hoppers(routes_res, index, route_i[i])
    size_tra <-  res$size_tra 
    size_tru <-  res$size_tru
    if ((size_tra > 0)&&(size_tru == 0)) {
      new_order <- c(new_order, route_i[i])
    }
  }
  
  for (i in 2:(length(route_i)-1)) {
    res <- return_size_hoppers(routes_res, index, route_i[i])
    size_tra <-  res$size_tra 
    size_tru <-  res$size_tru
    if ((size_tra > 0)&&(size_tru > 0)) {
      new_order <- c(new_order, route_i[i])
    }
  }
  
  for (i in 2:(length(route_i)-1)) {
    res <- return_size_hoppers(routes_res, index, route_i[i])
    size_tra <-  res$size_tra 
    size_tru <-  res$size_tru
    if ((size_tra == 0)&&(size_tru > 0)) {
      new_order <- c(new_order, route_i[i])
    }
  }
  
  new_order <- c(new_order, 0)

  flag_exit <- check_capacity_hoppers_MCTTRP_generic(new_order, input, 1) 

  return(flag_exit)
}



return_size_hoppers<-function(routes_res, index, route_i) {
  size_tra <- 0
  size_tru <- 0
  
  for (i in 1:length(routes_res[[index]]$clients_vc)) {
    if (routes_res[[index]]$clients_vc[[i]]$id == route_i) {
      size_tra <- size_tra + length(routes_res[[index]]$clients_vc[[i]]$hoppers_trailers)
      size_tru <- size_tru + length(routes_res[[index]]$clients_vc[[i]]$hoppers_trucks)
    }
  }
  
  res <- list()
  res$size_tra <- size_tra
  res$size_tru <- size_tru
  
  return(res)
}


return_occupied_hoppers_MCTTRP<-function(type, hoppers, id_vehicle) {
  counter <- 0
  
  for (i in 1:length(hoppers[,1])) {
    if ((hoppers[i,4]==id_vehicle)&&(hoppers[i,3]==type)) {
      counter <- counter + 1
    }
  }
  
  return(counter)
}

return_i_vehicle<-function(route_i, route_res) {
  node <- route_i[2]
  for (i in 1:length(route_res)) {
    if (sum(route_res[[i]]$route==node)>0) {
      id <- i
      break;
    }
  }
  
  return(id)
}