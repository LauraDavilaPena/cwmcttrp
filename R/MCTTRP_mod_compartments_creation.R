
boolean_available_compartments_destination_route <- function(string, result, intermediate_solution, inserted_client, destination_route){
  
  #resultado <- intermediate_solution
  
  total_truck_hoppers <- dim(result$H.camion_res)[2]
  total_trailer_hoppers <- dim(result$H.trailer_res)[2]
  
  capacity_trailer_hoppers <- input_MCTTRP(string)$h.trailer[1]
  capacity_truck_hoppers <- input_MCTTRP(string)$h.camion[1]
  
  
  n1 <- input_MCTTRP(string)$n1
  
  
  avail <- FALSE
  
  
  
  client_origin_route_index <- route_of_client(inserted_client, initial_solution)$index
  client_origin_route_info <- initial_solution[[client_origin_route_index]]
  
  
  if(inserted_client <= n1 ){ #vc
    client_vc_index <- which(client_origin_route_info$VCs==inserted_client)
    client_vc_info <- client_origin_route_info$clients_vc[[client_vc_index]]
    client_hoppers_trailers <- client_vc_info$hoppers_trailers
    client_hoppers_trucks <- client_vc_info$hoppers_trucks
    client_hoppers_demands <- client_vc_info$demands
    
    
    if(destination_route$type == "PTR"){
      needed_truck_hoppers_per_product <- ceiling(client_hoppers_demands/capacity_truck_hoppers)
      needed_truck_hoppers_total <- sum(needed_truck_hoppers_per_product)
      
      if(needed_truck_hoppers_total <=  total_truck_hoppers - destination_route$used_hoppers_truck){
        
        avail <- TRUE
        
        
      }
      
    }else{ #if destination_route is either CVR or PVR
      
      if( total_trailer_hoppers - destination_route$used_hoppers_trailer == 0){ # no hay tolvas de trailer suficientes: chequeo trucks
        needed_truck_hoppers_per_product <- ceiling(client_hoppers_demands/capacity_truck_hoppers)
        needed_truck_hoppers_total <- sum(needed_truck_hoppers_per_product)
        if(needed_truck_hoppers_total <=  total_truck_hoppers - destination_route$used_hoppers_truck){ # me caben
          
          avail <- TRUE 
          
        }
      }else{ # en caso de que si haya tolvas de trailer disponibles (en destination_route) hay que ver si me llegan (junto con las que haya disponibles de truck)
        needed_trailer_hoppers_per_product <- ceiling(client_hoppers_demands/capacity_trailer_hoppers)
        needed_trailer_hoppers_total <- sum(needed_trailer_hoppers_per_product)
        if(needed_trailer_hoppers_total <=  total_trailer_hoppers - destination_route$used_hoppers_trailer){ # me caben
          
          avail <- TRUE
          
          
        }else{
          
          checking_compartments_available <- is_there_available_compartments(total_truck_hoppers, total_trailer_hoppers, destination_route, 
                                                                             client_hoppers_demands, needed_trailer_hoppers_per_product, capacity_truck_hoppers, capacity_trailer_hoppers)
          
          available_compartments <- checking_compartments_available$available_compartments
          
          if(available_compartments){
            needed_truck_hoppers_per_product <- checking_compartments_available$needed_truck_hoppers_per_product
            needed_truck_hoppers_total <- sum(needed_truck_hoppers_per_product)
            
            
            avail <- TRUE 
            
          }
          
          
        }
        
        
      }
    }
    
    
  }else if(inserted_client > n1){ #tc
    
    client_tc_index <- which(client_origin_route_info$TCs==inserted_client)
    client_tc_info <- client_origin_route_info$clients_tc[[client_tc_index]]
    client_hoppers_trucks <- client_tc_info$hoppers_trucks
    client_hoppers_demands <- client_tc_info$demands
    
    
    #Como el cliente es TC, la "destination_route" si o si sera una PTR
    needed_hoppers_per_product <- ceiling(client_hoppers_demands/capacity_truck_hoppers)
    needed_hoppers_total <- sum(needed_hoppers_per_product)
    
    if(needed_hoppers_total <=  total_truck_hoppers - destination_route$used_hoppers_truck){
      
      avail <- TRUE
      
    }
    
    
    
    
  }
  
  
  
  
  return(avail)
}




create_only_one_vehicle_type_hoppers_for_vc <- function(inserted_client, destination_route, client_hoppers_the_other_vehicles_type, client_hoppers_vehicles_type, capacity_vehicle_type_hoppers,
                                                        client_vc_info, client_hoppers_demands, needed_vehicle_type_hoppers_per_product, used_hoppers_vehicle_type,
                                                        hoppers_the_other_vehicles_type, hoppers_vehicles_type){
  
  
  new_clients_vc <- list()
  new_clients_vc <- destination_route$clients_vc
  if( length(client_hoppers_the_other_vehicles_type)==0 ){ # no tiene tolvas de truck; todas son de trailer
    
    destination_route[[used_hoppers_vehicle_type]] <- destination_route[[used_hoppers_vehicle_type]] + length(client_hoppers_vehicles_type)
    destination_route$total_load <- destination_route$total_load + sum(client_hoppers_demands)
    new_clients_vc[[length(destination_route$clients_vc)+1]] <- client_vc_info
    destination_route$clients_vc <- new_clients_vc
  }else{ 
    new_hoppers_vehicles_type <- list()
    t <- 1
    client_hoppers_demands_res <- client_hoppers_demands
    for (f in 1:length(client_hoppers_demands)){
      if(needed_vehicle_type_hoppers_per_product[f]!=0){
        while(client_hoppers_demands_res[f]>0){
          quantity <- min(client_hoppers_demands_res[f], capacity_vehicle_type_hoppers)
          new_hoppers_vehicles_type[[t]] <- data.frame(f, quantity, quantity/capacity_vehicle_type_hoppers)
          colnames(new_hoppers_vehicles_type[[t]]) <- c("Pienso", "Cantidad", "Proporcion")
          client_hoppers_demands_res[f] <- client_hoppers_demands_res[f] - quantity
          t <- t + 1
        }
      }
    }
    
    destination_route$total_load <- destination_route$total_load  + sum(client_hoppers_demands)
    destination_route[[used_hoppers_vehicle_type]] <- destination_route[[used_hoppers_vehicle_type]] + length(new_hoppers_vehicles_type)
    new_clients_vc[[length(destination_route$clients_vc)+1]] <- client_vc_info
    new_clients_vc[[length(destination_route$clients_vc)+1]][[hoppers_the_other_vehicles_type]] <- list()
    new_clients_vc[[length(destination_route$clients_vc)+1]][[hoppers_vehicles_type]] <- new_hoppers_vehicles_type
    destination_route$clients_vc <- new_clients_vc
    destination_route$VCs <- c(destination_route$VCs, inserted_client)
    
  }
  
  
  return(destination_route)
}




is_there_available_compartments <- function(total_truck_hoppers, total_trailer_hoppers, destination_route, 
                                            client_hoppers_demands, needed_trailer_hoppers_per_product, capacity_truck_hoppers, capacity_trailer_hoppers){
  
  available_compartments <- FALSE
  
  available_trailer_hoppers <- total_trailer_hoppers - destination_route$used_hoppers_trailer
  
  needed_trailer_hoppers_per_product_res <- needed_trailer_hoppers_per_product
  needed_trailer_hoppers_per_product_res2 <- needed_trailer_hoppers_per_product_res
  load_res <- client_hoppers_demands
  
  
  for (i in 1:length(needed_trailer_hoppers_per_product)){
    while(available_trailer_hoppers>0 & needed_trailer_hoppers_per_product_res2[i]>0){
      needed_trailer_hoppers_per_product_res2[i] <- max(needed_trailer_hoppers_per_product_res[i]-available_trailer_hoppers,0 )
      load_res[i] <- max(0, load_res[i] - (needed_trailer_hoppers_per_product_res[i]-needed_trailer_hoppers_per_product_res2[i])*capacity_trailer_hoppers)
      available_trailer_hoppers <- available_trailer_hoppers - (needed_trailer_hoppers_per_product_res[i] - needed_trailer_hoppers_per_product_res2[i])
      needed_trailer_hoppers_per_product_res <- needed_trailer_hoppers_per_product_res2
    }
  }
  
  
  # Ahora he "consumido" todas las tolvas de trailer, y todavia no he acoplado load_res de mercancia. Tengo que ver si esto me cabe en las tolvas de truck que hay libres
  
  
  
  available_truck_hoppers <- total_truck_hoppers - destination_route$used_hoppers_truck
  needed_truck_hoppers_per_product <- ceiling(load_res/capacity_truck_hoppers)
  needed_truck_hoppers_total <- sum(needed_truck_hoppers_per_product)
  
  
  if(needed_truck_hoppers_total <=  total_truck_hoppers - destination_route$used_hoppers_truck){
    available_compartments <- TRUE
  }
  
  
  
  return(list(available_compartments=available_compartments, needed_truck_hoppers_per_product=needed_truck_hoppers_per_product)) 
}




create_truck_and_trailer_hoppers_for_vc <- function(inserted_client, needed_trailer_hoppers_total, total_truck_hoppers, total_trailer_hoppers, destination_route, client_hoppers_trailers, client_hoppers_trucks,
                                                    client_vc_info, client_hoppers_demands, needed_trailer_hoppers_per_product, capacity_truck_hoppers, capacity_trailer_hoppers, needed_truck_hoppers_per_product){
  
  
  available_trailer_hoppers <- total_trailer_hoppers - destination_route$used_hoppers_trailer
  client_hoppers_demands_res <- client_hoppers_demands
  
  
  new_hoppers_trailers <- list()
  t <- 1
  
  for (f in 1:length(client_hoppers_demands)){
    if(needed_trailer_hoppers_per_product[f]!=0){
      while(client_hoppers_demands_res[f]>0 & available_trailer_hoppers > 0){
        quantity <- min(client_hoppers_demands_res[f], capacity_trailer_hoppers)
        new_hoppers_trailers[[t]] <- data.frame(f, quantity, quantity/capacity_trailer_hoppers)
        colnames(new_hoppers_trailers[[t]]) <- c("Pienso", "Cantidad", "Proporcion")
        client_hoppers_demands_res[f] <- client_hoppers_demands_res[f] - quantity
        available_trailer_hoppers <- available_trailer_hoppers - 1
        t <- t + 1
      }
    }
  }
  
  
  # Ya he llenado las tolvas de trailer. Ahora vamos con las de truck
  available_truck_hoppers <- total_truck_hoppers - destination_route$used_hoppers_truck
  new_hoppers_trucks <- list()
  tt <- 1
  
  for (f in 1:length(client_hoppers_demands)){
    if(needed_truck_hoppers_per_product[f]!=0){
      while(client_hoppers_demands_res[f]>0 & available_truck_hoppers > 0){
        quantity <- min(client_hoppers_demands_res[f], capacity_truck_hoppers)
        new_hoppers_trucks[[tt]] <- data.frame(f, quantity, quantity/capacity_truck_hoppers)
        colnames(new_hoppers_trucks[[tt]]) <- c("Pienso", "Cantidad", "Proporcion")
        client_hoppers_demands_res[f] <- client_hoppers_demands_res[f] - quantity
        available_truck_hoppers <- available_truck_hoppers - 1
        tt <- tt + 1
      }
    }
  }
  
  
  
  new_clients_vc <- list()
  new_clients_vc <- destination_route$clients_vc
  
  
  destination_route$total_load <- destination_route$total_load  + sum(client_hoppers_demands)
  destination_route$used_hoppers_trailer <- destination_route$used_hoppers_trailer + length(new_hoppers_trailers)
  destination_route$used_hoppers_truck <- destination_route$used_hoppers_truck + length(new_hoppers_trucks)
  new_clients_vc[[length(destination_route$clients_vc)+1]] <- client_vc_info
  new_clients_vc[[length(destination_route$clients_vc)+1]]$hoppers_trailers <- new_hoppers_trailers
  new_clients_vc[[length(destination_route$clients_vc)+1]]$hoppers_trucks <- new_hoppers_trucks
  destination_route$clients_vc <- new_clients_vc
  destination_route$VCs <- c(destination_route$VCs, inserted_client)
  
  
  
  
  return(destination_route)
}




check_available_compartments <- function(string, result, intermediate_solution, inserted_client, destination_route){
  
  #resultado <- intermediate_solution
  
  total_truck_hoppers <- dim(result$H.camion_res)[2]
  total_trailer_hoppers <- dim(result$H.trailer_res)[2]
  
  capacity_trailer_hoppers <- input_MCTTRP(string)$h.trailer[1]
  capacity_truck_hoppers <- input_MCTTRP(string)$h.camion[1]
  
  
  n1 <- input_MCTTRP(string)$n1
  
  
  avail <- FALSE
  
  
  
  client_origin_route_index <- route_of_client(inserted_client, initial_solution)$index
  client_origin_route_info <- initial_solution[[client_origin_route_index]]
  
  
  if(inserted_client <= n1 ){ #vc
    client_vc_index <- which(client_origin_route_info$VCs==inserted_client)
    client_vc_info <- client_origin_route_info$clients_vc[[client_vc_index]]
    client_hoppers_trailers <- client_vc_info$hoppers_trailers
    client_hoppers_trucks <- client_vc_info$hoppers_trucks
    client_hoppers_demands <- client_vc_info$demands
    
    
    if(destination_route$type == "PTR"){
      needed_truck_hoppers_per_product <- ceiling(client_hoppers_demands/capacity_truck_hoppers)
      needed_truck_hoppers_total <- sum(needed_truck_hoppers_per_product)
      
      if(needed_truck_hoppers_total <=  total_truck_hoppers - destination_route$used_hoppers_truck){
        #destination_route <- create_only_truck_hoppers_for_vc(needed_hoppers_total, total_truck_hoppers, destination_route, client_hoppers_trailers, client_hoppers_trucks,
        #                                                       client_vc_info, client_hoppers_demands, needed_hoppers_per_product, capacity_truck_hoppers)
        
        destination_route <- create_only_one_vehicle_type_hoppers_for_vc(inserted_client, destination_route, client_hoppers_trailers, client_hoppers_trucks, capacity_truck_hoppers,
                                                                         client_vc_info, client_hoppers_demands, needed_truck_hoppers_per_product, "used_hoppers_truck",
                                                                         "hoppers_trailers", "hoppers_trucks")
        
        avail <- TRUE
        #client_origin_route_info$clients_vc <- client_origin_route_info$clients_vc[-client_vc_index]  
        #client_origin_route_info$clients_vc <- client_origin_route_info$clients_vc[-client_vc_index]  
        
      }
      
    }else{ #if destination_route is either CVR or PVR
      
      if( total_trailer_hoppers - destination_route$used_hoppers_trailer == 0){ # no hay tolvas de trailer suficientes: chequeo trucks
        needed_truck_hoppers_per_product <- ceiling(client_hoppers_demands/capacity_truck_hoppers)
        needed_truck_hoppers_total <- sum(needed_truck_hoppers_per_product)
        if(needed_truck_hoppers_total <=  total_truck_hoppers - destination_route$used_hoppers_truck){ # me caben
          
          #destination_route <- create_only_truck_hoppers_for_vc(needed_truck_hoppers_total, total_truck_hoppers, destination_route, client_hoppers_trailers, client_hoppers_trucks,
          #                                                 client_vc_info, client_hoppers_demands, needed_truck_hoppers_per_product, capacity_truck_hoppers)
          destination_route <- create_only_one_vehicle_type_hoppers_for_vc(inserted_client, destination_route, client_hoppers_trailers, client_hoppers_trucks, capacity_truck_hoppers,
                                                                           client_vc_info, client_hoppers_demands, needed_truck_hoppers_per_product, "used_hoppers_truck",
                                                                           "hoppers_trailers", "hoppers_trucks")
          
          avail <- TRUE 
          
        }
      }else{ # en caso de que si haya tolvas de trailer disponibles (en destination_route) hay que ver si me llegan (junto con las que haya disponibles de truck)
        needed_trailer_hoppers_per_product <- ceiling(client_hoppers_demands/capacity_trailer_hoppers)
        needed_trailer_hoppers_total <- sum(needed_trailer_hoppers_per_product)
        if(needed_trailer_hoppers_total <=  total_trailer_hoppers - destination_route$used_hoppers_trailer){ # me caben
          #destination_route <- create_only_trailer_hoppers_for_vc(needed_trailer_hoppers_total, total_trailer_hoppers, destination_route, client_hoppers_trailers, client_hoppers_trucks,
          #                                                                     client_vc_info, client_hoppers_demands, needed_trailer_hoppers_per_product, capacity_truck_hoppers)
          
          
          destination_route <- create_only_one_vehicle_type_hoppers_for_vc(inserted_client, destination_route, client_hoppers_trucks, client_hoppers_trailers, capacity_trailer_hoppers,
                                                                           client_vc_info, client_hoppers_demands, needed_trailer_hoppers_per_product, "used_hoppers_trailer",
                                                                           "hoppers_trucks", "hoppers_trailers")
          
          
          avail <- TRUE
        }else{
          
          checking_compartments_available <- is_there_available_compartments(total_truck_hoppers, total_trailer_hoppers, destination_route, 
                                                                             client_hoppers_demands, needed_trailer_hoppers_per_product, capacity_truck_hoppers, capacity_trailer_hoppers)
          
          available_compartments <- checking_compartments_available$available_compartments
          
          if(available_compartments){
            needed_truck_hoppers_per_product <- checking_compartments_available$needed_truck_hoppers_per_product
            needed_truck_hoppers_total <- sum(needed_truck_hoppers_per_product)
            
            destination_route <- create_truck_and_trailer_hoppers_for_vc(inserted_client, needed_trailer_hoppers_total, total_truck_hoppers, total_trailer_hoppers, destination_route, client_hoppers_trailers, client_hoppers_trucks,
                                                                         client_vc_info, client_hoppers_demands, needed_trailer_hoppers_per_product, capacity_truck_hoppers, capacity_trailer_hoppers, needed_truck_hoppers_per_product)
            
            avail <- TRUE 
            
          }
          
          
        }
        
        
      }
    }
    
    
  }else if(inserted_client > n1){ #tc
    
    client_tc_index <- which(client_origin_route_info$TCs==inserted_client)
    client_tc_info <- client_origin_route_info$clients_tc[[client_tc_index]]
    client_hoppers_trucks <- client_tc_info$hoppers_trucks
    client_hoppers_demands <- client_tc_info$demands
    
    
    #Como el cliente es TC, la "destination_route" si o si sera una PTR
    needed_hoppers_per_product <- ceiling(client_hoppers_demands/capacity_truck_hoppers)
    needed_hoppers_total <- sum(needed_hoppers_per_product)
    
    if(needed_hoppers_total <=  total_truck_hoppers - destination_route$used_hoppers_truck){
      new_clients_tc <- list()
      new_clients_tc <- destination_route$clients_tc
      
      destination_route$used_hoppers_truck <- destination_route$used_hoppers_truck + length(client_hoppers_trucks)
      destination_route$total_load <- destination_route$total_load + sum(client_hoppers_demands)
      new_clients_tc[[length(destination_route$clients_tc)+1]] <- client_tc_info
      destination_route$clients_tc <- new_clients_tc
      destination_route$TCs <- c(destination_route$TCs, inserted_client)
      
      avail <- TRUE
      
    }
    
    
    
    
  }
  
  
  
  
  
  
  
  return(list(avail = avail, destination_route = destination_route))
}




