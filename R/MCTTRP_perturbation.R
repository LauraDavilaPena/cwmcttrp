#perturbacion
perturbation_core<-function(input, current_solution, penalty_max, type_problem) {
  
  perturbation_not_obtained <- TRUE
  penalty <- 0
  
  while ((perturbation_not_obtained)) {
    print("*************************perturbation_not_obtained")
    print(all_routes(current_solution))
    
    perturbed_solution <-  perturbation(input, current_solution, penalty, type_problem)
    current_solution <- perturbed_solution[["perturbed_solution"]]
    perturbation_not_obtained <- perturbed_solution$perturbation_not_obtained
    phi <- perturbed_solution$phi
    penalty <- 100
  }
  
  current_solution <- update_solution(current_solution, input, type_problem)
  
  res_p <- list()
  res_p$current_solution <- current_solution
  res_p$phi <- phi
  
  return(res_p)
}

perturbation <- function(input, initial_solution, penalty_max, problem_type){
  
  intermediate_solution <- initial_solution
  perturbation_not_obtained <- FALSE
  
  n <- input$n - 1
  
  # Nuestros clientes candidatos a ser eliminados deben estar en PTR, PVR, o CVR y no ser parking
  removed_candidates <- numeric()

  for(i in 1:n){ #numero total de clientes
    route_i <- route_of_client(i,initial_solution)$route
    if((length(route_i)>3)&&(sum(route_i==i)==1)){
      removed_candidates <- c(removed_candidates,i) 
    }
  }

  removed_routes <- numeric()
  for (i in 1:length(initial_solution)) {
    route_i <- initial_solution[[i]]$route
    if((length(route_i)>3)&&(sum(route_i==i)==1)){
      removed_routes <- c(removed_routes,i) 
    }
  }
  
  # De esos candidatos, elijo entre un 5% y un 15% aleatoriamente 
  phi <- sample(ceiling(0.01*length(removed_candidates)):ceiling(0.05*length(removed_candidates)),1)
  phi <- 1

  # Ahora que ya sabemos que vamos a eliminar phi clientes, escogemos cuales 
  removed_clients <- sample(removed_candidates, phi)
  
  print(removed_clients)
  
  
  # Una vez los tenemos, vemos en que rutas estan, y agrupamos aquellos que esten en las mismas rutas
  routes_indices <- numeric()
  for(i in 1:length(removed_clients)){
    routes_indices[i] <- route_of_client(removed_clients[i], initial_solution)$index
  }
  
  aggregated_indices <- list()
  for(i in 1:length(removed_clients)){
    aggregated_indices[[i]] <- which(routes_indices == routes_indices[i])
  }
  
  aggregated_indices <- aggregated_indices[!duplicated(aggregated_indices)]
  
  aggregated_clients <- list()
  for (i in 1:length(aggregated_indices)){
    aggregated_clients[[i]] <- removed_clients[aggregated_indices[[i]]]
  }
  
  aggregated_routes <- list()
  for(i in 1:length(aggregated_clients)){
    aggregated_routes[[i]] <- route_of_client(aggregated_clients[[i]][1], initial_solution)$route
  }
  
  aggregated_routes_index <- list()
  for(i in 1:length(aggregated_clients)){
    aggregated_routes_index[[i]] <- route_of_client(aggregated_clients[[i]][1], initial_solution)$index
  }
  
  aggregated_list_info <- list(aggregated_clients = aggregated_clients, aggregated_routes = aggregated_routes, aggregated_routes_index = aggregated_routes_index)
  
  
  # Vamos eliminando secuencialmente a los clientes removed_clients de la ruta en la que estan
  aggregated_list_info_after_removal <- aggregated_list_info
  new_routes_after_removal <- list()
  no_route_left <- numeric()
  nr <- 1
  
  for(i in 1:length(aggregated_list_info_after_removal$aggregated_routes)){
    for (j in 1:length(aggregated_list_info_after_removal$aggregated_clients[[i]])){
      if(intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$type == "CVR"){
        if(client_in_main_tour(aggregated_list_info_after_removal$aggregated_clients[[i]][j], initial_solution)==1){
          #si el cliente esta en el main tour, hago lo que tenia 
          # Basicamente aqui lo que hago es: si la ruta es de tipo CVR y el cliente a eliminar esta en el main tour, entonces el "GENI_US" se lo voy a aplicar 
          # solo al main_tour. Pero luego tengo que reconstruir
          # la ruta completa, añadiendo los subtours en el orden en que estaban
          new_routes_after_removal[[i]] <- GENI_US(input, intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$main_tour, 
                                                   aggregated_list_info_after_removal$aggregated_clients[[i]][j])
          
          intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$main_tour <- new_routes_after_removal[[i]]
          subtours <- intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$subtours
          
          for(k in 1:length(subtours)){
            kk <- 1
            if(length(which(new_routes_after_removal[[i]]==subtours[[k]]$root)) > 1){
              kk <- sum(new_routes_after_removal[[i]]  == subtours[[k]]$root)
            }
            new_routes_after_removal[[i]] <- c(new_routes_after_removal[[i]][1:which(new_routes_after_removal[[i]]==subtours[[k]]$root)[kk] ], subtours[[k]]$tour[2:(length(subtours[[k]]$tour))], 
                                               new_routes_after_removal[[i]][(which(new_routes_after_removal[[i]]==subtours[[k]]$root)[kk]+1): length(new_routes_after_removal[[i]])] ) 
            
          }
          
          intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route <- new_routes_after_removal[[i]]
        }
        else{ 
          # en cambio, si el cliente esta en el subtour, procedo asi:
          # primero debo mirar cual es el subtour en el que se encuentra
          #print(paste0("DELETE -> ", aggregated_list_info_after_removal$aggregated_routes_index[[i]]))
          subtours <- intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$subtours
          for(k in 1:length(subtours)){
            if(sum(subtours[[k]]$tour == aggregated_list_info_after_removal$aggregated_clients[[i]][j])>0){
              subtour_origin <- subtours[[k]]
              subtour_index <- k
            }
          }
          tour_origin <- subtour_origin$tour
          if(length(tour_origin) == 3){ # si el subtour tuviese un unico cliente, entonces despues de eliminarlo nos quedamos sin subtour
            intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$subtours <-  intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$subtours[-subtour_index] 
            rclient_index <- which(intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route == aggregated_list_info_after_removal$aggregated_clients[[i]][j])
            new_routes_after_removal[[i]] <- intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route[-c(rclient_index,rclient_index+1)]
            if(length(subtours)==1){ # si ademas ese fuese el unico subtour de nuestra CVR, despues de eliminarlo tendriamos una PVR
              intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$type = "PVR"
            }
          }else{
            new_subtour_after_removal <- GENI_US(input, tour_origin, aggregated_list_info_after_removal$aggregated_clients[[i]][j])
            subtours_after_removal <- subtours
            subtours_after_removal[[subtour_index]]$tour <- new_subtour_after_removal
            if(aggregated_list_info_after_removal$aggregated_clients[[i]][j]<=input$n1){
              vc_index <- which(subtours_after_removal[[subtour_index]]$vc_clients == aggregated_list_info_after_removal$aggregated_clients[[i]][j])
              subtours_after_removal[[subtour_index]]$vc_clients <- subtours_after_removal[[subtour_index]]$vc_clients[-vc_index] 
            }else{
              tc_index <- which(subtours_after_removal[[subtour_index]]$tc_clients == aggregated_list_info_after_removal$aggregated_clients[[i]][j])
              subtours_after_removal[[subtour_index]]$tc_clients <- subtours_after_removal[[subtour_index]]$tc_clients[-tc_index] 
            }
            subtours_after_removal[[subtour_index]]$length <- subtours_after_removal[[subtour_index]]$length - 1
            intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$subtours <- subtours_after_removal
            new_routes_after_removal[[i]] <- intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$main_tour
            for(ind_k in 1:length(subtours_after_removal)){
              kk <- 1
              if(length(which(new_routes_after_removal[[i]]==subtours_after_removal[[ind_k]]$root)) > 1){
                kk <- sum(new_routes_after_removal[[i]]  == subtours_after_removal[[ind_k]]$root)
              }
              new_routes_after_removal[[i]] <- c(new_routes_after_removal[[i]][1:which(new_routes_after_removal[[i]]==subtours_after_removal[[ind_k]]$root)[kk] ], subtours_after_removal[[ind_k]]$tour[2:(length(subtours_after_removal[[ind_k]]$tour))], 
                                                 new_routes_after_removal[[i]][(which(new_routes_after_removal[[i]]==subtours_after_removal[[ind_k]]$root)[kk]+1): length(new_routes_after_removal[[i]])] ) 
              
            }
          }
          
          intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route <- new_routes_after_removal[[i]]
          #print(intermediate_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route)
        }
      }
      else{
        if( length(aggregated_list_info_after_removal$aggregated_routes[[i]]) == 3 ){
          no_route_left[nr] <- aggregated_list_info_after_removal$aggregated_routes_index[[i]]
          nr <- nr + 1
        }
        new_routes_after_removal[[i]] <- GENI_US(input, aggregated_list_info_after_removal$aggregated_routes[[i]], aggregated_list_info_after_removal$aggregated_clients[[i]][j])
      }
      
      aggregated_list_info_after_removal$aggregated_routes[[i]] <- new_routes_after_removal[[i]]
    }
  }
  
  # Ahora voy a modificar la intermediate_solution, poniendo las nuevas rutas, su coste, y eliminando a los removed_clients y todo lo relacionado con ellos 
  # (ojo: para el caso en que los clientes eliminados provienen de CVRs, ya modifique la info relativa al main.tour y subtours en el bucle anterior)
  for(i in 1:length(aggregated_list_info_after_removal$aggregated_routes_index)){
    intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route <- aggregated_list_info_after_removal$aggregated_routes[[i]]
    intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$cost <- calculateTotalDistance(input,intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route )
    
    for(j in 1:length(aggregated_list_info_after_removal$aggregated_clients[[i]])){
      if(aggregated_list_info_after_removal$aggregated_clients[[i]][j] <= input$n1){ # si el cliente que eliminamos es VC
        client_index <- which(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$VCs == aggregated_list_info_after_removal$aggregated_clients[[i]][j] )
        removed_load <- sum(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[[client_index]]$demands)
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load - removed_load
        if (problem_type == "MCTTRP") {
          removed_truck_hoppers <- length(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[[client_index]]$hoppers_trucks)
          removed_trailer_hoppers <- length(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[[client_index]]$hoppers_trailers)
          intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_trailer <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_trailer - removed_trailer_hoppers
          intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_truck <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_truck - removed_truck_hoppers
        }
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$VCs <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$VCs[-client_index]
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[-client_index]
        
      }
      else{ # si el cliente que elimino es de tipo TC
        client_index <- which(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$TCs == aggregated_list_info_after_removal$aggregated_clients[[i]][j] )
        removed_load <- sum(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc[[client_index]]$demands)
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load - removed_load
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load_tc_clients <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load_tc_clients - removed_load
        if (problem_type == "MCTTRP") {
          removed_truck_hoppers <- length(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc[[client_index]]$hoppers_trucks)
          intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_truck <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_truck - removed_truck_hoppers
        }
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$TCs <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$TCs[-client_index]
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc[-client_index]
      }
    }
    
  }
  
  # Ahora lo que tengo que hacer es, esos clientes que he eliminado de sus rutas originales, insertarlos en una
  # ruta distinta (donde sea factible; la idea es que tengo que intentar insertar todos)
  
  # Basicamente, para cada uno de los clientes que debo insertar (que son los que he eliminado), tengo que crearme 
  # una lista con sus posibles "destination_routes"
  
  # reordenar por carga ---> aggregated_list_info$aggregated_clients
  
  
  for(k in 1:length(aggregated_list_info$aggregated_clients)){
    for(i in 1:length(aggregated_list_info$aggregated_clients[[k]])){
      inserting_client <- aggregated_list_info$aggregated_clients[[k]][i]
      route_try <- list()
      index_ins <- list()
      new_route_ins <- list()
      delta_ins <- list()
      subtour_try <- list()
      t <- 1
      modified_subtours <- list()
      ind_subtour_wrt_route <- list()
      tt <- 1
      for(r in 1:length(intermediate_solution)){
        if(aggregated_list_info$aggregated_routes_index[[k]]!= r){ # hay que mirar en rutas distintas de la ruta origen
          if(inserting_client <= input$n1){ # si el cliente es VC entonces podria insertarse en cualquier ruta que no sea la propia de origen
            candidate_destination_route <- intermediate_solution[[r]]
            # La siguiente funcion nos dice si es (avail=TRUE) o no (avail=FALSE) posible que 
            avail <- 1
            if (problem_type == "MCTTRP") {
              res <- return_cap_and_route_permutation(candidate_destination_route, input, "MCTTRP")
              tcap <- res$tcap
              route <- res$route
              if (check_capacity_total_routes(inserting_client, route, input, tcap, penalty_max, "MCTTRP"))
                avail <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                          inserting_client, candidate_destination_route, initial_solution)
            }
            if (problem_type == "TTRP") { 
              res <- return_cap_and_route_permutation(candidate_destination_route, input, "TTRP")
              tcap <- res$tcap
              route <- res$route
              avail <- check_capacity_total_routes(c(inserting_client), route, input, tcap,penalty_max, "TTRP")
            }
            
            if(avail){ 
              if(candidate_destination_route$type == "CVR"){
                route_try[[t]] <- candidate_destination_route$main_tour
                index_ins[[t]] <- r
              }else{
                route_try[[t]] <- candidate_destination_route$route
                index_ins[[t]] <- r
              }
              init_time <- Sys.time()
              res_geni <- GENI(input, route_try[[t]], inserting_client)
              #print(paste0("GENI time ", difftime(Sys.time(), init_time, units = "secs","s)")))
              
              new_route_ins[[t]] <- res_geni$best_route
              delta_ins[[t]] <- res_geni$delta_GENI
              route_try[[t]] <- new_route_ins[[t]]
              
              t <- t + 1
              
            }
            
          }else{ # si el cliente es tc
            if(intermediate_solution[[r]]$type == "PTR"){
              candidate_destination_route <- intermediate_solution[[r]]
              # La siguiente funcion nos dice si es (avail=TRUE) o no (avail=FALSE) posible que 
              avail <- 1
              if (problem_type == "MCTTRP") {
                res <- return_cap_and_route_permutation(candidate_destination_route, input, "MCTTRP")
                tcap <- res$tcap
                route <- res$route
                # meter penalizacion
                if (check_capacity_total_routes(inserting_client, route, input, tcap, penalty_max, "MCTTRP"))
                  avail <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                            inserting_client, candidate_destination_route, initial_solution)
              }
              if (problem_type == "TTRP") { 
                res <- return_cap_and_route_permutation(candidate_destination_route, input, "TTRP")
                tcap <- res$tcap
                route <- res$route
                # meter penalizacion
                avail <- check_capacity_total_routes(inserting_client, route, input, tcap, penalty_max, "TTRP")
              }
              
              if(avail){ 
                route_try[[t]] <- candidate_destination_route$route
                index_ins[[t]] <- r
                
                init_time <- Sys.time()
                res_geni <- GENI(input, route_try[[t]], inserting_client)
                #print(paste0("GENI time ", difftime(Sys.time(), init_time, units = "secs","s)")))
                
                new_route_ins[[t]] <- res_geni$best_route
                delta_ins[[t]] <- res_geni$delta_GENI
                route_try[[t]] <- new_route_ins[[t]]
                
                t <- t + 1
              }
            }else if(intermediate_solution[[r]]$type == "CVR"){
              subtour_try <- list()
              index_subtour <- list()
              geni_subtour <- list()
              new_subtour_ins <- list()
              delta_subtour <- list()
              candidate_destination_route <- intermediate_solution[[r]]
              avail_subtour <- rep(1,length(intermediate_solution[[r]]$subtours))
              kk <- 1
              for(rr in 1:length(intermediate_solution[[r]]$subtours)){
                if (problem_type == "MCTTRP") {
                  res <- return_cap_and_route_permutation(candidate_destination_route, input, "MCTTRP")
                  tcap <- res$tcap
                  route <- res$route
                  # meter penalizacion
                  if (check_capacity_subroute_routes(inserting_client, route, intermediate_solution[[r]]$subtours, rr, input, "MCTTRP"))
                    avail_subtour[rr] <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                                          inserting_client, candidate_destination_route, initial_solution)
                }
                if (problem_type == "TTRP") { 
                  res <- return_cap_and_route_permutation(candidate_destination_route, input, "TTRP")
                  tcap <- res$tcap
                  route <- res$route
                  # meter penalizacion
                  avail_subtour[rr] <- check_capacity_subroute_routes(inserting_client, route, intermediate_solution[[r]]$subtours, rr, input, "TTRP")
                }
                
                if(avail_subtour[rr]){
                  
                  subtour_try[[kk]] <- candidate_destination_route$subtours[[rr]]$tour
                  index_subtour[[kk]] <- rr
                  
                  geni_subtour[[kk]] <- GENI(input, subtour_try[[kk]], inserting_client)
                  #print(paste0("GENI time ", difftime(Sys.time(), init_time, units = "secs","s)")))
                  
                  new_subtour_ins[[kk]] <- geni_subtour[[kk]]$best_route
                  delta_subtour[[kk]] <- geni_subtour[[kk]]$delta_GENI
                  
                  subtour_try[[kk]] <- new_subtour_ins[[kk]]
                  
                  kk <- kk + 1
                  
                  
                }
              }
              
              if(sum(avail_subtour)!=0){
                delta_min_subtour_pos <- which(delta_subtour == min(unlist(delta_subtour)))
                if(length(delta_min_subtour_pos) == 1){
                  delta_subtour_chosen_position <- delta_min_subtour_pos
                }else{
                  delta_subtour_chosen_position <- sample(delta_min_subtour_pos,1)
                }
                
                index_subtour_insertion <- index_subtour[[delta_subtour_chosen_position]]
                # La ruta de la solucion inicial cuyo indice es index_route_insertion la hay que actualizar, de modo que se le añade al cliente inserting_client
                
                best_subtour_ins <- new_subtour_ins[[delta_subtour_chosen_position]]
                
                
                modified_subtours[[tt]] <- candidate_destination_route$subtours
                modified_subtours[[tt]][[index_subtour_insertion]]$tour <- best_subtour_ins
                modified_subtours[[tt]][[index_subtour_insertion]]$length <- modified_subtours[[tt]][[index_subtour_insertion]]$length + 1
                modified_subtours[[tt]][[index_subtour_insertion]]$tc_clients <- c(modified_subtours[[tt]][[index_subtour_insertion]]$tc_clients, inserting_client)
                ind_subtour_wrt_route[[tt]] <- t
                
                info_modified_subtours <- list(modified_subtours = modified_subtours, ind_subtour_wrt_route=ind_subtour_wrt_route)
                
                new_route_ins[[t]] <- create_route_from_main_route_and_subroutes(modified_subtours[[tt]], candidate_destination_route$main_tour)
                delta_ins[[t]] <- delta_subtour[[delta_subtour_chosen_position]]
                index_ins[[t]] <- r
                route_try[[t]] <- new_route_ins[[t]]
                
                t <- t + 1
                tt <- tt + 1
              }
            }
          }
        }
      }
      
      if(length(delta_ins) == 0){
        perturbation_not_obtained <- TRUE
        break
        
      }else{
        
        delta_min_positions <- which(delta_ins == min(unlist(delta_ins)))
        if(length(delta_min_positions) == 1){
          delta_chosen_position <- delta_min_positions
        }else{
          delta_chosen_position <- sample(delta_min_positions,1)
        }
        
        index_route_insertion <- index_ins[[delta_chosen_position]]
        # La ruta de la solucion inicial cuyo indice es index_route_insertion la hay que actualizar, de modo que se le añade al cliente inserting_client
        
        best_route_ins <- new_route_ins[[delta_chosen_position]]
        
        # Actualizamos la intermediate_solution:
        
        if (intermediate_solution[[index_route_insertion]]$type == "CVR") {
          if(length(modified_subtours)==0){
            intermediate_solution[[index_route_insertion]]$main_tour <- best_route_ins
            intermediate_solution[[index_route_insertion]]$route <- create_route_from_main_route_and_subroutes(intermediate_solution[[index_route_insertion]]$subtours, best_route_ins)
          }else{
            intermediate_solution[[index_route_insertion]]$route <- best_route_ins
          }
          
        } else {
          intermediate_solution[[index_route_insertion]]$route <- best_route_ins
        }
        intermediate_solution[[index_route_insertion]]$cost <- calculateTotalDistance(input,best_route_ins)
        
        if (problem_type == "MCTTRP") {
          destination_route <- check_available_compartments(input, result, intermediate_solution, inserting_client, 
                                                            intermediate_solution[[index_route_insertion]], initial_solution)$destination_route
          
          intermediate_solution[[index_route_insertion]] <- destination_route
        }else if (problem_type == "TTRP"){
          intermediate_solution[[index_route_insertion]]$total_load <- intermediate_solution[[index_route_insertion]]$total_load + input$vector.demandas[inserting_client+1]
          new_client <- list()
          new_client$id <- inserting_client
          new_client$demands <- input$vector.demandas[inserting_client+1]
          if(inserting_client <= input$n1){ # si el cliente es VC
            intermediate_solution[[index_route_insertion]]$clients_vc[[length(intermediate_solution[[index_route_insertion]]$clients_vc)+1]] <- new_client
            intermediate_solution[[index_route_insertion]]$VCs <- c(intermediate_solution[[index_route_insertion]]$VCs, inserting_client) 
          }else{    # si el cliente es TC
            intermediate_solution[[index_route_insertion]]$total_load_tc_clients <- intermediate_solution[[index_route_insertion]]$total_load_tc_clients + new_client$demands
            intermediate_solution[[index_route_insertion]]$clients_tc[[length(intermediate_solution[[index_route_insertion]]$clients_tc)+1]] <- new_client
            intermediate_solution[[index_route_insertion]]$TCs <- c(intermediate_solution[[index_route_insertion]]$TCs, inserting_client)
            if( (intermediate_solution[[index_route_insertion]]$type == "CVR") && (length(modified_subtours)!=0) ){
              ind_subt = which(unlist(info_modified_subtours$ind_subtour_wrt_route)==delta_chosen_position)
              intermediate_solution[[index_route_insertion]]$subtours <- modified_subtours[[ind_subt]]
            }
          }
        }
        
        
      }
    }
    
    if(perturbation_not_obtained){
      perturbation_not_obtained <- TRUE
      break
    }
  }
  
  if(perturbation_not_obtained){
    perturbed_solution <- initial_solution
    
  }else{
    if(length(no_route_left) > 0){
      intermediate_solution <- intermediate_solution[-no_route_left]
    }
    intermediate_solution <- update_solution(intermediate_solution, input, problem_type)
    perturbed_solution <- intermediate_solution
    
  }
  
  #for(i in 1:length(perturbed_solution)){
  #  print(perturbed_solution[[i]]$route)
  #}
  
  return(list(perturbation_not_obtained = perturbation_not_obtained, perturbed_solution = perturbed_solution, phi = phi))
  
}




return_cap_and_route_permutation<-function(intermediate_solution, input, type) {
  if (intermediate_solution$type == "CVR")  {
    if (type == "MCTTRP") tcap <- input$capacidad.vehiculo[1]
    if (type == "TTRP") tcap <- input$capacidad.vehiculo
    route <- intermediate_solution$route#return_main_route(intermediate_solution$route)
  }
  if (intermediate_solution$type == "PVR")  {
    if (type == "MCTTRP") tcap <- input$capacidad.vehiculo[1]
    if (type == "TTRP") tcap <- input$capacidad.vehiculo
    route <- intermediate_solution$route
  }
  if (intermediate_solution$type == "PTR")  {
    if (type == "MCTTRP") tcap <- input$capacidad.truck[1]
    if (type == "TTRP") tcap <- input$capacidad.truck
    route <- intermediate_solution$route
  }
  
  res <- list()
  res$tcap <- tcap
  res$route <- route
  
  return(res)
}

