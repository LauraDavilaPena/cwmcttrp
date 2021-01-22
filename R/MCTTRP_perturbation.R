#perturbacion

# Aqui como argumento de entrada le habr?a que pasar una soluci?n "rutas_des" por ejemplo
# y la salida debe ser una soluci?n perturbada: "rutas_des_perturb"



perturbation <- function(input, result, initial_solution, problem_type, seed, tabulist){
  
  perturbation_not_obtained <- FALSE
  
  
  n <- input$n - 1
  intermediate_solution <- initial_solution
  #set.seed(1234)

  
  # Nuestros clientes candidatos a ser eliminados deben estar en PTR, PVR, o main.tour de CVR y no ser parking
  removed_candidates <- numeric()
  for(i in 1:n){ #numero total de clientes
    if(client_in_main_tour(i, initial_solution)==1 && client_is_parking(i, initial_solution)==0){
      removed_candidates <- c(removed_candidates,i) 
    }
  }
  
  
  
  # De esos candidatos, elijo entre un 5% y un 15% aleatoriamente 
  phi <- sample(ceiling(0.05*length(removed_candidates)):ceiling(0.15*length(removed_candidates)),1)
  
  # Ahora que ya sabemos que vamos a eliminar phi clientes, escogemos cuales 
  removed_clients <- sample(removed_candidates, phi)
  
  print(removed_clients)
  
  
  # Una vez los tenemos, vemos en que rutas estan, y agrupamos aquellos que esten en las mismas rutas
  routes_indices <- numeric()
  for(i in 1:length(removed_clients)){
    routes_indices[i] <- route_of_client(removed_clients[i], initial_solution)$index
  }
  routes_indices
  
  
  aggregated_indices <- list()
  for(i in 1:length(removed_clients)){
    aggregated_indices[[i]] <- which(routes_indices == routes_indices[i])
  }
  
  aggregated_indices
  aggregated_indices <- aggregated_indices[!duplicated(aggregated_indices)]
  
  aggregated_clients <- list()
  for (i in 1:length(aggregated_indices)){
    aggregated_clients[[i]] <- removed_clients[aggregated_indices[[i]]]
  }
  aggregated_clients
  
  aggregated_routes <- list()
  for(i in 1:length(aggregated_clients)){
    aggregated_routes[[i]] <- route_of_client(aggregated_clients[[i]][1], initial_solution)$route
  }
  aggregated_routes
  
  
  aggregated_routes_index <- list()
  for(i in 1:length(aggregated_clients)){
    aggregated_routes_index[[i]] <- route_of_client(aggregated_clients[[i]][1], initial_solution)$index
  }
  aggregated_routes_index
  
  
  aggregated_list_info <- list(aggregated_clients = aggregated_clients, aggregated_routes = aggregated_routes, aggregated_routes_index = aggregated_routes_index)
  
  
  
  
  # Vamos eliminando secuencialmente a los clientes removed_clients de la ruta en la que estan
  
  aggregated_list_info_after_removal <- aggregated_list_info
  new_routes_after_removal <- list()
  
  for(i in 1:length(aggregated_list_info_after_removal$aggregated_routes)){
    for (j in 1:length(aggregated_list_info_after_removal$aggregated_clients[[i]])){
      if(initial_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$type == "CVR"){
        # Basicamente aqui lo que hago es: si la ruta es de tipo CVR, entonces el "GENI_US" se lo voy a aplicar solo al main_tour. Pero luego tengo que reconstruir
        # la ruta completa, añadiendo los subtours en el orden en que estaban
        new_routes_after_removal[[i]] <- GENI_US(input, initial_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$main_tour, 
                                                 aggregated_list_info_after_removal$aggregated_clients[[i]][j])
        subtours <- initial_solution[[aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$subtours
        kk <- 1
        for(k in 1:length(subtours)){
          if(length(which(new_routes_after_removal[[i]]==subtours[[k]]$root)) > 1){
            kk <- sum(new_routes_after_removal[[i]]  == subtours[[k]]$root)
            
          }
          new_routes_after_removal[[i]] <- c(new_routes_after_removal[[i]][1:which(new_routes_after_removal[[i]]==subtours[[k]]$root)[kk] ], subtours[[k]]$tour[2:(length(subtours[[k]]$tour))], 
                                            new_routes_after_removal[[i]][(which(new_routes_after_removal[[i]]==subtours[[k]]$root)[kk]+1): length(new_routes_after_removal[[i]])] ) 
        }
      }else{
        new_routes_after_removal[[i]] <- GENI_US(input, aggregated_list_info_after_removal$aggregated_routes[[i]], aggregated_list_info_after_removal$aggregated_clients[[i]][j])
      }
      aggregated_list_info_after_removal$aggregated_routes[[i]] <- new_routes_after_removal[[i]]
    }
  }
  
  # Ahora voy a modificar la intermediate_solution, poniendo las nuevas rutas, su coste, y eliminando a los removed_clients y todo lo relacionado con ellos 
  print("2")
  for(i in 1:length(aggregated_list_info_after_removal$aggregated_routes_index)){
    intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route <- aggregated_list_info_after_removal$aggregated_routes[[i]]
    intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$cost <- calculateTotalDistance(input,intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$route )
    
    for(j in 1:length(aggregated_list_info_after_removal$aggregated_clients[[i]])){
      if(aggregated_list_info_after_removal$aggregated_clients[[i]][j] <= input$n1){ # si el cliente que eliminamos es VC
        client_index <- which(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$VCs == aggregated_list_info_after_removal$aggregated_clients[[i]][j] )
        removed_load <- sum(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[[client_index]]$demands)
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load - removed_load
        removed_truck_hoppers <- length(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[[client_index]]$hoppers_trucks)
        removed_trailer_hoppers <- length(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[[client_index]]$hoppers_trailers)
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_trailer <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_trailer - removed_trailer_hoppers
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_truck <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_truck - removed_truck_hoppers
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$VCs <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$VCs[-client_index]
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_vc[-client_index]
        
      }else{ # si el cliente que elimino es de tipo TC
        client_index <- which(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$TCs == aggregated_list_info_after_removal$aggregated_clients[[i]][j] )
        removed_load <- sum(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc[[client_index]]$demands)
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load - removed_load
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load_tc_clients <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$total_load_tc_clients - removed_load
        removed_truck_hoppers <- length(intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc[[client_index]]$hoppers_trucks)
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_truck <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$used_hoppers_truck - removed_truck_hoppers
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$TCs <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$TCs[-client_index]
        intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc <- intermediate_solution[[ aggregated_list_info_after_removal$aggregated_routes_index[[i]] ]]$clients_tc[-client_index]
      }
    }
    
  }
  
  # Ahora lo que tengo que hacer es, esos clientes que he eliminado de sus rutas originales, insertarlos en una
  # ruta distinta (donde sea factible; la idea es que tengo que intentar insertar todos)
  
  # Basicamente, para cada uno de los clientes que debo insertar (que son los que he eliminado), tengo que crearme 
  # una lista con sus posibles "destination_routes"
  
  for(k in 1:length(aggregated_list_info$aggregated_clients)){
    for(i in 1:length(aggregated_list_info$aggregated_clients[[k]])){
      inserting_client <- aggregated_list_info$aggregated_clients[[k]][i]
      route_try <- list()
      index_ins <- list()
      new_route_ins <- list()
      delta_ins <- list()
      t <- 1
      for(r in 1:length(intermediate_solution)){
        if(aggregated_list_info$aggregated_routes_index[[k]]!= r){ # hay que mirar en rutas distintas de la ruta origen
          if(inserting_client <= input$n1){ # si el cliente es VC entonces podria insertarse en cualquier ruta que no sea la propia de origen
            candidate_destination_route <- intermediate_solution[[r]]
            # La siguiente funcion nos dice si es (avail=TRUE) o no (avail=FALSE) posible que 
            avail <- 1
            if (problem_type == "MCTTRP") 
              avail <- boolean_available_compartments_destination_route(input, result, intermediate_solution, 
                                                                        inserting_client, candidate_destination_route, 
                                                                        initial_solution)
            
            if(avail){ 
              if(candidate_destination_route$type == "CVR"){
                route_try[[t]] <- candidate_destination_route$main_tour
                index_ins[[t]] <- r
              }else{
                route_try[[t]] <- candidate_destination_route$route
                index_ins[[t]] <- r
              }
              
              new_route_ins[[t]] <- GENI(input, route_try[[t]], inserting_client)$best_route
              delta_ins[[t]] <- GENI(input, route_try[[t]], inserting_client)$delta_GENI
              
              t <- t + 1
              
              
            }
          }else{ # si el cliente es tc
            if(intermediate_solution[[r]]$type == "PTR"){
              candidate_destination_route <- intermediate_solution[[r]]
              # La siguiente funcion nos dice si es (avail=TRUE) o no (avail=FALSE) posible que 
              avail <- 1
              if (problem_type == "MCTTRP") avail <- boolean_available_compartments_destination_route(input, result, intermediate_solution, inserting_client, candidate_destination_route, initial_solution)
              if(avail){ 
                route_try[[t]] <- candidate_destination_route$route
                index_ins[[t]] <- r
              
                new_route_ins[[t]] <- GENI(input, route_try[[t]], inserting_client)$best_route
                delta_ins[[t]] <- GENI(input, route_try[[t]], inserting_client)$delta_GENI
                
                t <- t + 1
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
        intermediate_solution[[index_route_insertion]]$route <- best_route_ins
        intermediate_solution[[index_route_insertion]]$cost <- calculateTotalDistance(input,best_route_ins)
        
        if (problem_type == "MCTTRP") {
          destination_route <- check_available_compartments(input, result, intermediate_solution, inserting_client, 
                                                            intermediate_solution[[index_route_insertion]], initial_solution)$destination_route
        
          intermediate_solution[[index_route_insertion]] <- destination_route
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
      perturbed_solution <- intermediate_solution
      
    }
  
  
  
  return(list(perturbation_not_obtained = perturbation_not_obtained, perturbed_solution = perturbed_solution))
  
}


