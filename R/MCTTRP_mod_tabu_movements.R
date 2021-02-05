# Algoritmo Tabu


tabu_search <- function(input, current_solution, type_problem, tabulist_data) {#tabulist, max_size_tabu_list, n_movs, type_problem, vecinity, perc_v, penalty_capacity){
  # Partimos de una solucion perturbada
  counter_tabu <- 1
  exist <- 0
  
  tabulist <- tabulist_data$tabulist
  max_size_tabu_list <- tabulist_data$max_size_tabu_list
  vecinity <-  input$vecinity
  perc_v <-  tabulist_data$perc_v
  penalty_capacity <- tabulist_data$penalty_capacity
  
  # select tau
  tau <- return_tau(input$n, length(current_solution))
  
  for (nn in 1:tabulist_data$n_movs) {
      #init_time <- Sys.time()
      res <- movements_imp(input, current_solution, type_problem, vecinity, perc_v, penalty_capacity)
      #print(paste0("mov", difftime(Sys.time(), init_time, units = "secs")))

      mov_list <- res$mov_list
      mov_list_cost <- res$mov_list_cost
      
      #concert list to vector
      if (length(mov_list_cost)) {
      
          mov_list_cost_vect <- c(mov_list_cost[[1]])
          for (i in 2:length(mov_list_cost)) mov_list_cost_vect <- c(mov_list_cost_vect ,mov_list_cost[[i]] )
          
          index_order <- order(mov_list_cost_vect, decreasing = TRUE)
          
          not_in_tabu_list <- 0
          counter_index_order <- 1
          
          while (!not_in_tabu_list) {
            if ((mov_list_cost_vect[index_order[counter_index_order]]>=0)) {
              old_route1 <- current_solution[[mov_list[[index_order[counter_index_order]]]$indexr1]]$route
              #old_route2 <- current_solution[[mov_list[[index_order[counter_index_order]]]$indexr2]]$route
              #print("insert_selected_mov")
              result_ins <- insert_selected_mov(input, mov_list[[index_order[counter_index_order]]] , current_solution, tabulist, max_size_tabu_list, type_problem)
              tabulist <- result_ins$tabulist
              current_solution <- result_ins$current_solution
              not_in_tabu_list <- result_ins$not_in_tabu_list
              
              #if (not_in_tabu_list) {
              #  print(paste0("ADD ", mov_list[[index_order[counter_index_order]]]$mov_name))
              #  print(old_route1)
              #  print("xxxxxxxxxxxxxxxxx")
              #  for (i in 1:length(current_solution)) {
              #    print(current_solution[[i]]$route)
              #  }
              #  readline()
              #  print("xxxxxxxxxxxxxxxxx")
              #}
            }
            else {
              not_in_tabu_list <- 1
              exist <- 1
              
            }
            
            counter_index_order <- counter_index_order + 1
            
          }
          
          if (exist) break
          
      } else break

      counter_tabu <- counter_tabu + 1
  }
  
  #print(paste0("COUNTER TABUs ", counter_tabu))
  result <- list()
  result$current_solution <- current_solution
  result$tabulist <- tabulist
  return(result)
}


movements_imp <- function(input, current_solution, type_problem, vecinity, perc_vecinity, penalty_capacity) {
  mov_list <- list()
  mov_list_cost <- list()

  # exchange moves
  res <- exchange_movement_client_subtour_and_vc_creating_subtour(input, current_solution, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- exchange_movement_change_parking(input, current_solution, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity ,penalty_capacity) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- exchange_movement_vc_main_tour_tc_subtour(input, current_solution, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- exchange_movement_client_short_subtour_and_client_in_main_tour(input, current_solution, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- move_subroute(input, current_solution, mov_list, mov_list_cost, type_problem, penalty_capacity) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- exchange_ptr_and_subtour(input, current_solution, mov_list, mov_list_cost, type_problem, penalty_capacity) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- exchange_movement_tc_PTR_and_vc_in_main_tour(input, current_solution, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity)
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost 
  
  # moves in the same root
  res <- move_subroute_same_route(input, current_solution, mov_list, mov_list_cost, type_problem, penalty_capacity) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- create_new_subtour_vc_same_route(input, current_solution, mov_list, mov_list_cost, type_problem, penalty_capacity)  
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- move_vc_client_subroute_to_main_tour_and_split_same_route(input, current_solution, mov_list, mov_list_cost, type_problem, penalty_capacity) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  # basic moves
  res <- exchange_tc_two_routes(input, current_solution, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- exchange_vc_two_routes(input, current_solution, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  
  return(res)
}


insert_selected_mov<-function(input, mov, current_solution, tabulist, max_size_tabu_list, type_problem){

  if (length(mov$route2)>1) {

    if (!check_in_tabulist(tabulist, mov$route1)||(!check_in_tabulist(tabulist, mov$route2))) {
          # first  modification
          res <- insert_element_in_solution(input, mov$route1, mov$indexr1, current_solution, tabulist, max_size_tabu_list, type_problem)
          current_solution <- res$current_solution
          tabulist <- res$tabulist
          
          # second modification
          res <- insert_element_in_solution(input, mov$route2, mov$indexr2, current_solution, tabulist, max_size_tabu_list, type_problem)
          current_solution <- res$current_solution
          tabulist <- res$tabulist
          
          not_in_tabu_list <- 1
    } 
    else not_in_tabu_list <- 0
  } else {
    if (!check_in_tabulist(tabulist, mov$route1)) {
          # modification
          res <- insert_element_in_solution(input, mov$route1, mov$indexr1, current_solution, tabulist, max_size_tabu_list, type_problem)
          current_solution <- res$current_solution
          tabulist <- res$tabulist
          
          not_in_tabu_list <- 1
    }
    else not_in_tabu_list <- 0
  }
    
  result <- list()
  result$tabulist <- tabulist
  result$current_solution <- current_solution
  result$not_in_tabu_list <- not_in_tabu_list
    
  return(result)
}


insert_element_in_solution<-function(input, new_route, pos, current_solution, tabulist, max_size_tabu_list, type_problem){
  
      # first modification
      all_vc <- 1
      subroutes <- 0
      type_root1 <- current_solution[[pos]]$type
      for (i in 2:(length(new_route)-1)) {
        if (new_route[i] >  input$n1) all_vc <- 0
        if (sum(new_route==new_route[i])>1) subroutes <- 1
      }
      
      # determine new type
      if ((all_vc)&&(!subroutes)&& (type_root1 == "CVR")) type_root1 <- "PVR"
      if ((!all_vc)&&(!subroutes)&&(type_root1 == "CVR")) type_root1 <- "PTR"
      else if (subroutes) type_root1 <- "CVR"
      
      current_solution[[pos]]$route <- new_route
      current_solution[[pos]]$type <-  type_root1
      
      
      if (current_solution[[pos]]$type == "CVR") {
        current_solution[[pos]]$main_tour <- return_main_route(current_solution[[pos]]$route)
        current_solution[[pos]]$subtours <- return_subroutes(current_solution[[pos]]$route, input$n1)
      }
      if (type_problem == "TTRP") {
        current_solution[[pos]]$total_load <- calc_load2(current_solution[[pos]]$route, input$vector.demandas)
        current_solution[[pos]]$total_load_tc_clients <- calc_load_only_truck(current_solution[[pos]]$route, input$vector.demandas, input)
      }
      if (type_problem == "MCTTRP") {
        current_solution[[pos]]$total_load <- calc_load2_MC(current_solution[[pos]]$route, input$matriz.demandas)
        current_solution[[pos]]$total_load_tc_clients <- calc_load_only_truck_MC(current_solution[[pos]]$route, input$matriz.demandas, input)
      }
      current_solution[[pos]]$cost <- local_cost(current_solution[[pos]]$route, input$matriz.distancia)
      if ((type_problem == "MCTTRP")&&(current_solution[[pos]]$type == "CVR")) {
        res_r <- insert_hoppers_MCTTRP_CVR(current_solution[[pos]]$route, current_solution, input)
        current_solution[[pos]]$clients_tc <- res_r$clients_tc
        current_solution[[pos]]$clients_vc <- res_r$clients_vc
        current_solution[[pos]]$used_hoppers_truck <- res_r$used_hoppers_truck
        current_solution[[pos]]$used_hoppers_trailer <- res_r$used_hoppers_trailer
      }
      
      tabulist <- insert_in_tabu_list(current_solution[[pos]]$route, tabulist, max_size_tabu_list) 

  
  res_i <- list()
  res_i$current_solution <- current_solution
  res_i$tabulist <- tabulist
  
  return(res_i)
}


exchange_tc_two_routes<-function(input, result, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity) {
  
  for (i in 1:(length(result)-1)) {
    if (result[[i]]$type != "PVR") {
      for (j in 2:(length(result[[i]]$route)-1)) {
        if ( sum(result[[i]]$route == result[[i]]$route[j]) == 1 ) {
          clienti <- result[[i]]$route[j]
          if (clienti > input$n1) {
            for (w in (i+1):length(result)) {
              if ((result[[w]]$type != "PVR")) {
                for (t in 2:(length(result[[w]]$route)-1)) {
                  if ( sum(result[[w]]$route == result[[w]]$route[t]) == 1 ) {
                    clientw <- result[[w]]$route[t]
                    if ((clientw > input$n1)&&(is_in_vecinity(clienti, clientw, vecinity, perc_vecinity))) {
                        # create routes  
                        route1 <- replace_route_client(clienti, clientw, result[[i]]$route)
                        route2 <- replace_route_client(clientw, clienti, result[[w]]$route)
                        # feasibility
                        feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                        feasible_route2 <- check_feasibility(result, route2, input, result[[w]]$type, type_problem, penalty_capacity) 
                        
                        # add to mov list
                        if (feasible_route1 && feasible_route2) {
                          res_mov <- add_movements_to_list(input, result, i, w, "exchange_tc_two_routes", 
                                                           route1, route2,  mov_list, mov_list_cost)
                          mov_list <- res_mov$mov_list
                          mov_list_cost <- res_mov$mov_list_cost 
                        } 
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  return(res)
  
}


exchange_vc_two_routes<-function(input, result, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity) {
  
  for (i in 1:(length(result)-1)) {
    for (j in 2:(length(result[[i]]$route)-1)) {
        if ( sum(result[[i]]$route == result[[i]]$route[j]) == 1 ) {
          clienti <- result[[i]]$route[j]
          if (clienti <= input$n1) {
            for (w in (i+1):length(result)) {
              for (t in 2:(length(result[[w]]$route)-1)) {
                  if ((i!=w)&&( sum(result[[w]]$route == result[[w]]$route[t]) == 1 )) {
                    clientw <- result[[w]]$route[t]
                    if ((clientw <= input$n1) && (is_in_vecinity(clienti, clientw, vecinity, perc_vecinity))) 
                    {
                      # create routes  
                      route1 <- replace_route_client(clienti, clientw, result[[i]]$route)
                      route2 <- replace_route_client(clientw, clienti, result[[w]]$route)
                      
                      # feasibility
                      feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                      feasible_route2 <- check_feasibility(result, route2, input, result[[w]]$type, type_problem, penalty_capacity) 
                      
                      
                      # add to mov list
                      if (feasible_route1 && feasible_route2) {
                        res_mov <- add_movements_to_list(input, result, i, w, "exchange_vc_two_routes", route1, route2,  mov_list, mov_list_cost)
                        mov_list <- res_mov$mov_list
                        mov_list_cost <- res_mov$mov_list_cost 
                      } 
                    }
                  }
                }
            }
          }
        }
      }
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  return(res)
  
}


exchange_ptr_and_subtour<-function(input, result, mov_list, mov_list_cost, type_problem, penalty_capacity) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "PTR") {
      subroutei <- result[[i]]$route
      for (z in 1:length(result)){
        if (result[[z]]$type == "CVR") {
          if (i!=z) {
            subroutes <- return_subroutes(result[[z]]$route, input$n1)
            for (s in 1:length(subroutes)) {
              subroutez <- subroutes[[s]]$tour
              
              route1 <- c(0, subroutez[2:(length(subroutez)-1)], 0)
              route2 <- replace_subroute(subroutez, subroutei, result[[z]]$route)
  
              # feasibility
              feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
              feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem, penalty_capacity) 
              # add to mov list
              if (feasible_route1 && feasible_route2) {
                res_mov <- add_movements_to_list(input, result, i, z, "exchange_ptr_and_subtour", route1, route2,  mov_list, mov_list_cost)
                mov_list <- res_mov$mov_list
                mov_list_cost <- res_mov$mov_list_cost 
              } 
            }
          }
        }
      }
    }
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  return(res)
  
}


move_vc_client_subroute_to_main_tour_and_split_same_route<-function(input, result, mov_list, mov_list_cost, type_problem, penalty_capacity) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes)) {
        for (j in 2:(length(subroutes[[s]]$tour)-1)) {
          clienti <- subroutes[[s]]$tour[j]
          prev <- subroutes[[s]]$tour[j-1]
          post <- subroutes[[s]]$tour[j+1]
          if ((clienti <= input$n1) && (sum(prev==subroutes[[s]]$tour)==1) &&
              (sum(post==subroutes[[s]]$tour)==1)){

            route1 <- split_subroute(clienti, result[[i]]$route)
            
            # feasibility
            feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
            # add to mov list
            if (feasible_route1) {
              res_mov <- add_movements_to_list(input, result, i, 0, "move_vc_client_subroute_to_main_tour_and_split_same_route", 
                                               route1, 0,  mov_list, mov_list_cost)
              mov_list <- res_mov$mov_list
              mov_list_cost <- res_mov$mov_list_cost 
            } 
            
          }
        }
      }
    }
    
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  return(res)
  
}


move_subroute<-function(input, result, mov_list, mov_list_cost, type_problem,penalty_capacity) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        subroutei <- subroutes[[s]]$tour
        for (z in 1:length(result)) {
          if ((i!=z)&&(result[[z]]$type != "PTR")) {
            if (result[[z]]$type == "CVR") main_root <- return_main_route(result[[z]]$route)
            else main_root <- result[[z]]$route
            for (t in 2:(length(main_root)-1)) {
              if ((main_root[t]<= input$n1)&&(i!=z)){
                clientw <- main_root[t]
                
                # routes
                route1 <- delete_subroute(subroutei, result[[i]]$route)
                route2 <- add_subroute(clientw, subroutei, result[[z]]$route)

                # feasibility
                feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem, penalty_capacity) 
                # add to mov list
                if (feasible_route1 && feasible_route2) {
                  res_mov <- add_movements_to_list(input, result, i, z, "move_subroute", route1, route2,  mov_list, mov_list_cost)
                  mov_list <- res_mov$mov_list
                  mov_list_cost <- res_mov$mov_list_cost 
                } 
              }
            }
          }
        }
      }
    }
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  return(res)
  
}


move_subroute_same_route<-function(input, result, mov_list, mov_list_cost, type_problem, penalty_capacity) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        subroutei <- subroutes[[s]]$tour
        main_root <- return_main_route(result[[i]]$route)
        for (t in 2:(length(main_root)-1)) {
              if ((main_root[t]<= input$n1)&&(sum(main_root[t]==result[[i]]$route)==1)){
                clientw <- main_root[t]
                # routes
                route1 <- delete_subroute(subroutei, result[[i]]$route)
                route1 <- add_subroute(clientw, subroutei, route1)
       
                # feasibility
                feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                # add to mov list
                if (feasible_route1) {
                  res_mov <- add_movements_to_list(input, result, i, 0, "move_subroute_same_route", 
                                                   route1, 0,  mov_list, mov_list_cost)
                  mov_list <- res_mov$mov_list
                  mov_list_cost <- res_mov$mov_list_cost 
                } 
              }
        }
      }
    }
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  return(res)
  
}


create_new_subtour_vc_same_route<-function(input, result, mov_list, mov_list_cost, type_problem, penalty_capacity) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type != "PTR") {
      if (result[[i]]$type == "CVR") main_root <- return_main_route(result[[i]]$route)
      else main_root <- result[[i]]$route
      for (t in 2:(length(main_root)-1)) {
        if ((main_root[t]<= input$n1)&&(sum(main_root[t]==result[[i]]$route)==1)){
          clienti <- main_root[t]
          for (z in 2:(length(main_root)-1)) {
            if ((main_root[z]<= input$n1)&&(sum(main_root[z]==result[[i]]$route)==1)&&(main_root[z]!=main_root[t])){
              
              clientz <- main_root[z]
               
              # routes
              route1 <- delete_client(clientz, result[[i]]$route)
              route1 <- add_subroute(clienti, c(clienti, clientz, clienti), route1)
              
              # feasibility
              feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
              # add to mov list
              if (feasible_route1) {
                res_mov <- add_movements_to_list(input, result, i, 0, "create_new_subtour_vc_unique", 
                                                 route1, 0,  mov_list, mov_list_cost)
                mov_list <- res_mov$mov_list
                mov_list_cost <- res_mov$mov_list_cost 
              } 
            }
          }
        }
      }
    }
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  return(res)
  
}


exchange_movement_change_parking<-function(input, result, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        clienti <- subroutes[[s]]$root
        if ( clienti <= input$n1 ) {
          for (z in 1:length(result)){
          if ((i!=z)&&(result[[z]]$type != "PTR")) {
            if (result[[z]]$type == "CVR") main_root <- return_main_route(result[[z]]$route)
            else main_root <- result[[z]]$route
            for (t in 2:(length(main_root)-1)) {
              if (main_root[t] <= input$n1) {
                clientz <- main_root[t]
                if (( clientz <= input$n1 )&&(is_in_vecinity(clienti, clientz, vecinity, perc_vecinity))) {
                  
                  route1 <- replace_subroute_vc(subroutes[[s]], clientz, result[[i]]$route)
                  route2 <- replace_route_client(clientz, clienti, result[[z]]$route)
                  route2 <- add_subroute(clienti, subroutes[[s]]$tour, route2)
    
                  # feasibility
                  feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                  feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem, penalty_capacity) 
                  # add to mov list
                  if (feasible_route1 && feasible_route2) {
                    res_mov <- add_movements_to_list(input, result, i, z, "exchange_movement_change_parking", 
                                                     route1, route2,  mov_list, mov_list_cost)
                    mov_list <- res_mov$mov_list
                    mov_list_cost <- res_mov$mov_list_cost 
                  }   
                }
              }
            }
          }
        }
        }
      }
    }
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost
  return(res)
  
}


exchange_movement_vc_main_tour_tc_subtour<-function(input, result, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity) {
  for (i in 1:length(result)) {
    if ((result[[i]]$type != "PTR")) {
        if (result[[i]]$type == "CVR") main_root <- return_main_route(result[[i]]$route)
        else main_root <- result[[i]]$route
        for (j in 2:(length(main_root)-1)) {
          clienti <- main_root[j]
          if (clienti <= input$n1) {
              for ( z in 1:length(result)) {
                if ((result[[z]]$type == "CVR") && (i!=z)) {
                  subroutes <- return_subroutes(result[[z]]$route, input$n1)
                  for (s in 1:length(subroutes))  {
                    for (t in 2:(length(subroutes[[s]]$tour)-1)) {
                      clientz <- subroutes[[s]]$tour[t]
                      if ((clientz > input$n1)&&(is_in_vecinity(clienti, clientz, vecinity, perc_vecinity))) {
                        if (result[[i]]$type == "CVR") main_root2 <- return_main_route(result[[i]]$route)
                        else main_root2 <- result[[i]]$route
                        for (w in 2:(length(main_root2)-1)) {
                          clientw <- main_root2[w]
                          if ((clientw <= input$n1)&&(clientw != clienti)&&(clienti != subroutes[[s]]$root)&&(i!=z)&&(sum(result[[i]]$route==clienti)==1)) {
                            # new routes
                            #print(paste0("add ", clienti, " ", clientz," ", clientw  ))
                            route1 <- replace_route_client_subroute(clienti, clientz, clientw, result[[i]]$route)
                            route2 <- replace_route_client(clientz, clienti, result[[z]]$route)
                            #print(route1)
                            #print(route2)
                            # feasibility
                            feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                            feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem, penalty_capacity) 
                            # add to mov list
                            if (feasible_route1 && feasible_route2) {
                              res_mov <- add_movements_to_list(input, result, i, z, "exchange_movement_vc_main_tour_tc_subtour", 
                                                               route1, route2,  mov_list, mov_list_cost)
                              mov_list <- res_mov$mov_list
                              mov_list_cost <- res_mov$mov_list_cost 
                            }                        
                          }
                        }
                      }
                    }
                  }
                }
              }
          }
        }
    }
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  return(res)
  
}



exchange_movement_tc_PTR_and_vc_in_main_tour<-function(input, result, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity) {
  for (i in 1:length(result)) {
    if ((result[[i]]$type == "PTR")) {
      for (j in 2:(length(result[[i]]$route)-1)) {
        if (result[[i]]$route[j] > input$n1) {
          clienti <- result[[i]]$route[j]
          for (z in 1:length(result)) {
            if ((i!=z)&&(result[[z]]$type != "PTR")) {
              if (result[[z]]$type == "CVR") main_root <- return_main_route(result[[z]]$route)
              else main_root <- result[[z]]$route
              for (t in 2:(length(main_root)-1)) {
                  clientz <- main_root[t]
                  if ((clientz <= input$n1)&&(sum(clientz==result[[z]]$route)==1)&&(is_in_vecinity(clienti, clientz, vecinity, perc_vecinity))) {
                    if (result[[z]]$type == "CVR") main_root2 <- return_main_route(result[[z]]$route)
                    else main_root2 <- result[[z]]$route
                    for (w in 2:(length(main_root2)-1)) {
                      clientw <- main_root2[w]
                      if ((clientw <= input$n1)&&(clientw != clientz)) {
                        # new routes
                        route1 <- replace_route_client(clienti, clientz, result[[i]]$route)
                        route2 <- replace_route_client_subroute(clientz, clienti, clientw, result[[z]]$route)

                        # feasibility
                        feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                        feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem, penalty_capacity) 
                        # add to mov list
                        if (feasible_route1 && feasible_route2) {
                          #print("OLD")
                          ##print(result[[i]]$route)
                          ##print(result[[z]]$route)
                          ##print(paste0("client ", clienti, " ", clientz, " ", clientw))
                          ##print("NEW")
                          #print(route1)
                          #print(route2)
                          
                          res_mov <- add_movements_to_list(input, result, i, z, "exchange_movement_tc_PTR_and_vc_in_main_tour", 
                                                           route1, route2,  mov_list, mov_list_cost)
                          mov_list <- res_mov$mov_list
                          mov_list_cost <- res_mov$mov_list_cost 
                        }   
                      }
                    }
                  }
              }
            }
          }
        }
      }
    }
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  return(res)
  
}


exchange_movement_client_short_subtour_and_client_in_main_tour<-function(input, result, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity) {

  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        if (length(subroutes[[s]]$tour)==3) {
            
            clienti <- subroutes[[s]]$tour[2]
              for (z in 1:length(result)) {
                if (i!=z) {
                    if (result[[z]]$type == "CVR") route_z <- result[[z]]$main_tour
                    else route_z <- result[[z]]$route
                    for (t in 2:(length(route_z)-1)) {
                      clientz <- route_z[t]
                      if ((clientz <= input$n1)&&(sum(clientz==result[[z]]$route)==1)&&(is_in_vecinity(clienti, clientz, vecinity, perc_vecinity))) {
                        if (result[[z]]$type == "PTR") {
                          # new routes
                          route1 <- replace_subroute_vc_2(subroutes[[s]], clientz, result[[i]]$route)
                          route2 <- replace_route_client(clientz, clienti, result[[z]]$route)

                          # feasibility
                          feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                          feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem, penalty_capacity) 
                          # add to mov list
                          if (feasible_route1 && feasible_route2) {
                            res_mov <- add_movements_to_list(input, result, i, z, "exchange_movement_client_short_subtour_and_client_in_main_tour", 
                                                             route1, route2,  mov_list, mov_list_cost)
                            mov_list <- res_mov$mov_list
                            mov_list_cost <- res_mov$mov_list_cost 
                          }
                        }
                        else {
                          for (w in 2:(length(route_z)-1)) {
                            clientw <- route_z[w]
                            if ((clientw <= input$n1)&&(clientw != clientz)) {
                                # new routes
                                #route1 <- replace_route_client_vc_subroute(clienti, clientz, result[[i]]$route)
                                route1 <- replace_subroute_vc_2(subroutes[[s]], clientz, result[[i]]$route)
                                route2 <- replace_route_client_subroute(clientz, clienti, clientw, result[[z]]$route)
                              
                                # feasibility
                                feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                                feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem, penalty_capacity) 
                                # add to mov list
                                if (feasible_route1 && feasible_route2) {

                                  res_mov <- add_movements_to_list(input, result, i, z, "exchange_movement_client_short_subtour_and_client_in_main_tour", 
                                                                   route1, route2,  mov_list, mov_list_cost)
                                  mov_list <- res_mov$mov_list
                                  mov_list_cost <- res_mov$mov_list_cost 
                                }
                            }
                          } 
                        }
                      }
                    }
                }
            }
          
        }
      }
    }
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost
  
  return(res)
}


exchange_movement_client_subtour_and_vc_creating_subtour<-function(input, result, mov_list, mov_list_cost, type_problem, vecinity, perc_vecinity, penalty_capacity) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        if (length(subroutes[[s]]$tour)==3) {
          for (j in 2:(length(subroutes[[s]]$tour)-1)) {
            clienti <- subroutes[[s]]$tour[j]
            for (z in 1:length(result)) {
              if ((i!=z)&&(result[[z]]$type == "CVR")) {
                subroutes2 <- return_subroutes(result[[z]]$route, input$n1)
                for (ss in 1:length(subroutes2))  {
                  for (t in 2:(length(subroutes2[[ss]]$tour)-1)) {
                    clientz <- subroutes2[[ss]]$tour[t]
                    if (( clientz <= input$n1)&&(is_in_vecinity(clienti, clientz, vecinity, perc_vecinity))) {

                          # new routes
                          route1 <- replace_route_client_vc_subroute(clienti, clientz, result[[i]]$route)
                          route2 <- replace_route_client(clientz, clienti, result[[z]]$route)
                          
                          # feasibility
                          feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                          feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem, penalty_capacity) 
                          # add to mov list
                          if (feasible_route1 && feasible_route2) {
                            res_mov <- add_movements_to_list(input, result, i, z, "exchange_movement_client_subtour_and_vc_creating_subtour", 
                                                             route1, route2,  mov_list, mov_list_cost)
                            mov_list <- res_mov$mov_list
                            mov_list_cost <- res_mov$mov_list_cost 
                          }
                        
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  
  return(res)
}


replace_subroute_vc<-function(subroute, clientvc, route){
  
  # delete subroutes
  route <- delete_subroute(subroute$tour, route)

  for (i in 1:length(route)) {
    if (route[i] == subroute$root) {
      route[i] <- clientvc
    }
  }
  

  return(route)
}


replace_subroute_vc_2<-function(subroute, clientvc, route){
  
  # delete subroutes
  route <- delete_subroute(subroute$tour, route)
  
  new_route <- c(0)
  no_more_root <- 1
  for (i in 2:length(route)) {
    if ((route[i] == subroute$root)&&(no_more_root)) {
      new_route <- c(new_route, route[i])
      new_route <- c(new_route, clientvc)
      no_more_root <- 0
    } else new_route <- c(new_route, route[i])
  }
  
  
  return(new_route)
}



replace_route_client<-function(clienti, clientj, route){
  for (i in 1:length(route)) {
    if (route[i] == clienti) {
      route[i] <- clientj
    }
  }
  
  return(route)
}



replace_route_client_subroute<-function(clienti, clientj, clientz, route){
  # delete clienti
  new_route <- c(0)
  counter <- 0
  for (i in 2:length(route)) {
    if (route[i] != clienti) {
       new_route <- c(new_route, route[i])
    }
    if (route[i] == clientz) {
      counter <- counter + 1
    }
  }
  # add clientj as a subtour in clientz
  if (counter == 1) {
    new_route2 <- c(0)
    for (i in 2:length(new_route)) {
      if ((new_route[i] == clientz)) {
        new_route2 <- c(new_route2, clientz)
        new_route2 <- c(new_route2, clientj)
        new_route2 <- c(new_route2, clientz)
      } else {
        new_route2 <- c(new_route2, new_route[i])
      }
    }
  } else {
    index <- which (new_route == clientz)
    new_route2 <- c(0)
    for (i in 2:length(new_route)) {
      if (i == index[1]) {
        new_route2 <- c(new_route2, clientz)
        new_route2 <- c(new_route2, clientj)
        new_route2 <- c(new_route2, clientz)
      } else {
        new_route2 <- c(new_route2, new_route[i])
      }
    }
  }
  
  return(new_route2)
}



replace_route_client_vc_subroute<-function(clienti, clientj, route){
  # delete clienti from subroute
  for (i in 2:length(route)) {
    if (route[i] == clienti) {
      index_client_i <- i    
    }
  }
  
  new_route <- c(route[1:(index_client_i-1)], clientj, route[(index_client_i+2):length(route)])
  
  return(new_route)
}



replace_subroute<-function(old_subroute, subroute, route){
  root <- old_subroute[1]

  route <- delete_subroute(old_subroute, route)
  
  route <- add_subroute(root, subroute, route)
  
  return(route)
}



add_subroute<-function(clienti, subroute, route){
  
  counter <- 0
  for (i in 2:length(route)) {
    if (route[i] == clienti) {
      counter <- counter + 1
    }
  }
  
  for (i in 2:length(route)) {
    if (route[i] == clienti) {
      index_client_i <- i
      break;
    }
  }

  if (counter == 1) {
    subroute <- subroute[2:(length(subroute)-1)]
    # add subroute
    new_route <- c(route[1:(index_client_i)], subroute, route[(index_client_i):length(route)])
  }
  else {
    subroute <- subroute[2:(length(subroute)-1)]
    # add subroute
    new_route <- c(route[1:(index_client_i)], subroute, route[(index_client_i):length(route)])
    
  }
  return(new_route)
}



delete_subroute<-function(subroute, route){
  
  subroute <- subroute[2:(length(subroute)-1)]
  new_route <- c(0)
  
  for (i in 2:length(route)) {
    if (sum(subroute==route[i])==0) {
      new_route <- c(new_route, route[i])
    }
  }
  
  new_route2 <- c(0)
  for (i in 2:length(new_route)) {
    if (new_route[i]!=new_route[i-1]) {
      new_route2 <- c(new_route2, new_route[i])
    }
  }
  
  return(new_route2)
}



delete_client<-function(client, route){
  
  new_route <- c(0)
  
  for (i in 2:length(route)) {
    if (client!=route[i]) {
      new_route <- c(new_route, route[i])
    }
  }
  
  return(new_route)
}


split_subroute<-function(clienti, route){
  
  for (i in 2:length(route)) {
    if (route[i] == clienti) {
      index_client_i <- i    
    }
  }
  
  for (i in index_client_i:1) {
    if (sum(route[i] == route)>1) {
      root1 <- i    
      break
    }
  }
  
  for (i in index_client_i:length(route)) {
    if (sum(route[i] == route)>1) {
      root2 <- i   
      break
    }
  }
  
  route <-c( route[1:(index_client_i-1)], route[root1],  clienti, route[root2], route[(index_client_i+1):length(route)])
  
  return(route)
}



add_movements_to_list<-function(input, result, indexr1, indexr2, string, route1, route2, mov_list, mov_list_cost) {
  
  if (length(route2) > 1)  {
    new_cost <- local_cost(route1, input$matriz.distancia) + local_cost(route2, input$matriz.distancia)
    old_cost <- local_cost(result[[indexr1]]$route, input$matriz.distancia) + local_cost(result[[indexr2]]$route, input$matriz.distancia)
  }
  else {
    new_cost <- local_cost(route1, input$matriz.distancia)
    old_cost <- local_cost(result[[indexr1]]$route, input$matriz.distancia) 
  }
  counter <- length(mov_list) + 1
      
  mov_list[[counter]] <- list()
  mov_list[[counter]]$indexr1 <- indexr1
  mov_list[[counter]]$indexr2 <- indexr2
  mov_list[[counter]]$mov_name <- string
  mov_list[[counter]]$route1 <- route1
  mov_list[[counter]]$route2 <- route2
  
  mov_list_cost[[counter]] <- old_cost - new_cost

  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost
  
  return(res)
}


is_in_vecinity<-function(clienti, clientw, vecinity, perc_vecinity){
  local_vecinity <- vecinity[[clienti]][1:ceiling(length(vecinity[[clienti]])*perc_vecinity)]
  
  if (sum(clientw %in% local_vecinity)) return (1)
  else return (0)
}
