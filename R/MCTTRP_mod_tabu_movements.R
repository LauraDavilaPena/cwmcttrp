# Algoritmo Tabu


tabu_search <- function(input, current_solution, current_cost, best_cost, type_problem, maxi, counter_i, phi, pen_max) {
  print("tabu_search")
  # Extract tabulist_data vars
  tabulist_data <- init_tabulist_data(input, current_solution, pen_max)
    
  alpha <- 1
  vecinity <-  input$vecinity
  
  # inititalize parameters
  tau <- return_tau(input$n, length(current_solution)) + 1
  zeta <- runif(1)
  gamma <- runif(1)
  stop_tabu <- ceiling(sqrt((maxi-counter_i)*phi))
  best_local_solution <- current_solution
  best_local_cost <- current_cost
  best_f_solution <- current_solution
  best_f_cost <- Inf
  
  counter_not_improve <- 0
  real_iterations <- 0
  num_imp_iter <- 10
  candidate_list <- 1:input$n #sample(1:input$n, sample(ceiling(input$n/10):input$n, 1))
  perc_v <- 1 #max (min (rnorm(1, mean = 0.3, sd = 0.1), 1.0), 0.1)
  
  
  while ( counter_not_improve < stop_tabu) {
    
      if (real_iterations %% num_imp_iter == (num_imp_iter-1)) {
        current_solution <- result_improvement(input, current_solution, type_problem)
      }
      current_cost <- calculateTotalDistanceTS(input, alpha, current_solution)

#print(paste0("CURRENT SOLUTION: ", current_cost, " INFEAS : ", (calc_penalty(input, current_solution))))
      
      res <- movements_imp(input, current_solution, type_problem, vecinity, perc_v, tabulist_data$penalty_capacity, 
                                  zeta, alpha, tabulist_data$table_freq, counter_i, candidate_list)
      
      mov_list <- res$mov_list
      mov_list_cost <- res$mov_list_cost
      mov_list_cost_pen <- res$mov_list_cost_pen
      

      #concert list to vector
      if (length(mov_list_cost)) {
          
          index_order <- order_movs(mov_list_cost)
          counter_index_order <- 1
          
          not_in_tabu_list <- 0
          enter_penalty_loop <- 0
############
#          print("NORMAL")
#          mov_list_cost_vect <- c(mov_list_cost[[1]])
#          for (i in 2:length(mov_list_cost)) mov_list_cost_vect <- c(mov_list_cost_vect ,mov_list_cost[[i]] )
#          print(paste0( mov_list_cost_vect[index_order[1:12]]- current_cost))
#          print(paste0( index_order[1:12]))
#          
#          
############
          while ((!not_in_tabu_list)&&(counter_index_order <= length(index_order))) {
            
            if (!check_result_in_tabu_list(tabulist_data$tabulist, mov_list, mov_list_cost, index_order, counter_index_order, best_cost)) {

              not_in_tabu_list <- 1
              if ( mov_list_cost[[index_order[counter_index_order]]] < current_cost ) {
                
                
############
#print(paste0(" mov ", mov_list[[index_order[counter_index_order]]]$mov_name, " alpha: ", alpha,
#             " mov_list_cost: " , mov_list_cost[[index_order[counter_index_order]]] , 
#             " fobj current: " , calculateTotalDistanceTS(input, alpha, current_solution),
#             " counter ", counter_index_order))
#print(paste0("   routes: ", mov_list[[index_order[counter_index_order]]]$indexr1, "  ", mov_list[[index_order[counter_index_order]]]$indexr2))
#print("   clients: ")
#print(mov_list[[index_order[counter_index_order]]]$client1)      
#print(mov_list[[index_order[counter_index_order]]]$client2)  
#print(mov_list[[index_order[counter_index_order]]]$route1)      
#print(mov_list[[index_order[counter_index_order]]]$route2)    
############                
                
                  current_solution <- insert_selected_mov(input, mov_list[[index_order[counter_index_order]]] , current_solution, type_problem)
                  tabulist_data <- insert_result_in_tabulist(mov_list, index_order, counter_index_order, tabulist_data, tau)
              } else enter_penalty_loop <- 1
              
            }
            else {
              not_in_tabu_list <- 0
            }
            
            counter_index_order <- counter_index_order + 1
            
          }
          
          index_order2 <- order_movs(mov_list_cost_pen)
          counter_index_order2 <- 1
          not_in_tabu_list <- 0
          
          
if (enter_penalty_loop) {
############
#  print("PROC")
#  mov_list_cost_vect <- c(mov_list_cost_pen[[1]])
##  for (i in 2:length(mov_list_cost_pen)) mov_list_cost_vect <- c(mov_list_cost_vect ,mov_list_cost_pen[[i]] )
#  print(paste0( mov_list_cost_vect[index_order[1:12]]- current_cost))
#  print(paste0( index_order[1:12]))
#  
############
}          
          
          while((enter_penalty_loop)&&(!not_in_tabu_list)&&(counter_index_order2 <= length(index_order2)))  {
            if ((!check_result_in_tabu_list(tabulist_data$tabulist, mov_list, mov_list_cost_pen, index_order2, counter_index_order2, best_cost))) {
############              
#print(paste0(" PEN mov ", mov_list[[index_order2[counter_index_order2]]]$mov_name, " alpha: ", alpha,
#             " mov_list_cost: " , mov_list_cost[[index_order2[counter_index_order2]]], 
#             " fobj current: " , calculateTotalDistanceTS(input, alpha, current_solution),
#             " counter ", counter_index_order2))
#print(paste0("   routes: ", mov_list[[index_order2[counter_index_order2]]]$indexr1, "  ", mov_list[[index_order2[counter_index_order2]]]$indexr2))
#print("   clients: ")
#print(mov_list[[index_order2[counter_index_order2]]]$client1)      
#print(mov_list[[index_order2[counter_index_order2]]]$client2)      
#print(mov_list[[index_order2[counter_index_order2]]]$route1)      
#print(mov_list[[index_order2[counter_index_order2]]]$route2)      
############
              
                  not_in_tabu_list <- 1
                  current_solution <- insert_selected_mov(input, mov_list[[index_order2[counter_index_order2]]] , current_solution, type_problem)
                  tabulist_data <- insert_result_in_tabulist(mov_list, index_order2, counter_index_order2, tabulist_data, tau)
            }
            else not_in_tabu_list <- 0
            
            counter_index_order2 <- counter_index_order2 + 1
          }        
          
          stop <- 0
          
      } else stop <- 1

      
########      
#      print_tabu_list(tabulist_data$tabulist)
########            
      
      current_cost <- calculateTotalDistanceTS(input, alpha, current_solution)
      if ((current_cost < best_local_cost)) { #&&(!calc_penalty(input, current_solution))) {
        best_local_solution <- current_solution
        best_local_cost <- current_cost
        counter_not_improve <- 0
      }
      else {
        counter_not_improve <- counter_not_improve + 1
      } 
      
      if ((current_cost < best_f_cost)&&(calc_penalty(input, current_solution)==0)) {
        best_f_solution <- current_solution
        best_f_cost <- current_cost
      }
      
      if (stop) counter_not_improve <- stop_tabu
        
      print(paste0("best  ", best_local_cost, " feasibility ", calc_penalty(input, best_local_solution)))
      print(paste0("fbest ", best_f_cost,     " feasibility ", calc_penalty(input, best_f_solution)))
      print(paste0("cbest ", current_cost   , " feasibility ", calc_penalty(input, current_solution)))
      
print(paste0("best ", best_local_cost, " feasibility ", calc_penalty(input, best_local_solution), " fea best ", best_f_cost,
                   " current ", current_cost, " feasibility ", calc_penalty(input, current_solution), " end iteration ", counter_not_improve, 
                   " max ", stop_tabu, " alpha ", alpha, " tau ", tau))

#readline()
      alpha <- update_penalties(input,  alpha, gamma, current_solution)
      tabulist_data$tabulist <- update_counters_tabu_list(tabulist_data$tabulist)
  }
  
  result <- list()
  result$current_solution <- best_local_solution
  result$current_cost <- best_local_cost
  result$best_f_solution <- best_f_solution
  result$best_f_cost <- best_f_cost
  
  return(result)
}

# order movs
order_movs<-function(mov_list_cost) {
   
    if (length(mov_list_cost)>1) {
        mov_list_cost_vect <- c(mov_list_cost[[1]])
        for (i in 2:length(mov_list_cost)) mov_list_cost_vect <- c(mov_list_cost_vect ,mov_list_cost[[i]] )
        index_order <- order(mov_list_cost_vect, decreasing = FALSE)
    } else index_order <- c(1)

    return(index_order)
}

# check_result_in_tabu_list 
check_result_in_tabu_list<-function(tabulist, mov_list, mov_list_cost, index_order, counter_index_order, bestcost) {
  
  #print(paste0("mov list ", mov_list_cost[[index_order[counter_index_order]]], " best ", bestcost))
  
  if (mov_list_cost[[index_order[counter_index_order]]] >= bestcost) {
    
    if ((length(mov_list[[index_order[counter_index_order]]]$client2)>=1)&&(mov_list[[index_order[counter_index_order]]]$client2 != 0)) {
      
      tabu_check <- check_in_tabulist(tabulist, mov_list[[index_order[counter_index_order]]]$client1,
                                      mov_list[[index_order[counter_index_order]]]$indexr2)
      
      tabu_check <- tabu_check + check_in_tabulist(tabulist, mov_list[[index_order[counter_index_order]]]$client2, 
                                                             mov_list[[index_order[counter_index_order]]]$indexr1)
    } else {
      
      tabu_check <- check_in_tabulist(tabulist, mov_list[[index_order[counter_index_order]]]$client1,
                                      mov_list[[index_order[counter_index_order]]]$indexr1)
      
    }
    #print(paste0("is in tabu --> ", tabu_check))
    
  } else {
    #print(paste0("aspiration criteria"))
    tabu_check <- 0
  }
    
    return (tabu_check)
}

# insert in tabu list
insert_result_in_tabulist<-function(mov_list, index_order, counter_index_order, tabulist_data, tau) {
    
    
    if ((length(mov_list[[index_order[counter_index_order]]]$client2)>=1)&&(mov_list[[index_order[counter_index_order]]]$client2!=0)) {
      
      # tabu list
      tabulist_data$tabulist <- insert_in_tabu_list(mov_list[[index_order[counter_index_order]]]$client1, 
                                                    mov_list[[index_order[counter_index_order]]]$indexr1, tau, tabulist_data$tabulist) 
      # table freq
      for (ii in 1:length(mov_list[[index_order[counter_index_order]]]$client1)) {
        client <- mov_list[[index_order[counter_index_order]]]$client1[ii]
        tabulist_data$table_freq <- update_table_freq(tabulist_data$table_freq, client, mov_list[[index_order[counter_index_order]]]$indexr2)
      }
      
      # tabu list
      tabulist_data$tabulist <- insert_in_tabu_list(mov_list[[index_order[counter_index_order]]]$client2, 
                                      mov_list[[index_order[counter_index_order]]]$indexr2, tau, tabulist_data$tabulist) 
      # table freq
      for (ii in 1:length(mov_list[[index_order[counter_index_order]]]$client2)) {
        client <- mov_list[[index_order[counter_index_order]]]$client2[ii]
        tabulist_data$table_freq <- update_table_freq(tabulist_data$table_freq, client, mov_list[[index_order[counter_index_order]]]$indexr1)
      }
    } else {
      
      # tabu list
      tabulist_data$tabulist <- insert_in_tabu_list(mov_list[[index_order[counter_index_order]]]$client1, 
                                                    mov_list[[index_order[counter_index_order]]]$indexr1, tau, tabulist_data$tabulist) 
      # table freq
      for (ii in 1:length(mov_list[[index_order[counter_index_order]]]$client1)) {
        client <- mov_list[[index_order[counter_index_order]]]$client1[ii]
        tabulist_data$table_freq <- update_table_freq(tabulist_data$table_freq, client, mov_list[[index_order[counter_index_order]]]$indexr1)
      }
      
    }
    
    return(tabulist_data)
}

# movements_imp
movements_imp <- function(input, current_solution, type_problem, vecinity, perc_vecinity, penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {
  mov_list <- list()
  mov_list_cost <- list()
  mov_list_cost_pen <- list()

  
  #start_time <- Sys.time()
  
  # exchange moves
  res <- exchange_movement_client_subtour_and_vc_creating_subtour(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, 
                                                                  vecinity, perc_vecinity, penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  #print(paste0("   exchange_movement_client_subtour_and_vc_creating_subtour ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  
  res <- exchange_movement_change_parking(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, vecinity, perc_vecinity ,
                                          penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  
  #print(paste0("   exchange_movement_change_parking ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  # MAS LENTA 1
  res <- exchange_movement_vc_main_tour_tc_subtour(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, vecinity, perc_vecinity, 
                                                   penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  
  #print(paste0("   exchange_movement_vc_main_tour_tc_subtour ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  # MAS LENTA 3
  res <- exchange_movement_client_short_subtour_and_client_in_main_tour(input, current_solution, mov_list, mov_list_cost,mov_list_cost_pen,  type_problem, 
                                                                        vecinity, perc_vecinity, penalty_capacity, zeta, alpha, table_freq, 
                                                                        counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  #print(paste0("   exchange_movement_client_short_subtour_and_client_in_main_tour ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  
  res <- move_subroute(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, penalty_capacity, zeta, alpha, table_freq, 
                       counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  #print(paste0("   move_subroute ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  res <- exchange_ptr_and_subtour(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, penalty_capacity, zeta, alpha, 
                                  table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  #print(paste0("   exchange_ptr_and_subtour ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  # MAS LENTA 2
  res <- exchange_movement_tc_PTR_and_vc_in_main_tour(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, vecinity, 
                                                      perc_vecinity, penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list)
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost 
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  #print(paste0("   exchange_movement_tc_PTR_and_vc_in_main_tour ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  # moves in the same root
  res <- move_subroute_same_route(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, penalty_capacity, zeta, alpha, 
                                  table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  #print(paste0("   move_subroute_same_route ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  res <- create_new_subtour_vc_same_route(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, penalty_capacity, 
                                          zeta, alpha, table_freq, counter_i, candidate_list)  
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  #print(paste0("   create_new_subtour_vc_same_route ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  res <- move_vc_client_subroute_to_main_tour_and_split_same_route(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, 
                                                                   penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  #print(paste0("   move_vc_client_subroute_to_main_tour_and_split_same_route ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  # basic moves
  res <- exchange_tc_two_routes(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen,  type_problem, vecinity, perc_vecinity, 
                                penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  #print(paste0("   exchange_tc_two_routes ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  
  # MAS LENTA 4
  res <- exchange_vc_two_routes(input, current_solution, mov_list, mov_list_cost,mov_list_cost_pen,  type_problem, vecinity, perc_vecinity, 
                                penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  #print(paste0("   exchange_vc_two_routes ", difftime(Sys.time(), start_time, units = "secs")))
  #start_time <- Sys.time()
  # move function
  res <- move_tc(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen,  type_problem, vecinity, perc_vecinity, 
                 penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  # move function
  res <- move_vc(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen,  type_problem, vecinity, perc_vecinity, 
                 penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  res$mov_list_cost_pen <- mov_list_cost_pen 
  
  return(res)
}


movements_imp_tabu <- function(input, current_solution, type_problem, vecinity, perc_vecinity, penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {
  mov_list <- list()
  mov_list_cost <- list()
  mov_list_cost_pen <- list()
  
  
  #start_time <- Sys.time()
  
  # exchange moves
  res <- exchange_movement_change_parking(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, vecinity, perc_vecinity ,
                                          penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen

  res <- move_subroute(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, penalty_capacity, zeta, alpha, table_freq, 
                       counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  res <- move_subroute_same_route(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, penalty_capacity, zeta, alpha, 
                                  table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  res <- exchange_tc_two_routes(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen,  type_problem, vecinity, perc_vecinity, 
                                penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  res <- exchange_vc_two_routes(input, current_solution, mov_list, mov_list_cost,mov_list_cost_pen,  type_problem, vecinity, perc_vecinity, 
                                penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  # move function
  res <- move_tc(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen,  type_problem, vecinity, perc_vecinity, 
                                penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  # move function
  res <- move_vc(input, current_solution, mov_list, mov_list_cost, mov_list_cost_pen,  type_problem, vecinity, perc_vecinity, 
                 penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  mov_list_cost_pen <- res$mov_list_cost_pen
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  res$mov_list_cost_pen <- mov_list_cost_pen 
  
  return(res)
}


insert_selected_mov<-function(input, mov, current_solution, type_problem){

  if (length(mov$route2)>1) {
          
          # first  modification
          current_solution <- insert_element_in_solution(input, mov$route1, mov$indexr1, current_solution, type_problem)

          # second modification
          current_solution <- insert_element_in_solution(input, mov$route2, mov$indexr2, current_solution, type_problem)

  } else {
    
          # modification
          current_solution <- insert_element_in_solution(input, mov$route1, mov$indexr1, current_solution, type_problem)

  }

  return(current_solution)
}


insert_element_in_solution<-function(input, new_route, pos, current_solution, type_problem){
  
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
      
      #tabulist <- insert_in_tabu_list(current_solution[[pos]]$route, tabulist, max_size_tabu_list) 

  
  #res_i <- list()
  #res_i$current_solution <- current_solution
  #res_i$tabulist <- tabulist
  
  return(current_solution)
}


exchange_tc_two_routes<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, vecinity, perc_vecinity, 
                                 penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {
  
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
                    if ((clientw > input$n1)&&(is_in_vecinity(clienti, clientw, vecinity, perc_vecinity))&&(sum(candidate_list==clienti)>0)) {
                        # create routes  
                        route1 <- replace_route_client(clienti, clientw, result[[i]]$route)
                        route2 <- replace_route_client(clientw, clienti, result[[w]]$route)
                        # feasibility
                        feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                        feasible_route2 <- check_feasibility(result, route2, input, result[[w]]$type, type_problem, penalty_capacity) 
                        
                        # add to mov list
                        if (feasible_route1 && feasible_route2) {
                          res_mov <- add_movements_to_list(input, result, i, w, c(clienti), c(clientw), "exchange_tc_two_routes", 
                                                           route1, route2,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                           zeta, alpha, table_freq, counter_i)
                          mov_list <- res_mov$mov_list
                          mov_list_cost <- res_mov$mov_list_cost 
                          mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                          
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
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
  
}


move_tc<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, vecinity, perc_vecinity, 
                                 penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {
  
  for (i in 1:(length(result)-1)) {
    if ((result[[i]]$type != "PVR") && (length(result[[i]]$route)>3)) {
      for (j in 2:(length(result[[i]]$route)-1)) {
        # is not a depot and is a tc
        if (( sum(result[[i]]$route == result[[i]]$route[j]) == 1 ) &&  (result[[i]]$route[j] > input$n1)) {
            clienti <- result[[i]]$route[j]
            for (w in (1:length(result))) {
              if (i!=w) {
                # subroute
                if (result[[w]]$type == "CVR") {
                  subroutes <- return_subroutes(result[[w]]$route, input$n1)
                  for (s in 1:length(subroutes)) {
                      for (t in 1:(length(subroutes[[s]]$tour)-1)) {
                        # create routes 
                        route1 <- delete_node_in_route(j, result[[i]]$route)
                        route2 <- add_node_in_route(which(subroutes[[s]]$tour[t]==result[[w]]$route), clienti, result[[w]]$route)
                        
                        # feasibility
                        feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                        feasible_route2 <- check_feasibility(result, route2, input, result[[w]]$type, type_problem, penalty_capacity) 
                        # add to mov list
                        if (feasible_route1 && feasible_route2) {
                          res_mov <- add_movements_to_list(input, result, i, w, c(clienti), 0, "move_tc", 
                                                           route1, route2,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                           zeta, alpha, table_freq, counter_i)
                          mov_list <- res_mov$mov_list
                          mov_list_cost <- res_mov$mov_list_cost 
                          mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                        } 
                      }
                  }
                }
                # PTR
                else if (result[[w]]$type == "PTR") {
                  for (t in 1:(length(result[[w]]$route)-1)) {
                    # create routes 
                    route1 <- delete_node_in_route(j, result[[i]]$route)
                    route2 <- add_node_in_route(t, clienti, result[[w]]$route)
                    
                    # feasibility
                    feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                    feasible_route2 <- check_feasibility(result, route2, input, result[[w]]$type, type_problem, penalty_capacity) 
                    # add to mov list
                    if (feasible_route1 && feasible_route2) {
                      res_mov <- add_movements_to_list(input, result, i, w, c(clienti), 0, "move_tc", 
                                                       route1, route2,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                       zeta, alpha, table_freq, counter_i)
                      mov_list <- res_mov$mov_list
                      mov_list_cost <- res_mov$mov_list_cost 
                      mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                    } 
                  }
                }
                # PVR
                else {
                  for (t in 2:(length(result[[w]]$route)-1)) {
                    # create routes 
                    route1 <- delete_node_in_route(j, result[[i]]$route)
                    route2 <- add_subroute_mov(t, clienti, result[[w]]$route)
                    
                    # feasibility
                    feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                    feasible_route2 <- check_feasibility(result, route2, input, result[[w]]$type, type_problem, penalty_capacity) 
                    # add to mov list
                    if (feasible_route1 && feasible_route2) {
                      res_mov <- add_movements_to_list(input, result, i, w, c(clienti), 0, "move_tc", 
                                                       route1, route2,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                       zeta, alpha, table_freq, counter_i)
                      mov_list <- res_mov$mov_list
                      mov_list_cost <- res_mov$mov_list_cost 
                      mov_list_cost_pen <- res_mov$mov_list_cost_pen 
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
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
  
}

move_vc<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, vecinity, perc_vecinity, 
                  penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {
  
  for (i in 1:(length(result)-1)) {
    if ((length(result[[i]]$route)>3)) {
      for (j in 2:(length(result[[i]]$route)-1)) {
        # is not a depot and is a vc
        if (( sum(result[[i]]$route == result[[i]]$route[j]) == 1 ) &&  (result[[i]]$route[j] <= input$n1)) {
          clienti <- result[[i]]$route[j]
          for (w in (1:length(result))) {
            if (i!=w) {
              if (result[[w]]$type == "CVR") main_root <- return_main_route(result[[w]]$route)
              else main_root <- result[[w]]$route
              for (t in 1:(length(main_root)-1)) {
                if (sum(main_root[t] == result[[w]]$route)==1) {
                    # create routes 
                    route1 <- delete_node_in_route(j, result[[i]]$route)
                    route2 <- add_node_in_route(which(main_root[t]==result[[w]]$route), clienti, result[[w]]$route)
                    
                    # feasibility
                    feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                    feasible_route2 <- check_feasibility(result, route2, input, result[[w]]$type, type_problem, penalty_capacity) 
                    # add to mov list
                    if (feasible_route1 && feasible_route2) {
                      res_mov <- add_movements_to_list(input, result, i, w, c(clienti), 0, "move_tc", 
                                                       route1, route2,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                       zeta, alpha, table_freq, counter_i)
                      mov_list <- res_mov$mov_list
                      mov_list_cost <- res_mov$mov_list_cost 
                      mov_list_cost_pen <- res_mov$mov_list_cost_pen 
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
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
  
}

exchange_vc_two_routes<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, vecinity,
                                 perc_vecinity, penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {
  
  for (i in 1:(length(result)-1)) {
    for (j in 2:(length(result[[i]]$route)-1)) {
        if ( sum(result[[i]]$route == result[[i]]$route[j]) == 1 ) {
          clienti <- result[[i]]$route[j]
          if (clienti <= input$n1) {
            for (w in (i+1):length(result)) {
              for (t in 2:(length(result[[w]]$route)-1)) {
                  if ((i!=w)&&(sum(result[[w]]$route == result[[w]]$route[t]) == 1)) {
                    clientw <- result[[w]]$route[t]
                    if ((clientw <= input$n1)&& (is_in_vecinity(clienti, clientw, vecinity, perc_vecinity)) && (sum(candidate_list==clienti)>0)) {
                      # create routes  
                      route1 <- replace_route_client(clienti, clientw, result[[i]]$route)
                      route2 <- replace_route_client(clientw, clienti, result[[w]]$route)
                      
                      # feasibility
                      feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                      feasible_route2 <- check_feasibility(result, route2, input, result[[w]]$type, type_problem, penalty_capacity) 
                      
                      
                      # add to mov list
                      if (feasible_route1 && feasible_route2) {
                        res_mov <- add_movements_to_list(input, result, i, w,  c(clienti), c(clientw), 
                                                         "exchange_vc_two_routes", route1, route2,  mov_list, 
                                                         mov_list_cost,  mov_list_cost_pen, zeta, alpha, table_freq, counter_i)
                        mov_list <- res_mov$mov_list
                        mov_list_cost <- res_mov$mov_list_cost 
                        mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                        
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
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
  
}


exchange_ptr_and_subtour<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, penalty_capacity,
                                   zeta, alpha, table_freq, counter_i, candidate_list) {
  
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
                res_mov <- add_movements_to_list(input, result, i, z,  subroutei[2:(length(subroutei)-1)], 
                                                 subroutez[2:(length(subroutez)-1)], "exchange_ptr_and_subtour", 
                                                 route1, route2,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                 zeta, alpha, table_freq, counter_i)
                mov_list <- res_mov$mov_list
                mov_list_cost <- res_mov$mov_list_cost 
                mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                
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
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
  
}


move_vc_client_subroute_to_main_tour_and_split_same_route<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, 
                                                                    penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes)) {
        for (j in 2:(length(subroutes[[s]]$tour)-1)) {
          clienti <- subroutes[[s]]$tour[j]
          prev <- subroutes[[s]]$tour[j-1]
          post <- subroutes[[s]]$tour[j+1]
          if ((clienti <= input$n1) && (sum(prev==subroutes[[s]]$tour)==1) &&(sum(post==subroutes[[s]]$tour)==1)&&(sum(candidate_list==clienti)>0)){

            route1 <- split_subroute(clienti, result[[i]]$route)
            
            # feasibility
            feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
            # add to mov list
            if (feasible_route1) {
              res_mov <- add_movements_to_list(input, result, i, 0, clienti, 0, 
                                               "move_vc_client_subroute_to_main_tour_and_split_same_route", 
                                               route1, 0,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                               zeta, alpha, table_freq, counter_i)
              mov_list <- res_mov$mov_list
              mov_list_cost <- res_mov$mov_list_cost 
              mov_list_cost_pen <- res_mov$mov_list_cost_pen 
              
            } 
            
          }
        }
      }
    }
    
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
  
}


move_subroute<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem,penalty_capacity, 
                        zeta, alpha, table_freq, counter_i, candidate_list) {
  
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
                  res_mov <- add_movements_to_list(input, result, i, z, subroutei[2:(length(subroutei)-1)], 0,"move_subroute", 
                                                   route1, route2,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                   zeta, alpha, table_freq, counter_i)
                  mov_list <- res_mov$mov_list
                  mov_list_cost <- res_mov$mov_list_cost 
                  mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                  
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
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
  
}


move_subroute_same_route<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, 
                                   penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {
  
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
                  res_mov <- add_movements_to_list(input, result, i, 0, subroutei[2:(length(subroutei)-1)], 0, 
                                                   "move_subroute_same_route", 
                                                   route1, 0,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                   zeta, alpha, table_freq, counter_i)
                  mov_list <- res_mov$mov_list
                  mov_list_cost <- res_mov$mov_list_cost 
                  mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                  
                } 
              }
        }
      }
    }
  }
  
  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost 
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
  
}


create_new_subtour_vc_same_route<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, 
                                           penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type != "PTR") {
      if (result[[i]]$type == "CVR") main_root <- return_main_route(result[[i]]$route)
      else main_root <- result[[i]]$route
      for (t in 2:(length(main_root)-1)) {
        if ((main_root[t]<= input$n1)&&(sum(main_root[t]==result[[i]]$route)==1)){
          clienti <- main_root[t]
          for (z in 2:(length(main_root)-1)) {
            if ((main_root[z]<= input$n1)&&(sum(main_root[z]==result[[i]]$route)==1)&&(main_root[z]!=main_root[t])&&(sum(candidate_list==clienti)>0)){
              
              clientz <- main_root[z]
               
              # routes
              route1 <- delete_client(clientz, result[[i]]$route)
              route1 <- add_subroute(clienti, c(clienti, clientz, clienti), route1)
              
              # feasibility
              feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
              # add to mov list
              if (feasible_route1) {
                res_mov <- add_movements_to_list(input, result, i, 0, c(clientz), 0, "create_new_subtour_vc_unique", 
                                                 route1, 0,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                 zeta, alpha, table_freq, counter_i)
                mov_list <- res_mov$mov_list
                mov_list_cost <- res_mov$mov_list_cost 
                mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                
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
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
  
}


exchange_movement_change_parking<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, 
                                           vecinity, perc_vecinity, penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {
  
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
                if (( clientz <= input$n1 )&&(is_in_vecinity(clienti, clientz, vecinity, perc_vecinity))&&(sum(candidate_list==clienti)>0)) {
                  
                  route1 <- replace_subroute_vc(subroutes[[s]], clientz, result[[i]]$route)
                  route2 <- replace_route_client(clientz, clienti, result[[z]]$route)
                  route2 <- add_subroute(clienti, subroutes[[s]]$tour, route2)
                  
                  # feasibility
                  feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                  feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem, penalty_capacity) 
                  # add to mov list
                  if (feasible_route1 && feasible_route2) {
                    subroutei <- subroutes[[s]]$tour
                    
                    
                    res_mov <- add_movements_to_list(input, result, i, z, subroutei[2:(length(subroutei)-1)], 0,
                                                     "exchange_movement_change_parking", 
                                                     route1, route2,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                     zeta, alpha, table_freq, counter_i)
                    mov_list <- res_mov$mov_list
                    mov_list_cost <- res_mov$mov_list_cost 
                    mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                    
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
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
  
}


exchange_movement_vc_main_tour_tc_subtour<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, 
                                                    type_problem, vecinity, perc_vecinity, penalty_capacity, 
                                                    zeta, alpha, table_freq, counter_i, candidate_list) {
  for (i in 1:length(result)) {
    if ((result[[i]]$type != "PTR")) {
        if (result[[i]]$type == "CVR") main_root <- return_main_route(result[[i]]$route)
        else main_root <- result[[i]]$route
        for (j in 2:(length(main_root)-1)) {
          clienti <- main_root[j]
          # vc
          if (clienti <= input$n1) {
              for ( z in 1:length(result)) {
                if ((result[[z]]$type == "CVR") && (i!=z)) {
                  subroutes <- return_subroutes(result[[z]]$route, input$n1)
                  for (s in 1:length(subroutes))  {
                    for (t in 2:(length(subroutes[[s]]$tour)-1)) {
                      clientz <- subroutes[[s]]$tour[t]
                      # tc
                      if ((clientz > input$n1)&&(is_in_vecinity(clienti, clientz, vecinity, perc_vecinity))&&(sum(candidate_list==clienti)>0)) {
                        if (result[[i]]$type == "CVR") main_root2 <- return_main_route(result[[i]]$route)
                        else main_root2 <- result[[i]]$route
                        clientw <- return_close_client(clienti, main_root2, vecinity) #main_root2[w]
                        if ((clientw <= input$n1)&&(clientw != clienti)&&
                              (clienti != subroutes[[s]]$root)&&(i!=z)&&(sum(result[[i]]$route==clienti)==1)) {
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
                                  res_mov <- add_movements_to_list(input, result, i, z, c(clienti), c(clientz), 
                                                                   "exchange_movement_vc_main_tour_tc_subtour", 
                                                                   route1, route2,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                                   zeta, alpha, table_freq, counter_i)
                                  mov_list <- res_mov$mov_list
                                  mov_list_cost <- res_mov$mov_list_cost 
                                  mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                                  
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
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
  
}



exchange_movement_tc_PTR_and_vc_in_main_tour<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, vecinity, perc_vecinity, 
                                                       penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {
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
                  if ((clientz <= input$n1)&&(sum(clientz==result[[z]]$route)==1)&&(is_in_vecinity(clienti, clientz, vecinity, perc_vecinity))&&(sum(candidate_list==clienti)>0)) {
                    if (result[[z]]$type == "CVR") main_root2 <- return_main_route(result[[z]]$route)
                    else main_root2 <- result[[z]]$route
                    
                    #for (w in 2:(length(main_root2)-1)) {
                    #  clientw <- main_root2[w]
                    clientw <- return_close_client(clienti, main_root2, vecinity)
                      if ((clientw <= input$n1)&&(clientw != clientz)) {
                        # new routes
                        route1 <- replace_route_client(clienti, clientz, result[[i]]$route)
                        route2 <- replace_route_client_subroute(clientz, clienti, clientw, result[[z]]$route)

                        # feasibility
                        feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                        feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem, penalty_capacity) 
                        
                        # add to mov list
                        if (feasible_route1 && feasible_route2) {
                          
                          res_mov <- add_movements_to_list(input, result, i, z,  c(clienti), c(clientz), 
                                                           "exchange_movement_tc_PTR_and_vc_in_main_tour", 
                                                           route1, route2,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                           zeta, alpha, table_freq, counter_i)
                          mov_list <- res_mov$mov_list
                          mov_list_cost <- res_mov$mov_list_cost 
                          mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                          
                        }   
                      }
                    #}
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
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
  
}


exchange_movement_client_short_subtour_and_client_in_main_tour<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, 
                                                                         vecinity, perc_vecinity, penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {

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
                      if ((clientz <= input$n1)&&(sum(clientz==result[[z]]$route)==1)
                        &&(is_in_vecinity(clienti, clientz, vecinity, perc_vecinity))&&
                          (sum(candidate_list==clienti)>0)&&(sum(candidate_list==clientz)>0)) {
                        if (result[[z]]$type == "PTR") {
                          # new routes
                          route1 <- replace_subroute_vc_2(subroutes[[s]], clientz, result[[i]]$route)
                          route2 <- replace_route_client(clientz, clienti, result[[z]]$route)

                          # feasibility
                          feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                          feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem, penalty_capacity) 
                          # add to mov list
                          if (feasible_route1 && feasible_route2) {
                            res_mov <- add_movements_to_list(input, result, i, z,  c(clienti), c(clientz), 
                                                             "exchange_movement_client_short_subtour_and_client_in_main_tour", 
                                                             route1, route2,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                             zeta, alpha, table_freq, counter_i)
                            mov_list <- res_mov$mov_list
                            mov_list_cost <- res_mov$mov_list_cost 
                            mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                            
                          }
                        }
                        else {
                          #for (w in 2:(length(route_z)-1)) {
                          #  clientw <- route_z[w]
                            clientw <- return_close_client(clienti, route_z, vecinity)
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

                                  res_mov <- add_movements_to_list(input, result, i, z,  c(clienti), c(clientz),
                                                                   "exchange_movement_client_short_subtour_and_client_in_main_tour", 
                                                                   route1, route2,  mov_list, mov_list_cost,  mov_list_cost_pen,
                                                                   zeta, alpha, table_freq, counter_i)
                                  mov_list <- res_mov$mov_list
                                  mov_list_cost <- res_mov$mov_list_cost 
                                  mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                                  
                                }
                            }
                          #} 
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
  res$mov_list_cost_pen  <- mov_list_cost_pen
  
  return(res)
}


exchange_movement_client_subtour_and_vc_creating_subtour<-function(input, result, mov_list, mov_list_cost, mov_list_cost_pen, type_problem, 
                                                                   vecinity, perc_vecinity, penalty_capacity, zeta, alpha, table_freq, counter_i, candidate_list) {
  
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
                    if ((clientz <= input$n1) &&(is_in_vecinity(clienti, clientz, vecinity, perc_vecinity))&&
                        (sum(candidate_list==clienti)>0)&&(sum(candidate_list==clientz)>0)) {

                          # new routes
                          route1 <- replace_route_client_vc_subroute(clienti, clientz, result[[i]]$route)
                          route2 <- replace_route_client(clientz, clienti, result[[z]]$route)
                          
                          # feasibility
                          feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem, penalty_capacity) 
                          feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem, penalty_capacity) 
                          # add to mov list
                          if (feasible_route1 && feasible_route2) {
                            res_mov <- add_movements_to_list(input, result, i, z,  c(clienti), c(clientz),
                                                             "exchange_movement_client_subtour_and_vc_creating_subtour", 
                                                             route1, route2,  mov_list, mov_list_cost, mov_list_cost_pen,
                                                             zeta, alpha, table_freq, counter_i)
                            mov_list <- res_mov$mov_list
                            mov_list_cost <- res_mov$mov_list_cost 
                            mov_list_cost_pen <- res_mov$mov_list_cost_pen 
                            
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
  res$mov_list_cost_pen  <- mov_list_cost_pen
  return(res)
}


delete_node_in_route<-function(j, route) {
  
  delete_subtour <- 0
  if ((j>2)&&(j<(length(route)-1))&&(route[j-1]==route[j+1])) delete_subtour <- 1
  
  if (!delete_subtour) {
    route <- c(route[1:(j-1)], route[(j+1):length(route)])
  }
  else {
    route <- c(route[1:(j-1)], route[(j+2):length(route)])
  }
  
  return(route)
}

add_node_in_route<-function(pos, client, route) {
  
  route <- c(route[1:pos[1]], client, route[(pos[1]+1):length(route)])
  
  return(route)
}


add_subroute_mov<-function(pos, client, route) {
  
  route <- c(route[1:pos[1]], client, route[(pos[1]):length(route)])
  
  return(route)
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



add_movements_to_list<-function(input, result, indexr1, indexr2, client1, client2, string, 
                                route1, route2, mov_list, mov_list_cost, mov_list_cost_pen, zeta, alpha, table_freq, counter_i) {
  
  if ((length(route2)>1))  {
    # type
    new_type1 <- check_new_type(result[[indexr1]]$type, route1, input)
    # penalty_freq
    if ((zeta!=0)&&(alpha!=0)) {
        penalty_freq <- 0
        for (i in 1:length(client1)) {
          penalty_freq <- penalty_freq + return_table_freq(table_freq, client1[i], indexr1)
        }
        if (length(client2)&&(client2!=0)) {
          for (i in 1:length(client2)) {
            penalty_freq <- penalty_freq + return_table_freq(table_freq, client2[i], indexr2)
          }
        }
        penalty_freq <- 1 + zeta * penalty_freq / counter_i
        # cost
        result[[indexr1]]$route <- route1
        result[[indexr2]]$route <- route2
        new_cost <- calculateTotalDistanceTS(input, alpha, result)
        new_cost_pen <- calculateTotalDistanceTS(input, alpha, result) * penalty_freq
    } else {
        # cost
        result[[indexr1]]$route <- route1
        result[[indexr2]]$route <- route2
        new_cost <- calculateTotalDistanceTS(input, alpha, result)  
        new_cost_pen <- 0
    }

  }
  else {
    
    # type
    new_type1 <- check_new_type(result[[indexr1]]$type, route1, input)
    # penalty_freq
    if ((zeta!=0)&&(alpha!=0)) {
      penalty_freq <- 0
      for (i in 1:length(client1)) {
        penalty_freq <- penalty_freq + return_table_freq(table_freq, client1[i], indexr1)
      }
      penalty_freq <- 1 + zeta * penalty_freq / counter_i
      # cost
      result[[indexr1]]$route <- route1
      new_cost <- calculateTotalDistanceTS(input, alpha, result)
      new_cost_pen <- calculateTotalDistanceTS(input, alpha, result) * penalty_freq
    } else {
      # cost
      result[[indexr1]]$route <- route1
      new_cost <- calculateTotalDistanceTS(input, alpha, result)    
      new_cost_pen <-0  
    }

    result[[indexr1]]$route <- route1
    new_cost <- calculateTotalDistanceTS(input, alpha, result)
    
  }
  counter <- length(mov_list) + 1
      
  mov_list[[counter]] <- list()
  mov_list[[counter]]$indexr1 <- indexr1
  mov_list[[counter]]$indexr2 <- indexr2
  mov_list[[counter]]$mov_name <- string
  mov_list[[counter]]$route1 <- route1
  mov_list[[counter]]$route2 <- route2
  mov_list[[counter]]$client1 <- client1
  mov_list[[counter]]$client2 <- client2
  mov_list_cost[[counter]] <- new_cost
  mov_list_cost_pen[[counter]] <- new_cost_pen

  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost
  res$mov_list_cost_pen <- mov_list_cost_pen
  
  return(res)
}

check_new_type<-function(type_root, route, input){
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
  
  return(type_root)
}

is_in_vecinity<-function(clienti, clientw, vecinity, perc_vecinity){
  local_vecinity <- vecinity[[clienti]][1:ceiling(length(vecinity[[clienti]])*perc_vecinity)]
  
  if (sum(clientw %in% local_vecinity)) return (1)
  else return (0)
}


return_close_client<-function(clienti, route, vecinity) {
  
  index <- Inf
  for (i in 2:(length(route)-1)) {
    index_i <- which(vecinity[[clienti]] == route[i])
    if (index_i < index) {
      index <- index_i
    }
  }
  
  return(vecinity[[clienti]][index])
}
