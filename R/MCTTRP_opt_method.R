MCTTRP_opt_method<-function(result, initial_solution, input, init_time, type_problem, seed){
  
    stopping_conditions <- 0
    iter <- 1
    tabulist <- list()
    max_size_tabu_list <- input$n 
    n_movs <- 10
    perc_v <- 0.1
    bestsolution_size <- floor(input$n/10)
    penalty_capacity <- 0
    
    init_cost <- calculateTotalDistance(input, all_routes(initial_solution))
    bestlist <- init_best_solution(bestsolution_size, initial_solution, init_cost)
    bestsolution <- initial_solution 
    bestcost <- init_cost
      

    current_time <- difftime(Sys.time(), init_time, units = "secs")

    print(paste0("fobj ", bestcost, " iter ", 0, " time ", difftime(Sys.time(), init_time, units = "secs")))
    
    
    while (!stopping_conditions) {
      
      current_solution <- return_best_solution(bestlist)

      # improvement
      current_solution <- result_improvement(input, current_solution)

      start_time <- Sys.time()
      perturbation_not_obtained <- TRUE
      counter <- 1
      while ((perturbation_not_obtained) && (counter < 5)) {
        perturbed_solution <-  perturbation(input, current_solution, type_problem, seed, tabulist)
        current_solution <- perturbed_solution[["perturbed_solution"]]
        perturbation_not_obtained <- perturbed_solution$perturbation_not_obtained
        counter <- counter + 1
      }
      if (perturbation_not_obtained) {
        res_perturb <- full_random_perturbation(input, current_solution, type_problem, seed, tabulist, max_size_tabu_list, input$vecinity, perc_v)
        current_solution <- res_perturb$current_solution
        tabulist <- res_perturb$tabulist
      }
      
      # tabu movements
      res_tabu <- tabu_movements_core(input, current_solution, tabulist, max_size_tabu_list, n_movs, type_problem, input$vecinity, perc_v, penalty_capacity)
      current_solution <- res_tabu$current_solution
      tabulist <- res_tabu$tabulist
      
      #totaltime <- difftime(Sys.time(), start_time, units = "secs")    
      #print(paste0("end perturbation ", totaltime))
      #print("end tabu")
      #for (i in 1:length(current_solution)) {
      #  print(current_solution[[i]]$route)
      #}
      
      # print iterations
      newcost <- calculateTotalDistance(input, all_routes(current_solution))
      if (bestcost >  newcost) {
            bestsolution <- current_solution 
            bestcost <- newcost
      }
      
      if (bestlist[[return_worst_cost(bestlist)]]$cost > newcost) {
        bestlist <- update_best_solution(bestlist, current_solution, newcost) 
      }
      
      print(paste0("fobj ", newcost, " iter ", iter, " (best fobj ", bestcost , ") perc_v ", perc_v, " time ", difftime(Sys.time(), init_time, units = "secs"), " s"))
      

      # check stopping conditions
      stopping_conditions <- check_stoppping_conditions(iter, init_time, newcost, input)
    
      iter <- iter + 1
    }
    
    
    return(bestsolution)
}



check_stoppping_conditions<-function(current_iteration, init_time, current_obj, input){
  current_time <- difftime(Sys.time(), init_time, units = "secs")

  if (current_iteration >= input$max_iter) {
    print(paste0("[[ Stopping criteria ]] Max iterations"))
    res <- 1
  }
  else if (input$max_time <= current_time) {
    print(paste0("[[ Stopping criteria ]] Max time"))
    res <- 1
  }
  else if (input$vtr >= current_obj){
    print(paste0("[[ Stopping criteria ]] The algorithm obtained the Value to reach"))
    res <- 1
  } 
  else {
    res <- 0
  }
    
  return(res) 
}


all_routes<-function(solution) {
  route <- c(solution[[1]]$route)
  if (length(solution)>2) {
    for (i in 2:length(solution)) {
      route <- c(route, solution[[i]]$route)
    }
  }
  
  route <- delete_dupl_zeros_route(route)
  
  return(route)
}

calc_vecinity<-function(input) {
   clients <- length(input$matriz.distancia[1, 2:input$n])  
  
   vecinity_list_values <- list()
   for (i in 1:clients) {
     vecinity_list_values[[i]] <-  input$matriz.distancia[i+1, 2:clients]
     vecinity_list_values[[i]][i] <- Inf
   }   

   vecinity_index_order <- list()
   for (i in 1:clients) {
     vecinity_index_order[[i]] <- order(vecinity_list_values[[i]], decreasing = FALSE)
   }
   
   
   return(vecinity_index_order)
}

init_best_solution<-function(best_list_max, init_solution, init_cost) {
  
  best_list <- list()

  for (i in 1:best_list_max) {
    best_list[[i]] <- list()
    best_list[[i]]$solution <- init_solution
    best_list[[i]]$cost <-  init_cost
  }
  
  return(best_list)
}

update_best_solution<-function(best_list, solution, cost) {
  
  # return worst
  index_worst_cost <- return_worst_cost(best_list)
  
  exist_in_list <- 0
  route_1 <- all_routes(solution)
  for (i in 1:length(best_list)) {
    route_2 <- all_routes(best_list[[i]]$solution)
    if (((length(route_1)==length(route_2))&&(all(route_1==route_2)))||(cost==best_list[[i]]$cost)) {
      exist_in_list <- 1
      break
    }
  }
  if ((best_list[[index_worst_cost]]$cost > cost)&&(!exist_in_list)) {
    best_list[[index_worst_cost]]$cost <- cost
    best_list[[index_worst_cost]]$solution <- solution
  }
    
  return(best_list)
}

return_worst_cost<-function(best_list) {
  # return worst
  index_worst_cost <- 1
  for (i in 2:length(best_list)) {
    if (best_list[[i]]$cost > best_list[[index_worst_cost]]$cost) {
      index_worst_cost <- i
    }
  }
  
  return(index_worst_cost)
}

return_best_solution<-function(best_list) {
  randomN <- floor(runif(1)*length(best_list))+1
  
  return(best_list[[randomN]]$solution)
}