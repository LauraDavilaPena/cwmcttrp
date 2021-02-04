MCTTRP_opt_method<-function(result, initial_solution, input, init_time, type_problem, seed){
  
    current_solution <- initial_solution
    stopping_conditions <- 0
    iter <- 1
    tabulist <- list()
    max_size_tabu_list <- input$n 
    n_movs <- 100
    perc_v <- 0.1

    bestroute <- all_routes(current_solution)
    bestcost <- calculateTotalDistance(input, bestroute)
    bestcost_local <- bestcost
    bestsolution <- current_solution

    current_time <- difftime(Sys.time(), init_time, units = "secs")

    print(paste0("fobj ", bestcost, " iter ", 0, " time ", difftime(Sys.time(), init_time, units = "secs")))
    
    
    while (!stopping_conditions) {
      
      # improvement
      #print("result_improvement")
      current_solution_iter <- result_improvement(input, current_solution)
      #print(paste0("result_improvement end ", difftime(Sys.time(), init_time, units = "secs")))
      
      #print("init")
      #for (i in 1:length(current_solution)) {
      #  print(current_solution[[i]]$route)
      #}
      
      # perturbation
      #print("perturbation")
      start_time <- Sys.time()
      perturbation_not_obtained <- TRUE
      counter <- 1
      while ((perturbation_not_obtained) && (counter < 10)) {
        perturbed_solution <-  perturbation(input, current_solution_iter, type_problem, seed, tabulist)
        current_solution_iter <- perturbed_solution[["perturbed_solution"]]
        perturbation_not_obtained <- perturbed_solution$perturbation_not_obtained
        counter <- counter + 1
      }
      if (perturbation_not_obtained) {
       # print("full_random_perturbation")
        
        res_perturb <- full_random_perturbation(input, current_solution_iter, type_problem, seed, tabulist, max_size_tabu_list, input$vecinity, perc_v)
        current_solution_iter <- res_perturb$current_solution
        tabulist <- res_perturb$tabulist
      }
      
      #totaltime <- difftime(Sys.time(), start_time, units = "secs")    
      #print(paste0("end perturbation ", totaltime))
      #for (i in 1:length(current_solution)) {
      #  print(current_solution_iter[[i]]$route)
      #}
      
      # tabu movements
      start_time <- Sys.time()
      
    #  print("tabu_movements_core")
      res_tabu <- tabu_movements_core(input, current_solution_iter, tabulist, max_size_tabu_list, n_movs, type_problem, input$vecinity, perc_v)
      current_solution_iter <- res_tabu$current_solution
      tabulist <- res_tabu$tabulist
      
      #totaltime <- difftime(Sys.time(), start_time, units = "secs")    
      #print(paste0("end perturbation ", totaltime))
      #print("end tabu")
      #for (i in 1:length(current_solution)) {
      #  print(current_solution[[i]]$route)
      #}
      
      # print iterations
      newcost <- calculateTotalDistance(input, all_routes(current_solution_iter))
      if (bestcost_local > newcost) {
        current_solution <- current_solution_iter
        bestcost_local <- newcost
        if (bestcost >  newcost) {
            bestsolution <- current_solution_iter 
            bestroute <- all_routes(current_solution_iter)
            bestcost <- newcost
        }
      }

      # accept the solution
      #if (runif(1)  < (iter/input$max_iter)^2) {
      #  current_solution <- current_solution_iter
      #  bestcost_local <- newcost
      #}
      
      print(paste0("fobj ", newcost, " iter ", iter, " (best fobj ", bestcost , " local best ", bestcost_local ,")  time ", difftime(Sys.time(), init_time, units = "secs"), " s"))
      

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
