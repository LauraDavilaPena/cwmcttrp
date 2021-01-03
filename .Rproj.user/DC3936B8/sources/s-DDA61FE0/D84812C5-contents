MCTTRP_opt_method<-function(result, initial_solution, input, init_time, verbose){
  
    current_solution <- initial_solution
    stopping_conditions <- 0
    iter <- 1
    
    while (!stopping_conditions) {
      # improvement
      current_solution <- result_improvement(result, current_solution)
        
      # perturbation
      perturbed_solution <- perturbation(input, result, current_solution)
      while(perturbed_solution$perturbation_not_obtained){
        perturbed_solution <-  perturbation(string, result, initial_solution)
      }
      current_solution <- perturbed_solution[["perturbed_solution"]]
      
      # tabu movements
      current_solution <- tabu_movements_core(input, result, current_solution)
      
      
      # check stopping conditions
      stopping_conditions <- check_stoppping_conditions(iter, init_time, current_solution$cost, input)
      iter <- iter + 1
    }
    
    
    return(current_solution)
}



check_stoppping_conditions<-function(current_iteration, init_time, current_obj, input){
  current_time <- as.numeric(init_time - Sys.time())
  
  if (current_iteration >= input$max_iter) {
    print(paste0("[[ Stopping criteria ]] Max iterations"))
    res <- 1
  }
  else if (input$max_time >= total_time) {
    print(paste0("[[ Stopping criteria ]] Max time"))
    res <- 1
  }
  else if (input$vtr <= vtr){
    print(paste0("[[ Stopping criteria ]] The algorithm obtained the Value to reach"))
    res <- 1
  } 
  else {
    print(paste0(current_iteration, " fobj ", current_obj, " iter ", current_iteration, " time ", current_time))
    res <- 0
  }
    
  return(res) 
}