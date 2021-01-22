MCTTRP_opt_method<-function(result, initial_solution, input, init_time, type_problem){
  
    current_solution <- initial_solution
    stopping_conditions <- 0
    iter <- 1
    tabulist <- list()
    max_size_tabu_list <- input$n
    n_movs <- 10
    
    while (!stopping_conditions) {
      # improvement
      #current_solution <- result_improvement(input, result, current_solution)
        
      # perturbation
      #perturbation_not_obtained <- TRUE
      #while(perturbation_not_obtained){
      #  perturbed_solution <-  perturbation(input, result, current_solution, problem_type, seed, tabulist)
      #  current_solution <- perturbed_solution[["perturbed_solution"]]
      #  perturbation_not_obtained <- perturbed_solution$perturbation_not_obtained
      #  print(perturbation_not_obtained)
      #}
      
      # tabu movements
      current_solution <- tabu_movements_core(input, current_solution, tabulist, max_size_tabu_list, n_movs, type_problem)
      
      
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
  else if (input$max_time >= current_time) {
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