MCTTRP_opt_method<-function(result, initial_solution, input, init_time, type_problem, seed){
  
    # init parameters
    stopping_conditions <- 0
    iter <- 1
    perc_v <- 1 #0.25
    alpha <- 1

    # init best solution
    bestsolution <- initial_solution 
    bestcost <- calculateTotalDistanceTS(input, alpha, initial_solution)
    
    # init current solution
    current_solution <- bestsolution
    current_cost <- bestcost
    
    # print init output
    print(paste0("fobj ", bestcost, " iter ", 0, " time ", difftime(Sys.time(), init_time, units = "secs")))
    
    
    while (!stopping_conditions) {

      start_time <- Sys.time()
      
      # perturbation
      res_p <- perturbation_core(input, current_solution, type_problem)
      current_solution <- res_p$current_solution
      phi <- res_p$phi
      
      # improvement
      current_solution <- result_improvement(input, current_solution)
      current_cost <- calculateTotalDistanceTS(input, alpha, current_solution)
      
      # tabu search
      res_tabu <- tabu_search (input, current_solution, current_cost, type_problem, perc_v, input$max_iter, iter, phi)
      current_solution <- res_tabu$current_solution
      current_cost <- res_tabu$current_cost
      
      # best solution
      newcost <- calculateTotalDistanceTS(input, alpha, current_solution)
      if (bestcost >  newcost) {
            bestsolution <- current_solution 
            bestcost <- newcost
      }
      
      # return to the best solution
      if (runif(1) < (iter/input$max_iter)^2) {
            current_solution <- bestsolution
            current_cost <- bestcost
      }
      
      print(paste0("fobj ", newcost, " infea ", calc_penalty(input, current_solution), " iter ", iter, " (best fobj ", bestcost , ") perc_v ", perc_v , " time ", difftime(Sys.time(), init_time, units = "secs"), " s"))
      

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



calculateTotalDistanceTS <- function(input, alpha, routes_res){
  route <- all_routes(routes_res)
  
  cost <- 0
  for (i in 1:(length(route)-1)){
    cost <- cost + input$matriz.distancia[route[i]+1, route[i+1]+1]
  }
  
  ## F(S,M) -- Diversification
  
  FS  <- cost+alpha*calc_penalty(input, routes_res)
  
  return(FS)
}

update_penalties <- function(input, alpha, gamma, current_solution){

  feasibility <- calc_penalty(input, current_solution)
  
  #print(paste0("CALIBRATE PENALTIES: current solution pen -> ", feasibility, " gamma ", gamma, " alpha ", alpha))
  if (feasibility > 0) {
    alpha <- ( 1 + gamma) * alpha
  }
  else {
    alpha <- ( 1 + gamma) / alpha
  }
  
  return(alpha)
}

