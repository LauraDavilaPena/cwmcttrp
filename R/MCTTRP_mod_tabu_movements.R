# Algoritmo Tabu


#string <- "instances/CHAO_MCTTRP_01.txt"
#matriz.distancia <- input_MCTTRP(string)$matriz.distancia
#result <- CW_algorithm(string, "MCTTRP", 5, 3, 0)


#initial_solution <- result$result_res



tabu_movements_core <- function(input, current_solution, tabulist, max_size_tabu_list, n_movs, type_problem){
  
  # Partimos de una solucion perturbada
  mov_list <- list()
  mov_list_cost <- list()
  
  #for (i in 1:length(current_solution)) {
  #  print(current_solution[[i]]$route)
  #  readline()
  #}
  
  res <- exchange_movement_client_subtour_and_vc(input, current_solution, mov_list, mov_list_cost, type_problem) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- exchange_movement_change_parking(input, current_solution, mov_list, mov_list_cost, type_problem) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- exchange_movement_vc_main_tour_tc_subtour(input, current_solution, mov_list, mov_list_cost, type_problem) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- exchange_movement_client_subtour_and_vc_creating_subtour(input, current_solution, mov_list, mov_list_cost, type_problem) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- move_subroute(input, current_solution, mov_list, mov_list_cost, type_problem) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- move_subroute_same_root(input, current_solution, mov_list, mov_list_cost, type_problem) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- move_client_subroute_to_main_tour(input, current_solution, mov_list, mov_list_cost, type_problem) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- move_vc_client_subroute_to_main_tour_and_split(input, current_solution, mov_list, mov_list_cost, type_problem) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  res <- exchange_ptr_and_subtour(input, current_solution, mov_list, mov_list_cost, type_problem) 
  mov_list <- res$mov_list
  mov_list_cost <- res$mov_list_cost
  
  #(input, current_solution, mov_list, mov_list_cost, type_problem) 
  #mov_list <- res$mov_list
  #mov_list_cost <- res$mov_list_cost
  
  
  #concert list to vector
  mov_list_cost_vect <- c(mov_list_cost[[1]])
  for (i in 2:length(mov_list_cost)) mov_list_cost_vect <- c(mov_list_cost_vect ,mov_list_cost[[i]] )
  
  index_order <- order(mov_list_cost_vect, decreasing = TRUE)
  
  print(mov_list[[index_order[1]]])
  
  +
    
    pos <- mov_list[[index_order[1]]]$indexr1
    
  print(current_solution[[pos]]$route)
  
  return(current_solution)
}

exchange_ptr_and_subtour<-function(input, result, mov_list, mov_list_cost, type_problem) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "PTR") {
      subroutei <- result[[i]]$route
      for (z in 1:length(result)){
        if (result[[z]]$type == "CVR") {
          subroutes <- return_subroutes(result[[z]]$route, input$n1)
          for (s in 1:length(subroutes)) {
            subroutez <- subroutes[[s]]$tour
            
            route1 <- c(0, subroutez[2:(length(subroutez)-1)], 0)
            route2 <- replace_subroute(subroutez, subroutei, result[[z]]$route)

            # feasibility
            feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem) 
            feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem) 
            # add to mov list
            if (feasible_route1 && feasible_route2) {
              res_mov <- add_movements_to_list(input, result, i, z, 0, 0, route1, route2,  mov_list, mov_list_cost)
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


move_vc_client_subroute_to_main_tour_and_split<-function(input, result, mov_list, mov_list_cost, type_problem) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes)) {
        for (j in 2:(length(subroutes[[s]]$tour)-1)) {
          clienti <- subroutes[[s]]$tour[j]
          prev <- subroutes[[s]]$tour[j-1]
          post <- subroutes[[s]]$tour[j+1]
          if ((clienti <= input$n1) && (sum(prev==subroutes[[s]]$tour)==1) &&  (sum(post==subroutes[[s]]$tour)==1)){

            route1 <- split_subroute(clienti, result[[i]]$route)
            
            # feasibility
            feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem) 
            # add to mov list
            if (feasible_route1) {
              res_mov <- add_movements_to_list(input, result, i, 0, clienti, 0, route1, 0,  mov_list, mov_list_cost)
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


move_client_subroute_to_main_tour<-function(input, result, mov_list, mov_list_cost, type_problem) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type != "PTR") {
      if (result[[i]]$type == "CVR") main_root <- return_main_route(result[[i]]$route)
      else main_root <- result[[i]]$route
      for (j in 2:(length(main_root)-1)) {
        ocurrences <- which(main_root[j]==result[[i]]$route)
        ocurrence_check <- result[[i]]$route[ocurrences[length(ocurrences)]]
        if ((main_root[j] <= input$n1)&&(main_root[j]==ocurrence_check)) {
          clienti <- main_root[j]
          for (z in 2:(length(main_root)-1)) {
            clientz <- main_root[z]
            if ((clienti != clientz)&&(sum(clientz==result[[i]]$route)==1)) {
                route1 <- delete_client(clientz, result[[i]]$route)
                route1 <- add_subroute(clienti, c(0,clientz,0), route1)
                
                # feasibility
                feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem) 
                # add to mov list
                if (feasible_route1) {
                  res_mov <- add_movements_to_list(input, result, i, 0, clienti, clientz, route1, 0,  mov_list, mov_list_cost)
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


move_subroute<-function(input, result, mov_list, mov_list_cost, type_problem) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        subroutei <- subroutes[[s]]$tour
        for (z in 1:length(result)) {
          if (result[[z]]$type != "PTR") {
            if (result[[z]]$type == "CVR") main_root <- return_main_route(result[[z]]$route)
            else main_root <- result[[z]]$route
            for (t in 2:(length(main_root)-1)) {
              if ((main_root[t]<= input$n1)&&(i!=z)&&(sum(main_root[t]==result[[z]]$route)==1)){
                clientw <- main_root[t]
                # routes
                route1 <- delete_subroute(subroutei, result[[i]]$route)
                route2 <- add_subroute(clientw, subroutei, result[[z]]$route)

                # feasibility
                feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem) 
                feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem) 
                # add to mov list
                if (feasible_route1 && feasible_route2) {
                  res_mov <- add_movements_to_list(input, result, i, z, 0, clientw, route1, route2,  mov_list, mov_list_cost)
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

move_subroute_same_root<-function(input, result, mov_list, mov_list_cost, type_problem) {
  
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
                feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem) 
                # add to mov list
                if (feasible_route1) {
                  res_mov <- add_movements_to_list(input, result, i, 0, clientw, 0, route1, 0,  mov_list, mov_list_cost)
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



exchange_movement_change_parking<-function(input, result, mov_list, mov_list_cost, type_problem) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        clienti <- subroutes[[s]]$root
        for (z in 1:length(result)){
          if (result[[z]]$type == "CVR") main_root <- return_main_route(result[[z]]$route)
          else main_root <- result[[z]]$route
          for (s in 2:(length(main_root)-1)) {
            if ((main_root[s] <= input$n1)&&(i!=z)) {
              clientz <- main_root[s]
              
              route1 <- replace_route_client(clienti, clientz, result[[i]]$route)
              route2 <- replace_route_client(clientz, clienti, result[[z]]$route)

              # feasibility
              feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem) 
              feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem) 
              # add to mov list
              if (feasible_route1 && feasible_route2) {
                res_mov <- add_movements_to_list(input, result, i, z, clienti, clientz, route1, route2,  mov_list, mov_list_cost)
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


exchange_movement_vc_main_tour_tc_subtour<-function(input, result, mov_list, mov_list_cost, type_problem) {
  
  for (i in 1:length(result)) {
    if ((result[[i]]$type != "PTR")) {
        if (result[[i]]$type == "CVR") main_root <- return_main_route(result[[i]]$route)
        else main_root <- result[[i]]$route
        for (j in 2:(length(main_root)-1)) {
          clienti <- main_root[j]
          for ( z in 1:length(result)) {
            if ((result[[z]]$type == "CVR")) {
              subroutes <- return_subroutes(result[[z]]$route, input$n1)
              for (s in 1:length(subroutes))  {
                for (t in 2:(length(subroutes[[s]]$tour)-1)) {
                  clientz <- subroutes[[s]]$tour[t]
                  if (clientz > input$n1) {
                    for (w in 2:(length( result[[i]]$route)-1)) {
                      clientw <- result[[i]]$route[w]
                      if ((clientw <= input$n1)&&(clientw != clienti)&&(clienti != subroutes[[s]]$root)&&(i!=z)&&(sum(result[[i]]$route==clienti)==1)) {

                        # new routes
                        route1 <- replace_route_client_subroute(clienti, clientz, clientw, result[[i]]$route)
                        route2 <- replace_route_client(clientz, clienti, result[[z]]$route)

                        # feasibility
                        feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem) 
                        feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem) 
                        # add to mov list
                        if (feasible_route1 && feasible_route2) {
                          res_mov <- add_movements_to_list(input, result, i, z, clienti, clientz, route1, route2,  mov_list, mov_list_cost)
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


exchange_movement_client_subtour_and_vc<-function(input, result, mov_list, mov_list_cost, type_problem) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        if (length(subroutes[[s]]$tour)==3) {
          for (j in 2:(length(subroutes[[s]]$tour)-1)) {
            clienti <- subroutes[[s]]$tour[j]
            for (z in 1:length(result)) {
              if (result[[z]]$type == "CVR") route_z <- result[[z]]$main_tour
              else route_z <- result[[z]]$route
              for (t in 2:(length(route_z)-1)) {
                if (( route_z[t] <= input$n1)&&(i!=z)) {

                  clientz <- route_z[t]
                  # new routes
                  route1 <- replace_route_client_vc_subroute(clienti, clientz, result[[i]]$route)
                  route2 <- replace_route_client(clientz, clienti, result[[z]]$route)
                  
                  # feasibility
                  feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem) 
                  feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem) 
                  # add to mov list
                  if (feasible_route1 && feasible_route2) {
                    res_mov <- add_movements_to_list(input, result, i, z, clienti, clientz, route1, route2,  mov_list, mov_list_cost)
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


exchange_movement_client_subtour_and_vc_creating_subtour<-function(input, result, mov_list, mov_list_cost, type_problem) {
  
  for (i in 1:length(result)) {
    if (result[[i]]$type == "CVR") {
      subroutes <- return_subroutes(result[[i]]$route, input$n1)
      for (s in 1:length(subroutes))  {
        if (length(subroutes[[s]]$tour)==3) {
          for (j in 2:(length(subroutes[[s]]$tour)-1)) {
            clienti <- subroutes[[s]]$tour[j]
            for (z in 1:length(result)) {
              if (result[[z]]$type == "CVR") route_z <- result[[z]]$main_tour
              else route_z <- result[[z]]$route
              for (t in 2:(length(route_z)-1)) {
                if (( route_z[t] <= input$n1)&&(i!=z)&&(sum(result[[z]]$route==route_z[t])==1)) {
                  clientz <- route_z[t]
                  for (w in 2:(length(route_z)-1)) {
                      clientw <-  route_z[w] 

                      if ((clientw <= input$n1)&&(clientw != clientz)&&(sum(result[[z]]$route==clientw)==1)) {
                          
                          # new routes
                          route1 <- replace_route_client_vc_subroute(clienti, clientz, result[[i]]$route)
                          route2 <- replace_route_client_subroute(clientz, clienti, clientw, result[[z]]$route)
                          
                          # feasibility
                          feasible_route1 <- check_feasibility(result, route1, input, result[[i]]$type, type_problem) 
                          feasible_route2 <- check_feasibility(result, route2, input, result[[z]]$type, type_problem) 
                          # add to mov list
                          if (feasible_route1 && feasible_route2) {
                            res_mov <- add_movements_to_list(input, result, i, z, clienti, clientz, route1, route2,  mov_list, mov_list_cost)
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
  for (i in 2:length(route)) {
    if (route[i] != clienti) {
       new_route <- c(new_route, route[i])
    }
  }
  # add clientj as a subtour in clientz
  new_route2 <- c(0)
  for (i in 2:length(new_route)) {
    if ((new_route[i] == clientz)&&(clienti!= clientz)) {
      new_route2 <- c(new_route2, new_route[i])
      new_route2 <- c(new_route2, clientj)
      new_route2 <- c(new_route2, new_route[i])
    } else {
      new_route2 <- c(new_route2, new_route[i])
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
  
  for (i in 2:length(route)) {
    if (route[i] == clienti) {
      index_client_i <- i    
    }
  }
  
  subroute <- subroute[2:(length(subroute)-1)]
  
  # add subroute
  new_route <- c(route[1:(index_client_i)], subroute, route[(index_client_i):length(route)])
  
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
  
  route <-c( route[1:(index_client_i-1)], route[root1],  clienti, route[(index_client_i+1):(root2-1)], clienti,  route[(root2+1):length(route)])
  
  return(route)
}

add_movements_to_list<-function(input, result, indexr1, indexr2, client1, client2, route1, route2, mov_list, mov_list_cost) {
  
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
  mov_list[[counter]]$client1 <- client1
  mov_list[[counter]]$client2 <- client2
  mov_list[[counter]]$route1 <- route1
  mov_list[[counter]]$route2 <- route2
  
  mov_list_cost[[counter]] <- old_cost - new_cost

  res <- list()
  res$mov_list <- mov_list
  res$mov_list_cost <- mov_list_cost
  
  return(res)
}