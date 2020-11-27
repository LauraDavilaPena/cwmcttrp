#' Postproc ...
#'
#' @param rutas
#' @param input
#' @return A list of results ...
postproc_add_disconnected_clients<-function(rutas, input, Tolvas, R, Rhat, H.camion_res,
                                            H.trailer_res, H.camion, H.trailer, demandas_res,
                                            n1, n, nf, t, num_clientes){ #rev1
  counter<-length(rutas)+1
  for (zz in 2:length(R[,1])){
    if ((R[zz,1]==0)&&(R[zz,3]==0)&&(Rhat[zz,1]==0)&&
        (Rhat[zz,3]==0)&&(!is.element(R[zz,2],rutas))){

      num_clientes <- num_clientes+1 #rev

      if (sum(zz==((n1+2):n))==1) {
        type<-1
        result1 = serve_customer(type,H.trailer_res,H.trailer,H.camion_res,H.camion,demandas_res,Tolvas, t, zz, nf)
        H.trailer_res<-result1$H.trailer_res
        H.camion_res<-result1$H.camion_res
        demandas_res<-result1$demandas_res
        Tolvas<-result1$Tolvas
        t<-result1$t
      } else {
        type<-2
        result1 = serve_customer(type,H.trailer_res,H.trailer,H.camion_res,H.camion,demandas_res,Tolvas, t, zz, nf)
        H.trailer_res<-result1$H.trailer_res
        H.camion_res<-result1$H.camion_res
        demandas_res<-result1$demandas_res
        Tolvas<-result1$Tolvas
        t<-result1$t
      }
      rutas[counter]<-R[zz,2]
      counter<-counter+1
      rutas[counter]<-0
      counter<-counter+1
    }
  }

  if(num_clientes==2){
    clientes_solos <- numeric(2)
    k <- 1

    for(i in 2:(length(rutas)-1)){
      if(rutas[i-1]==0 & rutas[i+1]==0){
        clientes_solos[k] <- rutas[i]
        k <- k + 1
      }
    }

    if(2*input$matriz.distancia[1,clientes_solos[1]+1]+
       2*input$matriz.distancia[1,clientes_solos[2]+1] >=
       input$matriz.distancia[1,clientes_solos[1]+1] +
       input$matriz.distancia[clientes_solos[1]+1,clientes_solos[2]+1] +
       input$matriz.distancia[clientes_solos[2]+1,1]) {

      if(Tolvas[Tolvas[,1]==clientes_solos[1],3][1] == "truck" &
         Tolvas[Tolvas[,1]==clientes_solos[2],3][1] == "truck"){

        trucks_usados_aux <- c(Tolvas[Tolvas[,1]==clientes_solos[1],4],
                               Tolvas[Tolvas[,1]==clientes_solos[2],4])

        trucks_usados <- as.numeric(c(trucks_usados_aux[1],
                                      trucks_usados_aux[length(trucks_usados_aux)]))

        Tolvas_usadas <- sum(H.camion_res[trucks_usados[1],]==-1) +
                         sum(H.camion_res[trucks_usados[2],]==-1)

        if( Tolvas_usadas <= dim(H.camion_res)[2]){
          H.camion_res[trucks_usados[1],1:Tolvas_usadas] = rep(-1,Tolvas_usadas)
          H.camion_res[trucks_usados[2],] = input$H.camion[trucks_usados[2],]
          Tolvas[Tolvas[,1]==clientes_solos[2],4] = rep(trucks_usados_aux[1],length(Tolvas[Tolvas[,1]==clientes_solos[2],4]))
          R[clientes_solos[1]+1,3] = clientes_solos[2]
          R[clientes_solos[2]+1,1] = clientes_solos[1]
          for (ii in 1:length(rutas)){
            if(rutas[ii]==clientes_solos[1]){
              rutas[ii+1] = clientes_solos[2]
              rutas[(ii+2):length(rutas)] = 0
            }

          }

        }
      }
    }
  }


  results<-list()
  results$rutas<-rutas
  results$Tolvas<-Tolvas
  results$H.trailer_res<-H.trailer_res
  results$H.camion_res<-H.camion_res
  results$demandas_res<-demandas_res
  results$Tolvas<-Tolvas
  results$t<-t
  results$num_clientes <- num_clientes  #rev1

  return(results)
}


#' Postproc ...
#'
#' @param rutas
#' @param input
#' @return A list of results ...
postproc_subroutes_trailer_routes<-function(rutas, matriz.distancia,
                                            Tolvas, R, Rhat, opt){

  rutas.des <- creation_routes_post(rutas, Tolvas)
  coste<-0
  for(i in 1:(length(rutas)-1)){
    coste<-coste+matriz.distancia[rutas[i]+1,rutas[i+1]+1]
  }

  for(i in 1:length(rutas.des)){
    if(rutas.des[[i]]$trailer!=0 && rutas.des[[i]]$truck==0){
      new_subroute<-0
      index_client<-1
      index_route<-1
      best_cost <- Inf
      for(j in 1:length(rutas.des)){
        tam1 <- length(rutas.des[[j]]$rutas)
        mincost <- matriz.distancia[rutas.des[[j]]$rutas[1]+1,
                                    rutas.des[[j]]$rutas[2]+1] +
          matriz.distancia[rutas.des[[j]]$rutas[tam1-1]+1,
                           rutas.des[[j]]$rutas[tam1]+1]
        rescost <- coste-mincost
        if(rutas.des[[j]]$trailer==0 && rutas.des[[j]]$truck!=0){
          for(z in 2:(length(rutas.des[[i]]$rutas)-1)){
            tam1 <- length(rutas.des[[i]]$rutas)
            tam2 <- length(rutas.des[[j]]$rutas)
            new_cost <- matriz.distancia[rutas.des[[i]]$rutas[z]+1,
                                         rutas.des[[j]]$rutas[2]+1] +
              matriz.distancia[rutas.des[[j]]$rutas[tam2-1]+1,
                               rutas.des[[i]]$rutas[z]+1]
            #print(paste(new_cost," nodos:", rutas.des[[j]]$rutas[2], rutas.des[[i]]$rutas[z], rutas.des[[j]]$rutas[tam1-2], rutas.des[[i]]$rutas[z], "best_cost:", best_cost))
            flag = 0
            if ((opt==0) &&
                ((new_cost < mincost)&&(new_cost < best_cost))){
              flag = 1
            }
            if ((opt==1) &&
                #((new_cost+rescost < (mincost+rescost)*1.15)&&(new_cost < best_cost))){
                ((new_cost+rescost < (mincost+rescost)*1.15)&&(new_cost < best_cost))){
              flag = 1
            }
            if (flag == 1) {
              index_client <- z
              index_route <- j
              best_cost <- new_cost
              new_subroute <- 1
            }
          }
        }
      }
      if (new_subroute == 1){
        subroute = rutas.des[[index_route]]$rutas
        # Delete subroute in R
        for (ii in 1:length(subroute)){
          for (jj in 2:length(R[,1])){
            if (R[jj,2] == subroute[ii]){
              R[jj,1] <- 0
              R[jj,3] <- 0
            }
          }
        }
        # Add subroute in Rhat (corregido con respecto a David)
        for (ii in 1:length(subroute)){
          for (jj in 2:length(Rhat[,1])){
            if (Rhat[jj,2] == subroute[ii]){
              if(length(subroute)!=3){ #rev1
                if (ii==2){
                  Rhat[jj,1] <- rutas.des[[i]]$rutas[index_client]
                  Rhat[jj,3] <- subroute[ii+1]
                }
                else if (ii==length(subroute)-1){
                  Rhat[jj,1] <- subroute[ii-1]
                  Rhat[jj,3] <- rutas.des[[i]]$rutas[index_client]
                } else {
                  Rhat[jj,1] <- subroute[ii-1]
                  Rhat[jj,3] <- subroute[ii+1]
                }
              }else{
                if(ii==2){ #rev1
                  Rhat[jj,1] <- rutas.des[[i]]$rutas[index_client]
                  Rhat[jj,3] <- rutas.des[[i]]$rutas[index_client]
                }
              }

            }
          }
        }
        Rhat[rutas.des[[i]]$rutas[index_client]+1,1]<-subroute[length(subroute)-1] # estaban intercambiadas las posiciones 1 y 3
        Rhat[rutas.des[[i]]$rutas[index_client]+1,3]<-subroute[2]

        # Add subroute in rutas
        new_rutas<-c(rutas[1])
        counter<-2
        subroute<-subroute[2:(length(subroute)-1)]
        for (ii in 2:length(rutas)){
          if (rutas[ii]!=rutas.des[[i]]$rutas[index_client]){
            if (!is.element(rutas[ii],subroute)){
              #if (rutas[ii]!=new_rutas[counter-1]){
              new_rutas[counter]<-rutas[ii]
              counter<-counter+1
              #}
            }
          }
          else {
            #if (rutas[ii]!=new_rutas[counter-1]){
            new_rutas[counter]<-rutas[ii]
            counter<-counter+1
            #}
            for (jj in 1:length(subroute)){
              #if (rutas[ii]!=new_rutas[counter-1]){
              new_rutas[counter]<-subroute[jj]
              counter<-counter+1
              #}
            }
            #if (rutas[ii]!=new_rutas[counter-1]){
            new_rutas[counter]<-rutas[ii]
            counter<-counter+1
            #}
          }
        }

        rutas<-new_rutas

      }
    }
  }

  # Add subroute in rutas.des
  rutas.des <- creation_routes_post(rutas, Tolvas)

  results<-list()
  results$rutas<-rutas
  results$R<-R
  results$Rhat<-Rhat
  results$rutas.des = rutas.des

  return(results)
}


#' Postproc ...
#'
#' @param rutas
#' @param input
#' @return A list of results ...
creation_routes_post <- function(rutas, Tolvas){

  n.rutas <- sum(rutas==0)-1
  rutas.depot.start <- which(rutas==0)[-length(which(rutas==0))]
  rutas.depot.end <- which(rutas==0)[-1]

  #for (i in n.rutas){
  #  rutas.indiv[i,] <- rutas[rutas.depot[i]:rutas.depot[i+1]]
  #}

  rutas.indiv <- mapply(function(x, y) t(rutas[seq(x, y)]), rutas.depot.start, rutas.depot.end)
  class(rutas.indiv)
  r.trailer <- numeric(n.rutas)
  r.truck <- numeric(n.rutas)

  for (i in 1:length(rutas.indiv)){
    #print(rutas.indiv)
    for (j in rutas.indiv[[i]][2:(length(rutas.indiv[[i]])-1)]){
      for(k in which(Tolvas[,1]==j)){
        if(Tolvas[k,3]=="trailer"){
          r.trailer[i] <- as.numeric(Tolvas[k,4])
        }else{
          r.truck[i] <- as.numeric(Tolvas[k,4])
        }
      }
    }
  }

  rutas.des=list()
  for(i in 1:length(rutas.indiv)){
    ruta.aux=list()
    ruta.aux=list(rutas=rutas.indiv[[i]],trailer=r.trailer[i],truck=r.truck[i])
    rutas.des[[i]]<-ruta.aux
  }

  return(rutas.des)

}

#' Postproc ...
#'
#' @param type
#' @param H.trailer_res
#' @return A list of results ...
serve_customer<-function(type,H.trailer_res,H.trailer,H.camion_res,H.camion,
                         demandas_res,Tolvas, t, zz, nf){
  if (type==1){
    s<-min(which(H.camion_res[,1]!=-1))
  } else {
    s<-min(which(H.trailer_res[,1]!=-1))
  }
  tc<-1
  for (i in 1:nf){
    while(demandas_res[zz,i]!=0 && sum(H.camion_res[s,])!=-dim(H.camion)[2] ){
      while (demandas_res[zz,i]>max(H.camion_res) && sum(H.camion_res[s,])!=-dim(H.camion)[2]){
        if(Tolvas[t-1,1]==0 && t>2){
          t <- min(which(Tolvas[,1]==0))
        }
        Tolvas[t,] <- c(zz-1,i,"truck",s,max(H.camion),1)
        demandas_res[zz,i] <- demandas_res[zz,i] - max(H.camion)
        H.camion_res[s,tc] <- -1

        if (tc < dim(H.camion)[2]){
          tc <- tc+1
        }
        else{
          tc <- 1
        }

        t <- t+1
      }

      if(sum(H.camion_res[s,])!=-dim(H.camion)[2]){
        if(Tolvas[t-1,1]==0 && t>2){
          t <- min(which(Tolvas[,1]==0))
        }
        if (type==1){
          Tolvas[t,] <- c(zz-1,i,"truck",s,demandas_res[zz,i],demandas_res[zz,i]/max(H.camion))
          demandas_res[zz,i] <- max(0,demandas_res[zz,i] - max(H.camion))
          H.camion_res[s,tc] <- -1
          if (tc < dim(H.camion)[2]){
            tc <- tc+1
          }else{
            tc <- 1
          }
        } else {
          Tolvas[t,] <- c(zz-1,i,"trailer",s,demandas_res[zz,i],demandas_res[zz,i]/max(H.trailer))
          demandas_res[zz,i] <- max(0,demandas_res[zz,i] - max(H.trailer))
          H.trailer_res[s,tc] <- -1
          if (tc < dim(H.trailer)[2]){
            tc <- tc+1
          }else{
            tc <- 1
          }
        }

        t <- t+1
      }
    }
  }

  results<-list()
  results$H.trailer_res<-H.trailer_res
  results$H.camion_res<-H.camion_res
  results$demandas_res<-demandas_res
  results$Tolvas<-Tolvas
  results$t<-t

  return(results)

}


#' Postproc ...
#'
#' @param rutas
#' @param input
#' @return A list of results ...
postproc_add_disconnected_clients_TTRP<-function(rutas_res, rutas, input, R, Rhat){

  for (i in 2:(length(R[,1]))) {
    if ((R[i,1]==0)&&(R[i,3]==0)&&(Rhat[i,1]==0)&&(Rhat[i,3]==0)) {
      node_to_add <- i - 1
      node_cap <- calc_load2(c(node_to_add), input$vector.demandas )
      min_value <- Inf
      index_to_insert <- -1
      subroute_selected <- -1
      for (j in 1:length(rutas_res)) {
        next_r <- 1
        if (rutas_res[[j]]$type == "PVR") {
          if (node_to_add > input$n1) {
            limit_cap <- input$capacidad.truck;
            change_type <- "PTR"
          } else {
            limit_cap <- input$capacidad.vehiculo;
            change_type <- "PVR"
          }
        }
        else if (rutas_res[[j]]$type == "PTR") {
          limit_cap <- input$capacidad.truck;
          change_type <- "PTR"
        }
        else if (rutas_res[[j]]$type == "CVR"){
          limit_cap <- input$capacidad.vehiculo;
          if (node_to_add > input$n1) next_r <- 0
          change_type <- "CVR"
        }

        if (((node_cap + rutas_res[[j]]$capacity) <= limit_cap) && (next_r)) {
          subroute <- append(0, node_to_add)
          subroute <- append(subroute, rutas_res[[j]]$route[2:length(rutas_res[[j]]$route)])
          cost <- calc_load2(subroute, input$vector.demandas)
          if (cost < min_value ) {
            min_value <- cost
            index_to_insert <- j
            subroute_selected <- subroute
          }
          subroute <- append(rutas_res[[j]]$route[1:(length(rutas_res[[j]]$route)-1)], node_to_add)
          subroute <- append(subroute, 0)
          cost <- calc_load2(subroute, input$vector.demandas)
          if (cost < min_value ) {
            min_value <- cost
            index_to_insert <- j
            subroute_selected <- subroute
            select_type <- change_type
          }
        }
      }
      if (index_to_insert != -1) {
        rutas_res[[index_to_insert]]$route <- subroute_selected
        rutas_res[[index_to_insert]]$type <- select_type
      } else {
        index_to_insert <- length(rutas_res)+1
        subroute <- append(0, node_to_add)
        subroute <- append(subroute, 0)
        rutas_res[[index_to_insert]] <- list()
        rutas_res[[index_to_insert]]$route <-  subroute
        if (node_to_add <= input$n1) { rutas_res[[index_to_insert]]$type <- "PVR"; }
        else {  rutas_res[[index_to_insert]]$type <- "PTR"; }
        rutas_res[[index_to_insert]]$capacity <-calc_load2(subroute, input$vector.demandas)
        rutas_res[[index_to_insert]]$capacity_truck <- calc_load_only_truck(subroute, input$vector.demandas, input)
        rutas_res[[index_to_insert]]$cost <- local_cost(subroute, input$matriz.distancia)
      }
    }
  }

  return(rutas_res)
}

#' Postproc ...
#'
#' @param rutas
#' @param input
#' @return A list of results ...
postproc_add_new_subroutes_TTRP<-function(rutas_res, rutas, input, opt){

  new_rutas_res <- rutas_res
  correc <- 1
  if (opt) {
    correc <- 1.15
  }

  for (i in 1:length(rutas_res)) {
    if (rutas_res[[i]]$type == "PVR" ) {
      success <- 0
      index_j <- -1
      selected_subroute <- 0
      min_cost <- local_cost(rutas, input$matriz.distancia) * correc
      for (j in 1:length(new_rutas_res)) {
        if (new_rutas_res[[j]]$type == "PTR" ) {
            if (((rutas_res[[i]]$capacity + new_rutas_res[[j]]$capacity) <= input$capacidad.vehiculo) &&
                ((rutas_res[[i]]$capacity_truck + new_rutas_res[[j]]$capacity_truck) <= input$capacidad.truck)){


                subroute_pvr <- rutas_res[[i]]$route
                subroute_ptr <- new_rutas_res[[j]]$route

                #print("INTENTANDO METER:")
                #print(subroute_pvr)
                #print("    EN:")
                #print(subroute_ptr)

                for (z in 2:(length(subroute_pvr)-1)) {
                    
                  new_route <- new_rutas_res
                  aux <- append(subroute_pvr[1:z], subroute_ptr[2:(length(subroute_ptr)-1)])
                  aux <- append(aux, subroute_pvr[z:length(subroute_pvr)])
                    new_route[[j]]$route <- aux
                    new_route[[j]]$type <- "CVR"
                    new_route[[j]]$capacity <- calc_load2(aux, input$vector.demandas)
                    new_route[[j]]$capacity_truck <- calc_load_only_truck(aux, input$vector.demandas, input)
                    new_route_eval <- new_route[-i]
                    cost <- local_cost(convert_in_route(new_route_eval), input$matriz.distancia)
                    feasible <- 0
                    if ((new_route[[j]]$capacity <= input$capacidad.vehiculo) &&
                        (new_route[[j]]$capacity_truck <= input$capacidad.truck)) feasible <- 1
                    if ((cost < min_cost) && (feasible == 1)) {
                      success <- 1
                      index_j <- j
                      selected_subroute <- new_route
                      min_cost <- cost
                    }

                }

            }
        }
      }

      if (success == 1) {
        #print("EXITO")
        #print("NEW ROUTE")
        #print(index_j)
        #print(selected_subroute)
        new_rutas_res <- selected_subroute[-i]

      }
    }
  }


  return(new_rutas_res)
}


convert_in_route<-function(route_res) {
  route <- list()
  route <- 0
  for (i in 1:length(route_res)) {
    route <- c(route, route_res[[i]]$route[2:length(route_res[[i]]$route)])
    route <- c(route, 0)
  }

  return (route)
}

select_route_TTRP<-function(counter_routes,routes,type_routes,input, dis_client){
  min_value = Inf
  min_index = -1
  index_selected_route = -1
  for ( ii in 1:counter_routes) {
    route_ii <- routes[[ii]]
    type_ii <- type_routes[[ii]]
    if (type_ii == 0) {
      capacity <- input$capacidad.truck[1]
    }
    else {
      capacity <- input$capacidad.vehiculo[1]
    }
    for ( jj in 1:length(route_ii)) {
      local_route <- list()
      local_route <- 0

      if (jj == 1) {
        local_route <- append(local_route, dis_client)
        local_route <- append(local_route, route_ii)
      }
      else if (jj == length(routes[[ii]])) {
        local_route <- append(local_route, route_ii)
        local_route <- append(local_route, dis_client)
      }
      else {
        local_route <- append(local_route, route_ii[1:jj])
        local_route <- append(local_route, dis_client)
        local_route <- append(local_route, route_ii[jj:length(route_ii)])
      }
      local_route <- append(local_route, 0)
      if (calc_load(local_route, input$vector.demandas, capacity) > 0 ) {
        cost <- local_cost(local_route, input$matriz.distancia)
        if (cost < min_value) {
          min_index <- jj
          min_value <- cost
          selected_route <- local_route
          index_selected_route <- ii
        }
      }
    }
  }

  result <- list()
  result$selected_route<-selected_route
  result$cost<-cost
  result$index_selected_route<-index_selected_route
  result$min_index<-min_index

  return(result)
}


select_subroute_to_add_TTRP<-function(counter_routes,routes,type_routes,input, dis_client){
  min_value = Inf
  min_index = -1
  index_selected_route = -1
  for ( ii in 1:counter_routes) {
    route_ii <- routes[[ii]]
    type_ii <- type_routes[[ii]]
    if (type_ii == 2) {
      duplicate1 = -1
      duplicate2 = -1
      for ( jj in 1:(length(route_ii)-1)) {
        for ( zz in (jj+1):length(route_ii)) {
          if (jj == zz) {
              duplicate1 = jj
              duplicate2 = zz
              break
          }
        }
        if (duplicate1!=-1) {
          break
        }
      }
      capacity <- input$capacidad.truck[1]
      for ( jj in duplicate1:duplicate2) {
        local_route <- list()
        local_route <- route_ii[1:duplicate1]
        if (jj == duplicate1) {
          local_route <- append(local_route, dis_client)
          local_route <- append(local_route, route_ii[(duplicate1+1):length(route_ii)])
        }
        else if (jj == duplicate2) {
          local_route <- append(local_route, route_ii[(duplicate1+1):(duplicate2-1)])
          local_route <- append(local_route, dis_client)
          local_route <- append(local_route, route_ii[duplicate2:duplicate2])
        }
        else {
          local_route <- append(local_route, route_ii[(duplicate1+1):(duplicate1+jj)])
          local_route <- append(local_route, dis_client)
          local_route <- append(local_route, route_ii[(duplicate1+1+jj):duplicate2])
        }
        local_route <- append(local_route, route_ii[(duplicate2+1):length(route_ii)])
        if (calc_load(local_route, input$vector.demandas, capacity) > 0 ) {
          cost <- local_cost(local_route, input$matriz.distancia)
          if (cost < min_value) {
            min_index <- jj
            min_value <- cost
            selected_route <- local_route
            index_selected_route <- ii
          }
        }
      }
    }

  }

  result <- list()
  result$selected_route<-selected_route
  result$cost<-cost
  result$index_selected_route<-index_selected_route
  result$min_index<-min_index

  return(result)
}


add_to_route_TTRP<-function(R, rutas, ruta_origin, position, newclient) {

  for (i in 1:length(rutas)){

    for (j in 1:length(ruta_origin)){

      if (rutas[i+j]!=ruta_origin[j]) {
        noEq <- FALSE
        break
      }
      else {
        noEq <- TRUE
      }

    }

    if (noEq == TRUE) {
      new_rutas <- 0
      new_rutas <- append(new_rutas, rutas[1:(i+position)])
      new_rutas <- append(new_rutas, newclient)
      new_rutas <- append(new_rutas, rutas[(i+position+1):length(rutas)])
      break
    }
  }

  init <- return_index_route(rutas, ruta_origin[1])+1
  for (zz in init:(init+length(ruta_origin))) {
    position <- new_rutas[zz]
    # add new values
    R[position+1,1] <- new_rutas[zz-1]
    R[position+1,3] <- new_rutas[zz+1]
  }


  results <- list()
  results$rutas <- new_rutas
  results$R <- R

  return(results)

}







