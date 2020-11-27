#' Create and initialize the main structure for MC-TTRP problem
#'
#' @param input Input structure list
#' @return The CWTTRPstruct list, where the global variables of the solver are
#' managed.
createCWMCTTRPStruct<-function(input){
  CWTTRP_struct = list()
  CWTTRP_struct$newPositionfilas <- 0
  CWTTRP_struct$newPositioncolumnas <- 0
  CWTTRP_struct$newPositionfilas2 <- 0
  CWTTRP_struct$newPositioncolumnas2 <- 0
  CWTTRP_struct$newPositionfilas3 <- 0
  CWTTRP_struct$newPositioncolumnas3 <- 0
  CWTTRP_struct$CargaT <- 0
  CWTTRP_struct$x <- 0
  CWTTRP_struct$y <- 0
  CWTTRP_struct$z <- 0
  CWTTRP_struct$a <- 0
  CWTTRP_struct$b <- 0
  CWTTRP_struct$aux <- 0
  CWTTRP_struct$aux1 <- 0
  CWTTRP_struct$aux2 <- 0
  CWTTRP_struct$iter <- 0
  CWTTRP_struct$primerclienteMT <- 0
  CWTTRP_struct$ultimoclienteMT <- 0

  CWTTRP_struct$demandas_res  <- input$matriz.demandas
  CWTTRP_struct$H.camion_res  <- input$H.camion
  CWTTRP_struct$H.trailer_res <- input$H.trailer
  CWTTRP_struct$parking <- numeric(input$n1)
  CWTTRP_struct$park_index <- 1

  CWTTRP_struct$t <- 1
  CWTTRP_struct$s <- 1
  CWTTRP_struct$tc <- 1
  CWTTRP_struct$tt <- 1
  CWTTRP_struct$ss <- 1


  return(CWTTRP_struct)
}

#' Create input structure for MC-TTRP problem
#'
#' @param matriz.demandas
#' @param matriz.distancia
#' @param capacidad.truck
#' @param capacidad.trailer
#' @param capacidad.vehiculo
#' @param H.camion
#' @param H.trailer
#' @return A input list with all input data.
createInputStruct<-function(matriz.demandas, matriz.distancia, capacidad.truck,
                            capacidad.trailer, capacidad.vehiculo, H.camion, H.trailer, n1){
  input <- list()
  input$matriz.demandas    <- matriz.demandas
  input$matriz.distancia   <- matriz.distancia
  input$capacidad.truck    <- capacidad.truck
  input$capacidad.trailer  <- capacidad.trailer
  input$capacidad.vehiculo <- capacidad.vehiculo
  input$H.camion  <- H.camion
  input$H.trailer <- H.trailer
  input$n1 <- n1

  return(input)
}

#' Create input structure for TTRP problem
#'
#' @param vector.demandas
#' @param matriz.distancia
#' @param capacidad.truck
#' @param capacidad.trailer
#' @param n1
#' @return A input list with all input data.
createInputStruct_TTRP<-function(vector.demandas, matriz.distancia, capacidad.truck,
                                 capacidad.trailer, n1){
  input <- list()
  input$vector.demandas <- rep(vector.demandas)
  input$matriz.distancia <- matrix(rep(matriz.distancia), nrow = nrow(matriz.distancia))
  input$capacidad.truck <- capacidad.truck
  input$capacidad.trailer <- capacidad.trailer
  input$capacidad.vehiculo <- input$capacidad.truck+input$capacidad.trailer
  input$n1 <- n1

  return(input)
}

#' Create and initialize the main structure for MC-TTRP problem
#'
#' @param input Input structure list
#' @return The CWTTRPstruct list, where the global variables of the solver are
#' managed.
createResultStruct_MCTTRP<-function(rutas, coste.total, R, Rhat, Tolvas, H.camion_res,
                                    H.trailer_res, demandas_res, rutas.des, input){
  # result$res <<- repasar
  result=list()
  result$routes <- rutas
  result$cost <- coste.total
  result$R <- R
  result$Rhat <- Rhat
  result$Tolvas <- Tolvas
  result$H.camion_res <- H.camion_res
  result$H.trailer_res <- H.trailer_res
  result$demandas_res <- demandas_res
  result$rutas.des <- rutas.des
  n_trailers <- 0
  n_trucks <- 0
  ptr <- 0
  pvr <- 0
  cvr <- 0
  result_res <- create_result_struct(rutas, input, "MCTTRP")
  for (i in 1:length(result_res)) {
    if (result_res[[i]]$type == "PTR") {
      n_trucks <- n_trucks + 1
      ptr <- ptr + 1
    } else {
      n_trucks <- n_trucks + 1
      n_trailers <- n_trailers + 1
      if (result_res[[i]]$type == "PVR") pvr <- pvr + 1
      if (result_res[[i]]$type == "CVR") cvr <- cvr + 1
    }
    result_res[[i]]$clients <- list() 
    num_clients <- delete_zeros(unique(result_res[[i]]$route))
    counter <- 1
    for (j in num_clients) {
      client <- list()
      client$id <- j
      client$demands <- result$demandas_res[j+1,]
      client$hoppers <- list()
      counter2 <- 1
      for (z in 1:length(result$Tolvas[,1])) {
        if (result$Tolvas[z,1] == j) {
          client$hoppers[[counter2]] <- result$Tolvas[z,]
          counter2 <- counter2 + 1
        }
      }
      result_res[[i]]$clients[[counter]] <- client
      counter <- counter + 1
    }
  }

  result$n_trucks <- n_trucks
  result$n_trailers <- n_trailers
  result$PTR <- ptr
  result$PVR <- pvr
  result$CVR <- cvr
  result$result_res <- result_res

  return(result)
}

#' Create S matrix
#'
#' @param matriz.distancia
#' @param n
#' @return Savings matrix
matrixS<-function(matriz.distancia,n){
  #matrixS: Calcular la matriz de ahorros "usual", donde S_ij=c_0i+c_j0-c_ij

  S<-matrix(0,nrow=n,ncol=n) #Matriz ahorros
  rownames(S)<-0:(n-1)
  colnames(S)<-0:(n-1)

  for(i in 2:n){
    for(j in 2:n){
      {
        if(i!=j){
          S[i,j]<-matriz.distancia[1,i]+matriz.distancia[1,j]-matriz.distancia[i,j]
        }
      }
    }
  }
  return(S)
}


#' Create S matrix
#'
#' @param matriz.distancia
#' @param n
#' @return Savings matrix
matrixS_2<-function(matriz.distancia,n, n1, cost){
  #matrixS: Calcular la matriz de ahorros "usual", donde S_ij=c_0i+c_j0-c_ij

  S<-matrix(0,nrow=n,ncol=n) #Matriz ahorros
  rownames(S)<-0:(n-1)
  colnames(S)<-0:(n-1)

  for(i in 2:n){
    for(j in 2:n){
      {
        if(i!=j){
          weight <- 1
          if (((i > n1 ) && (j <= n1)) || ((i <= n1 ) && (j > n1))) weight <- cost
          S[i,j]<-(matriz.distancia[1,i]+matriz.distancia[1,j]-matriz.distancia[i,j]) * weight

        }
      }
    }
  }
  #print(S)
  return(S)
}

#' Create Shat matrix
#'
#' @param matriz.distancia
#' @param n
#' @param n1
#' @return  Savings matrix in surtours
matrixShat<-function(matriz.distancia,n, n1){
  #matrixShat: Calcular la matriz de ahorros hat, modificada: cuando los dos clientes
  # son de tipo v.c. o de tipo t.c., los ahorros se calculan de la forma usual. Cuando
  # uno de los clientes es de tipo v.c. y el otro de tipo t.c., entonces la cosa cambia:
  # calculamos S_ij=2c_0i-2c_j0 (pues asumimos que se puede formar una ruta del tipo 0-i-j-i-0)

  Shat<-matrix(0,nrow=n,ncol=n) #Matriz ahorros hat
  colnames(Shat) <- 0:(n-1)
  rownames(Shat) <- 0:(n-1)

  for(i in 2:(n1+1)){
    for(j in 2:(n1+1)){
      {
        if(i!=j){
          Shat[i,j]<-matriz.distancia[1,i]+matriz.distancia[1,j]-matriz.distancia[i,j]
        }
      }
    }
  }
  for(i in (n1+2):n){
    for(j in (n1+2):n){
      if (i!=j){
        Shat[i,j]<-matriz.distancia[1,i]+matriz.distancia[1,j]-matriz.distancia[i,j]
      }
    }
  }
  for(i in 2:(n1+1)){
    for(j in (n1+2):n){
      Shat[i,j]<-2*matriz.distancia[j,1]-2*matriz.distancia[i,j]
    }
  }
  for(i in (n1+2):n){
    for(j in 2:(n1+1)){
      Shat[i,j]<-2*matriz.distancia[i,1]-2*matriz.distancia[i,j]
    }
  }
  return(Shat)
}

#' Create final output data in MC-TTRP
#'
#' @param input Input structure list
#' @return A list with all information about the route.
createFinalResult_TTRP<-function(rutas, R, Rhat, matriz.distancia, result_res, vector.demandas){
  # rutas[which(rutas==0)]<-1
  coste.total<-0
  for(i in 1:(length(rutas)-1)){
    coste.total<-coste.total+matriz.distancia[rutas[i]+1,rutas[i+1]+1]
  }
  # rutas<-rutas-1
  # Output
  n_trucks <- 0
  n_trailers <- 0
  ptr <- 0
  pvr <- 0
  cvr <- 0
  for (i in 1:length(result_res)) {
      if (result_res[[i]]$type == "PTR") {
        n_trucks <- n_trucks + 1
        ptr <- ptr + 1
      } else {
        n_trucks <- n_trucks + 1
        n_trailers <- n_trailers + 1
        if (result_res[[i]]$type == "PVR") pvr <- pvr + 1
        if (result_res[[i]]$type == "CVR") cvr <- cvr + 1
      }

      result_res[[i]]$clients <- list() 
      
        num_clients <- delete_zeros(unique(result_res[[i]]$route))
        counter <- 1
        for (j in num_clients) {
          client <- list()
          client$id <- j
          client$demands <- vector.demandas[j+1]
          result_res[[i]]$clients[[counter]] <- client
          counter <- counter + 1
        }    
  }

  result = list()
  result$routes = rutas
  result$cost = coste.total
  result$R = R
  result$Rhat = Rhat
  result$n_trucks = n_trucks
  result$n_trailers = n_trailers
  result$PVR <- pvr
  result$PTR <- ptr
  result$CVR <- cvr
  result$result_res <- result_res

  return(result)
}

#' Create final output data in TTRP
#'
#' @param input Input structure list
#' @return A list with all information about the route.
createCWTTRPStruct<-function(S, Sm, n){
  new = list()
  new$Positionfilas <- 0
  new$Positioncolumnas <- 0
  new$Positionfilas2 <- 0
  new$Positioncolumnas2 <- 0

  pos = list()
  pos$Positionfilas <- 0
  pos$Positioncolumnas <- 0

  CWTTRP_struct = list()
  CWTTRP_struct$new <- new
  CWTTRP_struct$pos <- pos
  CWTTRP_struct$CargaT <- 0
  CWTTRP_struct$current_type <- -1

  CWTTRP_struct$iter <- 0
  CWTTRP_struct$parking_list <- list()
  CWTTRP_struct$parking_list <- 0

  return(CWTTRP_struct)
}

