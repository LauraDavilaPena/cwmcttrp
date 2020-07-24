check_tolvas<-function(position, Tolvas, n.truck, n.trailer){

  if(sum(Tolvas[,1]==(position-1))>0){
    pos.min <- min(which(Tolvas[,1]==(position-1)))
    pos.max <- max(which(Tolvas[,1]==(position-1)))
    if (Tolvas[pos.min,3] == "truck" ){
      n.truck   <- Tolvas[pos.min,4];
      n.truck   <- as.numeric(n.truck)
    }
    if (Tolvas[pos.max,3] == "truck"){
      n.truck   <- Tolvas[pos.max,4];
      n.truck   <- as.numeric(n.truck)
    }
    if (Tolvas[pos.min,3] == "trailer"){
      n.trailer <- Tolvas[pos.min,4];
      n.trailer <- as.numeric(n.trailer)
    }
    if (Tolvas[pos.max,3] == "trailer"){
      n.trailer <- Tolvas[pos.max,4];
      n.trailer <- as.numeric(n.trailer)
    }
  }

  result <- list()
  result$n.truck   <- n.truck
  result$n.trailer <- n.trailer

  return(result)
}

delete_zeros_tolvas<-function(Tolvas,t){
  for(ii in (t-1):1){
    if (Tolvas[ii,1]==0){
      contador <- 0
      for (jj in ii:(t-1)){
        Tolvas[ii+contador,] <- Tolvas[ii+contador+1,]
        contador <- contador + 1
      }
      Tolvas[t,] <- rep(0,dim(Tolvas)[2])
    }
  }
  t <- min(which(Tolvas[,1]==0))

  results<-list()
  results$Tolvas<-Tolvas
  results$t<-t

  return(results)
}

is_parking<-function(Rhat, Tolvas, input, CWTTRP_struct, pos1, pos2, n.truck, n.trailer, parking, option){

  if (option=="row") {
    rr <- 1
  } else {
    rr <- 3
  }

  if (Rhat[pos1,rr]!=0) {
    if (parking == 1) {
      CWTTRP_struct$parking[CWTTRP_struct$park_index] <- pos1
      CWTTRP_struct$park_index <- CWTTRP_struct$park_index + 1
    }
    pos2 <- pos1-1

    result_sub <- addWorkload_Rhat(Rhat,Tolvas, input,   pos1,
                                   pos2, CWTTRP_struct$CargaT,
                                   n.truck, n.trailer, option)
    CWTTRP_struct$CargaT <- result_sub$CargaT
    pos1 <- result_sub$pos1
    n.truck <- result_sub$n.truck
    n.trailer <- result_sub$n.trailer

  }

  result <- list()
  result$CWTTRP_struct <- CWTTRP_struct
  result$pos1 <- pos1
  result$pos2 <- pos2
  result$n.truck <- n.truck
  result$n.trailer <- n.trailer

  return(result)
}

is_in_parking_list<-function(CWTTRP_struct, position) {
  result <- 0
  for (i in 1:length(CWTTRP_struct$parking_list)) {
      if (position == CWTTRP_struct$parking_list[i]) {
        result <- 1
        break
      }
  }
  return(result)
}


check_in_parking_list<-function(CWTTRP_struct, position, R, input, option) {

  if (option == "left") dir <-1
  else if (option == "right") dir <-3

  subroute <- c(position-1)
  condition <- is_in_parking_list(CWTTRP_struct, position)
  while ((R[position,dir]!=0) && (condition == 0)) {
    position<-R[position,dir]+1
    condition <- is_in_parking_list(CWTTRP_struct, position)
  }


  return(condition)
}

check_in_parking_list_and_tc<-function(CWTTRP_struct, position1, position2,
                                                                  R, input) {
  result <- 1

  condition1 <- exist_tc(input, R, position1, "row")
  condition2 <- check_in_parking_list(CWTTRP_struct, position2, R, input, "right")
  if (condition1 && condition2) {
    result <- 0
  }

  condition2 <- check_in_parking_list(CWTTRP_struct, position1, R, input, "left")
  condition1 <- exist_tc(input, R, position2, "col")
  if (condition1 && condition2) {
    result <- 0
  }

  return(result)
}


exist_tc<-function(input, R, position, option){

  if (option=="row") {
    subroute <- R[position,2]
    while(R[position,1]!=0){
      subroute <- append(subroute, R[position,1])
      position<-R[position,1]+1
    }
  }

  else if (option=="col") {
    subroute <- R[position,2]
    while(R[position,3]!=0){
      subroute <- append(subroute, R[position,3])
      position<-R[position,3]+1
    }
  }

  current_type <- 0
  for (i in 1:length(subroute)) {
    if (subroute[i] > input$n1) {
      current_type <- 1
    }
  }

  return(current_type)

}

only_tc<-function(input, R, position, option){

  if (option=="row") {
    subroute <- R[position,2]
    while(R[position,1]!=0){
      subroute <- append(subroute, R[position,1])
      position<-R[position,1]+1
    }
  }

  else if (option=="col") {
    subroute <- R[position,2]
    while(R[position,3]!=0){
      subroute <- append(subroute, R[position,3])
      position<-R[position,3]+1
    }
  }

  current_type <- 0
  for (i in 1:length(subroute)) {
    if (subroute[i] > input$n1) {
      current_type <- current_type + 1
    }
  }

  result <- 0
  if (current_type == length(subroute)) {
    result <- 1
  }
  return(result)

}

load_manager<-function(CWTTRP_struct, input, vc_load_route1 , vc_load_route2 ,
                       tc_load_route1 , tc_load_route2 , vc_load_subroute1,
                       vc_load_subroute2, tc_load_subroute1, tc_load_subroute2){
  unfeasibility <- 1
  capacity <- 0
  load_truck <- 0

  if ((vc_load_subroute1+vc_load_subroute2+tc_load_subroute1+tc_load_subroute2) == 0) {
    if ((tc_load_route1 > 0)||(tc_load_route2 > 0)) {
      capacity <- input$capacidad.truck
      CWTTRP_struct$CargaT = vc_load_route1 + vc_load_route2 + tc_load_route1 + tc_load_route2
      load_truck <- tc_load_route1 + tc_load_route2
      unfeasibility <- CWTTRP_struct$CargaT - capacity
    } else {
      capacity <- input$capacidad.vehiculo
      CWTTRP_struct$CargaT = vc_load_route1 + vc_load_route2
      unfeasibility <- CWTTRP_struct$CargaT - capacity
    }
  }
  else {
    capacity <- input$capacidad.vehiculo
    CWTTRP_struct$CargaT <- vc_load_route1 + vc_load_route2 + tc_load_route1 + tc_load_route2 +
      vc_load_subroute1 + vc_load_subroute2 + tc_load_subroute1 + tc_load_subroute2
    unfeasibility <- CWTTRP_struct$CargaT - capacity
    load_truck <- tc_load_route1 + tc_load_route2 + vc_load_subroute1 + vc_load_subroute2 +
      tc_load_subroute1 + tc_load_subroute2
    if (load_truck > input$capacidad.truck) unfeasibility <- 1
  }

  result <- list()
  result$unfeasibility <- unfeasibility
  result$capacity <- capacity
  result$load_truck <- load_truck
  result$CWTTRP_struct <- CWTTRP_struct

  return(result)
}


load_manager_create_subroute<-function(CWTTRP_struct, input, vc_load_route1 ,
                                       vc_load_route2 , tc_load_route1 , tc_load_route2,
                                       option){
  unfeasibility <- 0
  capacity <- 0
  load_truck <- 0

  subroute_load <- 0
  main_route_load <- 0

  if (option == "vctc"){
    subroute_load <- vc_load_route2 + tc_load_route2
    main_route_load <- vc_load_route1 + tc_load_route1
  }
  else if (option == "tcvc") {
    subroute_load <- vc_load_route1 + tc_load_route1
    main_route_load <- vc_load_route2 + tc_load_route2
  }

  load_truck <- tc_load_route2 + tc_load_route1
  if (subroute_load > input$capacidad.truck ) unfeasibility <- 1
  if ((main_route_load+subroute_load) > input$capacidad.vehiculo ) unfeasibility <- 1
  if (load_truck > input$capacidad.truck ) unfeasibility <- 1

  capacity <- input$capacidad.truck
  CWTTRP_struct$CargaT <- vc_load_route1 + tc_load_route1 + vc_load_route2 + tc_load_route2

  result <- list()
  result$unfeasibility <- unfeasibility
  result$capacity <- capacity
  result$load_truck <- load_truck
  result$CWTTRP_struct <- CWTTRP_struct

  return(result)
}
