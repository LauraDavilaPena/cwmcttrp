library(mcttrpcw)

options(show.error.locations = TRUE)
options(error=function()traceback(2))
options(max.print=3000)
#N <- 1
#init <- 1

#for (i in init:N) {
#  if (i < 10) {
#    string <- paste0("instances\\CHAO_MCTTRP_0",i,".txt")
#    result <- CW_algorithm(string, "MCTTRP", 0)
#  } else {
#    string <- paste0("instances\\CHAO_MCTTRP_",i,".txt")
#    result <- CW_algorithm(string, "MCTTRP", 0)
#  }
#}

result <- list()
result[[1]] <- CW_algorithm("instances/CHAO_MCTTRP_01.txt", "MCTTRP", 8, 5, 0)
result[[2]] <- CW_algorithm("instances/CHAO_MCTTRP_02.txt", "MCTTRP", 8, 5, 0)
result[[3]] <- CW_algorithm("instances/CHAO_MCTTRP_03.txt", "MCTTRP", 8, 5, 0)

result[[4]] <- CW_algorithm("instances/CHAO_MCTTRP_04.txt", "MCTTRP", 14, 8, 0)
result[[5]] <- CW_algorithm("instances/CHAO_MCTTRP_05.txt", "MCTTRP", 14, 8, 0)
result[[6]] <- CW_algorithm("instances/CHAO_MCTTRP_06.txt", "MCTTRP", 14, 8, 0)

result[[7]] <- CW_algorithm("instances/CHAO_MCTTRP_07.txt", "MCTTRP", 12, 8, 0)
result[[8]] <- CW_algorithm("instances/CHAO_MCTTRP_08.txt", "MCTTRP", 12, 8, 0)
result[[9]] <- CW_algorithm("instances/CHAO_MCTTRP_09.txt", "MCTTRP", 12, 8, 0)

result[[10]] <- CW_algorithm("instances/CHAO_MCTTRP_10.txt", "MCTTRP", 18, 12, 0)
result[[11]] <- CW_algorithm("instances/CHAO_MCTTRP_11.txt", "MCTTRP", 18, 12, 0)
result[[12]] <- CW_algorithm("instances/CHAO_MCTTRP_12.txt", "MCTTRP", 18, 12, 0)

result[[13]] <- CW_algorithm("instances/CHAO_MCTTRP_13.txt", "MCTTRP", 26, 14, 0)
result[[14]] <- CW_algorithm("instances/CHAO_MCTTRP_14.txt", "MCTTRP", 26, 14, 0)
result[[15]] <- CW_algorithm("instances/CHAO_MCTTRP_15.txt", "MCTTRP", 26, 14, 0)

result[[16]] <- CW_algorithm("instances/CHAO_MCTTRP_16.txt", "MCTTRP", 11, 6, 0)
result[[17]] <- CW_algorithm("instances/CHAO_MCTTRP_17.txt", "MCTTRP", 11, 6, 0)
result[[18]] <- CW_algorithm("instances/CHAO_MCTTRP_18.txt", "MCTTRP", 11, 6, 0)

result[[19]] <- CW_algorithm("instances/CHAO_MCTTRP_19.txt", "MCTTRP", 15, 8, 0)
result[[20]] <- CW_algorithm("instances/CHAO_MCTTRP_20.txt", "MCTTRP", 15, 8, 0)
result[[21]] <- CW_algorithm("instances/CHAO_MCTTRP_21.txt", "MCTTRP", 15, 8, 0)

for (i in 1:length(result)) {
  print(paste0(i,"  cost: ", result[[i]]$cost))
}



result <- list()
result[[1]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_01.txt", "MCTTRP", 8, 5, total_time=60, total_iterations=2, verbose=0)
result[[2]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_02.txt", "MCTTRP", 8, 5, total_time=60, total_iterations=2, verbose=0)
result[[3]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_03.txt", "MCTTRP", 8, 5, total_time=60, total_iterations=2, verbose=0) # x

result[[4]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_04.txt", "MCTTRP", 14, 8, total_time=60, total_iterations=2, verbose=0)
result[[5]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_05.txt", "MCTTRP", 14, 8, total_time=60, total_iterations=2, verbose=0)
result[[6]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_06.txt", "MCTTRP", 14, 8, total_time=60, total_iterations=2, verbose=0) # x

result[[7]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_07.txt", "MCTTRP", 12, 8, total_time=60, total_iterations=2, verbose=0)
result[[8]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_08.txt", "MCTTRP", 12, 8, total_time=60, total_iterations=2, verbose=0)
result[[9]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_09.txt", "MCTTRP", 12, 8, total_time=60, total_iterations=2, verbose=0)

result[[10]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_10.txt", "MCTTRP", 18, 12, total_time=60, total_iterations=2, verbose=0)
result[[11]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_11.txt", "MCTTRP", 18, 12, total_time=60, total_iterations=2, verbose=0)
result[[12]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_12.txt", "MCTTRP", 18, 12, total_time=60, total_iterations=2, verbose=0)

result[[13]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_13.txt", "MCTTRP", 26, 14, total_time=60, total_iterations=2, verbose=0)
result[[14]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_14.txt", "MCTTRP", 26, 14, total_time=60, total_iterations=2, verbose=0)
result[[15]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_15.txt", "MCTTRP", 26, 14, total_time=60, total_iterations=2, verbose=0)

result[[16]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_16.txt", "MCTTRP", 11, 6, total_time=60, total_iterations=2, verbose=0)
result[[17]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_17.txt", "MCTTRP", 11, 6, total_time=60, total_iterations=2, verbose=0)
result[[18]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_18.txt", "MCTTRP", 11, 6, total_time=60, total_iterations=2, verbose=0)

result[[19]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_19.txt", "MCTTRP", 15, 8, total_time=60, total_iterations=2, verbose=0)
result[[20]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_20.txt", "MCTTRP", 15, 8, total_time=60, total_iterations=2, verbose=0)
result[[21]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_21.txt", "MCTTRP", 15, 8, total_time=60, total_iterations=2, verbose=0)

for (i in 1:length(result)) {
  print(paste0(i,"  cost: ", result[[i]]$cost))
}

