library(mcttrpcw)

options(show.error.locations = TRUE)
options(error=function()traceback(2))
options(max.print=3000)
problems <- c("P1", "P2","P3","P4","P5","P6","P7","P8")

for (i in problems) {
    string <- paste0("instances\\", i ,".csv")
    result <- CW_algorithm(string, "MCTTRP", 2, 1, 0)
}


result1 <- MCTTRP_opt_solver("instances/P1.csv", "MCTTRP", 2, 1, total_time=60, total_iterations=1, verbose=0)



result2 <- CW_algorithm("instances/P1.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P2.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P3.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P4.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P5.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P6.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P7.csv", "MCTTRP", 2, 1, 0)
result2 <- CW_algorithm("instances/P8.csv", "MCTTRP", 2, 1, 0)



result[[1]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_02.txt", "MCTTRP", 8, 5, total_time=60, total_iterations=2, verbose=0)
result[[1]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_03.txt", "MCTTRP", 8, 5, total_time=60, total_iterations=2, verbose=0)
result[[1]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_04.txt", "MCTTRP", 8, 5, total_time=60, total_iterations=2, verbose=0)
result[[1]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_01.txt", "MCTTRP", 8, 5, total_time=60, total_iterations=2, verbose=0)
result[[1]] <- MCTTRP_opt_solver("instances/CHAO_MCTTRP_01.txt", "MCTTRP", 8, 5, total_time=60, total_iterations=2, verbose=0)
