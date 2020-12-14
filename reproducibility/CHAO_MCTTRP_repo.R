library(mcttrpcw)

options(show.error.locations = TRUE)
options(error=function()traceback(2))
options(max.print=3000)
N <- 1
init <- 1

for (i in init:N) {
  if (i < 10) {
    string <- paste0("instances\\CHAO_MCTTRP_0",i,".txt")
    result <- CW_algorithm(string, "MCTTRP", 0)
  } else {
    string <- paste0("instances\\CHAO_MCTTRP_",i,".txt")
    result <- CW_algorithm(string, "MCTTRP", 0)
  }
}
