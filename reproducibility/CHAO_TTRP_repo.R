library(mcttrpcw)

options(show.error.locations = TRUE)
options(error=function()traceback(2))
options(max.print=3000)
N <- 21
init <- 1

for (i in init:N) {
  if (i < 10) {
    string <- paste0("instances\\CHAO_TTRP_0",i,".txt")
    result <- CW_algorithm(string, "TTRP", 0)
  } else {
    string <- paste0("instances\\CHAO_TTRP_",i,".txt")
    result <- CW_algorithm(string, "TTRP", 0)
  }

}
