library(mcttrpcw)

options(show.error.locations = TRUE)
options(error=function()traceback(2))
options(max.print=3000)
problems <- c("P1", "P2","P3","P4","P5","P6","P7","P8")

for (i in problems) {
    string <- paste0("instances\\", i ,".csv")
    result <- CW_algorithm(string, "MCTTRP", 0)
}
