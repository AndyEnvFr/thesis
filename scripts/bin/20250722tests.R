names(runz) <- getNames(runz)
runz <- lapply(runz, function(x){
  names(x$Output) <- x$Model$runs
  return(x)
})

par(mfrow = c(7, 7), mar = c(1,3,3,1))  # 7x7 Layout für 49 Plots
for (name in names(runz)) {
  mat <- runz[[name]]$Output$`1000`$specMat
  image(mat, main = name)
}




test <- readRDS("~/cyber_synch/local/runs/mstr/20250722/runs.rds")
names(test) <- getNames(test)
test <- lapply(test, function(x){
  names(x$Output) <- x$Model$runs
  return(x)
})

par(mfrow = c(7, 7), mar = c(1,3,3,1))  # 7x7 Layout für 49 Plots
for (name in names(test)) {
  mat <- test[[name]]$Output$`262501`$specMat
  image(mat, main = name)
}

names(try128) <- getNames(try128)
try128 <- lapply(try128, function(x){
  names(x$Output) <- x$Model$runs
  return(x)
})

par(mfrow = c(7, 7), mar = c(1,3,3,1))  # 7x7 Layout für 49 Plots
for (name in names(try128)) {
  mat <- try128[[name]]$Output$`1000`$specMat
  image(mat, main = name)
}

getSpecTime(runs = test, ymax = 40)

