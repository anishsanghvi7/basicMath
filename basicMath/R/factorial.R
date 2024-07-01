factorial <-
function(x) {
  i <- x
  total <- 1
  while (i != 0){
    total <- total * i
    i <- i - 1
  }

  return(total)
}
