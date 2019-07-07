to_binary <- function(y){
  base_char <- unique(y)
  yx <- as.numeric(as.factor(y))
  yx <- ifelse(yx == 1, 0, 1)
  return(list(base_char = base_char, y = yx))
}
