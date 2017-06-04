# Display the digit using the array
display_digit <- function(X){
  m <- matrix(unlist(X),nrow = 28,byrow = T)
  m <- t(apply(m, 2, rev))
  image(m,col=grey.colors(255))
}