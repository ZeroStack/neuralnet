# Scale the input variables
scale_input <- function(X) {
  X.scale <- ((X/255.0)*0.99)+0.01
}
