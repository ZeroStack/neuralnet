

neuralNetwork <- setRefClass("neuralNetwork",
                             # input fields, weights paramters are not inputs but created from inputs
                             ## These fields will exist in the object and be changed over iterations
                             fields = list(inputnodes = 'numeric', hiddennodes = 'numeric', outputnodes = 'numeric',
                                           learningrate = 'numeric',
                                           weightshidden = 'matrix', weightsoutput = 'matrix'
                                           
                             ),
                             methods = list(
                               initalize = function(){
                                 # Create the hidden layer of weights
                                 .self$weightshidden <- matrix( 
                                   rnorm(hiddennodes*inputnodes, mean=0, sd=hiddennodes**-0.5), 
                                   nrow = hiddennodes, ncol = inputnodes
                                   )
                                 
                                 # Create the output layer of weights
                                 .self$weightsoutput <- matrix(
                                   rnorm(outputnodes*hiddennodes, mean=0, sd=outputnodes**-0.5),
                                   nrow = outputnodes, ncol=hiddennodes
                                     
                                   )
                                 
                                 # Return nothing as the object properties will be changed over iterations
                                 return(invisible(NULL))
                                 
                               },
                               # sigmoid function to supress weights to 0 to 1
                               activation_function = function(x){
                                 1 / (1 + exp(-x))
                               },
                               # training function
                               train = function(inputs_list, targets_list) {
                                 
                                 
                                 
                                 ip <<- inputs_list
                                 tr <<- targets_list
                                 
                                 # convert inputs to matrix
                                 inputs <- matrix(inputs_list)
                                 targets <- matrix(targets_list)
                                 
                                 # hidden inputs
                                 hidden_inputs <- .self$weightshidden %*% inputs
                                 # hidden outputs
                                 hidden_outputs <<- .self$activation_function(hidden_inputs)
                                 
                                 # final inputs
                                 final_inputs <- .self$weightsoutput %*% hidden_outputs
                                 # final outputs
                                 final_outputs <- .self$activation_function(final_inputs)
                                 
                                 # output errors (target-actual)
                                 output_errors <- targets - final_outputs
                                 
                                 
                                 hidden_errors <- crossprod(.self$weightsoutput, output_errors)
                                 

                                 # output weights update (3rd layer)
                                 .self$weightsoutput <- .self$weightsoutput + learningrate*( tcrossprod( x = (output_errors*final_outputs*(1-final_outputs)),
                                                                                                         y = hidden_outputs)  )
                                 
                                 # hidden weights update (2nd layer)
                                 .self$weightshidden <- .self$weightshidden + learningrate*( tcrossprod( x = (hidden_errors*hidden_outputs*(1-hidden_outputs)),
                                                                                                         y = inputs)  )
                                 
                                 return(invisible(NULL))
                               },
                               query = function(inputs_list){
                                 
                                 # conver to matrix
                                 inputs = matrix(inputs_list)
                                 
                                 # input layer
                                 hidden_inputs = .self$weightshidden %*% inputs
                                 hidden_outputs = .self$activation_function(hidden_inputs)
                                 
                                 # final layer
                                 final_inputs = .self$weightsoutput %*% hidden_outputs
                                 final_outputs = .self$activation_function(hidden_inputs)
                                 
                                 
                                 return(final_outputs)
                                 
                               }
                            )
                             
                             
  
)

display_digit <- function(X){
  m <- matrix(unlist(X),nrow = 28,byrow = T)
  m <- t(apply(m, 2, rev))
  image(m,col=grey.colors(255))
}

scale_input <- function(X) {
  
  X.numeric <- as.numeric(X)
  
  X.scale <- ((X.numeric/255.0)*0.99)+0.01
}

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

input_nodes = 784
hidden_nodes = 100
output_nodes = 10
learning_rate = 0.3

n <- neuralNetwork(inputnodes = input_nodes, hiddennodes = hidden_nodes, outputnodes = output_nodes, learningrate = learning_rate)
n$initalize()

library(data.table)

train <- fread('mnist_train.csv')
test <- fread('mnist_test.csv')

for(row in 1:nrow(train)) {

  # Select the row without the target (target = index 1)
  input <- scale_input(train[row,-1])
  
  # Assign the target variable
  target <- train[row,1]
  message("Target is : ", target)

  # Create a empty 0 matrix with the rows of possible outputs
  target.matrix <- matrix(0L, nrow = output_nodes) + 0.01

  # Note, R is zero indexed, so the target needs to have 1 added to it.
  ## If the target is 0, it should be 1 in the matrix
  target.matrix[target+1,] <- 0.99
  
  k <<- target.matrix

  print(target.matrix)
  n$train(inputs, targets)


  print(row)

}







