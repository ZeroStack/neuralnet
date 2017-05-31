

neuralNetwork <- setRefClass("neuralNetwork",
                             # input fields, weights paramters are not inputs but created from inputs
                             ## weights have been declared here because I cannot get them to be an assigned value otherwise
                             fields = list(inputnodes = 'numeric',
                                           hiddennodes = 'numeric',
                                           outputnodes = 'numeric',
                                           learningrate = 'numeric',
                                           weightshidden = 'matrix',
                                           weightsoutput = 'matrix'
                                           
                             ),
                             methods = list(
                               initalize = function(){
                                 # Create the hidden layer of weights
                                 .self$weightshidden <- matrix( 
                                   rnorm(hiddennodes*inputnodes, mean=0, sd=inputnodes**-0.5), 
                                   nrow = hiddennodes, ncol = inputnodes
                                   )
                                 
                                 # Create the output layer of weights
                                 .self$weightsoutput <- matrix(
                                   rnorm(outputnodes*hiddennodes, mean=0, sd=hiddennodes**-0.5),
                                   nrow = hiddennodes, ncol=inputnodes
                                     
                                   )
                                 
                                 return(invisible(NULL))
                                 
                               },
                               # sigmoid function to supress weights
                               activation_function = function(x){
                                 1 / (1 + exp(-x))
                               },
                               # training function
                               train = function(inputs_list, targets_list) {
                                 
                                 # convert inputs to relevant matrix
                                 inputs <- matrix(inputs_list)
                                 targets <- matrix(targets_list)
                                 
                
                                 
                                 # hidden inputs
                                 hidden_inputs <- crossprod(.self$weightshidden, inputs)
                                 # hidden outputs
                                 hidden_outputs <- .self$activation_function(hidden_inputs)
                                 
                                 # final inputs
                                 final_inputs <- crossprod(.self$weightsoutput, hidden_outputs)
                                 # final outputs
                                 final_outputs <- .self$activation_function(final_inputs)
                                 
                                 # output errors (target-actual)
                                 output_errors <- targets - final_outputs
                                 
                                 
                                 hidden_errors <- crossprod(t(.self$weightsoutput), output_errors)
                                 
                                 # output layer weights update
                                
                                 
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
                                 hidden_inputs = crossprod(.self$weightshidden, inputs)
                                 hidden_outputs = .self$activation_function(hidden_inputs)
                                 
                                 # final layer
                                 final_inputs = crossprod(.self$weightsoutput, hidden_outputs)
                                 final_outputs = .self$activation_function(hidden_inputs)
                                 
                                 
                                 return(final_outputs)
                                 
                               }
                            )
                             
                             
  
)

displayDigit <- function(X){
  m <- matrix(unlist(X),nrow = 28,byrow = T)
  m <- t(apply(m, 2, rev))
  image(m,col=grey.colors(255))
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
  
  observation <- as.numeric(train[row])
  
  inputs <- ((observation/255.0)*0.99)+0.01
  
  targets <- matrix(0L, nrow = output_nodes)
  
  targets[observation[1]] <- 0.99
  
  tr <<- targets
  
  ip <<- inputs
  n$train(inputs, targets)
  
  
  print(row)
  
}

train.c <- (train[1,-1]/255.0*0.99)+0.01


#image(matrix(unlist(train[1][,-1]), ncol=28, nrow=28))




