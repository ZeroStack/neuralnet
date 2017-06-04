
# Basic style guide 
## global variables and functions space separateed by _
### local variables space separated by .
#### No camelCase

# Neural network class
neural_network<- setRefClass("neuralNetwork",
                             # input fields, weights paramters are not inputs but created from inputs
                             ## These fields will exist in the object and be changed over iterations
                             fields = list(inputnodes = 'numeric', hiddennodes = 'numeric', outputnodes = 'numeric',
                                           learningrate = 'numeric',
                                           weightshidden = 'matrix', weightsoutput = 'matrix'
                                           
                             ),
                             methods = list(
                               initalize = function() {
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
                                 
                                 # convert inputs to matrix
                                 inputs <- matrix(inputs_list)
                                 targets <- matrix(targets_list)
                                 
                                 # hidden inputs
                                 hidden_inputs <- .self$weightshidden %*% inputs
                                 # hidden outputs
                                 hidden_outputs <- .self$activation_function(hidden_inputs)
                                 
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
                                 final_outputs = .self$activation_function(final_inputs)
                                 
                                 
                                 return(final_outputs)
                                 
                               }
                            )
                             
                             
  
)





# Set working directory properly
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

input_nodes = 784
hidden_nodes = 100
output_nodes = 10
learning_rate = 0.3

n <- neural_network(inputnodes = input_nodes, hiddennodes = hidden_nodes, outputnodes = output_nodes, learningrate = learning_rate)
n$initalize()

library(data.table)

train <- fread('mnist_train.csv')
test <- fread('mnist_test.csv')

train_m <- nrow(train)
for(row in 1:train_m) {

  # Convert row to single numeric
  in.train <- as.numeric(train[row,])
  
  # Select the row without the target (target = index 1)
  input <- scale_input(in.train[-1])
  
  # Assign the target variable
  target <- in.train[1]
  message("Target is : ", target)

  # Create a empty 0 matrix with the rows of possible outputs
  target.matrix <- matrix(0L, nrow = output_nodes) + 0.01

  
  # Note, R is zero indexed, so the target needs to have 1 added to it.
  ## If the target is 0, it should be 1 in the matrix
  target.matrix[target+1,] <- 0.99
  
 
  n$train(input, target.matrix)


  print(row)

}


# test the neural network
scorecard = list()

test_m <- nrow(test)

for(record in 1:test_m) {
  
  # intest
  intest <- as.numeric(test[record, ])
  
  correct.label <- intest[1]
  
  input <- scale_input(intest[-1])
  
  # predict the test
  output <- n$query(input)
  
  predicted <- which.max(output) - 1
  
  if(correct.label == predicted) {
    message(paste('correct label', correct.label, predicted))
    scorecard <- append(scorecard, 1)
  } else {
    message(paste('wrong label label', correct.label, predicted))
    scorecard <- append(scorecard, 0)
  }
  
  
}





