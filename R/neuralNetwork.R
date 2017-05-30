

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
                                 
                                 print(final_outputs)
                                 
                                 print(output_errors)
                                 
                                 hidden_errors <- crossprod(t(.self$weightsoutput), output_errors)
                                 
                                 .selfweightsoutput <- .selfweightsoutput + (learningrate *(crossprod((output_errors*final_inputs*(1-final_outputs)),  ))) 
                                 print(hidden_errors)
                                 
                               }
                            )
                             
                             
  
)

n <- neuralNetwork(inputnodes = 3, hiddennodes = 3, outputnodes = 3)
n$initalize()

n$train(c(1,2,3), c(1,2,3))

