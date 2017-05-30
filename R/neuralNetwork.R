

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
                                 
                               }
                            )
                             
                             
  
)

n <- neuralNetwork(inputnodes = 3, hiddennodes = 3, outputnodes = 3)
n$initalize()