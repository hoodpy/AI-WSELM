# AI-WSELM
R code for paper: Active learning with extreme learning machine for online imbalanced multiclass classification

The main function is model(), including the following parameters:  
c: A trad-off between the training error and the generalization ablilty.

  node: The number of nodes in the hidden layer.

  threshold: Average threshold A, using for similarity query.

  train_path: The file path of training dataset.

  samples_number: The number of samples in the training dataset.

  test_path: The file path of testing dataset.

  single: TRURE or FALSE. If only one dataset (training and test data in one csv file), choose TRUE.

  bound_seletion: Upper bound Q of marign sampling, the default is 10.

  margin_threshold: Uncertainty threshold G, using for margin sampling, the default is 0.2.

