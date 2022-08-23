# AI-WSELM
R code for paper: Active learning with extreme learning machine for online imbalanced multiclass classification

The main function is model(), including the following parameters:

	c: A trad-off between the training error and the generalization ablilty.  
	node: The number of nodes in the hidden layer.  
	threshold: Average threshold A, using for similarity query.  
	train_path: The file path of training dataset.  
	samples_number: The number of samples in the training dataset.  
	test_path: The file path of testing dataset.  
	single: TRURE or FALSE. If only one dataset (training and test data are included in one csv file), choose TRUE.  
	bound_seletion: Upper bound Q of marign sampling, the default is 10.  
	margin_threshold: Uncertainty threshold G, using for margin sampling, the default is 0.2.
	
If training data and test data are included in one csv file, single is TRUE, and the parameter train path is csv path, the test path is None. samples_number must be specified.

If training data and test data are two csv files, single is FALSE, train path and test path must be specified, samples_number can be None.

In the original code, since all the data used in the experiment have labels, the labels of samples selected by active learning are directly extracted. For the data set without labels, the code for obtaining the sample labels needs to be modified into the form of human-computer interaction, where the samples can be printed and the user can input the corresponding labels. (The variable instances_labels in line 121, 164, 225, 271)

![Figure_1](https://github.com/hoodpy/AI-WSELM/blob/main/%E5%BE%AE%E4%BF%A1%E5%9B%BE%E7%89%87_20220823165733.png)
![Figure_3](https://github.com/hoodpy/AI-WSELM/blob/main/%E5%BE%AE%E4%BF%A1%E5%9B%BE%E7%89%87_20220823165715.png)
