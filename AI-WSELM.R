matrix_vector <- function(number) {
    the_vector <- matrix(seq(1, by = 0, length = number), ncol = 1, nrow = number)
    return(the_vector)
}

sigmoid_function <- function(x) {
    x <- as.matrix(x)
    x_nrow <- nrow(x)
    x_ncol <- ncol(x)
    y <- matrix(nrow = x_nrow, ncol = x_ncol)
    for (i in 1:x_nrow) {
        for (j in 1:x_ncol) {
            y[i, j] <- 1 / (1 + exp(-x[i, j]))
        }
    }
    return(y)
}

I_function <- function(nrow) {
    y <- matrix(seq(0, by = 0, length = nrow ^ 2), nrow = nrow, ncol = nrow)
    for (i in 1:nrow(y)) {
        y[i, i] <- 1
    }
    return(y)
}

obtained_weight_matrix <- function(data_labels, original_labels_matrix) {
    present_labels_number <- ncol(data_labels)
    original_labels_number <- ncol(original_labels_matrix)
    if (present_labels_number != original_labels_number) {
        the_function <- matrix(seq(0, by = 0, length = original_labels_number * present_labels_number), nrow = original_labels_number, ncol = present_labels_number)
        for (i in 1:original_labels_number) {
            the_function[i, i] <- 1
        }
        original_labels_matrix <- original_labels_matrix %*% the_function
    }
    present_labels_data_number <- t(as.matrix(apply(data_labels, 2, sum)))
    original_labels_matrix <- original_labels_matrix + present_labels_data_number
    the_matrix_init <- numeric(length = present_labels_number)
    for (i in 1:present_labels_number) {
        the_matrix_init[i] <- original_labels_matrix[1] / original_labels_matrix[i]
    }
    the_matrix_init <- matrix(the_matrix_init, ncol = 1)
    the_matrix_next <- data_labels %*% the_matrix_init
    the_matrix_final <- matrix(seq(0, by = 0, length = (nrow(the_matrix_next)) ^ 2), nrow = nrow(the_matrix_next), ncol = nrow(the_matrix_next))
    for (i in 1:nrow(the_matrix_next)) {
        the_matrix_final[i, i] <- the_matrix_next[i]
    }
    the_labels_matrix <- matrix(seq(0, by = 0, length = original_labels_number * present_labels_number), nrow = original_labels_number, ncol = present_labels_number)
    for (i in 1:original_labels_number) {
        the_labels_matrix[i, i] <- 1
    }
    return(list(original_labels_matrix, the_matrix_final, the_labels_matrix))
}

sequence_detection <- function(data, dictionary, threshould) {
    the_output_index <- c()
    for (i in 1:nrow(data)) {
        the_matrix <- as.matrix(seq(1, by = 0, length = nrow(dictionary)), ncol = 1)
        the_example <- as.matrix(data[i,])
        the_example_dataframe <- as.data.frame(the_matrix %*% the_example)
        the_distance <- abs(dictionary - the_example_dataframe)
        the_manhadun_distance <- apply(the_distance, 1, sum)
        if (min(the_manhadun_distance) > threshould) {
            the_output_index <- append(the_output_index, as.numeric(row.names(data[i,])))
            dictionary <- rbind(dictionary, data[i,])
        }
    }
    return(list(dictionary, the_output_index))
}

normial <- function(x) {
    return((2 * (x - min(x)) / (max(x) - min(x))) - 1)
}

obtained_acc_G_mean <- function(x) {
    the_sum <- 0
    the_G_mean <- 1
    for (i in 1:nrow(x)) {
        the_sum <- the_sum + x[i, i]
        the_G_mean <- the_G_mean * (x[i, i] / sum(x[i,]))
    }
    the_acc <- the_sum / sum(x)
    the_G_mean <- the_G_mean ^ (1 / nrow(x))
    return(list(the_acc * 100, the_G_mean * 100))
}

model = function(c, node, threshold, train_path, samples_number = 0, test_path = "src", single = FALSE, bound_selection = 10, margin_threshold = 0.2) {
    if (single) {
        total_data = read.table(train_path, header = TRUE, sep = ",", stringsAsFactors = TRUE)
    } else {
        data_train = read.table(train_path, header = TRUE, sep = ",", stringsAsFactors = TRUE)
        data_test = read.table(test_path, header = TRUE, sep = ",", stringsAsFactors = TRUE)
        total_data = rbind(data_train, data_test)
        samples_number = nrow(data_train)
    }
    variables_number = ncol(total_data) - 1
    total_data$label = as.numeric(total_data$label)
    #start add label process
    #...
    #end add label process
    total_data_normial = as.data.frame(lapply(total_data[, c(1:variables_number)], normial))
    total_data = cbind(total_data_normial, total_data[variables_number + 1])
    threshold_distance = threshold * variables_number
    data <- total_data[c(1:samples_number),]
    testing_data <- total_data[-c(1:samples_number),]
    training_start = Sys.time()
    the_number_select <- 0
    original_labels_matrix <- matrix(c(0), nrow = 1, ncol = 1)
    number_read <- sample(1:(bound_selection * 10), 1, replace = TRUE)
    start_number <- 1
    end_number <- number_read
    data_batch <- data[c(start_number:end_number),]
    data_batch_variables <- data_batch[, c(1:variables_number)]
    dictionary <- data_batch_variables[1,]
    dictionary_output <- sequence_detection(data = data_batch_variables, dictionary = dictionary, threshould = threshold_distance)
    dictionary <- dictionary_output[[1]]
    samples_index <- append(1, dictionary_output[[2]])
    training_data <- data_batch[samples_index,]
    training_data_variables <- as.matrix(training_data[, c(1:variables_number)])
    instances_labels <- training_data[, variables_number + 1]
    categories <- unique(instances_labels)
    categories_length <- length(categories)
    training_data_labels <- as.data.frame(matrix(seq(0, by = 0, length = nrow(training_data_variables) * categories_length), nrow = nrow(training_data_variables), ncol = categories_length))
    names(training_data_labels) <- categories
    for (i in 1:categories_length) {
        position <- which(instances_labels == categories[i])
        training_data_labels[position, i] <- 1
    }
    training_data_labels <- as.matrix(training_data_labels)
    input_weight = matrix(rnorm(variables_number * node, mean = 0, sd = 1), nrow = variables_number, ncol = node)
    input_bisa = matrix(runif(node, min = -1, max = 1), nrow = 1, ncol = node)
    weight_result = obtained_weight_matrix(data_labels = training_data_labels, original_labels_matrix = original_labels_matrix)
    original_labels_matrix <- weight_result[[1]]
    H <- sigmoid_function(training_data_variables %*% input_weight + matrix_vector(number = nrow(training_data_variables)) %*% input_bisa)
    K <- I_function(nrow = node) / c + t(H) %*% weight_result[[2]] %*% H
    Beta <- solve(K) %*% t(H) %*% weight_result[[2]] %*% training_data_labels
    predicting_data <- data_batch[-samples_index,]
    if (nrow(predicting_data) != 0) {
        predicting_data_variables <- as.matrix(predicting_data[, c(1:variables_number)])
        H <- sigmoid_function(predicting_data_variables %*% input_weight + matrix_vector(number = nrow(predicting_data_variables)) %*% input_bisa)
        aim <- as.data.frame(H %*% Beta)
        if (ncol(aim) != 1) {
            aim$the_margin <- 1
            for (i in 1:nrow(aim)) {
                the_result_sample <- sort(aim[i, c(1:(ncol(aim) - 1))], decreasing = TRUE)
                aim[i, ncol(aim)] <- the_result_sample[1] - the_result_sample[2]
            }
            aim <- aim[order(aim$the_margin),]
            aim_select <- data.frame()
            for (i in 1:nrow(aim)) {
                if (aim[i, ncol(aim)] <= margin_threshold) {
                    aim_select <- rbind(aim_select, aim[i,])
                }
            }
            if (nrow(aim_select) > bound_selection) {
                aim_select <- aim_select[c(1:bound_selection),]
            }
            while (nrow(aim_select) != 0) {
                the_number_select <- the_number_select + nrow(aim_select)
                samples_index <- as.numeric(row.names(aim_select))
                training_data <- predicting_data[samples_index,]
                training_data_variables <- as.matrix(training_data[, c(1:variables_number)])
                instances_labels <- training_data[, variables_number + 1]
                categories_next <- unique(instances_labels)
                for (i in categories_next) {
                    if (i %in% categories == FALSE) {
                        categories <- append(categories, i)
                    }
                }
                categories_length <- length(categories)
                training_data_labels = as.data.frame(matrix(seq(0, by = 0, length = nrow(training_data_variables) * categories_length), nrow = nrow(training_data_variables), ncol = categories_length))
                names(training_data_labels) <- categories
                for (i in 1:categories_length) {
                    position <- which(instances_labels == categories[i])
                    training_data_labels[position, i] <- 1
                }
                training_data_labels <- as.matrix(training_data_labels)
                H = sigmoid_function(training_data_variables %*% input_weight + matrix_vector(number = nrow(training_data_variables)) %*% input_bisa)
                weight_result = obtained_weight_matrix(data_labels = training_data_labels, original_labels_matrix = original_labels_matrix)
                original_labels_matrix = weight_result[[1]]
                K = K + t(H) %*% weight_result[[2]] %*% H
                Beta = Beta %*% weight_result[[3]] + solve(K) %*% t(H) %*% weight_result[[2]] %*% (training_data_labels - H %*% Beta %*% weight_result[[3]])
                predicting_data = predicting_data[-samples_index,]
                if (nrow(predicting_data) == 0) {
                    break
                }
                predicting_data_variables <- as.matrix(predicting_data[, c(1:variables_number)])
                H <- sigmoid_function(predicting_data_variables %*% input_weight + matrix_vector(number = nrow(predicting_data_variables)) %*% input_bisa)
                aim <- as.data.frame(H %*% Beta)
                aim$the_margin <- 1
                for (i in 1:nrow(aim)) {
                    the_result_sample <- sort(aim[i, c(1:(ncol(aim) - 1))], decreasing = TRUE)
                    aim[i, ncol(aim)] <- the_result_sample[1] - the_result_sample[2]
                }
                aim <- aim[order(aim$the_margin),]
                aim_select <- data.frame()
                for (i in 1:nrow(aim)) {
                    if (aim[i, ncol(aim)] <= margin_threshold) {
                        aim_select <- rbind(aim_select, aim[i,])
                    }
                }
                if (nrow(aim_select) > bound_selection) {
                    aim_select <- aim_select[c(1:bound_selection),]
                }
            }
        }
    }
    while (end_number < samples_number) {
        original_samples_number <- end_number
        start_number <- end_number + 1
        number_read <- sample(1:(bound_selection * 10), 1, replace = TRUE)
        end_number <- end_number + number_read
        if (end_number > samples_number) {
            end_number <- samples_number
        }
        data_batch <- data[c(start_number:end_number),]
        data_batch_variables <- data_batch[, c(1:variables_number)]
        dictionary_output <- sequence_detection(data = data_batch_variables, dictionary = dictionary, threshould = threshold_distance)
        dictionary <- dictionary_output[[1]]
        if (length(dictionary_output[[2]]) != 0) {
            samples_index <- dictionary_output[[2]] - original_samples_number
            training_data <- data_batch[samples_index,]
            training_data_variables <- as.matrix(training_data[, c(1:variables_number)])
            instances_labels <- training_data[, variables_number + 1]
            categories_next <- unique(instances_labels)
            for (i in categories_next) {
                if (i %in% categories == FALSE) {
                    categories <- append(categories, i)
                }
            }
            categories_length <- length(categories)
            training_data_labels <- as.data.frame(matrix(seq(0, by = 0, length = nrow(training_data_variables) * categories_length), nrow = nrow(training_data_variables), ncol = categories_length))
            names(training_data_labels) <- categories
            for (i in 1:categories_length) {
                position <- which(instances_labels == categories[i])
                training_data_labels[position, i] <- 1
            }
            training_data_labels <- as.matrix(training_data_labels)
            H <- sigmoid_function(training_data_variables %*% input_weight + matrix_vector(number = nrow(training_data_variables)) %*% input_bisa)
            weight_result <- obtained_weight_matrix(data_labels = training_data_labels, original_labels_matrix = original_labels_matrix)
            original_labels_matrix <- weight_result[[1]]
            K <- K + t(H) %*% weight_result[[2]] %*% H
            Beta <- Beta %*% weight_result[[3]] + solve(K) %*% t(H) %*% weight_result[[2]] %*% (training_data_labels - H %*% Beta %*% weight_result[[3]])
            predicting_data <- data_batch[-samples_index,]
            if (nrow(predicting_data) != 0) {
                predicting_data_variables <- as.matrix(predicting_data[, c(1:variables_number)])
                H <- sigmoid_function(predicting_data_variables %*% input_weight + matrix_vector(number = nrow(predicting_data_variables)) %*% input_bisa)
                aim <- as.data.frame(H %*% Beta)
                if (ncol(aim) != 1) {
                    aim$the_margin <- 1
                    for (i in 1:nrow(aim)) {
                        the_result_sample <- sort(aim[i, c(1:(ncol(aim) - 1))], decreasing = TRUE)
                        aim[i, ncol(aim)] <- the_result_sample[1] - the_result_sample[2]
                    }
                    aim <- aim[order(aim$the_margin),]
                    aim_select <- data.frame()
                    for (i in 1:nrow(aim)) {
                        if (aim[i, ncol(aim)] <= margin_threshold) {
                            aim_select <- rbind(aim_select, aim[i,])
                        }
                    }
                    if (nrow(aim_select) > bound_selection) {
                        aim_select <- aim_select[c(1:bound_selection),]
                    }
                    while (nrow(aim_select) != 0) {
                        the_number_select <- the_number_select + nrow(aim_select)
                        samples_index <- as.numeric(row.names(aim_select))
                        training_data <- predicting_data[samples_index,]
                        training_data_variables <- as.matrix(training_data[, c(1:variables_number)])
                        instances_labels <- training_data[, variables_number + 1]
                        categories_next <- unique(instances_labels)
                        for (i in categories_next) {
                            if (i %in% categories == FALSE) {
                                categories <- append(categories, i)
                            }
                        }
                        categories_length <- length(categories)
                        training_data_labels <- as.data.frame(matrix(seq(0, by = 0, length = nrow(training_data_variables) * categories_length), nrow = nrow(training_data_variables), ncol = categories_length))
                        names(training_data_labels) <- categories
                        for (i in 1:categories_length) {
                            position <- which(instances_labels == categories[i])
                            training_data_labels[position, i] <- 1
                        }
                        training_data_labels <- as.matrix(training_data_labels)
                        H <- sigmoid_function(training_data_variables %*% input_weight + matrix_vector(number = nrow(training_data_variables)) %*% input_bisa)
                        weight_result <- obtained_weight_matrix(data_labels = training_data_labels, original_labels_matrix = original_labels_matrix)
                        original_labels_matrix <- weight_result[[1]]
                        K <- K + t(H) %*% weight_result[[2]] %*% H
                        Beta <- Beta %*% weight_result[[3]] + solve(K) %*% t(H) %*% weight_result[[2]] %*% (training_data_labels - H %*% Beta %*% weight_result[[3]])
                        predicting_data <- predicting_data[-samples_index,]
                        if (nrow(predicting_data) == 0) {
                            break
                        }
                        predicting_data_variables <- as.matrix(predicting_data[, c(1:variables_number)])
                        H <- sigmoid_function(predicting_data_variables %*% input_weight + matrix_vector(number = nrow(predicting_data_variables)) %*% input_bisa)
                        aim <- as.data.frame(H %*% Beta)
                        aim$the_margin <- 1
                        for (i in 1:nrow(aim)) {
                            the_result_sample <- sort(aim[i, c(1:(ncol(aim) - 1))], decreasing = TRUE)
                            aim[i, ncol(aim)] <- the_result_sample[1] - the_result_sample[2]
                        }
                        aim <- aim[order(aim$the_margin),]
                        aim_select <- data.frame()
                        for (i in 1:nrow(aim)) {
                            if (aim[i, ncol(aim)] <= margin_threshold) {
                                aim_select <- rbind(aim_select, aim[i,])
                            }
                        }
                        if (nrow(aim_select) > bound_selection) {
                            aim_select <- aim_select[c(1:bound_selection),]
                        }
                    }
                }
            }
        }
    }
    training_end = Sys.time()
    testing_start = Sys.time()
    testing_data_variables <- as.matrix(testing_data[, c(1:variables_number)])
    H <- sigmoid_function(testing_data_variables %*% input_weight + matrix_vector(number = nrow(testing_data_variables)) %*% input_bisa)
    aim <- as.data.frame(H %*% Beta)
    testing_end = Sys.time()
    aim_result <- aim[, order(as.numeric(colnames(aim)))]
    aim_result$result <- 0
    for (i in 1:nrow(aim_result)) {
        aim_result[i, ncol(aim_result)] <- which.max(aim_result[i, c(1:(ncol(aim_result) - 1))])
    }
    table0 <- table(testing_data$label, aim_result$result)
    if (ncol(table0) == 1){
        Acc = NA
        Gmean = NA
        No = 1
    } else if (ncol(table0) < nrow(table0)) {
        Acc = NA
        Gmean = NA
        No = 2
    } else {
        final_result <- obtained_acc_G_mean(table0)
        Acc = final_result[[1]]
        Gmean = final_result[[2]]
        No = 0
    }
    Dic <- (nrow(dictionary) / samples_number) * 100
    Mar <- (the_number_select / samples_number) * 100
    Tol <- (nrow(dictionary) + the_number_select) / samples_number * 100
    training_time = as.numeric(as.character(difftime(training_end, training_start, units = "secs")))
    testing_time = as.numeric(as.character(difftime(testing_end, testing_start, units = "secs")))
    comp <- data.frame(Acc, Gmean, Dic, Mar, Tol, No, training_time, testing_time)
    names(comp) <- c("Acc", "Gmean", "Dic", "Mar", "Tol", "No", "training_time", "testing_time")
    print(comp)
    
    #save the results in "data.csv", run the following three lines
    #saver <- read.table("data.csv", header = TRUE, sep = ",")
    #saver <- rbind(saver, comp)
    #write.csv(saver, "data.csv", row.names = FALSE)
}

#model(64, 300, 0.05, "D:/Documents/program/pageblocks_train.csv", 3000, "D:/Documents/program/pageblocks_test.csv")
