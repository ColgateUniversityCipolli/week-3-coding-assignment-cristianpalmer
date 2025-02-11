library(gmp)

#####################Finding Incorrect Number###################################
matrix_data <- matrix(c(6, 22, 38, 57, 75, 87,
                        10, 26, 39, 58, 77, 91,
                        14, 33, 46, 62, 82, 93,
                        15, 34, 51, 65, 85, 94,
                        21, 35, 55, 69, 86, 95), 
                      nrow = 5, byrow = TRUE)

factorization_results <- list()

positions_with_duplicates <- c()

for (i in 1:length(matrix_data)) {
  factors <- factorize(matrix_data[i])
  
  if (any(duplicated(factors))) {
    positions_with_duplicates <- c(positions_with_duplicates, i)
  }
}
Incorrect_Number = matrix_data[positions_with_duplicates]
Position_Below_Incorrect_Number = positions_with_duplicates - 1
Position_Above_Incorrect_Number = positions_with_duplicates + 1
Lower_Bound = matrix_data[Position_Below_Incorrect_Number] + 1
Upper_Bound = matrix_data[Position_Above_Incorrect_Number] -1

#####################Finding Replaceement Number################################

New_Matrix = matrix(Lower_Bound:Upper_Bound)

factorization_results_new <- list()

positions_with_duplicates_new <- c()

for (i in 1:length(New_Matrix)) {
  factors_new <- factorize(New_Matrix[i])
  
  if (!any(duplicated(factors_new))) {
    positions_with_duplicates_new <- c(positions_with_duplicates_new, i)
  }
}
New_Possible_Numbers = New_Matrix[positions_with_duplicates_new]
for (i in 1:length(New_Possible_Numbers)) {
  Final_Numbers <- factorize(New_Possible_Numbers[i])
  if (length(Final_Numbers) == 2) {
    return(New_Possible_Numbers[i])
  }
}
Replacement_Number = New_Possible_Numbers[i]

#####################Printing Answers##########################################
Incorrect_Number
Replacement_Number
