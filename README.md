

The scripts are distributed across the project goals as the following:

Goal 1:
- first_question.r 
    Here we do the significance testing of horse on symmetry scores, and do various test for assumptions as normality and homogenous variance. 

Goal 2:
- data_splits.ipynb
    This notebook contains the cross-validation loops for our models. There are three loops, one for each set of attributes, and several plots.
- data_splits_permutation.r
    Here we do permutation test for the data generated in the data_splits.ipynb notebook. 
- McNemar.ipynb
    Contains code for the mcnemar tests
- permutation_modeller.r
    #Dumped, contains permutation test for comparison of models. 


Goal 3:
- McNemar.ipynb
    Contains code for the mcnemar tests
- diagonal_elements.ipynb 
    #Dumped, contains cross-validation loop for grouped classes
- diagonal permutation.r
    #Dumped, contains permutation test for diagonal_elements.ipynb


misc:
- p.adjust.r
    Used to do the correction of p-values with 'BH'