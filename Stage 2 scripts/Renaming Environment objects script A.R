### optional data merging section
rm(CL, fdpth, fldpth, folderpath, my_order, paths_list, performance_metrics, 
   PMs1, PMs2, PMs3, LASSO_fits, LASSO_Coeffs, Headers, num_null_FPR)

datasetsA <- datasets

Structural_Variables1 <- Correctly_Selected_Variables
Structural_Variables1 <- True_Regressors
Structural_Variables1 <- Structural_Variables
#Structural_Variables1 <- Structural_IVs_chr


Nonstructural_Variables1 <- Nonstructural_Variables


DS_names_listA <- DS_names_list


rm(datasets, DS_names_list, Structural_Variables, 
   Structural.Variables, Nonstructural_Variables)

rm(fldpth, my_order, paths_list, datasets, DS_names_list)
rm(Correctly_Selected_Variables, Structural_Variables, True_Regressors,
   Structural_IVs_chr)



