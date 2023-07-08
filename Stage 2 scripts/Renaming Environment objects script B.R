### optional data merging section
rm(CL, fdpth, fldpth, folderpath, my_order, paths_list, performance_metrics, 
   PMs1, PMs2, PMs3, LASSO_fits, LASSO_Coeffs, Headers, num_null_FPR)

datasetsB <- datasets

Structural_Variables2 <- Correctly_Selected_Variables
Structural_Variables2 <- True_Regressors
Structural_Variables2 <- Structural_Variables
Structural_Variables2 <- Structural_IVs_chr

Nonstructural_Variables2 <- Nonstructural_Variables


DS_names_listB <- DS_names_list


rm(fldpth, my_order, paths_list, datasets, DS_names_list)
rm(Correctly_Selected_Variables, Structural_Variables, True_Regressors,
   Structural_IVs_chr)



