### optional data merging section
rm(fdpth, my_order, paths_list)
rm(fldpth, my_order, paths_list)
rm(CL, folderpath)
rm(fldpth, my_order, paths_list, datasets, Correctly_Selected_Variables, Structural_Variables)


datasets <- c(datasetsA, datasetsB)
datasets <- c(datasetsA, datasetsB, datasetsC)
rm(datasetsA, datasetsB, datasetsC)


Structural_Variables <- c(Structural_Variables1, Structural_Variables2)
Structural_Variables <- c(Structural_Variables1, Structural_Variables2, 
                          Structural_Variables3)
rm(Structural_Variables1, Structural_Variables2, Structural_Variables3)


Nonstructural_Variables <- c(Nonstructural_Variables1, Nonstructural_Variables2)
rm(Nonstructural_Variables1, Nonstructural_Variables2)


DS_names_list <- c(DS_names_listA, DS_names_listB)
DS_names_list <- c(DS_names_listA, DS_names_listB, DS_names_listC)
rm(DS_names_listA, DS_names_listB)


