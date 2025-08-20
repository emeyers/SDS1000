


# Try out some of the basic functions/workflow for the package


# To make sure the package really works, we will use the default directories. One
# should be careful if code already exists in these directories.


# detach("package:SDS1000", unload = TRUE)
# devtools::install_github("emeyers/SDS1000")
# devtools::install_github("emeyers/SDS1000", ref = "download_github_directories")

# #SDS1000:::initial_setup()

original_wd <- getwd()

library(SDS1000)

# set the rootpath to the Documents/ directory
set_class_material_root_path() 

# check that the root path is the documents folder
get_class_material_root_path()

# go to the homework directory
goto_homework(-1)  # click 1 to download the test homework

# check that we have gone to the homework -1 directory
getwd()

# check that the test homework 1 files are there
list.files(getwd())

# get the class -1 test material
goto_class(-1)

# check that we have gone to the class -1 directory
getwd()

# check that the class -1 files are there
list.files(getwd())

# delete one of the files from the class -1 directory
file.remove("class_-1.Rmd")

# check that the file is deleted
list.files(getwd())

# go back to the homework -1 directory
goto_homework(-1)

# go back to the class -1 directory and see that the missing file is restored
goto_class(-1)

# check that the class -1 files are there
list.files(getwd())

# could also go to the practice session
goto_practice_session(-1)

# Could go to the final project, but I haven't upload the data for this yet
# Should get an error message that is hopefully informative
goto_final_project()

# go back to the original working directory
setwd(original_wd)
rstudioapi::filesPaneNavigate(original_wd)

