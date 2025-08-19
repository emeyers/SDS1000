

# Code that can be run to initially set up a student's R installation.
#  Here we are installing LaTeX through the tidytex package
initial_setup <- function() {

  update_installed_packages()
  set_class_material_root_path()
  tinytex::install_tinytex()

}







