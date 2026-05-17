# Set the root path where class materials will be downloaded

This function sets the root path where the sds1000_class_materials/
folder will be located. By default, it sets the location to be in the
Documents folder of a user on Windows and Mac, and creates a Documents
folder in the home directory of Unix users. It also adds metadata
listing where this directory is so that get_class_material_root_path()
works correctly after it is set across R sessions.

## Usage

``` r
set_class_material_root_path(dir_name = NULL)
```

## Arguments

- dir_name:

  A string indicating the directory where the class materials should be
  saved. If NULL, the default directory is used.
