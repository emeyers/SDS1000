# Installs packages that are used in the class

This function installs packages that are used in the class based on a
list of packages described in a file on the class GitHub repository.

## Usage

``` r
update_installed_packages(force_reinstall = FALSE)
```

## Arguments

- force_reinstall:

  If this is set to TRUE, then any class packages that have already been
  installed will be reinstalled. If this is FALSE, then only packages
  that have not been installed yet will be installed.

## Examples

``` r
 # Intall all the needed class packages
 if (FALSE) update_installed_packages() # \dontrun{}
```
