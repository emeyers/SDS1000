# Sets the working directory and has RStudio file pane set to that directory If the directory does not exist, the user is prompted to download the files.

Sets the working directory and has RStudio file pane set to that
directory If the directory does not exist, the user is prompted to
download the files.

## Usage

``` r
goto_directory(dir_path, download_message = "")
```

## Arguments

- dir_path:

  A string indicating the path to the directory relative to the base
  save path.

- download_message:

  A string that will display what files are being downloaded (e.g.,
  "homework 1").

## Examples

``` r
 # Go to the first homework directory, downloading files if necessary
 if (FALSE) goto_directory("homework/homework_01", "homework 1") # \dontrun{}
```
