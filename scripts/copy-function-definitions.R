library(fs)
library(magrittr)
library(purrr)
library(dplyr)
library(readr)
library(tibble)

main <- function() {
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) < 2)
        stop("missing arguments")

    input_dirpath <- args[1]

    output_dirpath <- args[2]

    print(input_dirpath)

    print(output_dirpath)


    input_filepaths <-
        input_dirpath %>%
        dir_ls(type = "directory") %>%
        map(dir_ls, type = "directory") %>%
        unlist() %>%
        path("functions") %>%
        map(dir_ls, type = "file") %>%
        unlist()

    output_filepaths <-
        input_filepaths %>%
        path_file() %>%
        path(output_dirpath, .)

    #print(output_filepaths)

    function_table <-
        dir_map(input_dirpath,
                function(package_dir) {
                    dir_map(package_dir,
                            function(vignette_dir) {
                                function_filepaths <- dir_ls(path(vignette_dir, "functions"),
                                                             type = "file")
                                tibble(vignette = path_file(vignette_dir),
                                       function_id = path_file(function_filepaths),
                                       input_filepath = function_filepaths,
                                       output_filepath = path(output_dirpath, function_id),
                                       definition = map_chr(function_filepaths, read_file))
                            },
                            type = "directory") %>%
                        bind_rows() %>%
                        add_column(package = path_file(package_dir),
                                   .before = 1)
                },
                type = "directory") %>%
        bind_rows()

    pwalk(function_table,
          function(package, vignette, function_id, input_filepath, output_filepath, definition) {
              #print(input_filepath)
              #print(output_filepath)
              file_copy(input_filepath, output_filepath, overwrite = TRUE)
          })

    write.csv(function_table, "function-table.csv")
    ## map2(input_filepaths,
    ##      output_filepaths,
    ##      function(input_filepath, output_filepath) {
    ##          if(file_exists())
    ##      }
    ##      )

    ## dir_walk(input_dirpath,
    ##          function(package_dirpath) {
    ##              dir_walk(package_dirpath,
    ##                       function(vignette_dirpath) {
    ##                           dir_walk(path(vignette_dirpath, "functions"),
    ##                                    function(function_filepath) {
    ##                                        print(function_filepath)
    ##                                        function_filename <- path_file(function_filepath)
    ##                                        file_copy(function_filepath,
    ##                                                  path(output_dirpath, function_filename),
    ##                                                  overwrite = TRUE)
    ##                                    },
    ##                                    type = "file")
    ##                       },
    ##                       type = "directory")
    ##          },
    ##          type = "directory")
}

main()
warnings()
