library(fs)
library(purrr)
library(stringr)
library(optparse)
library(readr)


CRAN_REPO_URL <- "http://cran.us.r-project.org"


printf <- function(...) invisible(cat(sprintf(...)))


parse_command_line_arguments <- function() {

    option_list <- list(
        make_option("--dependents",
                    action = "store_true",
                    default = FALSE,
                    help = "find dependents of the packages",
                    metavar = "dependents"),
        make_option("--dependencies",
                    action = "store_true",
                    default = FALSE,
                    help = "find dependencies of the packages",
                    metavar = "dependencies")
    )

    args <- parse_args(OptionParser(option_list = option_list),
                       positional_arguments = TRUE)

    list(input_filepath = args$args[1],
         output_filepath = args$args[2],
         dependents = args$options$dependents,
         dependencies = args$options$dependencies)
}


extract_dependencies <- function(pkg_info) {

    split_package_names <- function(name_str) {
        names <- str_trim(str_split(unname(name_str), ",")[[1]])
        ## remove NA
        names <- names[!is.na(names)]
        ## replace "<package-name> (>x.y)" dependency with "<package-name>"
        names <- str_replace(names, "\\s*\\(.*\\)", "")
        ## remove R as a dependency
        names[names != "R"]
    }

    imports <- split_package_names(pkg_info["Imports"])
    depends <- split_package_names(pkg_info["Depends"])
    dependencies <- union(imports, depends)
}


has_dependency <- function(required, all) {
    length(intersect(required, all)) > 0
}


select_dependent_packages <- function(input_packages, all_packages) {
    dependent_selector <-
        apply(all_packages,
              1,
              function(pkg_info) {
                  has_dependency(input_packages,
                                 extract_dependencies(pkg_info))
              })
    dependents <- unname(all_packages[unname(dependent_selector), "Package"])
    dependents
}


select_dependency_packages <- function(input_packages, all_packages) {
    dependencies <-
        unlist(apply(all_packages,
                     1,
                     function(pkg_info) {
                         if (unname(pkg_info["Package"]) %in% input_packages) {
                             extract_dependencies(pkg_info)
                         } else {
                             c()
                         }
                     }))
    dependencies
}

## We care about the order in which we write package names
## first, we want to trace the packages in the input list,
## then the ones in the dependent list and finally the ones
## in the dependency list. We also need to remove duplicates.
combine_packages <- function(input_packages,
                             dependent_packages,
                             dependency_packages) {
    dependent_packages <- setdiff(dependent_packages, input_packages)
    dependency_packages <- setdiff(setdiff(dependency_packages,
                                           input_packages),
                                   dependent_packages)
    ## unique ensures removal of duplicates
    combination <- c(input_packages,
                     dependent_packages,
                     dependency_packages)
    combination
}


main <- function() {
    args <- parse_command_line_arguments()
    input_packages <- unname(unlist(read_csv(args$input_filepath,
                                             col_names = FALSE)))
    all_packages <- available.packages(contrib.url(CRAN_REPO_URL))

    dependent_packages <- c()
    if (args$dependents) {
        printf("finding dependent packages ...\n")
        dependent_packages <- select_dependent_packages(input_packages,
                                                        all_packages)
        printf("Found %d dependent packages\n", length(dependent_packages))
    }

    dependency_packages <- c()
    if (args$dependencies) {
        printf("finding dependency packages ...\n")
        dependency_packages <- select_dependency_packages(input_packages,
                                                          all_packages)
        printf("Found %d dependency packages\n", length(dependency_packages))
    }

    printf("combining packages ...\n")
    combination <- combine_packages(input_packages,
                                    dependent_packages,
                                    dependency_packages)
    write_csv(data.frame(combination),
              args$output_filepath,
              col_names = FALSE)
}


main()
