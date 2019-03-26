suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("compiler"))
suppressPackageStartupMessages(library("fs"))
suppressPackageStartupMessages(library("purrr"))
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("magrittr"))
suppressPackageStartupMessages(library("progress"))

info <- function(...) cat((paste0(...)))

copy_tests <- function(settings, installed_packages) {

    info("=> Copying tests\n")

    if(dir_exists(settings$test_dirpath)) {
        dir_delete(settings$test_dirpath)
    }

    dir_create(settings$test_dirpath)

    source_paths <- path(
        map_chr(
            installed_packages,
            function(package) {
                find.package(package)[1]
            }),
        "tests")

    destination_paths <- path(settings$test_dirpath,
                              installed_packages,
                              "tests")

    valid_paths <- dir_exists(source_paths)

    source_paths <- source_paths[valid_paths]

    destination_paths <- destination_paths[valid_paths]

    pb <- progress_bar$new(format = ":package [:bar] :current/:total (:percent) eta: :eta",
                           total = length(source_paths))

    map2_chr(source_paths,
             destination_paths,
             function(source, destination) {
                 pb$tick(tokens = list(package = path_file(destination)))
                 dir_copy(source, destination)
             })

    info("=> Copied tests\n")

    invisible(NULL)
}


copy_examples <- function(settings, installed_packages) {

    info("=> Copying examples\n")

    if(dir_exists(settings$example_dirpath)) {
        dir_delete(settings$example_dirpath)
    }

    dir_create(settings$example_dirpath)

    pb <- progress_bar$new(format = ":package [:bar] :current/:total (:percent) eta: :eta",
                           total = length(installed_packages))

    copy_package_examples <- function(package) {

        example_dirpath <- path(settings$example_dirpath, package, "examples")

        dir_create(example_dirpath)

        pb$tick(tokens = list(package = package))

        db <- tryCatch({
            tools::Rd_db(package)
        }, error=function(e) {
            print(e)
            list()
        })

        iwalk(db, function(rd_data, rd_name) {
            example_filepath <-
                path(example_dirpath, path_file(rd_name)) %>%
                path_ext_set("R")

            tools::Rd2ex(rd_data, example_filepath, defines=NULL)
        })

        example_dirpath
    }

    map_chr(installed_packages, copy_package_examples)

    info("=> Copied examples\n")

    invisible(NULL)
}


copy_vignettes <- function(settings, installed_packages) {

    info("=> Copying vignettes\n")

    if(dir_exists(settings$vignette_dirpath)) {
        dir_delete(settings$vignette_dirpath)
    }


    dir_create(settings$vignette_dirpath)

    pb <- progress_bar$new(format = ":package [:bar] :current/:total (:percent) eta: :eta",
                           total = length(installed_packages))


    copy_package_vignettes <- function(package) {

        package_dirpath <- path(settings$vignette_dirpath, package)

        dir_create(package_dirpath)

        pb$tick(tokens = list(package = package))

        ## this package's vignette does not generate
        if(package == "varSel") {
            return(package)
        }

        vignettes <- tools::pkgVignettes(package, source=TRUE)

        tryCatch({
            ## if there are vignettes and there are no vignette sources,
            ## then compile the vignettes to sources. This compilation
            ## will result in .R files in the doc directory of package
            ## and will be picked up by the next step of the program
            if (length(vignettes$docs) != 0 &&
                length(vignettes$sources) == 0) {

                tools::checkVignettes(package,
                                      find.package(package)[1],
                                      tangle = TRUE,
                                      weave = FALSE,
                                      workdir = "src")
            }

            vignettes <- vignette(package = package)$results

            if(nrow(vignettes) != 0) {
                source_dirpath <- path(vignettes[1, "LibPath"],
                                       vignettes[1, "Package"],
                                       "doc")
                if(dir_exists(source_dirpath))
                    dir_copy(source_dirpath, package_dirpath)
            }
        },
        error = function(e) {
            print(e)
        })

        package_dirpath
    }

    map_chr(installed_packages, copy_package_vignettes)

    info("=> Copied vignettes\n")

    invisible(NULL)
}


parse_command_line <- function() {

    usage <- "%prog vignette-dirpath test-dirpath example-dirpath"

    description <- paste(
        "test-dirpath     directory containing corpus files",
        "example-dirpath  directory containing raw analysis data",
        "vignette-dirpath filepath for valid scripts",
        sep = "\n")

    option_parser <- OptionParser(usage = usage,
                                  description = description,
                                  add_help_option = TRUE,
                                  option_list = list())

    arguments <- parse_args2(option_parser)

    list(test_dirpath = arguments$args[1],
         example_dirpath = arguments$args[2],
         vignette_dirpath = arguments$args[3])
}


main <- function() {

    settings <- parse_command_line()

    print(settings)

    installed_packages <- sort(unique(installed.packages()[, 1]))

    copy_tests(settings, installed_packages)
    copy_examples(settings, installed_packages)
    copy_vignettes(settings, installed_packages)

    invisible(NULL)
}


main()
