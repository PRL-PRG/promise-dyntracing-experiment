library(optparse)
library(fs)


setup_cran <- function(settings) {
    options(repos = settings$cran_mirror_url)

    dir_create(settings$cran_src_dirpath)
    dir_create(settings$cran_lib_dirpath)

    install.packages(available.packages()[,1],
                     lib = settings$cran_lib_dirpath,
                     dependencies = TRUE,
                     destdir = settings$cran_src_dirpath,
                     INSTALL_opts = c('--example',
                                      '--install-tests',
                                      '--with-keep.source',
                                      '--no-multiarch'),
                     Ncpus = settings$ncpus)
}


setup_bioc <- function(settings) {
    library(BiocManager)

    dir_create(settings$bioc_src_dirpath)
    dir_create(settings$bioc_lib_dirpath)

    BiocManager::install(BiocManager::available(),
                         lib = settings$bioc_lib_dirpath,
                         dependencies = TRUE,
                         destdir = settings$bioc_src_dirpath,
                         INSTALL_opts = c('--example',
                                          '--install-tests',
                                          '--with-keep.source',
                                          '--no-multiarch'),
                         Ncpus = settings$ncpus,
                         update = TRUE,
                         ask = FALSE)
}


parse_program_arguments <- function() {
    usage <- "%prog [OPTIONS] ..."
    description <- ""

    option_list <- list(
        make_option(c("--ncpus"),
                    action = "store",
                    type = "integer",
                    default = 8,
                    help = "number of CPUs to use for installing packages",
                    metavar = "ncpus"),

        make_option(c("--setup-cran"),
                    action = "store_true",
                    default = FALSE,
                    help = "setup CRAN packages",
                    metavar = "setup_cran"),

        make_option(c("--setup-bioc"),
                    action = "store_true",
                    default = FALSE,
                    help = "setup BIOC packages",
                    metavar = "setup_bioc"),

        make_option(c("--cran-mirror-url"),
                    action = "store",
                    type = "character",
                    help = "CRAN mirror URL",
                    metavar = "cran_mirror_url"),

        make_option(c("--cran-lib-dirpath"),
                    action = "store",
                    type = "character",
                    help = "directory in which to store the CRAN package libraries",
                    metavar = "cran_lib_dirpath"),

        make_option(c("--cran-src-dirpath"),
                    action = "store",
                    type = "character",
                    help = "directory in which to store the CRAN package sources",
                    metavar = "cran_src_dirpath"),

        make_option(c("--bioc-lib-dirpath"),
                    action = "store",
                    type = "character",
                    help = "directory in which to store the BIOC package libraries",
                    metavar = "bioc_lib_dirpath"),

        make_option(c("--bioc-src-dirpath"),
                    action = "store",
                    type = "character",
                    help = "directory in which to store the BIOC package sources",
                    metavar = "bioc_src_dirpath")
    )


    option_parser <- OptionParser(usage = usage,
                                  description = description,
                                  add_help_option = TRUE,
                                  option_list = option_list)

    arguments <- parse_args2(option_parser)

    list(ncpus = arguments$options$ncpus,
         setup_cran = arguments$options$setup_cran,
         setup_bioc = arguments$options$setup_bioc,
         cran_mirror_url = arguments$options$cran_mirror_url,
         cran_lib_dirpath = arguments$options$cran_lib_dirpath,
         cran_src_dirpath = arguments$options$cran_src_dirpath,
         bioc_lib_dirpath = arguments$options$bioc_lib_dirpath,
         bioc_src_dirpath = arguments$options$bioc_src_dirpath)
}


main <- function() {
    settings <- parse_program_arguments()

    print(settings)

    if(settings$setup_cran) {
        setup_cran(settings)
    }

    if(settings$setup_bioc) {
        setup_bioc(settings)
    }
}


main()
