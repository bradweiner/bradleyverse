core <- c("abind", "acepack", "acs", "anytime", "AsioHeaders", "askpass", "assertthat", "athenar", "AUC", "AWR.Athena", "aws.s3", "aws.signature", "babynames", "backports", "base", "base64enc", "BaylorEdPsych", "BH", "bindr", "bindrcpp", "bit", "bit64", "bitops", "blob", "blogdown", "bmp", "bookdown", "boot", "brew", "broom", "BurStMisc", "C3", "Cairo", "callr", "car", "carData", "caret", "caTools", "cellranger", "checkmate", "class", "classInt", "cli", "clipr", "clisymbols", "cluster", "coda", "codetools", "colorspace", "commonmark", "compiler", "corrplot", "countrycode", "covr", "crayon", "crosstalk", "curl", "data.table", "data.world", "datapasta", "datasets", "dbConnect", "DBI", "dbplyr", "ddiff", "deldir", "desc", "devtools", "diagram", "dichromat", "digest", "downloader", "dplyr", "DT", "dtt", "dummies", "dwapi", "e1071", "edeR", "ellipsis", "evaluate", "extrafont", "extrafontdb", "fansi", "ff", "FinCal", "fivethirtyeight", "flextable", "forcats", "foreach", "forecast", "foreign", "formatR", "Formula", "fracdiff", "fs", "gdata", "gdtools", "generics", "genius", "geosphere", "GGally", "ggjoy", "ggmap", "ggplot2", "ggplot2movies", "ggridges", "ggstance", "ggthemes", "ggwordcloud", "gh", "git2r", "glue", "goftest", "gower", "gplots", "graphics", "grDevices", "grid", "gridBase", "gridExtra", "gtable", "gtools", "gWidgets", "haven", "here", "highr", "Hmisc", "hms", "htmlTable", "htmltools", "htmlwidgets", "httpuv", "httr", "hunspell", "huxtable", "igraph", "imager", "ineq", "ini", "ipred", "ISOcodes", "iterators", "janeaustenr", "janitor", "jpeg", "jsonlite", "jtools", "kableExtra", "KernSmooth", "knitr", "labeling", "LaCroixColoR", "Lahman", "later", "lattice", "latticeExtra", "lava", "lazyeval", "leaflet", "lme4", "lmtest", "log4r", "lubridate", "magick", "magrittr", "mailR", "mapproj", "maps", "maptools", "markdown", "MASS", "Matrix", "MatrixModels", "memoise", "methods", "metricsgraphics", "mgcv", "mime", "miniUI", "minqa", "mnormt", "ModelMetrics", "modelr", "mongolite", "munsell", "network", "nlme", "nloptr", "NLP", "nnet", "noncensus", "nord", "numDeriv", "ochRe", "odbc", "officer", "opencpu", "openssl", "openxlsx", "pacman", "pagedown", "pander", "parallel", "pbkrtest", "pdftools", "pillar", "pkgbuild", "pkgconfig", "pkgload", "pkgverse", "plogr", "plotrix", "plyr", "png", "polyclip", "praise", "prettyunits", "processx", "prodlim", "progress", "promises", "proto", "protolite", "ps", "psych", "purrr", "quadprog", "quantmod", "quantreg", "R.methodsS3", "R.oo", "R.utils", "R6", "RApiDatetime", "rappdirs", "raster", "rcmdcheck", "RColorBrewer", "Rcpp", "RcppArmadillo", "RcppEigen", "RcppRoll", "RCurl", "readbitmap", "readr", "readxl", "recipes", "rematch", "remotes", "reprex", "reshape", "reshape2", "reticulate", "rex", "rgdal", "RgoogleMaps", "rio", "rJava", "RJDBC", "rjson", "RJSONIO", "rJython", "rlang", "rmarkdown", "RMySQL", "ROCR", "roxygen2", "rpart", "rprojroot", "rstudioapi", "Rttf2pt1", "rtweet", "rversions", "rvest", "satconcordance", "scales", "SDMTools", "selectr", "sendmailR", "servr", "sessioninfo", "sf", "shape", "shiny", "shinyBS", "shinydashboard", "shinyFiles", "shinyjs", "shinythemes", "showtext", "showtextdb", "skimr", "slam", "sna", "snakecase", "SnowballC", "sourcetools", "sp", "SparseM", "spatial", "spatstat", "spatstat.data", "spatstat.utils", "spData", "splines", "SQUAREM", "ssh.utils", "staplr", "statnet.common", "stats", "stats4", "stopwords", "stringdist", "stringi", "stringr", "survival", "sys", "sysfonts", "syuzhet", "tcltk", "tensor", "testthat", "textshape", "tibble", "tidycensus", "tidyr", "tidyselect", "tidytext", "tiff", "tigris", "timeDate", "timevis", "tinytex", "titanic", "tm", "tokenizers", "tools", "totalcensus", "triebeard", "truncnorm", "tseries", "TTR", "twitteR", "units", "urca", "urltools", "uroot", "usethis", "usmap", "utf8", "utils", "uuid", "vctrs", "viridis", "viridisLite", "webshot", "websocket", "webutils", "whisker", "withr", "wordcloud", "xfun", "xlsx", "xlsxjars", "XML", "xml2", "xmltools", "xopen", "xtable", "xts", "yaml", "zeallot", "zip", "zipcode", "zoo")

core_loaded <- function() {
  search <- paste0("package:", core)
  core[search %in% search()]
}
core_unloaded <- function() {
  search <- paste0("package:", core)
  core[!search %in% search()]
}


bradleyverse_attach <- function() {
  to_load <- core_unloaded()
  if (length(to_load) == 0)
    return(invisible())

  msg(
    cli::rule(
      left = crayon::bold("Attaching packages"),
      right = paste0("bradleyverse ", package_version("bradleyverse"))
    ),
    startup = TRUE
  )

  versions <- vapply(to_load, package_version, character(1))
  packages <- paste0(
    crayon::green(cli::symbol$tick), " ", crayon::blue(format(to_load)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  suppressPackageStartupMessages(
    lapply(to_load, library, character.only = TRUE, warn.conflicts = FALSE)
  )

  invisible()
}

package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- crayon::red(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}

#' Conflicts between the bradleyverse and other packages
#'
#' This function lists all the conflicts between packages in the bradleyverse
#' and other packages that you have loaded.
#'
#' If dplyr is one of the select packages, then the following four conflicts
#' are deliberately ignored: \code{intersect}, \code{union}, \code{setequal},
#' and \code{setdiff} from dplyr. These functions make the base equivalents
#' generic, so shouldn't negatively affect any existing code.
#'
#' @export
#' @examples
#' bradleyverse_conflicts()
bradleyverse_conflicts <- function() {
  envs <- purrr::set_names(search())
  objs <- invert(lapply(envs, ls_env))

  conflicts <- purrr::keep(objs, ~ length(.x) > 1)

  pkg_names <- paste0("package:", bradleyverse_packages())
  conflicts <- purrr::keep(conflicts, ~ any(.x %in% pkg_names))

  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)

  structure(conflict_funs, class = "bradleyverse_conflicts")
}

bradleyverse_conflict_message <- function(x) {
  if (length(x) == 0) return("")

  header <- cli::rule(
    left = crayon::bold("Conflicts"),
    right = "bradleyverse_conflicts()"
  )

  pkgs <- x %>% purrr::map(~ gsub("^package:", "", .))
  others <- pkgs %>% purrr::map(`[`, -1)
  other_calls <- purrr::map2_chr(
    others, names(others),
    ~ paste0(crayon::blue(.x), "::", .y, "()", collapse = ", ")
  )

  winner <- pkgs %>% purrr::map_chr(1)
  funs <- format(paste0(crayon::blue(winner), "::", crayon::green(paste0(names(x), "()"))))
  bullets <- paste0(
    crayon::red(cli::symbol$cross), " ", funs,
    " masks ", other_calls,
    collapse = "\n"
  )

  paste0(header, "\n", bullets)
}

#' @export
print.bradleyverse_conflicts <- function(x, ..., startup = FALSE) {
  cli::cat_line(bradleyverse_conflict_message(x))
}

#' @importFrom magrittr %>%
confirm_conflict <- function(packages, name) {
  # Only look at functions
  objs <- packages %>%
    purrr::map(~ get(name, pos = .)) %>%
    purrr::keep(is.function)

  if (length(objs) <= 1)
    return()

  # Remove identical functions
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1)
    return()

  packages
}

ls_env <- function(env) {
  x <- ls(pos = env)
  if (identical(env, "package:dplyr")) {
    x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
  }
  x
}

#' Update bradleyverse packages
#'
#' This will check to see if all bradleyverse packages (and optionally, their
#' dependencies) are up-to-date, and will install after an interactive
#' confirmation.
#'
#' @param recursive If \code{TRUE}, will also check all dependencies of
#'   bradleyverse packages.
#' @export
#' @examples
#' \dontrun{
#' bradleyverse_update()
#' }
bradleyverse_update <- function(recursive = FALSE) {

  deps <- bradleyverse_deps(recursive)
  behind <- dplyr::filter(deps, behind)

  if (nrow(behind) == 0) {
    cli::cat_line("All bradleyverse packages up-to-date")
    return(invisible())
  }

  cli::cat_line("The following packages are out of date:")
  cli::cat_line()
  cli::cat_bullet(
    format(behind$package), " (", behind$local, " -> ", behind$cran, ")")

  cli::cat_line()
  cli::cat_line("Start a clean R session then run:")

  pkg_str <- paste0(deparse(behind$package), collapse = "\n")
  cli::cat_line("install.packages(", pkg_str, ")")

  invisible()
}

#' List all bradleyverse dependencies
#'
#' @param recursive If \code{TRUE}, will also list all dependencies of
#'   bradleyverse packages.
#' @export
bradleyverse_deps <- function(recursive = FALSE) {
  pkgs <- utils::available.packages()
  deps <- tools::package_dependencies("bradleyverse", pkgs, recursive = recursive)

  pkg_deps <- unique(sort(unlist(deps)))

  base_pkgs <- c(
    "base", "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tools", "tcltk",
    "utils"
  )
  pkg_deps <- setdiff(pkg_deps, base_pkgs)

  cran_version <- lapply(pkgs[pkg_deps, "Version"], base::package_version)
  local_version <- lapply(pkg_deps, utils::packageVersion)

  behind <- purrr::map2_lgl(cran_version, local_version, `>`)

  tibble::tibble(
    package = pkg_deps,
    cran = cran_version %>% purrr::map_chr(as.character),
    local = local_version %>% purrr::map_chr(as.character),
    behind = behind
  )
}

msg <- function(..., startup = FALSE) {
  packageStartupMessage(text_col(...))
}

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)

}

#' List all packages in the bradleyverse
#'
#' @param include_self Include bradleyverse in the list?
#' @export
#' @examples
#' bradleyverse_packages()
bradleyverse_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("bradleyverse")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "bradleyverse")
  }

  names
}

invert <- function(x) {
  if (length(x) == 0) return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(
    paste0(...),
    crayon::make_style(grDevices::grey(level), grey = TRUE)
  )
}

.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  crayon::num_colors(TRUE)
  bradleyverse_attach()

  if (!"package:conflicted" %in% search()) {
    x <- bradleyverse_conflicts()
    msg(bradleyverse_conflict_message(x), startup = TRUE)
  }

}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
