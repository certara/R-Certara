#' List all packages developed by Certara
#'
#' @param include_self Logical; Set to \code{TRUE} to include \code{Certara.R}
#' @param include_JFROG_deps Logical; Set to \code{TRUE} to include Certara package dependencies on Jfrog
#' @return Character vector of package names
#' @examples
#' certara_packages()
#' @export

certara_packages <- function(include_self= FALSE, include_JFROG_deps = FALSE) {

  pkgs <- c("Certara.RsNLME",
            "Certara.Xpose.NLME",
            "Certara.RsNLME.ModelExecutor",
            "Certara.ModelResults",
            "Certara.VPCResults",
            "Certara.RsNLME.ModelBuilder",
            "tidyvpc",
            "ggquickeda")

  if(include_self){
    pkgs <- c("Certara.R", pkgs)
  }

  if(include_JFROG_deps){
    pkgs <- c(pkgs, certara_jfrog_deps())
  }

  return(pkgs)
}

certara_jfrog_deps <- function() {

  pkgs <- c("Certara.NLME8",
            "Certara.shinyAce.NLME",
            "Certara.shinymeta.NLME")

  return(pkgs)
}



#' Install Certara R Packages
#'
#' A user friendly wrapper to easily install R packages and Shiny applications developed by Certara
#'
#' @param CRAN_repo Character;  The URL of a CRAN mirror which defaults to \code{"https://cloud.r-project.org"}
#' @param clean_install Logical; Set to \code{TRUE} to remove existing Certara packages prior to installing
#' @param quiet Logical; Set to \code{TRUE} to suppress installation messages and reduce console output
#'
#' @return \code{TRUE} if all packages successfully install
#' @export
#'
install_certara_packages <- function(CRAN_repo = "https://cloud.r-project.org",
                                     clean_install = FALSE,
                                     quiet = FALSE) {

  stopifnot(is_valid_url(CRAN_repo))

  if(clean_install){
    remove_certara_packages()
  }

  JFROG_repo <- "https://certara.jfrog.io/artifactory/certara-cran-release-public/"

  pkgs_display <- c("None", "All", certara_packages(include_JFROG_deps = FALSE))

  pkgs_num_entry <- paste(0:(length(pkgs_display) - 1))

  stopifnot(length(pkgs_display) == length(pkgs_num_entry))

  names(pkgs_display) <- pkgs_num_entry

  message("The following packages will be installed from ", CRAN_repo,
          " and Certara's package repository ", JFROG_repo, "\n\n",
          "Packages exclusively available on GitHub will be installed using 'remotes::install_github()'",
          "\n\n", paste(paste(names(pkgs_display), "\t", pkgs_display), collapse = "\n"))

  answer <- trimws(readline("Enter number, or an empty line to skip installation: "))

  if(nchar(answer) == 0 || answer == "0"){
    warning("Installation of Certara packages will not proceed")
    return(FALSE)
  } else {
    if(answer == "1"){
      pkgs_to_install <- certara_packages(include_JFROG_deps = TRUE)
    } else if(answer %in% as.character(2:(length(pkgs_display) - 1))) {
      pkgs_to_install <- pkgs_display[names(pkgs_display) == answer]
    } else {
      warning("Input entered is invalid, installation of Certara packages will not proceed")
      return(FALSE)
    }

    if(get_os()[[1]] == "windows"){
      method <- "libcurl"
    } else {
      method <- rlang::missing_arg()
    }

    message("Installing packages...")

    install.packages(pkgs_to_install, repos =c(JFROG_repo, CRAN_repo), method = method, quiet = quiet)

    pkgs_installed <- check_installed_certara_packages()

    if(!all(pkgs_installed)){
      if(answer == "1"){
        warning("The following packages did not successfully install: ",
              paste(names(pkgs_installed)[!pkgs_installed], collapse = "\n"))
        return(FALSE)
      } else if(!check_installed_certara_packages(pkgs_to_install)){
        warning(pkgs_to_install, " did not successfully install")
        return(FALSE)
      } else {
        message("Successfully installed", pkgs_to_install)
        return(TRUE)
      }
    } else {
      message("Successfully installed", pkgs_to_install)
      return(TRUE)
    }
  }
}

install_cran_remote <- function(){

}


install_github_remote <- function(){

}

#' Remove Certara R Packages
#'
#' A user friendly wrapper to easily remove R packages developed by Certara
#'
#' @return \code{TRUE} if all packages successfully removed
#' @export
#'
remove_certara_packages <- function(){

  pkgs <- certara_packages(include_JFROG_deps = TRUE)

  loaded.Certara.pkgs <-
    pkgs[pkgs %in% loadedNamespaces()]
  for(pkg in loaded.Certara.pkgs) {
    try(detach(name = paste0("package:", pkg), unload=TRUE, character.only=TRUE, force = TRUE),
        silent = TRUE)
  }

  installed.Certara.pkgs <-
    basename(find.package(pkgs, quiet = TRUE))

  if(length(installed.Certara.pkgs > 0)) {
    remove.packages(installed.Certara.pkgs)
  }

  return(TRUE)
}

#' Check if Certara packages are installed
#'
#' @return Named logical vector indicating whether Certara package is installed
#' @examples
#' check_installed_certara_packages()
#' @export

check_installed_certara_packages <- function(pkgs){

  if(missing(pkgs)){
    pkgs <- certara_packages(include_JFROG_deps = TRUE)
  }

  pkgs_logical <- sapply(pkgs, function(x)
    if(system.file(package = x) == ""){
      return(FALSE)
    } else {
      return(TRUE)
    }
  )

  return(pkgs_logical)
}

#' Check if Certara packages are installed
#'
#' @return Named logical vector indicating whether Certara package is installed
#' @examples
#' check_installed_certara_packages()
#' @export

check_certara_package_versions <- function(){

  pkgs <- certara_packages(include_JFROG_deps = TRUE)

  pkgs_version <- sapply(pkgs, function(x)
    if(system.file(package = x) == ""){
      return(NA)
    } else {
      return(packageVersion(x))
    })

  return(pkgs_version)
}


get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}

is_valid_url <- function(url, timeout=2){

  con <- url(url)

  check <- suppressWarnings(try(
    open.connection(con, open="rt", timeout = timeout),
    silent = TRUE)[1])

  suppressWarnings(try(
    close.connection(con),
    silent = TRUE))

  if(is.null(check)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
