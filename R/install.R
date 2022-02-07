JFROG_repo <- "https://certara.jfrog.io/artifactory/certara-cran-release-public/"

#' List all packages developed by Certara
#'
#' @param include_self Logical; Set to \code{TRUE} to include \code{Certara.R}
#' @param include_github Logical; Set to \code{TRUE} to include packages only available on GitHub
#' @param include_JFROG_deps Logical; Set to \code{TRUE} to include Certara package dependencies on Jfrog
#' @return Character vector of package names
#' @examples
#' certara_packages()
#' @export

certara_packages <- function(include_self= FALSE, include_github = TRUE, include_JFROG_deps = FALSE) {

  # Define character vector of pkgs installed via install.packages()
  pkgs <- c("Certara.RsNLME",
            "Certara.Xpose.NLME",
            "Certara.RsNLME.ModelExecutor",
            "Certara.ModelResults",
            "Certara.VPCResults",
            "Certara.RsNLME.ModelBuilder",
            "tidyvpc",
            "ggquickeda",
            "coveffectsplot")

  if(include_self){
    pkgs <- c("Certara.R", pkgs)
  }

  if(include_github){
    pkgs <- c(pkgs, names(certara_github_packages()))
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

certara_github_packages <- function() {
  list(table1c = "certara/table1c",
       ggcertara = "certara/ggcertara")
}



#' Install Certara packages
#'
#' A user friendly wrapper to easily install R packages and Shiny applications developed by Certara
#'
#' @param CRAN_repo Character;  The URL of a CRAN mirror which defaults to \code{"https://cloud.r-project.org"}
#' @param clean_install Logical; Set to \code{TRUE} to remove all existing Certara packages prior to installing
#' @param quiet Logical; Set to \code{TRUE} to suppress installation messages and reduce console output
#'
#' @return \code{TRUE} if all packages successfully install
#' @examples
#' \dontrun{
#' install_certara_packages()
#' }
#' @export
#'
install_certara_packages <- function(CRAN_repo = "https://cloud.r-project.org",
                                     clean_install = FALSE,
                                     quiet = FALSE) {

  stopifnot(is_valid_url(CRAN_repo))

  if(clean_install){
    rm_ok <- remove_certara_packages()
    if(!rm_ok){
      stop("Cannot remove Certara packages, see below warnings")
    }
  }

  pkgs_display <- c("None", "All", certara_packages(include_JFROG_deps = FALSE))

  pkgs_num_entry <- paste(0:(length(pkgs_display) - 1))

  stopifnot(length(pkgs_display) == length(pkgs_num_entry))

  names(pkgs_display) <- pkgs_num_entry

  message("The following packages will be installed from ", CRAN_repo,
          " and Certara's package repository ", JFROG_repo, "\n\n",
          "Packages exclusively available on GitHub will be installed using 'remotes::install_github()'",
          "\n\n", paste(paste(names(pkgs_display), "\t", pkgs_display), collapse = "\n"), "\n")

  answer <- trimws(readline("Enter number, or an empty line to skip installation: "))

  message("\nInstalling packages...")

  cran_ok <- install_cran_remote(pkgs_display, answer, repos = c(JFROG_repo, CRAN_repo), quiet)

  if(!cran_ok){
    stop("Installation of packages from CRAN/JFrog remote repository was not successful, see below warnings")
  }

  github_ok <- install_github_remote(pkgs_display, answer, quiet)

  if(!github_ok){
    stop("Installation of packages from GitHub remote repository was not successful, see below warnings")
  }
}

install_cran_remote <- function(pkgs_display, answer, repos, quiet){
  if(nchar(answer) == 0 || answer == "0"){
    warning("Installation of Certara packages has been exited by user and will not proceed")
    return(FALSE)
  } else {
    if(answer == "1"){
      pkgs_to_install <- certara_packages(include_JFROG_deps = TRUE, include_github = FALSE)
    } else if(answer %in% as.character(2:(length(pkgs_display) - 3))) {
      pkgs_to_install <- pkgs_display[names(pkgs_display) == answer]
    } else if(answer %in% as.character(11:(length(pkgs_display) - 1))) {
      return(TRUE)
    } else {
      warning("Input entered is invalid, installation of Certara packages will not proceed")
      return(FALSE)
    }

    utils::install.packages(pkgs_to_install, repos = repos, method = "libcurl", quiet = quiet)

    pkgs_installed <- check_installed_certara_packages(certara_packages(include_github = FALSE))

    if(!all(pkgs_installed)){
      if(answer == "1"){
        warning("The following packages from CRAN/JFrog remote did not successfully install:\n",
                paste(names(pkgs_installed)[!pkgs_installed], collapse = "\n"))
        return(FALSE)
      } else if(!check_installed_certara_packages(pkgs_to_install)){
        warning(pkgs_to_install, " did not successfully install")
        return(FALSE)
      } else {
        message("Successfully installed the following package from CRAN/JFrog remote:\n", pkgs_to_install)
        return(TRUE)
      }
    } else {
      message("Successfully installed the following packages from CRAN/JFrog remote:\n", paste(pkgs_to_install, collapse = "\n"))
      return(TRUE)
    }
  }

}


install_github_remote <- function(pkgs_display, answer, quiet){

  if(answer == "1"){
    pkgs_to_install <- names(certara_github_packages())
    lapply(certara_github_packages(), function(x){
      remotes::install_github(x, upgrade = "never", quiet = quiet)
    })
  } else if(answer %in% as.character(11:(length(pkgs_display) - 1))) {
    pkgs_to_install <- pkgs_display[names(pkgs_display) == answer]
    repo_to_install <- certara_github_packages()[[pkgs_to_install]]
    remotes::install_github(repo_to_install,
                            upgrade = "never", quiet = quiet)
  } else {
    return(TRUE)
  }

  pkgs_installed <- check_installed_certara_packages(pkgs_to_install)

  if(!all(pkgs_installed)){
    warning("The following packages from GitHub remote did not successfully install:\n",
              paste(names(pkgs_installed)[!pkgs_installed], collapse = "\n"))
    return(FALSE)
  } else {
    message("Successfully installed the following packages from GitHub remote:\n", paste(pkgs_to_install, collapse = "\n"))
    return(TRUE)
  }
}

#' Remove Certara packages
#'
#' A user friendly wrapper to easily remove R packages developed by Certara
#'
#' @return \code{TRUE} if all packages successfully removed
#' @examples
#' \dontrun{
#' remove_certara_packages()
#' }
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

  find_packages <- function(pkgs){
    basename(find.package(pkgs, quiet = TRUE))
  }

  installed.Certara.pkgs <- find_packages(pkgs)

  if(length(installed.Certara.pkgs) > 0) {
    utils::remove.packages(installed.Certara.pkgs)
    pkgs_not_removed <- find_packages(pkgs)
    if(length(pkgs_not_removed) > 0){
      warning("Cannot remove the following package(s):\n", paste(pkgs_not_removed, collapse = "\n"))
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(TRUE)
  }
}

#' Check if Certara packages are installed
#'
#' @param pkgs Character or character vector of package names. If missing, defaults to \code{certara_packages()}
#' @return Named logical vector indicating whether Certara package is installed
#' @examples
#' \dontrun{
#' check_installed_certara_packages()
#' }
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

#' Check Certara package versions
#'
#' @param pkgs Character or character vector of package names. If missing, defaults to \code{certara_packages()}
#' @return Named character vector indicating package version, \code{NA} is returned if Certara package is not installed
#' @examples
#' \dontrun{
#' check_certara_package_versions()
#' }
#' @export

check_certara_package_versions <- function(pkgs){

  if(missing(pkgs)){
    pkgs <- certara_packages(include_self = TRUE, include_JFROG_deps = TRUE)
  }

  pkgs_version <- sapply(pkgs, function(x)
    if(system.file(package = x) == ""){
      return(NA)
    } else {
      return(as.character(utils::packageVersion(x)))
    })

  return(pkgs_version)
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
