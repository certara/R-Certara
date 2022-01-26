
#' Install Certara R Packages
#'
#' A user friendly wrapper to easily install R packages developed Certara and hosted on Certara's CRAN repo
#'
#' @return \code{TRUE} if all packages successfully install
#' @export
#'
install_certara_packages <- function() {

  pkgs <- c("Certara.RsNLME",
            "Certara.NLME8",
            "Certara.Xpose.NLME",
            "Certara.RsNLME.ModelExecutor",
            "Certara.ModelResults",
            "Certara.VPCResults",
            "Certara.DataQC",
            "Certara.RsNLME.ModelBuilder")

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

  if(get_os()[[1]] == "windows"){
    install.packages(released_pkgs,
                     repos =c("https://certara.jfrog.io/artifactory/certara-cran-release-public/",
                              "https://cloud.r-project.org"),
                     method = "libcurl")
  } else {
    install.packages(released_pkgs,
                     repos =c("https://certara.jfrog.io/artifactory/certara-cran-release-public/",
                              "https://cloud.r-project.org"))
  }

  return(TRUE)
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
  tolower(os)
}

