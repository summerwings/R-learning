package_web<-function(package_name){
  mamual_url <- paste(
    "https://cran.r-project.org/web/packages/",
    package_name,
    "/",
    package_name,
    ".pdf",
    sep=""
  )
  package_url <- paste(
    "https://cran.r-project.org/web/packages/",
    package_name,
    "/index.html",
    sep=""
  )
  browseURL(mamual_url)
  browseURL(package_url)
}