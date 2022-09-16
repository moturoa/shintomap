
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "mapicons",
    directoryPath = system.file(
      "icons",
      package = "shintomap"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("mapicons")
}
