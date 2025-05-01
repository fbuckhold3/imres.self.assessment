.onLoad <- function(libname, pkgname) {
  # This runs when the package is loaded
  # Set up resource paths only, avoid API calls here
  img_path <- system.file("www", package = "imres")
  if (dir.exists(img_path)) {
    message("Found imres www directory at: ", img_path)
    addResourcePath("imres-images", img_path)
  }
}
