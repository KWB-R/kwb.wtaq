file <- if (WINDOWS) {
  "wtaq2_1.exe"}  else {
    "wtaq2_1"}

# working directory is ./src
if (file.exists(file)) {
  dest <- file.path(R_PACKAGE_DIR, "bin", R_ARCH)
  message("Installing ", file, " to ", dest)
  dir.create(dest, recursive=TRUE, showWarnings=FALSE)
  file.copy(file, dest, overwrite=TRUE)
  file.remove(c(file, list.files(pattern = "\\.o$")))

}

