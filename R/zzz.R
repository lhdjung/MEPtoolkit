.onLoad <- function(libname, pkgname) {
  if (requireNamespace("sysfonts", quietly = TRUE)) {
    fonts_dir <- system.file("fonts", package = pkgname)
    if (nzchar(fonts_dir)) {
      reg <- file.path(fonts_dir, "Poppins-Regular.ttf")
      b   <- file.path(fonts_dir, "Poppins-Bold.ttf")
      it  <- file.path(fonts_dir, "Poppins-Italic.ttf")
      bi  <- file.path(fonts_dir, "Poppins-BoldItalic.ttf")

      # add the family only if the main file exists and family not already present
      if (file.exists(reg) && !("Poppins" %in% sysfonts::font_families())) {
        sysfonts::font_add(family = "Poppins",
                           regular = reg,
                           bold    = if (file.exists(b))   b   else NULL,
                           italic  = if (file.exists(it))  it  else NULL,
                           bolditalic = if (file.exists(bi)) bi else NULL)
      }
    }
  }

  # enable showtext rendering if available (idempotent)
  if (requireNamespace("showtext", quietly = TRUE)) {
    showtext::showtext_auto()
  }
}


#' Internal package environment
#' @keywords internal
#' @noRd
.MEPenv <- new.env(parent = emptyenv())
.MEPenv$mepcolors = c("teal"      = "#498B6C",
                      "navy"      = "#335770",
                      "olive"     = "#A0A642",
                      "darkteal"  = "#18360C",
                      "softnavy"  = "#C6DCE3",
                      "softolive" = "#FDF9E6",
                      "softteal"  = "#BACF9A")
