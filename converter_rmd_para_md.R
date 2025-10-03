rmarkdown::render("baixar_raster/readme.Rmd",
                  rmarkdown::md_document(variant = "gfm"),
                  output_options = list(fig_path = "man/figures/"))
