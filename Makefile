.PHONY:
Rscripts:
	Rscript -e "lapply(list.files(pattern = '*.Rmd', \
                                      recursive = TRUE, \
                                      full.names = TRUE), \
                           function(x) {knitr::purl(x, \
                                                    documentation = 0, \
                                                    output = paste0(tools::file_path_sans_ext(x), \
                                                                    '.R'))})"
.PHONY:
html:
	Rscript -e "lapply(list.files(pattern = '*.Rmd', \
                                      recursive = TRUE, \
                                      full.names = TRUE), \
                           function(x) {rmarkdown::render(x, \
                                                          'html_document', \
                                                          params = list(presentation = FALSE))})"
