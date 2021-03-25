#' Template de relatório para o Itamaraty

template_itamaraty <- function(toc = TRUE) {

    # get the locations of resource files located within the package
    css <- system.file("rmarkdown/templates/template_itamaraty/skeleton/tese.css", package = "barao")
    template <- system.file("rmarkdown/templates/template_itamaraty/skeleton/template.html", package = "barao")

    # call the base html_document function
    pagedown::html_paged(css = css, template = template)
}
