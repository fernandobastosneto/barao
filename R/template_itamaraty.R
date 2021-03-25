#' Template de relatório para o Itamaraty
#' @export
template_itamaraty <- function(file) {

    arq <- paste0(file,".Rmd")

    rmarkdown::draft(arq, template="template_itamaraty", package = "barao")
}
