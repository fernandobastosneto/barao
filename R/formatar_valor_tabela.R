#' Formata valores monetários com escala dinâmica
#'
#' Esta função formata um vetor numérico de valores monetários, ajustando a escala e o sufixo
#' de acordo com o valor máximo do vetor. Se o valor máximo for maior ou igual a 1 bilhão,
#' utiliza a escala de 1e-9 e o sufixo "B"; se for maior ou igual a 1 milhão, usa 1e-6 e "M";
#' se for maior ou igual a 1 mil, usa 1e-3 e "K"; caso contrário, não aplica escala.
#'
#' @param x Vetor numérico contendo os valores a serem formatados.
#' @param accuracy Precisão da formatação. Default: 0.1.
#'
#' @return Um vetor de caracteres com os valores formatados.
#' @export
#'
#' @examples
#' formatar_valor_individual(c(500, 200000, 3000000000))
#'
formatar_valor_individual <- function(x, accuracy = 0.01) {
  dplyr::case_when(
    x >= 1e9 ~ scales::dollar(x * 1e-9, suffix = "B", accuracy = accuracy, big.mark = ".", decimal.mark = ","),
    x >= 1e6 ~ scales::dollar(x * 1e-6, suffix = "M", accuracy = accuracy, big.mark = ".", decimal.mark = ","),
    x >= 1e3 ~ scales::dollar(x * 1e-3, suffix = "K", accuracy = accuracy, big.mark = ".", decimal.mark = ","),
    TRUE ~ scales::dollar(x, accuracy = accuracy, big.mark = ".", decimal.mark = ",")
  )
}

#' Formata uma coluna inteira de valores monetários
#'
#' Esta função aplica `formatar_valor_individual` a uma coluna inteira de valores monetários.
#'
#' @param x Vetor numérico contendo os valores a serem formatados.
#' @param accuracy Precisão da formatação. Default: 0.1.
#'
#' @return Um vetor de caracteres com os valores formatados.
#' @export
#'
#' @examples
#' formatar_valor_tabela_produto(c(500, 200000, 3000000000))
#'
formatar_valor_tabela_produto <- function(x, accuracy = 0.01) {
  purrr::map_chr(x, ~ formatar_valor_individual(.x, accuracy = accuracy))
}
