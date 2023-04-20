#' Dados de fluxo de comércio bilateral entre Brasil e um determinado país
#'
#' @param pais um país
#' @param periodo "anual" ou "mensal"
#'
#' @return tibble com dados de comércio bilateral de 2010 em diante.
#'
#' @export
comerciobr_dados_corrente <- function(pais, periodo) {

# A função tem dois argumentos: "pais" e "periodo". A função carrega um conjunto de dados de comércio exterior
# brasileiro (da biblioteca comerciobr2) e filtra-o para manter apenas as linhas que correspondem aos países
# especificados no argumento "pais". Em seguida, o conjunto de dados é manipulado de acordo com o argumento "periodo".

  df <- comerciobr2::sh1_df %>%
    dplyr::filter(no_pais %in% pais)

  if (periodo == "anual") {

# Se "periodo" for definido como "anual", o conjunto de dados é filtrado para manter apenas as observações com anos menores
# ou iguais ao ano mais recente no conjunto de dados menos um. Em seguida, os dados são agrupados por ano e "path" (provavelmente
# um tipo de produto ou categoria) e a soma dos valores é calculada.

    df <- df %>%
      dplyr::filter(co_ano <= max(co_ano)-1) %>%
      dplyr::group_by(co_ano, path) %>%
      dplyr::summarise(value = sum(value))
  }

  else {
# Se "periodo" for definido como "mensal", o conjunto de dados é transformado para incluir um novo campo "co_mes", convertido em
# número. O conjunto de dados é filtrado para manter apenas as observações com meses menores ou iguais ao mês mais recente
# disponível (usando uma função externa do pacote "barao2"). Os dados são agrupados por ano e "path" e a soma dos valores é calculada.

    df <- df %>%
      dplyr::mutate(co_mes = as.numeric(co_mes)) %>%
      dplyr::filter(co_mes <= barao2::comerciobr_get_ultimomes()) %>%
      dplyr::group_by(co_ano, path) %>%
      dplyr::summarise(value = sum(value))

  }

# Em ambos os casos, o conjunto de dados é transformado em um formato mais largo (usando a função "pivot_wider") e os valores
# ausentes são substituídos por 0. Em seguida, o conjunto de dados é transformado novamente em um formato mais longo (usando a
# função "pivot_longer") e as colunas "Exportacoes", "Importacoes", "Saldo" e "Corrente" são renomeadas.

  df <- df %>%
    tidyr::pivot_wider(names_from = path, values_from = value)

  df[is.na(df)] <- 0

  df <- df %>%
    dplyr::mutate(Corrente = .data$EXP + .data$IMP,
                  Saldo = .data$EXP - .data$IMP) %>%
    dplyr::rename(Exportacoes = "EXP", Importacoes = "IMP") %>%
    tidyr::pivot_longer(c("Exportacoes", "Importacoes", "Saldo", "Corrente"), names_to = "trade_flow", values_to = "value")

  df
}
