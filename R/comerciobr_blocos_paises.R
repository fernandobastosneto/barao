#'
#' Lista de países a partir de um bloco
#'
#' @param bloco o nome de um bloco, de acordo com a base dic_blocos do pacote "comerciobr2"
#'
#' @return lista de países associados a determinado bloco
#'
#' @export
comerciobr_blocos_paises <- function(bloco) {

    paises <- comerciobr2::dic_blocos %>%
      dplyr::filter(no_bloco == bloco) %>%
      dplyr::distinct(no_pais) %>%
      dplyr::pull(no_pais)

}



#Dentro da função, a primeira linha usa o pacote "comerciobr2" para acessar um dicionário de blocos comerciais e países membros.
#A próxima linha usa o operador "%>%" para encadear uma sequência de operações usando o pacote "dplyr" para manipulação de dados.

#A primeira operação "filter" filtra as linhas do dicionário onde a coluna "no_bloco" é igual ao nome do bloco que foi passado como
#parâmetro para a função. A segunda operação "distinct" filtra as linhas distintas com base na coluna "no_pais". A terceira operação
#"pull" extrai a coluna "no_pais" em um vetor.

#Finalmente, a função retorna o vetor de países membros do bloco comercial.
