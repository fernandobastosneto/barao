#' @export
comerciobr_dados_fatores <- function(pais, periodo, fator) {

  if (fator == "isic") {
    isic <- comerciobr::dic_ncm_isic %>%
      dplyr::select(-co_ncm) %>%
      dplyr::distinct()

    df <- comerciobr::isic_df %>%
      dplyr::filter(co_ano == max(co_ano)) %>%
      dplyr::group_by(co_isic_secao, co_pais, path) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::rename(co_pais = co_pais) %>%
      dplyr::left_join(comerciobr::dic_paises, by = "co_pais") %>%
      dplyr::filter(no_pais == pais) %>%
      dplyr::left_join(isic)

  }

  else if (fator == "cuci") {
    cuci <- comerciobr::dic_ncm_cuci %>%
      dplyr::select(-co_ncm) %>%
      dplyr::distinct()

    df <- comerciobr::cuci_df %>%
      dplyr::filter(co_ano == max(co_ano)) %>%
      dplyr::group_by(co_cuci_sec, co_pais, path) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::rename(co_pais = co_pais) %>%
      dplyr::left_join(comerciobr::dic_paises, by = "co_pais") %>%
      dplyr::filter(no_pais == pais) %>%
      dplyr::left_join(cuci)

  }

  else if (fator == "fator") {

    fator <- comerciobr::dic_ncm_fator %>%
      dplyr::select(-co_ncm) %>%
      dplyr::distinct()

    df <- comerciobr::fator_df %>%
      dplyr::filter(co_ano == max(co_ano)) %>%
      dplyr::group_by(co_fat_agreg, co_pais, path) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::rename(co_pais = co_pais) %>%
      dplyr::left_join(comerciobr::dic_paises, by = "co_pais") %>%
      dplyr::filter(no_pais == pais) %>%
      dplyr::left_join(fator)

  }

  else if (fator == "cgce") {

    cgce <- comerciobr::dic_ncm_cgce %>%
      dplyr::select(-co_ncm) %>%
      dplyr::distinct() %>%
      dplyr::mutate(co_cgce_n1 = as.integer(co_cgce_n1))

    df <- comerciobr::cgce_df %>%
      dplyr::filter(co_ano == max(co_ano)) %>%
      dplyr::group_by(co_cgce_n1, co_pais, path) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::rename(co_pais = co_pais) %>%
      dplyr::left_join(comerciobr::dic_paises, by = "co_pais") %>%
      dplyr::filter(no_pais == pais) %>%
      dplyr::left_join(cgce, by = "co_cgce_n1")

  }

  df
}

#
#   dplyr::group_by(co_ano, co_cuci_sec, co_pais) %>%
#   dplyr::summarise(value = sum(value)) %>%
#
