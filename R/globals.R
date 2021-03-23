#' @importFrom rlang .data
#' @importFrom stats setNames
#' @import utils

utils::globalVariables(c(".",
                  "where",
                  names(c(
                        comerciobr::cgce_df,
                        comerciobr::cuci_df,
                        comerciobr::dic_blocos,
                        comerciobr::dic_ncm_cgce,
                        comerciobr::dic_ncm_cuci,
                        comerciobr::dic_ncm_fator,
                        comerciobr::dic_ncm_isic,
                        comerciobr::dic_paises,
                        comerciobr::dic_paises_isoa3,
                        comerciobr::dic_sh6_sh1,
                        comerciobr::dic_sh6_sh2,
                        comerciobr::dic_sh6_sh4,
                        comerciobr::fator_df,
                        comerciobr::isic_df,
                        comerciobr::cuci_df,
                        comerciobr::cgce_df,
                        comerciobr::sh1_df,
                        comerciobr::sh4_df,
                        comerciomundo::comtrade,
                        comerciomundo::dic_comtrade_mdic,
                        comerciomundo::dic_partners,
                        comerciomundo::dic_reporters))))
