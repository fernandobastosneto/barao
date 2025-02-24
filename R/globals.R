#' @importFrom rlang .data
#' @importFrom stats setNames
#' @import utils

utils::globalVariables(c(".",
                  "where",
                  names(c(
                        comerciobr2::cgce_df,
                        comerciobr2::cuci_df,
                        comerciobr2::dic_blocos,
                        comerciobr2::dic_ncm_cgce,
                        comerciobr2::dic_ncm_cuci,
                        comerciobr2::dic_ncm_fator,
                        comerciobr2::dic_ncm_isic,
                        comerciobr2::dic_paises,
                        comerciobr2::dic_paises_isoa3,
                        comerciobr2::dic_sh6_sh1,
                        comerciobr2::dic_sh6_sh2,
                        comerciobr2::dic_sh6_sh4,
                        comerciobr2::fator_df,
                        comerciobr2::isic_df,
                        comerciobr2::cuci_df,
                        comerciobr2::cgce_df,
                        comerciobr2::sh1_df,
                        comerciobr2::sh4_df,
                        comerciomundo2::comtrade,
                        comerciomundo2::dic_comtrade_mdic,
                        comerciomundo2::dic_partners,
                        comerciomundo2::dic_reporters))))
