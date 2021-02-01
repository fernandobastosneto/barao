## code to prepare `comexstat_dados` dataset goes here
# baixa csvs de do comexstat

library(magrittr)

anos <- c(2026:2010)
url_exp <- paste0("https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_")
url_exp_lista <- purrr::map_chr(anos, ~ paste0(url_exp, .x, ".csv"))
purrr::walk2(url_exp_lista, anos, ~ httr::GET(.x, config = httr::config(ssl_verifypeer = F),
                                              httr::write_disk(paste0(here::here("inst", "extdata", "comex_stat/"), "EXP_", .y, ".csv"),
                                                               overwrite = T)))

url_imp <- paste0("https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_")
url_imp_lista <- purrr::map_chr(anos, ~ paste0(url_imp, .x, ".csv"))
purrr::walk2(url_imp_lista, anos, ~ httr::GET(.x, config = httr::config(ssl_verifypeer = F),
                                              httr::write_disk(paste0(here::here("inst", "extdata", "comex_stat/"), "IMP_", .y, ".csv"),
                                                               overwrite = T)))

files_exp <- fs::dir_ls(here::here("inst", "extdata", "comex_stat"), regexp = "EXP")

files_exp <- purrr::map_dfr(files_exp, file.info, .id = "path") %>%
  tibble::as_tibble() %>%
  dplyr::filter(size > 25000) %>%
  dplyr::pull(path)

files_imp <- fs::dir_ls(here::here("inst", "extdata", "comex_stat"), regexp = "IMP")

files_imp <- purrr::map_dfr(files_imp, file.info, .id = "path") %>%
  tibble::as_tibble() %>%
  dplyr::filter(size > 25000) %>%
  dplyr::pull(path)

# Baixa Dicionários

httr::GET("https://balanca.economia.gov.br/balanca/bd/tabelas/NCM.csv",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("inst", "extdata", "dicionarios", "comexstat_NCM.csv"), overwrite = T))

ncm_sh6 <- vroom::vroom(here::here("inst", "extdata", "dicionarios", "comexstat_NCM.csv"),
                        col_select = c("CO_NCM", "CO_SH6"))

ncm_fatores <- vroom::vroom(here::here("inst", "extdata", "dicionarios", "comexstat_NCM.csv"),
                            col_select = c("CO_NCM", "CO_PPE", "CO_PPI", "CO_FAT_AGREG", "CO_ISIC_CLASSE"))

httr::GET("https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_SH.csv",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("inst", "extdata", "dicionarios", "comexstat_SH.csv"),
                           overwrite = T))
ncm_sh4 <- vroom::vroom(here::here("inst", "extdata", "dicionarios", "comexstat_SH.csv"),
                       col_select = c("CO_SH6", "NO_SH4_POR", "CO_SH4"),
                       locale = vroom::locale(encoding = "ISO-8859-1"))

ncm_sh1 <- vroom::vroom(here::here("inst", "extdata", "dicionarios", "comexstat_SH.csv"),
             col_select = c("CO_SH6", "CO_NCM_SECROM"),
             locale = vroom::locale(encoding = "ISO-8859-1"))

httr::GET("https://balanca.economia.gov.br/balanca/bd/tabelas/PAIS.csv",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("inst", "extdata", "dicionarios", "comexstat_paises.csv"),
                           overwrite = T))

comexstat_paises <- vroom::vroom(here::here("inst", "extdata", "dicionarios", "comexstat_paises.csv"),
                                 col_select = c("CO_PAIS", "NO_PAIS"),
                                 locale = vroom::locale(encoding = "ISO-8859-1"))


# pegar produto em sh6

get_sh6 <- function(file) {

  nome <- file %>%
    stringr::str_extract("\\w{3}_\\d{4}(?=.csv$)")
  nome <- paste0(nome, "_sh6.csv")

  df <- vroom::vroom(file, altrep = F,
                     col_select = c("CO_ANO", "CO_MES", "CO_NCM", "CO_PAIS", "VL_FOB"),
                     col_types = c(CO_ANO = "i", CO_MES = "c",
                                   CO_NCM = "c", CO_PAIS = "c",
                                   VL_FOB = "d"))

  df %>%
    dplyr::left_join(ncm_sh6) %>%
    dplyr::group_by(CO_ANO, CO_MES, CO_PAIS, CO_SH6) %>%
    dplyr::summarise(value = sum(VL_FOB)) %>%
    vroom::vroom_write(paste0(here::here("inst", "extdata", "comex_stat/", nome)))
}

purrr::walk(files_exp, get_sh6)
purrr::walk(files_imp, get_sh6)

# pegar produto em sh4

exp_sh6 <- fs::dir_ls(here::here("inst", "extdata", "comex_stat"), regexp = "EXP_\\d{4}_sh6.csv$")
imp_sh6 <- fs::dir_ls(here::here("inst", "extdata", "comex_stat"), regexp = "IMP_\\d{4}_sh6.csv$")

get_sh4 <- function(file) {

  nome <- file %>%
    stringr::str_extract("\\w{3}_\\d{4}(?=_sh6.csv$)")
  nome <- paste0(nome, "_sh4.csv")

  df <- vroom::vroom(file, altrep = F,
                     col_select = c("CO_ANO", "CO_MES", "CO_SH6", "CO_PAIS", "value"),
                     col_types = c(CO_ANO = "i", CO_MES = "c",
                                   CO_SH6 = "c", CO_PAIS = "c",
                                   value = "d"))

  df %>%
    dplyr::left_join(ncm_sh4) %>%
    dplyr::group_by(CO_ANO, CO_MES, CO_PAIS, CO_SH4) %>%
    dplyr::summarise(value = sum(value)) %>%
    vroom::vroom_write(paste0(here::here("inst", "extdata", "comex_stat/", nome)))
}

purrr::walk(exp_sh6, get_sh4)
purrr::walk(imp_sh6, get_sh4)

# pegar produto em sh1

get_sh1 <- function(file) {

  nome <- file %>%
    stringr::str_extract("\\w{3}_\\d{4}(?=_sh6.csv$)")
  nome <- paste0(nome, "_sh1.csv")

  df <- vroom::vroom(file, altrep = F,
                     col_select = c("CO_ANO", "CO_MES", "CO_SH6", "CO_PAIS", "value"),
                     col_types = c(CO_ANO = "i", CO_MES = "c",
                                   CO_SH6 = "c", CO_PAIS = "c",
                                   value = "d"))

  df %>%
    dplyr::left_join(ncm_sh1) %>%
    dplyr::group_by(CO_ANO, CO_MES, CO_PAIS, CO_NCM_SECROM) %>%
    dplyr::summarise(value = sum(value)) %>%
    vroom::vroom_write(paste0(here::here("inst", "extdata", "comex_stat/", nome)))
}

purrr::walk(exp_sh6, get_sh1)
purrr::walk(imp_sh6, get_sh1)

ncm_sh4 <- ncm_sh4 %>%
  dplyr::select(NO_SH4_POR, CO_SH4) %>%
  dplyr::distinct()

# pegar nome do país

sh1_files <- fs::dir_ls(here::here("inst", "extdata", "comex_stat"), regexp = "sh1.csv$")

sh1_df <- purrr::map_dfr(sh1_files, ~ vroom::vroom(.x, id = "path")) %>%
  dplyr::mutate(path = stringr::str_extract(path, "[:upper:]{3}")) %>%
  dplyr::left_join(comexstat_paises)

sh4_files <- fs::dir_ls(here::here("inst", "extdata", "comex_stat"), regexp = "sh4.csv$")

sh4_df <- purrr::map_dfr(sh4_files, ~ vroom::vroom(.x, id = "path",
                                                        col_types = c(path = "c", CO_ANO = "i",
                                                                      CO_MES = "c", CO_PAIS = "c",
                                                                      CO_SH4 = "c", value = "d"))) %>%
  dplyr::mutate(path = stringr::str_extract(path, "[:upper:]{3}")) %>%
  dplyr::left_join(comexstat_paises) %>%
  dplyr::left_join(ncm_sh4)

sh6_files <- fs::dir_ls(here::here("inst", "extdata", "comex_stat"), regexp = "sh6.csv$")

sh6_df <- purrr::map_dfr(sh6_files, ~ vroom::vroom(.x, id = "path",
                                                        col_types = c(path = "c", CO_ANO = "i",
                                                                      CO_MES = "c", CO_PAIS = "c",
                                                                      CO_SH6 = "c", value = "d"))) %>%
  dplyr::mutate(path = stringr::str_extract(path, "[:upper:]{3}"))

usethis::use_data(sh1_df, sh4_df, sh6_df, overwrite = TRUE)
