# baixa csvs de do comexstat

library(magrittr)

if(!fs::dir_exists(here::here("temp/"))) {
  fs::dir_create("temp")
  fs::dir_create("temp/filtrados")
}

anos <- c(2022:2010)
url_exp <- paste0("https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/EXP_")
url_exp_lista <- purrr::map_chr(anos, ~ paste0(url_exp, .x, ".csv"))
purrr::walk2(url_exp_lista, anos, ~ httr::GET(.x, config = httr::config(ssl_verifypeer = F),
                                              httr::write_disk(paste0(here::here("temp/"), "EXP_", .y, ".csv"),
                                                               overwrite = T)))

url_imp <- paste0("https://balanca.economia.gov.br/balanca/bd/comexstat-bd/ncm/IMP_")
url_imp_lista <- purrr::map_chr(anos, ~ paste0(url_imp, .x, ".csv"))
purrr::walk2(url_imp_lista, anos, ~ httr::GET(.x, config = httr::config(ssl_verifypeer = F),
                                              httr::write_disk(paste0(here::here("temp/"), "IMP_", .y, ".csv"),
                                                               overwrite = T)))

files_exp <- fs::dir_ls(here::here("temp"), regexp = "EXP")

files_exp <- purrr::map_dfr(files_exp, file.info, .id = "path") %>%
  tibble::as_tibble() %>%
  dplyr::filter(size > 250000) %>%
  dplyr::pull(path)

files_imp <- fs::dir_ls(here::here("temp"), regexp = "IMP")

files_imp <- purrr::map_dfr(files_imp, file.info, .id = "path") %>%
  tibble::as_tibble() %>%
  dplyr::filter(size > 250000) %>%
  dplyr::pull(path)

# Baixa Dicionários

httr::GET("https://balanca.economia.gov.br/balanca/bd/tabelas/NCM.csv",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("data-raw", "ncm.csv"), overwrite = T))

ncm_sh6 <- vroom::vroom(here::here("data-raw", "ncm.csv"),
                        col_select = c("CO_NCM", "CO_SH6"))

httr::GET("https://balanca.economia.gov.br/balanca/bd/tabelas/NCM_SH.csv",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("data-raw", "sh.csv"),
                           overwrite = T))

dic_sh6_sh4 <- readr::read_csv2(here::here("data-raw", "sh.csv"),
                                locale = readr::locale(encoding = "ISO-8859-1")) %>%
  dplyr::select("CO_SH6", "NO_SH4_POR", "CO_SH4")

dic_sh6_sh2 <- readr::read_csv2(here::here("data-raw", "sh.csv"),
                                locale = readr::locale(encoding = "ISO-8859-1")) %>%
  dplyr::select(CO_SH6, CO_SH2, NO_SH2_POR)

# dic_sh6_sh2 <- readr::read_csv2(here::here("data-raw", "sh.csv"),
#                             col_select = c("CO_SH6", "CO_SH2", "NO_SH2_POR"),
#                             locale = vroom::locale(encoding = "ISO-8859-1"))

dic_sh6_sh1 <- readr::read_csv2(here::here("data-raw", "sh.csv"),
                                locale = readr::locale(encoding = "ISO-8859-1")) %>%
  dplyr::select(CO_SH6, CO_NCM_SECROM, NO_SEC_POR)

# dic_sh6_sh1 <- vroom::vroom(here::here("data-raw", "sh.csv"),
#                             col_select = c("CO_SH6", "CO_NCM_SECROM", "NO_SEC_POR"),
#                             locale = vroom::locale(encoding = "ISO-8859-1"))


httr::GET("https://balanca.economia.gov.br/balanca/bd/tabelas/PAIS.csv",
          config = httr::config(ssl_verifypeer = F),
          httr::write_disk(here::here("data-raw", "dic_paises.csv"),
                           overwrite = T))

dic_paises <- vroom::vroom(here::here("data-raw", "dic_paises.csv"),
                           col_select = c("CO_PAIS", "NO_PAIS"),
                           locale = vroom::locale(encoding = "ISO-8859-1"))

dic_paises_isoa3 <- vroom::vroom(here::here("data-raw", "dic_paises.csv"),
                                 col_select = c("CO_PAIS", "CO_PAIS_ISOA3", "NO_PAIS_ING", "NO_PAIS"),
                                 locale = vroom::locale(encoding = "ISO-8859-1"))

# CUCI

dic_ncm_cuci <- readxl::read_excel(here::here("data-raw", "tabelas_auxiliares.xlsx"),
                                   sheet = 3) %>%
  dplyr::select(CO_NCM, CO_CUCI_SEC,	NO_CUCI_SEC)

# CGCE

dic_ncm_cgce <- readxl::read_excel(here::here("data-raw", "tabelas_auxiliares.xlsx"),
                                   sheet = 4) %>%
  dplyr::select(CO_NCM, CO_CGCE_N1,	NO_CGCE_N1)

# ISIC

dic_ncm_isic <- readxl::read_excel(here::here("data-raw", "tabelas_auxiliares.xlsx"),
                                   sheet = 5) %>%
  dplyr::select(CO_NCM, NO_ISIC_SECAO, CO_ISIC_SECAO)

# SIIT

dic_ncm_siit <- readxl::read_excel(here::here("data-raw", "tabelas_auxiliares.xlsx"),
                                   sheet = 6) %>%
  dplyr::select(CO_NCM, CO_SIIT, NO_SIIT)

# FAT AGREG

dic_ncm_fator <- readxl::read_excel(here::here("data-raw", "tabelas_auxiliares.xlsx"),
                                    sheet = 8) %>%
  dplyr::select(CO_NCM, CO_FAT_AGREG, NO_FAT_AGREG)

# SET

dic_ncm_set <- readxl::read_excel(here::here("data-raw", "tabelas_auxiliares.xlsx"),
                                  sheet = 11) %>%
  dplyr::select(CO_NCM, CO_EXP_SET,	NO_EXP_SET)

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
    vroom::vroom_write(paste0(here::here("temp", "filtrados/", nome)))
}

purrr::walk(files_exp, get_sh6)
purrr::walk(files_imp, get_sh6)

# pegar produto em sh4

exp_sh6 <- fs::dir_ls(here::here("temp", "filtrados"), regexp = "EXP_\\d{4}_sh6.csv$")
imp_sh6 <- fs::dir_ls(here::here("temp", "filtrados"), regexp = "IMP_\\d{4}_sh6.csv$")

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
    dplyr::left_join(dic_sh6_sh4) %>%
    dplyr::group_by(CO_ANO, CO_MES, CO_PAIS, CO_SH4) %>%
    dplyr::summarise(value = sum(value)) %>%
    vroom::vroom_write(paste0(here::here("temp", "filtrados/", nome)))

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
    dplyr::left_join(dic_sh6_sh1) %>%
    dplyr::group_by(CO_ANO, CO_MES, CO_PAIS, CO_NCM_SECROM) %>%
    dplyr::summarise(value = sum(value)) %>%
    vroom::vroom_write(paste0(here::here("temp", "filtrados/", nome)))
}

purrr::walk(exp_sh6, get_sh1)
purrr::walk(imp_sh6, get_sh1)

get_fator <- function(file) {

  nome <- file %>%
    stringr::str_extract("\\w{3}_\\d{4}(?=.csv$)")
  nome <- paste0(nome, "_fator.csv")

  df <- vroom::vroom(file, altrep = F,
                     col_select = c("CO_ANO", "CO_MES", "CO_NCM", "CO_PAIS", "VL_FOB"),
                     col_types = c(CO_ANO = "i", CO_MES = "c",
                                   CO_NCM = "c", CO_PAIS = "c",
                                   VL_FOB = "d"))

  df %>% dplyr::group_by(CO_ANO, CO_PAIS, CO_NCM) %>%
    dplyr::summarise(value = sum(VL_FOB)) %>%
    dplyr::left_join(dic_ncm_fator) %>%
    dplyr::group_by(CO_FAT_AGREG, CO_ANO, CO_PAIS) %>%
    dplyr::summarise(value = sum(value))%>%
    vroom::vroom_write(paste0(here::here("temp", "filtrados/", nome)))
}

purrr::walk(files_exp, get_fator)
purrr::walk(files_imp, get_fator)

get_cgce <- function(file) {

  nome <- file %>%
    stringr::str_extract("\\w{3}_\\d{4}(?=.csv$)")
  nome <- paste0(nome, "_cgce.csv")

  df <- vroom::vroom(file, altrep = F,
                     col_select = c("CO_ANO", "CO_MES", "CO_NCM", "CO_PAIS", "VL_FOB"),
                     col_types = c(CO_ANO = "i", CO_MES = "c",
                                   CO_NCM = "c", CO_PAIS = "c",
                                   VL_FOB = "d"))

  df %>% dplyr::group_by(CO_ANO, CO_PAIS, CO_NCM) %>%
    dplyr::summarise(value = sum(VL_FOB)) %>%
    dplyr::left_join(dic_ncm_cgce) %>%
    dplyr::group_by(CO_CGCE_N1, CO_ANO, CO_PAIS) %>%
    dplyr::summarise(value = sum(value))%>%
    vroom::vroom_write(paste0(here::here("temp", "filtrados/", nome)))
}

purrr::walk(files_exp, get_cgce)
purrr::walk(files_imp, get_cgce)

get_cuci <- function(file) {

  nome <- file %>%
    stringr::str_extract("\\w{3}_\\d{4}(?=.csv$)")
  nome <- paste0(nome, "_cuci.csv")

  df <- vroom::vroom(file, altrep = F,
                     col_select = c("CO_ANO", "CO_MES", "CO_NCM", "CO_PAIS", "VL_FOB"),
                     col_types = c(CO_ANO = "i", CO_MES = "c",
                                   CO_NCM = "c", CO_PAIS = "c",
                                   VL_FOB = "d"))

  df %>% dplyr::group_by(CO_ANO, CO_PAIS, CO_NCM) %>%
    dplyr::summarise(value = sum(VL_FOB)) %>%
    dplyr::left_join(dic_ncm_cuci) %>%
    dplyr::group_by(CO_CUCI_SEC, CO_ANO, CO_PAIS) %>%
    dplyr::summarise(value = sum(value))%>%
    vroom::vroom_write(paste0(here::here("temp", "filtrados/", nome)))
}

purrr::walk(files_exp, get_cuci)
purrr::walk(files_imp, get_cuci)

get_isic <- function(file) {

  nome <- file %>%
    stringr::str_extract("\\w{3}_\\d{4}(?=.csv$)")
  nome <- paste0(nome, "_isic.csv")

  df <- vroom::vroom(file, altrep = F,
                     col_select = c("CO_ANO", "CO_MES", "CO_NCM", "CO_PAIS", "VL_FOB"),
                     col_types = c(CO_ANO = "i", CO_MES = "c",
                                   CO_NCM = "c", CO_PAIS = "c",
                                   VL_FOB = "d"))

  df %>% dplyr::group_by(CO_ANO, CO_PAIS, CO_NCM) %>%
    dplyr::summarise(value = sum(VL_FOB)) %>%
    dplyr::left_join(dic_ncm_isic) %>%
    dplyr::group_by(CO_ISIC_SECAO, CO_ANO, CO_PAIS) %>%
    dplyr::summarise(value = sum(value))%>%
    vroom::vroom_write(paste0(here::here("temp", "filtrados/", nome)))
}

purrr::walk(files_exp, get_isic)
purrr::walk(files_imp, get_isic)


# pegar nome do país

sh1_files <- fs::dir_ls(here::here("temp", "filtrados"), regexp = "sh1.csv$")

sh1_df <- purrr::map_dfr(sh1_files, ~ vroom::vroom(.x, id = "path")) %>%
  dplyr::mutate(path = stringr::str_extract(path, "[:upper:]{3}")) %>%
  dplyr::left_join(dic_paises) %>%
  janitor::clean_names()

sh4_files <- fs::dir_ls(here::here("temp", "filtrados"), regexp = "sh4.csv$")

sh4_df <- purrr::map_dfr(sh4_files, ~ vroom::vroom(.x, id = "path",
                                                   col_types = c(path = "c", CO_ANO = "i",
                                                                 CO_MES = "c", CO_PAIS = "c",
                                                                 CO_SH4 = "c", value = "d"))) %>%
  dplyr::mutate(path = stringr::str_extract(path, "[:upper:]{3}")) %>%
  dplyr::left_join(dic_paises) %>%
  janitor::clean_names()

sh6_files <- fs::dir_ls(here::here("temp", "filtrados"), regexp = "sh6.csv$")

sh6_df <- purrr::map_dfr(sh6_files, ~ vroom::vroom(.x, id = "path",
                                                   col_types = c(path = "c", CO_ANO = "i",
                                                                 CO_MES = "c", CO_PAIS = "c",
                                                                 CO_SH6 = "c", value = "d"))) %>%
  dplyr::mutate(path = stringr::str_extract(path, "[:upper:]{3}")) %>%
  janitor::clean_names()

fator_files <- fs::dir_ls(here::here("temp", "filtrados"), regexp = "fator.csv$")

fator_df <- purrr::map_dfr(fator_files, ~ vroom::vroom(.x, id = "path",
                                                       col_types = c(path = "c", CO_ANO = "i",
                                                                     CO_FAT_AGREG = "c", CO_PAIS = "c",
                                                                     value = "d"))) %>%
  dplyr::mutate(path = stringr::str_extract(path, "[:upper:]{3}")) %>%
  janitor::clean_names()

cgce_files <- fs::dir_ls(here::here("temp", "filtrados"), regexp = "cgce.csv$")

cgce_df <- purrr::map_dfr(cgce_files, ~ vroom::vroom(.x, id = "path",
                                                     col_types = c(path = "c", CO_ANO = "i",
                                                                   CO_CGCE_N1 = "d", CO_PAIS = "c",
                                                                   value = "d"))) %>%
  dplyr::mutate(path = stringr::str_extract(path, "[:upper:]{3}")) %>%
  janitor::clean_names()

cuci_files <- fs::dir_ls(here::here("temp", "filtrados"), regexp = "cuci.csv$")

cuci_df <- purrr::map_dfr(cuci_files, ~ vroom::vroom(.x, id = "path",
                                                     col_types = c(path = "c", CO_ANO = "i",
                                                                   CO_CUCI_SEC = "c", CO_PAIS = "c",
                                                                   value = "d"))) %>%
  dplyr::mutate(path = stringr::str_extract(path, "[:upper:]{3}")) %>%
  janitor::clean_names()

isic_files <- fs::dir_ls(here::here("temp", "filtrados"), regexp = "isic.csv$")

isic_df <- purrr::map_dfr(isic_files, ~ vroom::vroom(.x, id = "path",
                                                     col_types = c(path = "c", CO_ANO = "i",
                                                                   CO_ISIC_SECAO = "c", CO_PAIS = "c",
                                                                   value = "d"))) %>%
  dplyr::mutate(path = stringr::str_extract(path, "[:upper:]{3}")) %>%
  janitor::clean_names()

dic_ncm_fator <- dic_ncm_fator %>%
  janitor::clean_names()

dic_ncm_cgce <- dic_ncm_cgce %>%
  janitor::clean_names()

dic_ncm_cuci <- dic_ncm_cuci %>%
  janitor::clean_names()

dic_ncm_isic <- dic_ncm_isic %>%
  janitor::clean_names()

dic_sh6_sh1 <- dic_sh6_sh1 %>%
  janitor::clean_names()

dic_sh6_sh2 <- dic_sh6_sh2 %>%
  janitor::clean_names()

dic_sh6_sh4 <- dic_sh6_sh4 %>%
  janitor::clean_names()

dic_paises <- dic_paises %>%
  janitor::clean_names()

dic_paises_isoa3 <- dic_paises_isoa3 %>%
  janitor::clean_names()

dic_blocos <- vroom::vroom(here::here("data-raw", "dic_blocos.csv")) %>%
  janitor::clean_names()

usethis::use_data(cgce_df, cuci_df, fator_df, isic_df,
                  sh1_df, sh4_df,
                  # sh6_df,
                  dic_sh6_sh1, dic_sh6_sh2,
                  dic_sh6_sh4, dic_paises,
                  dic_paises_isoa3, dic_blocos,
                  dic_ncm_cgce, dic_ncm_cuci,
                  dic_ncm_fator, dic_ncm_isic, overwrite = T)

devtools::install()
