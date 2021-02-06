library(magrittr)

dic_paises_fmi <- vroom::vroom(here::here("data-raw", "dic_paises_fmi_mdic.csv"))

usethis::use_data(dic_paises_fmi)
