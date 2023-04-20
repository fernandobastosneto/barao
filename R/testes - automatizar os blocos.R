blocos_paises <- list(
  "G20" = c("África do Sul", "Alemanha", "Arábia Saudita", "Argentina", "Austrália", "Canadá", "China", "Coreia do Sul", "Estados Unidos", "França", "Índia", "Indonésia", "Itália", "Japão", "México", "Reino Unido", "Rússia", "Turquia", "Áustria", "Bélgica", "Bulgária", "Chipre", "Croácia", "Dinamarca", "Eslováquia", "Eslovênia", "Espanha", "Estônia", "Finlândia", "Grécia", "Hungria", "Irlanda", "Itália", "Letônia", "Lituânia", "Luxemburgo", "Malta", "Polônia", "Portugal", "Romênia", "Suécia", "Países Baixos (Holanda)", "Tcheca, República"),
  "Todos os países africanos" = c("África do Sul", "Angola", "Argélia", "Benin", "Botsuana", "Burkina Faso", "Burundi", "Cabo Verde", "Camarões", "Chade", "Comores", "Costa do Marfim","Egito", "Etiópia", "Gabão", "Gâmbia", "Gana", "Guiné Equatorial", "Djibuti", "Guiné-Bissau", "Madagascar", "Lesoto", "Libéria", "Líbia", "Malavi", "mali","Maurício", "Marrocos", "Quênia",  "Mauritânia", "Moçambique", "Namíbia", "Níger", "Nigéria", "Congo", "República Democrática", "República Centro-Africana","Seicheles", "Ruanda", "Saara Ocidental", "São Tomé e Príncipe", "Senegal", "Serra Leoa", "Somália", "Suazilândia", "Sudão", "Sudão do Sul", "Tanzânia", "Togo","Tunísia", "Uganda", "Zâmbia", "Zimbábue"),
  "CPLP" = c("Angola", "Cabo Verde", "Guiné-Bissau", "Guiné Equatorial", "Moçambique", "Portugal", "São Tomé e Príncipe", "Timor Leste"),
  "Todos os países Americanos" = c("Canadá", "Estados Unidos", "México", "Antígua e Barbuda", "Bahamas", "Barbados", "Belize", "Costa Rica", "Cuba", "Dominica", "El Salvador", "Granada", "Guatemala", "Haiti", "Honduras", "Jamaica", "Nicarágua", "Panamá", "República Dominicana", "São Cristóvão e Névis", "São Vicente e Granadinas", "Trinidad e Tobago", "Argentina", "Bolívia", "Chile", "Colômbia", "Equador", "Guiana", "Paraguai", "Peru", "Suriname", "Uruguai", "Venezuela", "Virgens, Ilhas (Americanas)", "Sint Maarten", "Cayman, Ilhas", "Martinica", "Zona do Canal do Panamá", "Anguilla", "Montserrat", "Santa Lúcia", "São Bartolomeu", "Aruba", "Guadalupe", "Bermudas", "Virgens, Ilhas (Britânicas)", "Nicarágua", "Curaçao", "Porto Rico", "Bonaire", "Saint Eustatius e Saba", "São Martinho, Ilha de (parte francesa)", "Turcas e Caicos, Ilhas", "Falkland (Malvinas)", "Guiana Francesa"),
  "Países Americanos (exceto EUA e Canadá)" =  c( "México", "Antígua e Barbuda", "Bahamas", "Barbados", "Belize", "Costa Rica", "Cuba", "Dominica", "El Salvador", "Granada", "Guatemala", "Haiti", "Honduras", "Jamaica", "Nicarágua", "Panamá", "República Dominicana", "São Cristóvão e Névis", "São Vicente e Granadinas", "Trinidad e Tobago", "Argentina", "Bolívia", "Chile", "Colômbia", "Equador", "Guiana", "Paraguai", "Peru", "Suriname", "Uruguai", "Venezuela", "Virgens, Ilhas (Americanas)", "Sint Maarten", "Cayman, Ilhas", "Martinica", "Zona do Canal do Panamá", "Anguilla", "Montserrat", "Santa Lúcia", "São Bartolomeu", "Aruba", "Guadalupe", "Bermudas", "Virgens, Ilhas (Britânicas)", "Nicarágua", "Curaçao", "Porto Rico", "Bonaire", "Saint Eustatius e Saba", "São Martinho, Ilha de (parte francesa)", "Turcas e Caicos, Ilhas", "Falkland (Malvinas)", "Guiana Francesa"),
  "America do sul" =  c("Colômbia", "Venezuela", "Equador", "Guiana", "Suriname", "Peru", "Bolivia", "Chile", "Argentina", "Paraguai", "Uruguai"),
  "mercosul (sem Venezuela)" = c("Argentina", "Uruguai", "Paraguai"),
  "mercosul (com Venezuela)" = c("Argentina", "Uruguai", "Paraguai", "Venezuela"),
  "OCDE" = c("Alemanha", "Austrália", "Áustria", "Bélgica", "Canadá", "Chile", "Colômbia", "Coréia do Sul", "Costa Rica", "Dinamarca", "Eslováquia", "Eslovênia", "Espanha", "Estados Unidos", "Estônia", "Finlândia", "França", "Grécia", "Hungria", "Irlanda", "Islândia", "Israel", "Itália", "Japão", "Letônia", "Lituânia", "Luxemburgo", "México", "Noruega", "Nova Zelândia","Países Baixos (Holanda)", "Polônia", "Portugal", "Reino Unido", "Tcheca, República", "Suécia", "Suíça" ,"Turquia"),
  "União Europeia" = c("Bélgica","Bulgária" , "Chipre", "Dinamarca", "Alemanha", "Estônia", "Finlândia", "França", "Grécia", "Hungria", "Irlanda", "Itália", "Croácia", "Letônia", "Lituânia", "Luxemburgo", "Malta", "Países Baixos (Holanda)", "Áustria", "Polônia", "Portugal", "Romênia", "Eslovênia", "Eslováquia", "Espanha", "Tcheca, República", "Suécia"),
  "EFTA" = c("Liechtenstein", "Noruega", "Islândia", "Suíça"),
  "ASEAN" = c("Brunei","Camboja","Cingapura","Filipinas", "Indonésia", "Laos", "Malásia", "Mianmar", "Tailândia", "Vietnã"),
  "Aliança do Pacífico" = c("Colômbia", "Costa Rica", "Chile", "Equador", "El Salvador", "Honduras", "Nicarágua", "México")

)


bloco_nome <- c("G20", "Todos os países africanos", "CPLP", "Todos os países Americanos", "Países Americanos (exceto EUA e Canadá)", "America do sul",
                "mercosul (sem Venezuela)", "mercosul (com Venezuela)", "OCDE", "União Europeia", "EFTA", "ASEAN", "Aliança do Pacífico" )


# Loop para gerar um relatório para cada bloco de países
aaa <- for (bloco_nome in names(blocos_paises)) {
  # Atribui a variável bloco_paises com os países específicos para o bloco atual
  bloco_paises <- blocos_paises[[bloco_nome]]

  # Renderiza o relatório para o bloco atual
  rmarkdown::render(system.file("rmd", "comerciobr_report_pais_bloco.Rmd", package = "barao2"),
                    params = list(
                      bloco_paises = bloco_paises,
                      title = paste0("Brasil-", bloco_paises, " , Dados Comerciais")),
                      output_dir = here::here("data/relatorios_comerciobr_blocos"),
                      output_file = paste0("comerciobr_", bloco_nome))
}


