#' ETL Economic activity
#'
#' @encoding UTF-8
#' @import dplyr tidyr
#' @importFrom utils lsf.str
#' @return RDATA
#' @export
#'
#' @examples
#' \dontrun{
#' etl_econ_activity()
#' }
#'
#'
etl_econ_activity <- function(){

### Economic activity ###


# This R code provides the extraction and data wrangling of macroeconomic
# variables referring to the Brazilian economic activity.


# Packages ----------------------------------------------------------------


#Install/load packages
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   "tidyverse",
#   "sidrar",
#   "readxl",
#   "lubridate",
#   "GetBCBData",
#   "zoo",
#   "mgsub",
#   "janitor",
#   "rio",
#   "devtools"
#   )



# Parameters --------------------------------------------------------------


# Parameters used in the code to import, download or cleaning data


# Load useful functions
# source("./R/utils.R")


# List of URLs to get data from different sources
url_list <- list(

  # ICVA (spreadsheet data of the Cielo Index)
  url_icva = "https://api.mziq.com/mzfilemanager/v2/d/4d1ebe73-b068-4443-992a-3d72d573238c/163a9070-918c-2f20-035e-b40da951bc9f?origin=2",

  # Vehicle Production (spreadsheet data from ANFAVEA)
  url_anfavea = "http://www.anfavea.com.br/docs/SeriesTemporais_Autoveiculos.xlsm"

)

#https://apicatalog.mziq.com/filemanager/v2/d/4d1ebe73-b068-4443-992a-3d72d573238c/3e864198-0b72-c970-1771-80cd8c338a30?origin=2

# List of parameters to get data from SIDRA/IBGE website
api_sidra <- list(

  # GDP (percent changes)
  api_gdp = "/t/5932/n1/all/v/all/p/all/c11255/90687,90691,90696,90707,93404,93405,93406,93407,93408/d/v6561%201,v6562%201,v6563%201,v6564%201",

  # GDP (R$ million, current prices)
  api_gdp_brl = "/t/1846/n1/all/v/all/p/all/c11255/90687,90691,90696,90707,93404,93405,93406,93407,93408/d/v585%200",

  # PMC (retail trade from IBGE) #DEPRECATED
  api_pmc = "/t/3416/n1/all/v/all/p/all/c11046/90668/d/v564%201,v565%201",

  # PMC ATUALIZADO (Retail trade IBGE)
  api_pmc_ATT = "/t/8185/n1/all/v/11709/p/all/c11046/all/d/v11709%201",

  # PMC (expanded retail trade from IBGE) #DEPRECATED
  api_pmc_expanded = "/t/3417/n1/all/v/all/p/all/c11046/90668/d/v1186%201,v1190%201",

  # PMC (expanded retail trade from IBGE)
  api_pmc_expanded_ATT = "/t/8186/n1/all/v/11709/p/all/c11046/all/d/v11709%201",

  # PMS (Monthly Service Survey from IBGE) #DEPRECATED
  api_pms = "/t/6442/n1/all/v/all/p/all/c11046/90668/d/v8676%201,v8677%201",

  # PMS (Monthly Service Survey from IBGE)
  api_pms_ATT = "/t/8162/n1/all/v/11624/p/all/c11046/all/c12355/107071/d/v11624%201",

  # PIM (Monthly Industrial Survey from IBGE - YoY rate of change) #DEPRECATED
  api_pim = "/t/3653/n1/all/v/3139/p/all/c544/129314,129315,129316,129338/d/v3139%201",

  # PIM (Monthly Industrial Survey from IBGE - YoY rate of change)
  api_pim_ATT = "/t/8159/n1/all/v/11602/p/all/c544/129314,129315,129316,129338/d/v11602%201"

  )


# List of parameters to get data from Central Bank
api_bcb <- list(

  # Installed Capacity Utilization Level (NUCI/FGV)
  api_nuci = c(
    "Utiliza????o da capacidade instalada ??? ind??stria de transforma????o (FGV) - Dados dessazonalizados" = 28561
    ),

  # IBC-Br (economic activity index from BCB)
  api_ibc = c(
    "Brasil"       = 24364,
    "Centro-Oeste" = 25382,
    "Nordeste"     = 25389,
    "Norte"        = 25407,
    "Sul"          = 25403,
    "Sudeste"      = 25395
    )

  )



# Import data -------------------------------------------------------------


# This section performs the import of data from different sources


# GDP growth (rate of change of the quarterly volume index from IBGE)
raw_gdp <- sidrar::get_sidra(api = api_sidra$api_gdp) %>%
  select(
    date     = "Trimestre (C\u00f3digo)",
    variable = "Vari\u00e1vel",
    sector   = "Setores e subsetores",
    measure  = "Unidade de Medida",
    value    = "Valor"
  )


# GDP (values at current prices from IBGE)
raw_gdp_cur_prices <- sidrar::get_sidra(api = api_sidra$api_gdp_brl) %>%
  select(
    date    = "Trimestre (C\u00f3digo)",
    sector  = "Setores e subsetores",
    measure = "Unidade de Medida",
    value   = "Valor"
  )


# PMC (retail trade from IBGE)
raw_pmc <- sidrar::get_sidra(api = api_sidra$api_pmc_ATT) %>%
  select(
    date     = "M??s (C??digo)",
    #variable = "Vari??vel",
    #tipo_cod = "Tipos de ??ndice (C??digo)", # Nova vari??vel incluida em 071022
    variable = "Tipos de ??ndice", # nova variavel incluida em 071022, equivalente ?? variable
    measure  = "Unidade de Medida",
    value    = "Valor"
  )


# PMC (expanded retail trade from IBGE)
raw_pmc_expanded <- sidrar::get_sidra(api = api_sidra$api_pmc_expanded_ATT) %>%
  select(
    date     = "M\u00eas (C\u00f3digo)",
    #variable = "Vari\u00e1vel",
    #tipo_cod = "Tipos de ??ndice (C??digo)", # Nova vari??vel incluida em 071022
    variable = "Tipos de ??ndice", # nova variavel incluida em 071022, equivalente ?? variable
    measure  = "Unidade de Medida",
    value    = "Valor"
  )

# PMS (Monthly Service Survey from IBGE)
raw_pms <- sidrar::get_sidra(api = api_sidra$api_pms_ATT) %>%
  select(
    date     = "M\u00eas (C\u00f3digo)",
    #variable = "Vari\u00e1vel",
    #tipo_cod = "Tipos de ??ndice (C??digo)", # Nova vari??vel incluida em 071022
    variable = "Tipos de ??ndice", # nova variavel incluida em 071022, equivalente ?? measure
    measure  = "Unidade de Medida",
    value    = "Valor"
  )


# PIM (Monthly Industrial Survey from IBGE - YoY rate of change)
raw_pim <- sidrar::get_sidra(api = api_sidra$api_pim_ATT) %>%
  select(
    date     = "M\u00eas (C\u00f3digo)",
    variable = "Se\u00e7\u00f5es e atividades industriais (CNAE 2.0)",
    measure  = "Unidade de Medida",
    value    = "Valor"
  )


# ICVA (Cielo)
raw_icva <- rio::import(
  file   = url_list$url_icva,
  format = "xlsx",
  sheet  = "\u00cdndice Mensal",
  skip   = 6,
  n_max  = 4,
  na     = "-"
  )


# Vehicle Production (ANFAVEA)
raw_vehicle <- rio::import(
  file   = url_list$url_anfavea,
  format = "xlsx",
  skip   = 4
  )


# Installed Capacity Utilization Level (NUCI/FGV)
raw_nuci <- GetBCBData::gbcbd_get_series(
  id          = api_bcb$api_nuci,
  first.date  = "2001-01-01",
  use.memoise = FALSE
  )


# IBC-Br (economic activity index from Central Bank of Brazil)
raw_ibc <- GetBCBData::gbcbd_get_series(
  id          = api_bcb$api_ibc,
  first.date  = "2003-01-27",
  use.memoise = FALSE # ALTERADO P/ NAO DAR ERRO NA BUSCA
  )



# Data wrangling ----------------------------------------------------------


# This section performs data wrangling


# GDP Growth (percent changes)
gdp <- raw_gdp %>%
  mutate(
    variable = recode(
      variable,
      "Taxa trimestral (em rela\u00e7\u00e3o ao mesmo per\u00edodo do ano anterior)" = "Year over Year (%)",
      "Taxa acumulada em quatro trimestres (em rela\u00e7\u00e3o ao mesmo per\u00edodo do ano anterior)" = "Accumulated in 4 Quarters (%)",
      "Taxa acumulada ao longo do ano (em rela\u00e7\u00e3o ao mesmo per\u00edodo do ano anterior)" = "Year to Date (%)",
      "Taxa trimestre contra trimestre imediatamente anterior" = "Quarter over Quarter (%)"
    ),
    sector = recode(
      sector,
      "Agropecu\u00e1ria - total"                        = "Agriculture",
      "Ind\u00fastria - total"                           = "Industry",
      "Servi\u00e7os - total"                            = "Services",
      "PIB a pre\u00e7os de mercado"                     = "GDP",
      "Despesa de consumo das fam\u00edlias"             = "Consumption",
      "Despesa de consumo da administra\u00e7\u00e3o p\u00fablica" = "Government",
      "Forma\u00e7\u00e3o bruta de capital fixo"              = "Investment",
      "Exporta\u00e7\u00e3o de bens e servi\u00e7os"               = "Exports",
      "Importa\u00e7\u00e3o de bens e servi\u00e7os (-)"           = "Imports"
    ),
    date = stringr::str_replace(date, "(\\d{4})0(\\d{1})", "\\1 Q\\2")
  ) %>%
  arrange(date)


# GDP Growth (input for valueBox)
gdp_growth <- gdp %>%
  filter(
    sector   == "GDP" &
    variable == "Accumulated in 4 Quarters (%)"
  ) %>%
  filter(date == last(date)) %>%
  mutate(value = paste0(value, "%"))


# GDP, Current Prices (R$ trillions)
gdp_cur_prices <- raw_gdp_cur_prices %>%
  filter(sector == "PIB a pre\u00e7os de mercado") %>%
  arrange(date) %>%
  slice_tail(n = 4) %>%
  summarise(
    value = paste0("R$ ", round(sum(value) / 1e+06, 2), "b")
    ) %>%
  mutate(
    date  = stringr::str_replace(
      last(raw_gdp_cur_prices$date), "(\\d{4})0(\\d{1})", "\\1 Q\\2"
      )
    )


# Cielo Retail Index
icva <- raw_icva %>%
  dplyr::as_tibble() %>%
  dplyr::select(-c("Setor", "Localidade")) %>%
  tidyr::pivot_longer(cols = -1, names_to = "date") %>%
  dplyr::rename("variable" = 1) %>%
  dplyr::filter(`variable` == "Deflacionado - Com Ajuste Calend\u00e1rio") %>%
  dplyr::select(`date`, `value`) %>%
  dplyr::mutate(
    value = paste0(value * 100, "%"), # convert to % format
    date  = janitor::excel_numeric_to_date(as.numeric(date)),
    ) %>%
  dplyr::filter(date == dplyr::last(date))


# Vehicle Production
vehicle <- raw_vehicle %>%
  janitor::clean_names() %>%
  select(
    date  = x1,
    value = producao_5
    ) %>%
  dplyr::na_if(0) %>%
  tidyr::drop_na() %>%
  mutate(
    date = lubridate::as_date(date),
    value = round(value / 1e3, 1) %>% paste0("k")
    ) %>%
  filter(date == last(date))



# Installed Capacity Utilization Level (NUCI/FGV)
nuci <- raw_nuci %>%
  janitor::clean_names() %>%
  rename_with(~c("date", "value"), 1:2) %>%
  filter(date == last(date))


# Table of GDP measures

# Levels for factor column
levels_gdp <- c(
  "GDP",
  "Agriculture",
  "Industry",
  "Services",
  "Consumption",
  "Government",
  "Investment",
  "Exports",
  "Imports"
  )

# Footnote
footnote_gdp <- gdp %>%
  slice_tail(n = 1) %>%
  mutate(
    note = paste0("Note: updated until ", date, ".")
    ) %>%
  pull(note)


# Create the table
gdp_measures <- gdp %>%
  filter(date == max(date)) %>%
  pivot_wider(
    id_cols     = sector,
    names_from  = variable,
    values_from = value
    ) %>%
  relocate(1, 5, 2, 4, 3) %>%
  mutate(
    sector = factor(sector, levels = levels_gdp),
    ) %>%
  rename_with(~stringr::str_remove(.x, " \\(%\\)"), cols = 2:5) %>%
  arrange(sector)


# GDP growth by sectors
gdp_growth_sector <- gdp %>%
  filter(
    variable == "Accumulated in 4 Quarters (%)",
    sector %in% c(
      "Agriculture",
      "Industry",
      "Services",
      "GDP"
      )
    ) %>%
  select(date, sector, value) %>%
  slice_tail(n = 20)


# IBC-Br growth by region
ibc_growth <- raw_ibc %>%
  select(
    date     = ref.date,
    variable = series.name,
    value
    ) %>%
  group_by(variable) %>%
  mutate(
    yoy =
      (((rolling(value, sum, 12) / rolling(dplyr::lag(value, 12), sum, 12)) - 1) * 100) %>%
      round(2)
    ) %>%
  tidyr::drop_na()


# PMC growth
pmc <- bind_rows(raw_pmc_expanded, raw_pmc) %>%
  mutate(
    date     = lubridate::ym(date),
    variable = recode(
      variable,
      "\u00cdndice de volume de vendas no com\u00e9rcio varejista ampliado"          = "Retail sales volume expanded",
      "\u00cdndice de receita nominal de vendas no com\u00e9rcio varejista ampliado" = "Retail sales revenue expanded",
      "\u00cdndice de volume de vendas no com\u00e9rcio varejista"                   = "Retail sales volume",
      "\u00cdndice de receita nominal de vendas no com\u00e9rcio varejista"          = "Retail sales revenue"
      )
    ) %>%
  tidyr::drop_na()

# PMS growth
pms <- raw_pms %>%
  mutate(
    variable = recode(
      variable,
      "\u00cdndice de receita nominal de servi\u00e7os" = "Revenue",
      "\u00cdndice de volume de servi\u00e7os"          = "Volume"
      ),
    date     = lubridate::ym(date),
    ) %>%
  tidyr::drop_na()


# PIM growth
pim <- raw_pim %>%
  mutate(
    variable = recode(
      variable,
      "1 Ind\u00fastria geral"                                               = "General industry",
      "2 Ind\u00fastrias extrativas"                                         = "Extractive industries",
      "3 Ind\u00fastrias de transforma\u00e7\u00e3o"                                   = "Manufacturing industries",
      "3.29 Fabrica\u00e7\u00e3o de ve\u00edculos automotores, reboques e carrocerias" = "Manufacture of motor vehicles"
      ),
    date = lubridate::ym(date),
  ) %>%
  tidyr::drop_na()




# Save data ---------------------------------------------------------------


if (0L %in% purrr::map_dbl(mget(ls()), length)) {

  stop("Some objects are zero in length.", call. = FALSE)

  } else
    {
    # Aggregate data
    imported_data_eco_activity <- mget(ls(pattern = "raw_|api_|url_"))


    # Remove unnecessary objects
    rm(
      list  = c(lsf.str(), ls(pattern = "raw_|api_|url_")),
      envir = environment()
      )


    # Save RDATA file
    save(
      list  = ls(),
      file  = file.path(file.path("./inst/extdata"), "econ_activity.Rdata"),
      envir = environment()
      )
    }

}
