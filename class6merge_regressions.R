#Jonah Danziger
#Class 6 MErging and Regressions
rm(list=ls()) # clear the environment
#Load required packages
library(readr)
library(dplyr)
library(sf)
library(tidycensus)
library(tidyverse)
library(purrr)
library(stringr)
library(lubridate)
library(fixest)

temp <- tempfile(fileext = ".zip")
download.file("https://data-seattlecitygis.opendata.arcgis.com/api/download/v1/items/f068ed28bace4d00bd0d91a1c4626068/shapefile?layers=0", temp, mode = "wb")


unzip_dir <- tempdir()
unzip(temp, exdir = unzip_dir)


shp_file <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
#May need to run this line again
seattle_parcels <- st_read(shp_file[1])

sales_url <- "https://aqua.kingcounty.gov/extranet/assessor/Real%20Property%20Sales.zip"
parcel_url <- "https://aqua.kingcounty.gov/extranet/assessor/Residential%20Building.zip"


sales_temp <- tempfile(fileext = ".zip")
parcel_temp <- tempfile(fileext = ".zip")


download.file(sales_url, sales_temp, mode = "wb")
download.file(parcel_url, parcel_temp, mode = "wb")


sales_files <- unzip(sales_temp, list = TRUE)
parcel_files <- unzip(parcel_temp, list = TRUE)


unzip(sales_temp, files = "EXTR_RPSale.csv", exdir = tempdir())
unzip(parcel_temp, files = "EXTR_ResBldg.csv", exdir = tempdir())


sales_df <- read_csv(file.path(tempdir(), "EXTR_RPSale.csv"))
parcel_df <- read_csv(file.path(tempdir(), "EXTR_ResBldg.csv"))


sales_df <- sales_df |> mutate(PIN = paste0(Major,Minor))

zip_url <- "https://aqua.kingcounty.gov/extranet/assessor/Real%20Property%20Appraisal%20History.zip"


temp_zip <- tempfile(fileext = ".zip")
unzip_dir <- tempfile()

options(timeout = 300)


download.file(zip_url, temp_zip, mode = "wb")


unzip(temp_zip, exdir = unzip_dir)


unzipped_files <- list.files(unzip_dir, full.names = TRUE)


csv_file <- unzipped_files[grepl("\\.csv$", unzipped_files, ignore.case = TRUE)]


appraisal_history_df <- read_csv(csv_file)


head(appraisal_history_df)


# Your variable list
vars <- c(
  total_pop     = "B01003_001",
  med_income    = "B19013_001",
  pct_white     = "B02001_002",
  pct_black     = "B02001_003",
  pct_hispanic  = "B03003_003",
  total_25plus  = "B15003_001",
  hs_grad       = "B15003_017",
  bachelors     = "B15003_022",
  masters       = "B15003_023",
  professional  = "B15003_024",
  doctorate     = "B15003_025",
  male          = "B01001_002",
  female        = "B01001_026"
)

years <- 2010:2022


all_demographics <- map_dfr(years, function(y) {
  
  v <- load_variables(y, "acs5", cache = TRUE)
  
  
  available_vars <- vars[vars %in% v$name]
  missing_vars <- setdiff(vars, available_vars)
  
  if (length(missing_vars) > 0) {
    message(glue::glue("Skipping {length(missing_vars)} vars in {y}: {paste(names(missing_vars), collapse = ', ')}"))
  }
  
  
  get_acs(
    geography = "tract",
    variables = available_vars,
    state = "WA",
    county = "King",
    year = y,
    geometry = TRUE,  # geometry slows it down, add later if needed
    survey = "acs5"
  ) %>%
    mutate(year = y)
})


geo_tracts <- get_acs(
  geography = "tract",
  variables = "B01003_001",  # just a dummy variable
  state = "WA",
  county = "King",
  year = 2020,
  geometry = TRUE,
  survey = "acs5"
) %>%
  dplyr::select(GEOID, geometry)

all_demographics <- all_demographics %>%
  dplyr::select(GEOID, year, variable, estimate) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate
  )

st_crs(seattle_parcels)
st_crs(geo_tracts)


if (st_crs(seattle_parcels) != st_crs(geo_tracts)) {
  seattle_parcels <- st_transform(seattle_parcels, st_crs(geo_tracts))
}


parcels_with_tracts <- st_join(seattle_parcels, geo_tracts[, c("GEOID")], left = FALSE)


head(parcels_with_tracts)



sales_parcel_df <- inner_join(sales_df, parcels_with_tracts, by="PIN")
sales_false_df <- left_join(sales_df, parcels_with_tracts, by="PIN")

sales_parcel_df <- inner_join(sales_parcel_df, parcel_df, by=c("Major", "Minor"))


sales_select_df<- sales_parcel_df |>
  dplyr::select(-c(Volume,Page, PlatNbr, PlatType, PlatBlock,
            SellerName,BuyerName, SaleInstrument,
            STACK, SaleWarning, PUB_OWN_TY, MHA_VALUE, LU_FUTURE, 
            ZONE_GEO, ZONELUT, LU_FUTURE, COUNCIL_DI,BldgNbr, 
            StreetName, Fraction, DirectionPrefix, StreetType, DirectionSuffix,
            SqFtHalfFloor, SqFt1stFloor,SqFt2ndFloor,SqFtUpperFloor,
            SqFtTotBasement, SqFtFinBasement, SqFtGarageBasement))|>
  mutate(
    DocumentDate = mdy(DocumentDate),   
    SaleYear = year(DocumentDate))|>           
  mutate(Reno10yr = ifelse(YrRenovated>=SaleYear-10,1,0),
         DayBase = ifelse(DaylightBasement=="Y",1,0)) |>
  filter(SalePrice>0)

  
    



model <- lm(SalePrice ~ SaleYear+SqFtTotLiving+Bedrooms+BathFullCount+DayBase+YrBuilt+Reno10yr+Condition+LAND_SQFT, data = sales_select_df)


summary(model)


sales_demo_parcel_df <- all_demographics |>
  st_drop_geometry() |>  
  rename(SaleYear = year) |>
  inner_join(sales_select_df, by = c("SaleYear", "GEOID")) |>
  mutate(
    pct_white = pct_white / total_pop,
    pct_black = pct_black / total_pop,
    pct_hispanic = pct_hispanic / total_pop,
    male = male / total_pop
  )
#Where did all my data go?



model2 <- lm(SalePrice ~ SaleYear+SqFtTotLiving+Bedrooms+BathFullCount+DayBase+YrBuilt+Reno10yr+Condition+LAND_SQFT+male+pct_white+pct_black+pct_hispanic+med_income, data = sales_demo_parcel_df)


summary(model2)




geojson_url <- "https://services.arcgis.com/ZOyb2t4B0UYuYNYH/arcgis/rest/services/Mandatory_Housing_Affordability_Zoning/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"


mha_zoning_sf <- st_read(geojson_url)


mha_zoning_sf <- mha_zoning_sf %>%
  mutate(EFFECTIVE_DATE = as.POSIXct(EFFECTIVE / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(EFFECTIVE_DATE = as.Date(EFFECTIVE_DATE)) |>
  mutate(mha_year = year(EFFECTIVE_DATE))# optional: drop time portion


unique(mha_zoning_sf$EFFECTIVE_DATE)


sales_tract <- sales_demo_parcel_df |> group_by(SaleYear, GEOID) |>
  summarize(average_sale = mean(SalePrice),
          male=first(male),
          total_pop = first(total_pop),
          pct_white = first(pct_white),
          pct_black = first(pct_black),
          pct_hispanic = first(pct_hispanic),
          med_income = first(med_income)  )


recent_app_hist <-appraisal_history_df |>
  mutate(SaleYear = year(SelectDate), SalePrice = LandVal+ImpsVal) |> filter(SaleYear>=2010) |>
  mutate(PIN=paste0(Major,Minor))|>
  inner_join(parcels_with_tracts, by=c("PIN"))


combined_data <- recent_app_hist |> 
  dplyr::select(SaleYear, GEOID, SalePrice)|>
  rbind(dplyr::select(sales_demo_parcel_df, SaleYear, GEOID,SalePrice))

sales_tract<-combined_data |>
  group_by(SaleYear, GEOID) |>
  summarize(average_sale = mean(SalePrice))


mha_2017 <- mha_zoning_sf |> filter(mha_year == 2017)


mha_2017 <- st_transform(mha_2017, st_crs(geo_tracts))

geo_tracts <- geo_tracts %>%
  mutate(mha_2017 = lengths(st_intersects(., mha_2017)) > 0)

sales_tract_mha <- sales_tract %>%
  left_join(
    geo_tracts %>% st_drop_geometry() %>% select(GEOID, mha_2017),
    by = "GEOID"
  ) %>%
  mutate(mha_dummy = as.integer(mha_2017))  
sales_tract_mha <- sales_tract_mha |> mutate(post2017 = ifelse(SaleYear>2017,1,0))|>
  inner_join(all_demographics|> mutate(SaleYear=year),
             by=c("GEOID", "SaleYear"))|>
  mutate(
    pct_white = pct_white / total_pop,
    pct_black = pct_black / total_pop,
    pct_hispanic = pct_hispanic / total_pop,
    male = male / total_pop,
    PostxMHA = mha_2017 * post2017
  )


fe_model <- feols(
  log(average_sale) ~ mha_dummy + post2017 + PostxMHA +
    pct_white + pct_black + pct_hispanic + male +
    med_income | GEOID + SaleYear,
  data = sales_tract_mha
)
summary(fe_model)
