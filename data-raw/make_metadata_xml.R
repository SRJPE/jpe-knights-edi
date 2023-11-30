library(EMLaide)
library(tidyverse)
library(readxl)
library(EML)

datatable_metadata <-
  dplyr::tibble(filepath = c("data/knights_trap_edi.csv",
                             "data/knights_catch_edi.csv",
                             "data/knights_recapture_edi.csv",
                             "data/knights_release_fish_edi.csv",
                             "data/knights_release_edi.csv"),
                attribute_info = c("data-raw/metadata/trap_metadata.xlsx",
                                   "data-raw/metadata/catch_metadata.xlsx",
                                   "data-raw/metadata/recaptures_metadata.xlsx",
                                   "data-raw/metadata/release_fish_metadata.xlsx",
                                   "data-raw/metadata/release_metadata.xlsx"),
                datatable_description = c("Daily trap operations",
                                          "Daily catch",
                                          "Recaptured catch",
                                          "Release fish measurements",
                                          "Release trial summary"),
                datatable_url = paste0("https://raw.githubusercontent.com/SRJPE/jpe-knights-edi/main/data/",
                                       c("knights_trap_edi.csv",
                                         "knights_catch_edi.csv",
                                         "knights_recapture_edi.csv",
                                         "knights_release_fish_edi.csv",
                                         "knights_release_edi.csv")))
# save cleaned data to `data/`
excel_path <- "data-raw/metadata/KDL_project_metadata.xlsx"
sheets <- readxl::excel_sheets(excel_path)
metadata <- lapply(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
names(metadata) <- sheets

abstract_docx <- "data-raw/metadata/abstract.docx"
methods_docx <- "data-raw/metadata/methods.md"

#edi_number <- reserve_edi_id(user_id = Sys.getenv("edi_user_id"), password = Sys.getenv("edi_password"))
# edi_number <- "edi.1501.1" # reserved 9-20-2023 under srjpe account

vl <- readr::read_csv("data-raw/version_log.csv", col_types = c('c', "D"))
previous_edi_number <- tail(vl['edi_version'], n=1)
previous_edi_number <- previous_edi_number$edi_version
previous_edi_ver <- as.numeric(stringr::str_extract(previous_edi_number, "[^.]*$"))
current_edi_ver <- as.character(previous_edi_ver + 1)
previous_edi_id_list <- stringr::str_split(previous_edi_number, "\\.")
previous_edi_id <- sapply(previous_edi_id_list, '[[', 2)
current_edi_number <- paste0("edi.", previous_edi_id, ".", current_edi_ver)

new_row <- data.frame(
    edi_version = current_edi_number,
    date = as.character(Sys.Date())
)
vl <- bind_rows(vl, new_row)
write.csv(vl, "data-raw/version_log.csv", row.names=FALSE)

dataset <- list() %>%
  add_pub_date() %>%
  add_title(metadata$title) %>%
  add_personnel(metadata$personnel) %>%
  add_keyword_set(metadata$keyword_set) %>%
  add_abstract(abstract_docx) %>%
  add_license(metadata$license) %>%
  add_method(methods_docx) %>%
  add_maintenance(metadata$maintenance) %>%
  add_project(metadata$funding) %>%
  add_coverage(metadata$coverage, metadata$taxonomic_coverage) %>%
  add_datatable(datatable_metadata)

# GO through and check on all units
custom_units <- data.frame(id = c("count of fish", "Nephelometric Turbidity Units (NTU)", "day", "number of rotations",
                                  "revolutions per minute", "microSiemens per centimeter"),
                           unitType = c("dimensionless", "dimensionless", "dimensionless",
                                        "dimensionless", "dimensionless", "dimensionless"),
                           parentSI = c(NA, NA, NA, NA, NA, NA),
                           multiplierToSI = c(NA, NA, NA, NA, NA, NA),
                           description = c("Count of fish",
                                           "Unit of measurement for turbidity",
                                           "Number of days",
                                           "Total rotations",
                                           "Number of revolutions per minute",
                                           "Unit of measurement for conductivity"))

unitList <- EML::set_unitList(custom_units)

eml <- list(packageId = edi_number,
            system = "EDI",
            access = add_access(),
            dataset = dataset,
            additionalMetadata = list(metadata = list(unitList = unitList))
)

EML::write_eml(eml, paste0(edi_number, ".xml"))
message("EML Metadata generated")

EMLaide::update_edi_package(user_id = secret_edi_username,
                            password = secret_edi_password,
                            eml_file_path = paste0(getwd(), "/", current_edi_number, ".xml"),
                            existing_package_identifier = paste0("edi.",previous_edi_id, ".", previous_edi_ver, ".xml"),
                            environment = "staging")

# EML::eml_validate(paste0(edi_number, ".xml"))

# EMLaide::evaluate_edi_package(Sys.getenv("edi_user_id"), Sys.getenv("edi_password"), paste0(edi_number, ".xml"))
# report_df |> filter(Status == "error")
# EMLaide::upload_edi_package(Sys.getenv("edi_user_id"), Sys.getenv("edi_password"), paste0(edi_number, ".xml"))

# doc <- read_xml("edi.1243.1.xml")
# edi_number<- data.frame(edi_number = doc %>% xml_attr("packageId"))
# update_number <- edi_number %>%
#   separate(edi_number, c("edi","package","version"), "\\.") %>%
#   mutate(version = as.numeric(version) + 1)
# edi_number <- paste0(update_number$edi, ".", update_number$package, ".", update_number$version)

# preview_coverage <- function(dataset) {
#   coords <- dataset$coverage$geographicCoverage$boundingCoordinates
#   north <- coords$northBoundingCoordinate
#   south <- coords$southBoundingCoordinate
#   east <- coords$eastBoundingCoordinate
#   west <- coords$westBoundingCoordinate
#
#   leaflet::leaflet() |>
#     leaflet::addTiles() |>
#     leaflet::addRectangles(
#       lng1 = west, lat1 = south,
#       lng2 = east, lat2 = north,
#       fillColor = "blue"
#     )
# }

# preview_coverage(dataset)
