#
# Global environmental variables
#

source("common.R", local = TRUE)

# Install or load dependencies
required_packages <- c("dplyr")
LoadRequiredPackages(required_packages)

source("./api_tabulation_plugins/include_api_fhir_service.R", local = TRUE)
source("./api_tabulation_plugins/clovoc_api_fhir_service.R", local = TRUE)


dataset <- list(
    "Patient" = bind_rows(
        include_api_fhir_service[["Patient"]],
        clovoc_api_fhir_service[["Patient"]]
    ),
    "Condition" = bind_rows(
        include_api_fhir_service[["Condition"]],
        clovoc_api_fhir_service[["Condition"]]
    ),
    "Specimen" = bind_rows(
        include_api_fhir_service[["Specimen"]],
        clovoc_api_fhir_service[["Specimen"]]
    ),
    "DocumentReference" = bind_rows(
        include_api_fhir_service[["DocumentReference"]],
        clovoc_api_fhir_service[["DocumentReference"]]
    )
)
