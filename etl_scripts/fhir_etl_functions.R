library(data.table)
library(dplyr)
library(tidyr)

ParsePatientID <- function(x) {
  return(unlist(strsplit(x, "/"))[2])
}

ReplaceNA <- function(df) {
  return(replace(df, is.na(df), ""))
}

grab_fhir <- function(resource_name,
                      desc_cols,
                      url,
                      # cookie,
                      sep = " ~ ",
                      brackets = c("<<", ">>"),
                      format = "compact",
                      params = NULL) {

  # Build a request URL for resource
  if (is.null(params)) {
    resource_request <-
      fhircrackr::fhir_url(url = fhir_api_url,
                           resource = resource_name)
  } else {
    resource_request <-
      fhircrackr::fhir_url(url = fhir_api_url,
                           resource = resource_name,
                           parameters = params)
  }

  # Download bundles of resources
  resource_bundles <-
    fhircrackr::fhir_search(request = resource_request,
                            # add_headers = cookie,
                            verbose = 2)

  # Define a resource table description
  resource_description <-
    fhircrackr::fhir_table_description(resource = resource_name,
                                       cols = desc_cols,
                                       sep = sep,
                                       brackets = brackets,
                                       rm_empty_cols = FALSE,
                                       format = format)

  # Flatten resources
  data <-
    fhircrackr::fhir_crack(bundles = resource_bundles,
                           design = resource_description,
                           verbose = 2)

  # Return dataset
  data
}

sparc_phenotype <- function (phenotype_id = NULL,
                             scicrunch_key = Sys.getenv("SCICRUNCH_KEY")) {
  stopifnot("ERROR! Must provide a phenotype ID." = !is.null(phenotype_id),
            "ERROR! Must provide an API key." = !is.na(scicrunch_key))
  base_url <- "https://scicrunch.org/api/1/sparc-scigraph/dynamic/prod/sparc/anatomyPhenotypes/"
  scicrunch_url <- paste0(base_url, phenotype_id, ".json")
  sparc_table <- httr::GET(scicrunch_url,
                           query = list(api_key = scicrunch_key)) |>
    httr::content(as = "text") |>
    jsonlite::fromJSON() |>
    getElement("nodes") |>
    (\(x) cbind("id" = x$id, "lbl" = x$lbl, x$meta))()

  return(sparc_table)
}
