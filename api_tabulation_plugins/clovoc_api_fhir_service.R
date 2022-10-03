#
# This moudle extracts and tabulates FHIR resources from CLOVoc FHIR API
#

setwd(getwd())
source("common.R", local = TRUE)

# Install or load dependencies
required_packages <- c(
    # "dotenv",
    # "fhircrackr",
    "remotes",
    "data.table"
)
LoadRequiredPackages(required_packages)

remotes::install_github(repo = "https://github.com/POLAR-fhiR/fhircrackr", ref = "master")
library(fhircrackr)

# Load environmental variables
# load_dot_env()

# Get FHIR credentails
fhir_api_url <- "https://clovoc-api-fhir-service-dev.kf-strides.org/"
fhir_api_cookie <- Sys.getenv("CLOVOC_FHIR_API_COOKIE")

# Define headers
cookies <- c(Cookie = fhir_api_cookie)


# /Group
# Build a request URL for Group
group_request <- fhir_url(
    url = fhir_api_url, resource = "Group", parameters = c("_count" = "100")
)

# Download bundles of Group resources
group_bundles <- fhir_search(
    request = group_request, add_headers = cookies, verbose = 2
)

# Define a Group table description
group_description <- fhir_table_description(
    resource = "Group",
    cols = c(
        "ResearchStudy Identifier" = "meta/tag/code",
        "Group Identifier" = "identifier/value",
        "Patient ID" = "member/entity/reference"
    ),
    sep = " ~ ",
    brackets = c("<<", ">>"),
    rm_empty_cols = FALSE,
    format = "compact"
)

# Flatten Group resources
groups <- fhir_crack(
    bundles = group_bundles, design = group_description, verbose = 2
)

# Melt columns
groups <- fhir_melt(
    groups, columns = c("Patient ID"),
    sep = " ~ ",
    brackets = c("<<", ">>"),
    all_columns = TRUE
)

# Remove indices
groups <- fhir_rm_indices(groups, brackets = c("<<", ">>"))

# Extract patient IDs
groups$"Patient ID" <- unlist(lapply(groups$"Patient ID", ParsePatientID))

# Drop columns
groups <- within(groups, rm("resource_identifier"))

# Replace NA with empty string
groups <- ReplaceNA(groups)


# /Patient
# Build a request URL for Patient
patient_request <- fhir_url(
    url = fhir_api_url, resource = "Patient", parameters = c("_count" = "100")
)

# Download bundles of Patient resources
patient_bundles <- fhir_search(
    request = patient_request, add_headers = cookies, verbose = 2
)

# Define a Patient table description
patient_description <- fhir_table_description(
    resource = "Patient",
    cols = c(
        "Patient ID" = "id",
        "Patient Identifier" = "identifier/value",
        "Race ~ Ethnicity" = "extension/extension/valueString",
        "Gender" = "gender"
    ),
    sep = " ~ ",
    brackets = c("<<", ">>"),
    rm_empty_cols = FALSE,
    format = "wide"
)

# Flatten Patient resources
patients <- fhir_crack(
    bundles = patient_bundles, design = patient_description, verbose = 2
)

# Change column names
setnames(
    patients,
    old = c(
        "<<1>>Patient ID",
        "<<1.1>>Patient Identifier",
        "<<1.1.1>>Race ~ Ethnicity",
        "<<2.1.1>>Race ~ Ethnicity",
        "<<1>>Gender"
    ),
    new = c("Patient ID", "Patient Identifier", "Race", "Ethnicity", "Gender")
)

# Replace NA with empty string
patients <- ReplaceNA(patients)

# Right-join groups and patients on Patient ID
patients <- merge(
    groups, patients, by.x = "Patient ID", by.y = "Patient ID", all.y = TRUE
)

# Cache Patient IDs and Identifiers
patient_ids <- patients[, c("Patient ID", "Patient Identifier")]

# Drop columns
patients <- within(patients, rm("Patient ID"))


# /Condition
# Build a request URL for Condition
condition_request <- fhir_url(
    url = fhir_api_url,
    resource = "Condition",
    parameters = c("_count" = "100")
)

# Download bundles of Condition resources
condition_bundles <- fhir_search(
    request = condition_request, add_headers = cookies, verbose = 2
)

# Define a Condition table description
condition_description <- fhir_table_description(
    resource = "Condition",
    cols = c(
        "Patient ID" = "subject/reference",
        "Clinical Status" = "clinicalStatus/text",
        "Verification Status" = "verificationStatus/text",
        "Condition Name" = "code/text",
        "Condition Ontology URI" = "code/coding/system",
        "Condition Code" = "code/coding/code",
        "Body Site Name" = "bodySite/text",
        "Body Site Ontology URI" = "bodySite/coding/system",
        "Body Site Code" = "bodySite/coding/code"
    ),
    sep = " ~ ",
    brackets = c("<<", ">>"),
    rm_empty_cols = FALSE,
    format = "compact"
)

# Flatten Condition resources
conditions <- fhir_crack(
    bundles = condition_bundles, design = condition_description, verbose = 2
)

# Remove indices
conditions <- fhir_rm_indices(conditions, brackets = c("<<", ">>"))

# Extract patient IDs
conditions$"Patient ID" <- unlist(
    lapply(conditions$"Patient ID", ParsePatientID)
)

# Right-join patient_ids and conditions on Patient ID
conditions <- merge(
    patient_ids,
    conditions,
    by.x = "Patient ID",
    by.y = "Patient ID",
    all.y = TRUE
)

# Drop columns
conditions <- within(conditions, rm("Patient ID"))

# Replace NA with empty string
conditions <- ReplaceNA(conditions)


# /Specimen
# Build a request URL for Specimen
specimen_request <- fhir_url(
    url = fhir_api_url, resource = "Specimen", parameters = c("_count" = "100")
)

# Download bundles of Specimen resources
specimen_bundles <- fhir_search(
    request = specimen_request, add_headers = cookies, verbose = 2
)

# Define a table description
specimen_description <- fhir_table_description(
    resource = "Specimen",
    cols = c(
        "Patient ID" = "subject/reference",
        "Specimen Identifier" = "identifier/value",
        "Specimen Status" = "status",
        "Specimen Type Name" = "type/text",
        "Specimen Type Ontology URI" = "type/coding/system",
        "Specimen Type Code" = "type/coding/code",
        "Body Site Name" = "collection/bodySite/text",
        "Body Site Ontology URI" = "collection/bodySite/coding/system",
        "Body Site Code" = "collection/bodySite/coding/code"
    ),
    sep = " ~ ",
    brackets = c("<<", ">>"),
    rm_empty_cols = FALSE,
    format = "compact"
)

# Flatten Specimen resources
specimens <- fhir_crack(
    bundles = specimen_bundles, design = specimen_description, verbose = 2
)

# Remove indices
specimens <- fhir_rm_indices(specimens, brackets = c("<<", ">>"))

# Replace NA with empty string
specimens <- ReplaceNA(specimens)

# Extract patient IDs
specimens$"Patient ID" <- unlist(
    lapply(specimens$"Patient ID", ParsePatientID)
)

# Right-join patient_ids and specimens on Patient ID
specimens <- merge(
    patient_ids,
    specimens,
    by.x = "Patient ID",
    by.y = "Patient ID",
    all.y = TRUE
)

# Drop columns
specimens <- within(specimens, rm("Patient ID"))


# /DocumentReference
# Build a request URL for DocumentReference
document_reference_request <- fhir_url(
    url = fhir_api_url,
    resource = "DocumentReference",
    parameters = c("_count" = "100")
)

# Download bundles of DocumentReference resources
document_reference_bundles <- fhir_search(
    request = document_reference_request, add_headers = cookies, verbose = 2
)

# Define a table description
document_reference_description <- fhir_table_description(
    resource = "DocumentReference",
    cols = c(
        "Patient ID" = "subject/reference",
        "DocumentReference Status" = "status",
        "Document Status" = "docStatus",
        "Document Type" = "type/text",
        "Experiment Strategy " = "_experiment_strategy",
        "Data Category" = "category/coding/display",
        "URL" = "content/attachment/url"
    ),
    sep = " ~ ",
    brackets = c("<<", ">>"),
    rm_empty_cols = FALSE,
    format = "compact"
)

# Flatten DocumentReference resources
document_references <- fhir_crack(
    bundles = document_reference_bundles,
    design = document_reference_description,
    verbose = 2
)

# Remove indices
document_references <- fhir_rm_indices(
    document_references, brackets = c("<<", ">>")
)

# Replace NA with empty string
document_references <- ReplaceNA(document_references)

# Extract patient IDs
document_references$"Patient ID" <- unlist(
    lapply(document_references$"Patient ID", ParsePatientID)
)

# Right-join patient_ids and document_references on Patient ID
document_references <- merge(
    patient_ids,
    document_references,
    by.x = "Patient ID",
    by.y = "Patient ID",
    all.y = TRUE
)

# Drop columns
document_references <- within(document_references, rm("Patient ID"))


# Cache data frames
clovoc_api_fhir_service <- list(
    "Patient" = patients,
    "Condition" = conditions,
    "Specimen" = specimens,
    "DocumentReference" = document_references
)
