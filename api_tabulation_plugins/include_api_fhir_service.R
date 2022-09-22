#
# This moudle extracts and tabulates FHIR resources from INCLUDE FHIR API
#

setwd(getwd())
source("common.R", local = TRUE)

# Install or load dependencies
required_packages <- c(
    "dotenv",
    "fhircrackr",
    "data.table"
)
LoadRequiredPackages(required_packages)

# Load environmental variables
load_dot_env()

# Get FHIR credentails
fhir_api_url <- "https://include-api-fhir-service.includedcc.org/"
fhir_api_cookie <- Sys.getenv("INCLUDE_FHIR_API_COOKIE")

# Define parameters and headers
tags <- c("DS360-CHD", "DS-COG-ALL", "DS-PCGC")
tags <- paste(tags, collapse = ",")
cookies <- c(Cookie = fhir_api_cookie)


# /Group
# Build a request URL for Group
group_request <- fhir_url(
    url = fhir_api_url,
    resource = "Group",
    parameters = c("_count" = 100, "_tag" = tags)
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
        "Group Identifier System" = "identifier/system",
        "Group Identifier Value" = "identifier/value",
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
    groups,
    columns = c(
        "Group Identifier System",
        "Group Identifier Value"
    ),
    sep = " ~ ",
    brackets = c("<<", ">>"),
    all_columns = TRUE
)
groups <- fhir_melt(
    groups, columns = c("Patient ID"),
    sep = " ~ ",
    brackets = c("<<", ">>"),
    all_columns = TRUE
)

# Remove indices
groups <- fhir_rm_indices(groups, brackets = c("<<", ">>"))

# Replace NA with empty string
groups <- ReplaceNA(groups)

# Filter rows
group_filter <- "https://kf-api-dataservice.kidsfirstdrc.org/families/"
groups <- groups[groups$"Group Identifier System" != group_filter, ]

# Extract patient IDs
groups$"Patient ID" <- unlist(lapply(groups$"Patient ID", ParsePatientID))

# Drop columns
groups <- within(groups, rm("Group Identifier System", "resource_identifier"))

# Change column names
setnames(
    groups, old = c("Group Identifier Value"), new = c("Group Identifier")
)


# /Patient
# Build a request URL for Patient
patient_request <- fhir_url(
    url = fhir_api_url,
    resource = "Patient",
    parameters = c("_count" = 100, "_tag" = tags)
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
        "Patient Identifier System" = "identifier/system",
        "Patient Identifier Value" = "identifier/value",
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

# Drop columns
patients <- within(
    patients,
    rm("<<1.1>>Patient Identifier System", "<<1.1>>Patient Identifier Value")
)

# Change column names
setnames(
    patients,
    old = c(
        "<<1>>Patient ID",
        "<<2.1>>Patient Identifier Value",
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
    parameters = c("_count" = 100, "_tag" = tags)
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
    url = fhir_api_url,
    resource = "Specimen",
    parameters = c("_count" = 100, "_tag" = tags)
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
        "Specimen Identifier System" = "identifier/system",
        "Specimen Identifier Value" = "identifier/value",
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
    format = "wide"
)

# Flatten Specimen resources
specimens <- fhir_crack(
    bundles = specimen_bundles, design = specimen_description, verbose = 2
)

# Change column names
setnames(
    specimens,
    old = c(
        "<<1.1>>Patient ID",
        "<<1.1>>Specimen Identifier Value",
        "<<1>>Specimen Status",
        "<<1.1>>Specimen Type Name",
        "<<1.1.1>>Specimen Type Ontology URI",
        "<<1.1.1>>Specimen Type Code"
    ),
    new = c(
        "Patient ID",
        "Specimen Identifier",
        "Specimen Status",
        "Specimen Type Name",
        "Specimen Type Ontology URI",
        "Specimen Type Code"
    )
)

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
specimens <- within(
    specimens,
    rm(
        "Patient ID",
        "<<2.1>>Specimen Identifier System",
        "<<2.1>>Specimen Identifier Value",
        "<<3.1>>Specimen Identifier Value"
    )
)

# Replace NA with empty string
specimens <- ReplaceNA(specimens)


# /DocumentReference
# Build a request URL for DocumentReference
document_reference_request <- fhir_url(
    url = fhir_api_url,
    resource = "DocumentReference",
    parameters = c("_count" = 100, "_tag" = tags)
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
        "Experiment Strategy ~ Data Category" = "category/coding/display",
        "URL" = "content/attachment/url"
    ),
    sep = " ~ ",
    brackets = c("<<", ">>"),
    rm_empty_cols = FALSE,
    format = "wide"
)

# Flatten DocumentReference resources
document_references <- fhir_crack(
    bundles = document_reference_bundles,
    design = document_reference_description,
    verbose = 2
)

# Change column names
setnames(
    document_references,
    old = c(
        "<<1.1>>Patient ID",
        "<<1>>DocumentReference Status",
        "<<1>>Document Status",
        "<<1.1>>Document Type",
        "<<1.1.1>>Experiment Strategy ~ Data Category",
        "<<2.1.1>>Experiment Strategy ~ Data Category",
        "<<1.1.1>>URL"
    ),
    new = c(
        "Patient ID",
        "DocumentReference Status",
        "Document Status",
        "Document Type",
        "Experiment Strategy",
        "Data Category",
        "URL"
    )
)

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

# Replace NA with empty string
document_references <- ReplaceNA(document_references)


# Cache data frames
include_api_fhir_service <- list(
    "Patient" = patients,
    "Condition" = conditions,
    "Specimen" = specimens,
    "DocumentReference" = document_references
)
