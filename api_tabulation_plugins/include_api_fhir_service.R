#
# This moudle extracts and tabulates FHIR resources
#

setwd(getwd())
source("common.R", local = TRUE)

# Install or load dependencies
required_packages <- c(
    "fhircrackr",
    "data.table"
)
LoadRequiredPackages(required_packages)

# Define constants
fhir_api_url <- "https://include-api-fhir-service.includedcc.org/"
fhir_api_cookie <- "AWSELBAuthSessionCookie-0=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"

# Definte parameters and headers
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
patient_ids = patients[ , c("Patient ID", "Patient Identifier")]

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
