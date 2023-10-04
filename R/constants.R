# Missing values
MISSING_EXPLICIT <- "Unk" # missing value code for values that are explicitily indicated as missing from data source e.g. "X" in grade
MISSING_IMPLICIT <- "N/A" # missing because of some "not-applicable" condition
MISSING_UNK <- "Unk" # missing because values was not found (e.g. in data files) but the value must exist somewhere.
MISSING_NOT_FOUND_IN_DATA_FILE <- "" # data point not mentioned in data file.
MISSING_BIOMARKER_EXPLICIT <- MISSING_UNK # missing value code for values that are explicitily indicated as missing from data source e.g. "X" score sheet
MISSING_BIOMARKER_NA <- MISSING_EXPLICIT # biomarker value missing because of some "not-applicable" condition
MISSING_BIOMARKER_NOT_FOUND_IN_DATA_FILE <- "" # data point not mentioned in data file.
ALL_MISSING <- unique(
  c(
    MISSING_EXPLICIT,
    MISSING_IMPLICIT,
    MISSING_NOT_FOUND_IN_DATA_FILE,
    MISSING_UNK,
    MISSING_BIOMARKER_EXPLICIT,
    MISSING_BIOMARKER_NOT_FOUND_IN_DATA_FILE
  )
)

# Attributes
ATTR_VAR_NAME <- "var_name"
ATTR_VAR_DATA_TYPE <- "var_data_type"
ATTR_VAR_DATA_TYPE_FACTOR <- "factor"
ATTR_VAR_DATA_TYPE_NUMERIC <- "numeric"
ATTR_VAR_DATA_TYPE_DATE <- "date"
ATTR_VAR_DATA_TYPE_CHARACTER <- "character"
ATTR_VAR_DATA_CAT <- "var_data_category"
ATTR_VAR_DATE_FORMAT <- "var_date_format"
ATTR_VAR_VALUE_LABELS <- "var_value_labels"
