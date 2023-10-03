#' Change variables back to their native types
#'
#' Takes data package objects and converts variables back to native types
#' (e.g. numeric, character, factor, Date).
#'
#' @param x dataset from data packages
#' @return dataset with variables converted back to native types
#' @author Derek Chiu
#' @export
to_native_type <- function(x) {
  UseMethod("to_native_type", x)
}

#' @rdname to_native_type
#' @export
to_native_type.data.frame <- function(x) {
  result <- x
  for (var.name in names(result)) {
    result[, var.name] <- to_native_type(result[, var.name])
  }
  return(result)
}

#' @rdname to_native_type
#' @export
to_native_type.bccaEndometrial <- function(x) {
  attr.names <- names(attributes(x))
  if (!ATTR_VAR_DATA_TYPE %in% attr.names) {
    return(x) # cannot go further since no data type specified
  }
  result <- x
  result [result%in%ALL_MISSING] <- NA # set all missing values to NA
  data.type <- attr(result,ATTR_VAR_DATA_TYPE)
  if (data.type == ATTR_VAR_DATA_TYPE_FACTOR) {
    levels <- unique(attr(result ,ATTR_VAR_VALUE_LABELS)$label)
    result <- droplevels(factor(
      attr(result ,ATTR_VAR_VALUE_LABELS)$label[match(result ,attr(result ,ATTR_VAR_VALUE_LABELS)$value)],
      levels=levels[!is.na(levels)])
    )
  } else if (data.type == ATTR_VAR_DATA_TYPE_NUMERIC) {
    result <- as.numeric(result)
  } else if (data.type == ATTR_VAR_DATA_TYPE_DATE) {
    date_format <- "%Y-%m-%d"
    if  (!ATTR_VAR_DATE_FORMAT %in% attr.names) {
      warning("unspecified date format for",attr(x,ATTR_VAR_NAME),"; using default format:",date_format,"\n")
    } else {
      date_format <- attr(x,ATTR_VAR_DATE_FORMAT)
    }
    result <- sapply(result,as.Date,format=date_format)
  }
  for (attr.name in attr.names[attr.names!="class"]) {
    # do not copy class attribute!!!
    attr(result,attr.name) <- attr(x,attr.name)
  }
  return(result)
}

#' @rdname to_native_type
#' @export
to_native_type.deEndometrial <- function (x) {
  attr.names <- names(attributes(x))
  if (!ATTR_VAR_DATA_TYPE %in% attr.names) {
    return(x)
  }
  result <- x
  result[result %in% ALL_MISSING] <- NA
  data.type <- attr(result, ATTR_VAR_DATA_TYPE)
  if (data.type == ATTR_VAR_DATA_TYPE_FACTOR) {
    result <- factor(attr(result, ATTR_VAR_VALUE_LABELS)$label[match(result, attr(result, ATTR_VAR_VALUE_LABELS)$value)],
                     levels = unique(attr(result, ATTR_VAR_VALUE_LABELS)$label))
  }
  else if (data.type == ATTR_VAR_DATA_TYPE_NUMERIC) {
    result <- as.numeric(result)
  }
  else if (data.type == ATTR_VAR_DATA_TYPE_DATE) {
    date_format <- "%Y-%m-%d"
    if (!ATTR_VAR_DATE_FORMAT %in% attr.names) {
      warning("unspecified date format for", attr(x, ATTR_VAR_NAME),
              "; using default format:", date_format, "\n")
    }
    else {
      date_format <- attr(x, ATTR_VAR_DATE_FORMAT)
    }
    result <- sapply(result, as.Date, format = date_format)
  }
  for (attr.name in attr.names[attr.names != "class"]) {
    attr(result, attr.name) <- attr(x, attr.name)
  }
  return(result)
}

#' @rdname to_native_type
#' @export
to_native_type.caEndometrial <- function (x) {
  attr.names <- names(attributes(x))
  if (!ATTR_VAR_DATA_TYPE %in% attr.names) {
    return(x)
  }
  result <- x
  result[result %in% ALL_MISSING] <- NA
  data.type <- attr(result, ATTR_VAR_DATA_TYPE)
  if (data.type == ATTR_VAR_DATA_TYPE_FACTOR) {
    levels <- unique(attr(result, ATTR_VAR_VALUE_LABELS)$label)
    result <- factor(attr(result, ATTR_VAR_VALUE_LABELS)$label[match(result, attr(result, ATTR_VAR_VALUE_LABELS)$value)],
                     levels = levels[!is.na(levels)])
  }
  else if (data.type == ATTR_VAR_DATA_TYPE_NUMERIC) {
    result[result == "not tested"] <- NA
    result <- as.numeric(result)
  }
  else if (data.type == ATTR_VAR_DATA_TYPE_DATE) {
    date_format <- "%Y-%m-%d"
    if (!ATTR_VAR_DATE_FORMAT %in% attr.names) {
      warning("unspecified date format for", attr(x, ATTR_VAR_NAME),
              "; using default format:", date_format, "\n")
    }
    else {
      date_format <- attr(x, ATTR_VAR_DATE_FORMAT)
    }
    result <- as.Date(as.character(result), format = date_format)
  }
  for (attr.name in attr.names[attr.names != "class"]) {
    attr(result, attr.name) <- attr(x, attr.name)
  }
  return(result)
}

#' @rdname to_native_type
#' @export
to_native_type.CBOCOUv5 <- function (x) {
  attr.names <- names(attributes(x))
  if (!ATTR_VAR_DATA_TYPE %in% attr.names) {
    return(x)
  }
  result <- x
  result[result %in% ALL_MISSING] <- NA
  data.type <- attr(result, ATTR_VAR_DATA_TYPE)
  if (data.type == ATTR_VAR_DATA_TYPE_FACTOR) {
    levels <- unique(attr(result, ATTR_VAR_VALUE_LABELS)$label)
    result <- factor(attr(result, ATTR_VAR_VALUE_LABELS)$label[match(result,
                                                                     attr(result, ATTR_VAR_VALUE_LABELS)$value)],
                     levels = levels[!is.na(levels)])
  }
  else if (data.type == ATTR_VAR_DATA_TYPE_NUMERIC) {
    result[result == "not tested"] <- NA
    result <- as.numeric(result)
  }
  else if (data.type == ATTR_VAR_DATA_TYPE_DATE) {
    date_format <- "%Y-%m-%d"
    if (!ATTR_VAR_DATE_FORMAT %in% attr.names) {
      warning("unspecified date format for", attr(x, ATTR_VAR_NAME),
              "; using default format:", date_format, "\n")
    }
    else {
      date_format <- attr(x, ATTR_VAR_DATE_FORMAT)
    }
    result <- as.Date(as.character(result), format = date_format)
  }
  else if (data.type == ATTR_VAR_DATA_TYPE_CHARACTER) {
    result <- as.character(result)
  }
  for (attr.name in attr.names[attr.names != "class"]) {
    attr(result, attr.name) <- attr(x, attr.name)
  }
  return(result)
}
