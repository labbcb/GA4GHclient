#' @export as.VCFHeader
as.VCFHeader <- function(variantSet)
{
    info <- with(variantSet$metadata, DataFrame(Number = number, Type = type,
        Description = description, row.names = sub("^INFO\\.", "", key)))
    VCFHeader(header = DataFrameList(INFO = info))
}
