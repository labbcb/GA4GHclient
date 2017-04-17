# Based on "Template for Resource Queries"
# Reference: http://bioconductor.org/developers/how-to/web-query/
request.post <- function(host, resource, request, N.TRIES = 1L)
{
    N.TRIES <- as.integer(N.TRIES)
    stopifnot(length(N.TRIES) == 1L, !is.na(N.TRIES))

    if (!endsWith(host, "/"))
        host <- paste0(host, "/")
    url <- paste0(host, resource)

    while (N.TRIES > 0L) {
        response <- tryCatch(
            POST(url, content_type_json(), body = toJSON(request),
                accept_json()),
            error = identity)
        if (!inherits(response, "error"))
            break
        N.TRIES <- N.TRIES - 1L
    }

    if (N.TRIES == 0L) {
        stop("'request.post()' failed:",
             "\n  URL: ", url,
             "\n  HTTP error: ", conditionMessage(response))
    }

    if (http_error(response)) {
        stop("'request.post()' failed:",
            "\n  URL: ", url,
            "\n  HTTP error: ", http_status(response)$message,
            "\n  Server error: ", response)
    }

    fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
}

# Based on "Template for Resource Queries"
# Reference: http://bioconductor.org/developers/how-to/web-query/
request.get <- function(host, resource, request, N.TRIES = 1L)
{
    N.TRIES <- as.integer(N.TRIES)
    stopifnot(length(N.TRIES) == 1L, !is.na(N.TRIES))

    if (!endsWith(host, "/"))
        host <- paste0(host, "/")
    url <- paste0(host, resource, "/", request)

    while (N.TRIES > 0L) {
        response <- tryCatch(GET(url, content_type_json(), accept_json()),
            error = identity)
        if (!inherits(response, "error"))
            break
        N.TRIES <- N.TRIES - 1L
    }

    if (N.TRIES == 0L) {
        stop("'request.get()' failed:",
             "\n  URL: ", url,
             "\n  HTTP error: ", conditionMessage(response))
    }

    if (http_error(response)) {
        stop("'request.get()' failed:",
            "\n  URL: ", url,
            "\n  HTTP error: ", http_status(response)$message,
            "\n  Server error: ", response)
    }

    fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
}

# This function translates genotype values from server to VCF format.
getGT <- function(genotype, phaseset)
{
    paste(ifelse(is.na(genotype), ".", genotype),
        collapse = ifelse(phaseset == "True", "|", "/"))
}

# This function extracts genotype data from call sets.
# It inputs a list of call sets (from searchVariants) and returns VCF geno data.
getGeno <- function(call)
{
    info.idx <- startsWith(names(call), "info.")
    geno <- call[info.idx]
    names(geno) <- sub("^info\\.", "", names(geno))
    geno$GT <- mapply(getGT, genotype = call$genotype, phaseset = call$phaseset)
    geno
}
