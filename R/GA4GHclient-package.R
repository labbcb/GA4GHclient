#' A Bioconductor package for accessing GA4GH API data server
#'
#' GA4GHclient provides an easy way to access public data servers through Global
#' Alliance for Genomics and Health (GA4GH) genomics API. It provides low-level
#' access to GA4GH API and translates response data into Bioconductor-based
#' class objects.
#'
#' @name GA4GHclient-package
#' @aliases GA4GHclient GA4GHclient-package
#' @docType package
#' @author Welliton Souza, Benilton Carvalho, Cristiane Rocha
#'
#' Maintainer: Welliton Souza <well309@gmail.com>
#' @keywords package
#'
#' @importClassesFrom GenomicRanges GRangesList
#' @importMethodsFrom BiocGenerics as.data.frame start end lengths
#' @importMethodsFrom GenomeInfoDb seqnames
#' @importFrom Biostrings DNAStringSet BString
#' @importFrom dplyr %>% bind_rows select starts_with mutate
#' @importFrom GenomicRanges makeGRangesFromDataFrame
#' @importFrom httr content content_type_json GET POST http_error http_status accept_json
#' @importFrom IRanges CharacterList DataFrameList
#' @importFrom jsonlite fromJSON toJSON unbox
#' @importFrom methods as
#' @importFrom S4Vectors DataFrame Rle
#' @importFrom VariantAnnotation VCF VCFHeader header header<-
NULL
