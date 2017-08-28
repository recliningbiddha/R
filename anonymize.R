# Function to anonymise data in a data.frame
# ==========================================
# In code body use cols_to_mask as a vector with the column names to anonymise
# e.g. cols_to_mask <- c("name","address","post-code)
# then call the funciton to anonymise the data.frame
# data[,cols_to_mask := lapply(.SD, anonymise), .SDcols_to_mask, with=FALSE]

# If returns error may need to be passed data.table not data.frame

# algo=c("md5", "sha1", "crc32", "sha256", "sha512","xxhash32", "xxhash64", "murmur32")

# code by Jan Górecki, posted November 6, 2014
# https://www.r-bloggers.com/data-anonymization-in-r/
# ==========================================

anonymize <- function(x, algo="crc32") {
  unq_hases <- vapply(unique(x), function(object) digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)
  unname(unq_hases[x])
}