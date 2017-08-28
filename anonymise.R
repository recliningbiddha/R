## Function to anonymise data in a data.frame
# ==========================================
# In code body use cols_to_anon as a vector with the column names to anonymise
# e.g. cols_to_mask <- c("name","address","post-code)
# then call the function to anonymise the data.table e.g.
# anonymised <- anonymise(data, cols_to_anon)
# OR
# dt[,cols_to_anon] <- anonymise(dt,cols_to_anon)
# Needs data.table - may return error if passed data.frame

# algo=c("md5", "sha1", "crc32", "sha256", "sha512","xxhash32", "xxhash64", "murmur32")

# code by richierocks, posted August 23, 2011
#https://www.r-bloggers.com/anonymising-data/
# ==========================================

function(data, cols_to_anon, algo = "crc32")
{
  if(!require(digest)) stop("digest package is required")
  to_anon <- subset(data, select = cols_to_anon)
  unname(apply(to_anon, 1, digest, algo = algo))
}
