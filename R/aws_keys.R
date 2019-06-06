#------------------------------------------------------------------------------
# Helper function which should not be included in GitHub repository.  The
# function sets the security tokens for writing and saving data to the 
# Amazon S3 bucket.
#------------------------------------------------------------------------------

setKeys <- function(){
  access_key <- "AKIAIDHDNCDATLJLBRQQ"
  secret_key <- "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"
  region <- "us-west-2"
  
  Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
             "AWS_SECRET_ACCESS_KEY" = secret_key,
             "AWS_DEFAULT_REGION" = region)
}
