#------------------------------------------------------------------------------
# Helper function which should not be included in GitHub repository.  The
# function sets the security tokens for writing and saving data to the 
# Amazon S3 bucket.
#------------------------------------------------------------------------------

setKeys <- function(){
  # access_key <- "AKIAIDHDNCDATLJLBRQQ"
  # secret_key <- "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"
  
   access_key <- "AKIAUM3ZNN5GRD6SDEM2"
   secret_key <- "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"
  
  
  region <- "us-west-2"
  
  Sys.setenv("AWS_ACCESS_KEY_ID" = access_key,
             "AWS_SECRET_ACCESS_KEY" = secret_key,
             "AWS_DEFAULT_REGION" = region)
}
