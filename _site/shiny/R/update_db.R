#' Update DB
#' 
#' Update a table
#' @param data The data to be uploaded
#' @param table_name Name of table
#' @param connection_object The connection object
#' @return The transcriptions relation in the postgresql database will be updated
#' @import dplyr
#' @import RPostgreSQL
#' @export
#' @examples
#' 2+2

update_db <- function(data,
                      table_name = 'transcriptions',
                      connection_object = NULL){
  # If not connection object, try to find one
  if(is.null(connection_object)){
    message(paste0('No connection_object provided. Will try ',
                   'to find a credentials file.'))
    # Get credentials
    the_credentials <- credentials_extract()
    # Establish the connection
    connection_object <- credentials_connect(the_credentials)
  }
  
  # Replace the table in db
  dbWriteTable(conn = connection_object,
               name = table_name,
               value = data,
               append=TRUE, row.names=FALSE)
}