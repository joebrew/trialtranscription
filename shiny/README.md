# Set up

## Create local database
- In psql, `create database trialtranscription;`

## Create AWS RDS database
- Endpoint: `trialtranscription.cfejspjhdciw.us-east-2.rds.amazonaws.com`
- Web: https://us-east-2.console.aws.amazon.com/rds/home?region=us-east-2#dbinstance:id=trialtranscription

## SSH into the RDS dbinstance
`psql --host=trialtranscription.cfejspjhdciw.us-east-2.rds.amazonaws.com --port=5432 --username=trialtranscription`


## Create tables in R and upload to local database
- Run `assign_chunks.R`


