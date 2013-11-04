# functions to scrape mg supplier emails and insert into db for analysis

## workflow 
- email added to mgfarm
- nightly yield email

###  gmail -> fethcmail -> procmail -> stdin -> scraper.R -> postgres DB db 'milkmon', tb 'obs'

- issues with raw observations - IRREGULAR PICKUPs
- (FRESH) mulitple tanks per day - need to sum daily
- (STALE) changing interval between tanks - assume equal amount collected from each day - divide total by nday.
- all done by 'daysum.R', then thrown into tb 'daysums'

## config failes
- .fetchmailrc - login to gmail (has pword in plaintext)
- .procmailrc - reads email matching subject 'MG Supplier Notification' to scraper.R
