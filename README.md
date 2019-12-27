# s4w
Ships for Whales coordinating repository

Contents:
<!-- 
To update table of contents run: `cat README.md | ./gh-md-toc -` 
Uses: https://github.com/ekalinin/github-markdown-toc
-->

## Repositories

Moving up stack based on dependencies, here are the Github repositories for this project:

- [**s4w-docker**](https://github.com/BenioffOceanInitiative/s4w-docker)<br>
  Software stack containerized using Docker for quickly spinning up on server and local machines
- [**s4w-lib**](https://github.com/BenioffOceanInitiative/s4w_ais_manager)<br>
  R library of functions used by apps, api and scripts
- [**s4w**](https://github.com/BenioffOceanInitiative/s4w)<br>
  Project management and scripts to process the data, including ones executed at regular intervals, eg fetching AIS or emailing companies
- [**s4w-api**](https://github.com/BenioffOceanInitiative/s4w-api)<br>
  Application programming interface (API) for fetching data stored in database, tabular or geographical
- [**s4w-apps**](https://github.com/BenioffOceanInitiative/s4w-apps)<br>
  Shiny applications for interactive user interfaces to data
  
## Scripts

Scripts, some to setup to run at regular intervals (ie cron jobs) on the server.

### Process AIS

[s4w_ais_manager](https://github.com/BenioffOceanInitiative/s4w_ais_manager):

1.  [logfile](https://github.com/BenioffOceanInitiative/s4w_ais_manager/blob/master/R/logfile_funs.R)
2.  [crawlers](https://github.com/BenioffOceanInitiative/s4w_ais_manager/blob/master/R/crawlers.R)
3.  [parsers](https://github.com/BenioffOceanInitiative/s4w_ais_manager/blob/master/R/readers.R)
4.  [database](https://github.com/BenioffOceanInitiative/s4w_ais_manager/blob/master/R/db_init_lite.R)

Processes:

1. Fetch AIS data
1. Create segments
1. Intersect with VSR zones
1. Calculate % compliance
1. Push to geodatabase

### Email Companies

TODO, see [Develop email templates and mailers · Issue #6 · BenioffOceanInitiative/s4w](https://github.com/BenioffOceanInitiative/s4w/issues/6).

## Reports

TODO