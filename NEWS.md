# [kwb.pilot 0.3.0](https://github.com/KWB-R/kwb.pilot/releases/tag/v0.3.0) <small>2021-06-04</small>

* **ULTIMATE**: add functions for data import and upload to InfluxDB cloud, for 
details checkout the [Ultimate](../articles/ultimate.html) article. In addition
the workflow is now automatically performed form Monday-Friday at 5am UTC (i.e. 
if rawdata files exist on Nextcloud these will be imported and finally moved to 
the `data_imported` directory on Nextcloud)

* Added function `move_nextcloud_files()` to move files from `raw data` to `imported`
directory on Nextcloud auto

# [kwb.pilot 0.2.1](https://github.com/KWB-R/kwb.pilot/releases/tag/v0.2.1) <small>2021-11-10</small>

* MBR4.0: bugfix as data format changed from `de` to `en` for download from webportal
of Martin Systems

# [kwb.pilot 0.2.0]

* Add functions for project [MBR 4.0](https://https://www.kompetenz-wasser.de/en/project/mbr40/)

# [kwb.pilot 0.1.0](https://github.com/KWB-R/kwb.pilot/releases/tag/v0.1.0) <small>2021-02-26</small>

* Add functions for project [SULEMAN](https://https://www.kompetenz-wasser.de/en/project/suleman/) 

* Copied functions from latest version of R package aquanes.report (https://github.com/KWB-R/aquanes.report/commit/7df99ab71b6fbdf05a7b64024d8f7555361e3903)

* Updated vignettes to fit to new pkgdown v1.4.1 behavior (https://github.com/r-lib/pkgdown/issues/1158#issuecomment-539079871)

