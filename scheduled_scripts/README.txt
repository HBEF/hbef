parse_sensor_data.R is sorta derelict. at some point in prehistory, we received a file called CR1000_HBF_WQual_W3.csv, a compilation of data from various UNH sensors in watershed 3. parse_sensor_data.R has been replaced by process_unh_data.R, which handles raw, updating sensor files from watersheds 3 and 9.

20240419
okay, now it's watersheds 3 and 6. and all the sensor data (wqual and Q) can be found on the loggernet service. so no need to rsync directly from usfs server. this is accounted for in the scripts,
	names of which may be slightly inaccurate as a result

get_usfs_weirfiles.sh runs periodically as a cron job. the rest of the updating weir files arrive as part of an rsync (also a cron job on this machine)

process_unh_data.R and process_S.CAN_data.R must run as cron tasks. process_unh_data.R must run first, because it begins by dropping the sensor4 table that they both use. the easiest way to ensure this is to source the latter from the former. so, that's what you'll find is happening. if you are rebuilding this server, you may have to update the path being sourced

to incorporate a new S.CAN file from Tammy:
    update process_S.CAN_data.R
    push changes; pull them to the server
    sftp the new file to the server
    execute Rscript process_unh_data.R on the server (or wait for it to run as a cron job)
    probably safest to: sudo systemctl restart shiny-server

archive_merger.R is not actually scheduled (yet). Amey sends per-watershed xlsx files (e.g. "w6 sample archive collection through 2026.xlsx"). Put them, unmodified, in restricted_QAQC/data/archive_data/stream_updates/ on the server; each replaces that watershed's rows from "HB physical archives stream samples.csv" (see header of archive_merger.R). Then run:
    Rscript /home/mike/shiny/scheduled_scripts/archive_merger.R
    It merges the archive bottles with chemistry from the hbef database and writes HTML/archive_explore/archive_data.js (gitignored; loaded by archive_explore.html, so the page updates without a commit) and ../misc/edi_prep_files/archive_samples.csv (barcodes for edi_upload_prep.R).
    restricted_QAQC/data/ is gitignored, so the archive files (HB physical archives stream samples.csv, HB precipitation.csv, stream_updates/) must be copied to the server by hand, e.g. after a rebuild.
After running archive_merger.R, run sudo systemctl restart shiny-server.
