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

archive_merger.R is not actually scheduled (yet). Amey will just send a new version of "HB physical archives stream samples.csv" periodically, and then you can run:
    Rscript /home/mike/git/hbef/shiny/scheduled_scripts/archive_merger.R
    That will overwrite the archive table in MariaDB with the new version. Note that this table isn't even used. The archive data get merged with the field sample data in R and then get written as raw text into archive_explore.html.
After running archive_merger.R, run sudo systemctl restart shiny-server.
