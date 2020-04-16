these files are not in use. instead, there are two cron tasks that take care of quote removal.
view cron tasks with:

crontab -e

i left these files here in case somebody needs to run a complex sql stored procedure down the road.
we'll have to use a local config to store the mysql credentials, rather than loading it from /home/aaron/sp

---update 2019-08-06 ----
the hbef website has been migrated to a new server. to use these example files, you'll now need to install a virtual environment manager, install python packages into it, change paths and credential files, etc.
