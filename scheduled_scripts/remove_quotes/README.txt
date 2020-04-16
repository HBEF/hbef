these files are not in use. instead, there are two cron tasks that take care of quote removal.
view cron tasks with:

crontab -e

i left these files here in case somebody needs to run a complex sql stored procedure down the road.
we'll have to use a local config to store the mysql credentials, rather than loading it from /home/aaron/sp
