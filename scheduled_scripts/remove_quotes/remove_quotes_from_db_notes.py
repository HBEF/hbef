import os
import sys
import sqlalchemy as sa
import pandas as pd
from sqlalchemy.ext.declarative import declarative_base

#import credentials from streampulse flask app config file
app_dir = '/home/aaron/sp'
# app_dir = '/home/mike/git/streampulse/server_copy/sp'
sys.path.insert(0, app_dir)
os.chdir(app_dir)
import config as cfg

#configure database connection
pw = cfg.MYSQL_PW
db = sa.create_engine('mysql://root:{0}@localhost/sp'.format(pw))
session = sa.orm.Session(bind=db.engine)

with open('/home/hbef/scheduled_scripts/remove_quotes/remove_quotes_from_db_notes.sql', 'r') as f:
    t = f.read()

session.execute(t)
session.execute('CALL remove_quotes_from_notes();')
session.commit()

session.close()
