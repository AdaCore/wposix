
from test_support import *
import os

gprbuild('changeo')

f=open('check1', 'w')
f.close()

run('changeo')
