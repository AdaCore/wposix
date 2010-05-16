
from test_support import *
import os

gnatmake('changeo')

f=open('check1', 'w')
f.close()

run('changeo')
