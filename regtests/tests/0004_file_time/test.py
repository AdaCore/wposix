
from test_support import *
import os
import time
import calendar
import datetime


def check_n(name, current, expected, error=0):
    v = int(current)
    if abs(v - expected) <= error:
        print "OK " + name
    else:
        print "NOK " + name + ": '" + str(expected) + "' != '" + current + "'"


def check_date(name, lines, index, day, month, year, hour, minute, second=0):
    check_n(name+"-"+str(index), lines[index][:-1], day)
    check_n(name+"-"+str(index+1), lines[index+1][:-1], month)
    check_n(name+"-"+str(index+2), lines[index+2][:-1], year)
    if hour != 0:
        check_n(name+"-"+str(index+3), lines[index+3][:-1], hour, error=2)
    check_n(name+"-"+str(index+4), lines[index+4][:-1], minute)
    if second != 0:
        check_n(name+"-"+str(index+5), lines[index+5][:-1], second, error=5)

ts_access = time.strptime('03 Feb 2001 14:05', '%d %b %Y %H:%M')
ts_mod = time.strptime('9 Sep 1967 10:33:01', '%d %b %Y %H:%M:%S')

file = open('file.txt', 'w')
file.close()
os.utime("file.txt", (calendar.timegm(ts_access), calendar.timegm(ts_mod)))

gprbuild('demo7')

now = datetime.datetime.now()

#  Make sure we are not going to change minute in the middle of the test

while now.second > 56:
    time.sleep(7)
    now = datetime.datetime.now()

run('demo7', output_file='demo7.out')

file = open('demo7.out')
lines = file.readlines()

check_date("now", lines, 1, now.day, now.month, now.year, 0,
           now.minute, now.second)

check_date("access", lines, 8, day=3, month=2, year=2001, hour=0, minute=5)
check_date("mod", lines, 15, day=9, month=9, year=1967, hour=0,
           minute=33, second=1)
check_date("change", lines, 22, day=9, month=9, year=1967, hour=0,
           minute=33, second=1)
