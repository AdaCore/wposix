
from test_support import *
import os

USER=os.getenv("USER")

def remove_acl(file):
    exec_cmd('icacls', [file, '/remove', 'Everyone'], output_file='acl.log')
    exec_cmd('icacls', [file, '/remove', 'None'], output_file='acl.log')
    exec_cmd('icacls', [file, '/remove', 'cyg_server'], output_file='acl.log')
    exec_cmd('icacls', [file, '/remove', 'SYSTEM'], output_file='acl.log')
    exec_cmd('icacls', [file, '/remove', 'Administrators'],
             output_file='acl.log')
    exec_cmd('icacls', [file, '/remove', USER], output_file='acl.log')

def create(file):
    f=open(file, 'w')
    f.close()
    remove_acl(file)

gnatmake('cperms')

create('check1')

exec_cmd('icacls', ['check1', '/grant', 'Everyone:R'], output_file='acl.log')
exec_cmd('icacls', ['check1', '/grant', USER+':R'], output_file='acl.log')
exec_cmd('icacls', ['check1', '/grant', 'None:R'], output_file='acl.log')

run('cperms')
