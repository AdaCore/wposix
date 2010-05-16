
from test_support import *
import os

USER=os.getenv("USER")

def remove_acl(file):
    exec_cmd('icacls', [file, '/remove', 'Everyone'], output_file='acl.log')
    exec_cmd('icacls', [file, '/remove', 'None'], output_file='acl.log')
    exec_cmd('icacls', [file, '/remove', USER], output_file='acl.log')

def create(file):
    f=open(file, 'w')
    f.close()
    remove_acl(file)

gnatmake('fperms')

create('check1')
create('check2')
create('check3')
create('check4')
create('check5')

exec_cmd('icacls', ['check1', '/grant', 'Everyone:R'], output_file='acl.log')

exec_cmd('icacls', ['check2', '/grant', 'Everyone:RW'], output_file='acl.log')

exec_cmd('icacls', ['check3', '/grant', 'Everyone:RW'], output_file='acl.log')
exec_cmd('icacls', ['check3', '/grant', 'None:RW'], output_file='acl.log')

exec_cmd('icacls', ['check4', '/grant', 'Everyone:RX'], output_file='acl.log')
exec_cmd('icacls', ['check4', '/grant', 'None:RX'], output_file='acl.log')
exec_cmd('icacls', ['check4', '/grant', USER+':RX'], output_file='acl.log')

exec_cmd('icacls', ['check5', '/grant', USER+':R'], output_file='acl.log')

run('fperms')
