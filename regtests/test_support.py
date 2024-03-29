"""
This module contains support functions for all test.py
"""

import logging
import os
import sys

#  Change directory

TEST = sys.modules["__main__"]
TESTDIR = os.path.dirname(TEST.__file__)
TEST_NAME = os.path.basename(TESTDIR)
os.chdir(TESTDIR)

from e3.os.process import Run


def gnatmake(prj):
    """Compile a project with gnatmake"""
    cmd = ["gnatmake", "-p", "-gnat2012", "-P" + prj, "-bargs", "-E"]
    process = Run(cmd)
    if process.status:
        print(process.out)


def gprbuild(prj):
    """Compile a project with gprbuild"""
    cmd = ["gprbuild", "-p", "-gnat2012", "-P" + prj, "-bargs", "-E"]
    process = Run(cmd)
    if process.status:
        logging.error(process.out)


def run(bin, options=None, output_file=None):
    """Run a test"""
    if options is None:
        options = []
    if "TIMEOUT" in os.environ:
        timeout = int(os.environ["TIMEOUT"])
    else:
        timeout = 300
    Run(["./" + bin] + options, output=output_file, timeout=timeout)


def exec_cmd(bin, options=None, output_file=None, ignore_error=False):
    """Execute a binary"""
    if options is None:
        options = []
    process = Run([bin] + options, output=output_file)
    if process.status and not ignore_error:
        #  Exit with error
        logging.error(open(output_file).read())
