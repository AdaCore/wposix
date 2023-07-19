#!/usr/bin/env python
"""./testsuite.py [options] [test name]

Run the wPOSIX testsuite
"""


import argparse
import os

from e3.testsuite import Testsuite
from e3.testsuite.driver.adacore import AdaCoreLegacyTestDriver
from e3.testsuite.testcase_finder import ParsedTest, TestFinder

# class WPOSIXTestDriver(AdaCoreLegacyTestDriver):


class WPOSIXTestFinder(TestFinder):
    """Look for testcases.

    Every directory whoch contains a "test.py" file is identified as a testcase.
    """

    def probe(
        self,
        testsuite,
        dirpath,
        dirnames,
        filenames,
    ):
        test_name = os.path.basename(dirpath)

        if "test.py" in filenames:
            return ParsedTest(test_name, AdaCoreLegacyTestDriver, {}, dirpath)
        else:
            return None


class WPOSIXTestsuite(Testsuite):
    """Run the wposix testsuite."""

    @property
    def test_finders(self):
        return [WPOSIXTestFinder()]

    def add_options(self, parser: argparse.ArgumentParser):
        parser.add_argument(
            "--discs",
            type=str,
        )

    def set_up(self):
        self.env.test_environ = dict(os.environ)

        self.env.discs = self.env.discriminants

        if self.env.options.discs:
            self.env.discs += self.env.options.discs.split(",")

        if "PYTHONPATH" not in self.env.test_environ:
            self.env.test_environ["PYTHONPATH"] = os.getcwd()
        else:
            self.env.test_environ["PYTHONPATH"] += os.path.pathsep + os.getcwd()


if __name__ == "__main__":
    # Run the wPOSIX testsuite
    WPOSIXTestsuite().testsuite_main()
