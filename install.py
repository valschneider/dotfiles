#!/usr/bin/python3

import os

PATH = os.path.dirname(os.path.realpath(__file__))
HOME = os.path.expanduser("~")

filespath = os.path.join(PATH, "files")
files = os.listdir(filespath)
for f in files:
    linkpath = os.path.join(HOME, ".{}".format(f))
    if os.path.exists(linkpath):
        print("{} already exists, skipping".format(linkpath))
    else:
        os.symlink(os.path.join(filespath, f), linkpath)
