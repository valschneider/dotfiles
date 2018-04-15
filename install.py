#!/usr/bin/python3

import os

PATH = os.path.dirname(os.path.realpath(__file__))
HOME = os.path.expanduser("~")

def install(directory, path, dotify=False):
    """
    Create symlinks in 'path' to all files in 'directory'
    """
    if dotify:
        prefix = "."
    else:
        prefix = ""

    for f in os.listdir(directory):
        linkpath = os.path.join(path, "{}{}".format(prefix, f))

        if os.path.exists(linkpath):
            print("{} already exists, skipping...".format(linkpath))
        else:
            os.symlink(os.path.join(directory, f), linkpath)

install(os.path.join(PATH, "files"), HOME, True)

emacs_package_dir = os.path.join(HOME, ".emacs.d", "packages")
install(os.path.join(PATH, "emacs-packages"), emacs_package_dir)
