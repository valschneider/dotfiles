#!/usr/bin/python3

import os

PATH = os.path.dirname(os.path.realpath(__file__))
HOME = os.path.expanduser("~")

def link(src_path, link_path):
    if os.path.exists(link_path):
        print("{} already exists, skipping...".format(link_path))
    else:
        os.symlink(src_path, link_path)

def install(directory, path, dotify=False):
    """
    Create symlinks in 'path' to all files in 'directory'
    """
    if dotify:
        prefix = "."
    else:
        prefix = ""

    for f in os.listdir(directory):
        link_path = os.path.join(path, "{}{}".format(prefix, f))
        link(os.path.join(directory, f), link_path)

install(os.path.join(PATH, "files"), HOME, True)

emacs_dir = os.path.join(HOME, ".emacs.d")

# TODO: can symlink the whole dir
emacs_package_dir = os.path.join(emacs_dir, "packages")
install(os.path.join(PATH, "emacs-dir", "packages"), emacs_package_dir)

link(os.path.join(PATH, "emacs-dir", "init"), os.path.join(emacs_dir, "init"))
