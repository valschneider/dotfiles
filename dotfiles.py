#!/usr/bin/env python3

import os
import yaml

from pathlib import Path

HOME = os.path.expanduser("~")

def build_constructor_map(cls):
    def constructor(loader, node) :
        fields = loader.construct_mapping(node)
        return cls(**fields)
    return constructor

def build_constructor_seq(cls):
    def constructor(loader, node) :
        fields = loader.construct_sequence(node)
        return cls(*fields)
    return constructor

# Maintain abspath, because os.listdir doesn't
def listdir(path):
    return [os.path.join(path, p) for p in os.listdir(path)]

class PromptPath(Path):
    # This is fugly but it works™
    def __new__(self, query, default=None):
        return Path(input(f"Specify {query} " +
                          (f"({default})" if default else "") +
                          ": ") or default)

yaml.add_constructor('!PromptPath', build_constructor_map(PromptPath))
yaml.add_constructor('!Path', build_constructor_seq(Path))

class Dotfile:

    VERBOSE = True

    def __init__(self, source, target, dotify=False, create_parent=False):
        self.source = source

        target_dir = os.path.dirname(target)
        target_file = os.path.basename(target)

        if dotify:
            target_file = ".{}".format(target_file)

        self.target = os.path.expanduser(os.path.join(target_dir, target_file))
        self.create_parent = create_parent

    def install(self):
        if os.path.exists(self.target):
            raise FileExistsError("Destination file {} already exists".format(self.target))

        d = os.path.dirname(self.target)

        if not os.path.exists(d):
            if self.create_parent:
                if self.VERBOSE:
                    print(f"Creating directory {d}")
                else:
                    os.makedirs(d, exist_ok=True)
            else:
                raise FileNotFoundError("Parent directory doesn't exist")

        if self.VERBOSE:
            print(f"Symlinking {self.source} to {self.target}")
        else:
            os.symlink(self.source, self.target)

class DotfileDirectory:
    # TODO(?): make this nestable

    def __init__(self, directory):
        self.directory = directory

        layout_file = os.path.join(directory, "layout.yaml")
        self.layout = dict()
        self.dir_settings = {}
        self.defaults = {}

        if os.path.exists(layout_file):
            with open(layout_file, "r") as fh:
                self.layout = yaml.load(fh)

            self.dir_settings = self.layout.pop("__directory__", {})
            self.defaults = self.dir_settings.get("defaults", {})

    def install(self):
        handled = set([os.path.join(self.directory, p)
                       for p in self.dir_settings.get("ignore", []) +
                       # XXX: make this tweakable via a root layout.yaml
                       ["layout.yaml"]]
        )

        handled |= self.install_specials(handled)
        handled |= self.install_rest(handled)

    def install_rest(self, exclude):
        """
        Install files in directory
        """
        handled = set()

        for source in listdir(self.directory):
            if source in exclude:
                continue

            handled.add(source)

            file_args = self.defaults.copy()
            link_name = os.path.basename(source)
            target = os.path.join(file_args.pop("target_dir", HOME), link_name)

            dotfile = Dotfile(source, target, **file_args)

            try:
                dotfile.install()
            except (FileExistsError, FileNotFoundError) as exc:
                print(exc)

        return handled

    def install_specials(self, exclude):
        """
        Install special files called out in the directory's layout.yaml
        """
        handled = set()
        if not self.layout:
            return handled

        for source_name, args in self.layout.items():
            source = os.path.join(self.directory, source_name)

            if source in exclude:
                continue

            handled.add(source)

            # Use __directory__ defaults
            file_args = self.defaults.copy()
            # Update with file-specific attributes
            file_args.update(args)

            target_dir = file_args.pop("target_dir", HOME)
            target = os.path.join(target_dir, file_args.pop("link_name", source_name))
            dotfile = Dotfile(source, target, **file_args)

            try:
                dotfile.install()
            except (FileExistsError, FileNotFoundError) as exc:
                print(exc)

        return handled

def main():
    here = os.path.dirname(os.path.realpath(__file__))

    for d in listdir(here):
        if not os.path.isdir(d):
            continue

        if os.path.basename(d).startswith('.'):
            continue

        DotfileDirectory(d).install()

if __name__ == "__main__":
    main()
