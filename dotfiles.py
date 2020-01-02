#!/usr/bin/env python3

import os
import yaml

HOME = os.path.expanduser("~")

class Dotfile:
    def __init__(self, source, target_dir, dotify=False, create_parent=False):
        self.source = source
        target_file = os.path.basename(self.source)

        if dotify:
            target_file = ".{}".format(target_file)

        self.target = os.path.join(target_dir, target_file)
        self.create_parent = create_parent

    def install(self):
        if os.path.exists(self.target):
            raise FileExistsError("Destination file {} already exists".format(self.target))

        if not os.path.exists(os.path.dirname(self.target)):
            if self.create_parent:
                os.makedirs(os.path.dirname(self.target), exist_ok=True)
            else:
                raise FileNotFoundError("Parent directory doesn't exist")

        os.symlink(self.source, self.target)

# Maintain abspath, because os.listdir doesn't
def listdir(path):
    return [os.path.join(path, p) for p in os.listdir(path)]

def handle_dotfiles(directory):

    def get_target_dir(defaults_dict):
        if "target_dir" in defaults_dict:
            return os.path.expanduser(defaults_dict.pop("target_dir"))

        return HOME

    visited = set()
    defaults = {}

    layout_file = os.path.join(directory, "layout.yaml")
    if os.path.exists(layout_file):
        with open(layout_file, "r") as fh:
            layout = yaml.safe_load(fh)

        visited.add(layout_file)
        if "__directory__" in layout:
            dir_settings = layout.pop("__directory__")
            defaults = dir_settings.get("defaults", {})
            visited.update(
                [os.path.join(directory, p) for p in dir_settings.get("ignore", [])])

        # Install files called out in layout.yaml
        for source, args in layout.items():
            source = os.path.join(directory, source)
            visited.add(source)

            def_args = defaults.copy()
            def_args.update(args)

            target_dir = get_target_dir(def_args)
            dotfile = Dotfile(source, target_dir, **def_args)
            dotfile.install()

    # Install all other files
    target_dir = get_target_dir(defaults)
    for source in listdir(directory):
        if source in visited:
            continue

        dotfile = Dotfile(source, target_dir, **defaults)
        dotfile.install()

def main():
    here = os.path.dirname(os.path.realpath(__file__))

    for d in listdir(here):
        if not os.path.isdir(d):
            continue

        if os.path.basename(d).startswith('.'):
            continue

        try:
            handle_dotfiles(d)
        except (FileExistsError, FileNotFoundError) as exc:
            print(exc)

if __name__ == "__main__":
    main()
