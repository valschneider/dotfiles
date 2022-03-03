# dotfiles
My dotfiles

# layout.yaml
file-or-directory:
  target_dir: !Path [path, to, thing] or !PathPrompt or str
  link_name: str
  create_parent: boolean, create parent directory
  dotify: boolean, prepend '.' to the symlink


__directory__:
  defaults:
    # default values, same entries as for a dir or file
  ignore: [list, of, files, to, ignore]