#!/usr/bin/env python3

from argparse import ArgumentParser
from collections import namedtuple

from elftools.elf.elffile import ELFFile

def get_symtab(elffile):
    res = {}
    symtab = elffile.get_section_by_name(".symtab")

    for symbol in symtab.iter_symbols():
        if "OBJECT" in symbol.entry["st_info"]["type"]:
            continue

        res[symbol.name] = symbol.entry["st_size"]

    return res

SymDiff = namedtuple("SymDiff", ["name", "before", "after", "delta"])

def elfdiff(base, other):
    """
    Compute a per-symbol diff between two ELF files.

    :param base: The reference ELF file
    :type base: ELFFile

    :param base: The ELF file to diff against
    :type other: ELFFile

    :returns: list(SymDiff) of changed (including new or removed) symbols

    .. todo:: I saw an example that iterated over symbol tables,
              seems like there can be more than one per ELF file?
    """
    symtab_base = get_symtab(base)
    symtab_other = get_symtab(other)

    visited = set()
    res = []
    for symbol, size in symtab_other.items():
        name = symbol
        before = 0
        after = size
        delta = size

        if symbol in symtab_base:
            before = symtab_base[symbol]
            delta = after - before

        visited.add(symbol)

        # Skip the symbol if it hasn't been changed
        if delta:
            res.append(SymDiff(name, before, after, delta))

    # Symbols present in base but not in other have been removed
    removed = set(symtab_base.keys()).difference(visited)
    for symbol in removed:
        size = symtab_base[symbol]
        res.append(SymDiff(symbol, size, 0, -size))

    return res

def main():
    parser = ArgumentParser()
    parser.add_argument("base")
    parser.add_argument("other")

    args = parser.parse_args()

    with open(args.base, "rb") as fh_base, open(args.other, "rb") as fh_other:
        elf_base = ELFFile(fh_base)
        elf_other = ELFFile(fh_other)

        diff = elfdiff(elf_base, elf_other)

        # Sort them by descending delta order
        diff.sort(key=lambda x : x.delta, reverse=True)

        total = 0
        align = "{:<36}{:<10}{:<10}{:<10}"
        print(align.format("NAME", "BEFORE", "AFTER", "DELTA"))
        for name, before, after, delta in diff:
            total += delta
            print(align.format(
                name, before, after,
                str(delta) if delta <= 0 else "+{}".format(delta))
            )
        print("------------------------------------------------------------------")
        print(align.format("DELTA SUM", "", "", "{:+d}".format(total)))

if __name__ == "__main__":
    main()
