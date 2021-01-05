#!/bin/bash

DIR="$(dirname $(realpath $0))"
notmuch tag --batch < "$DIR"/notmuch-tags
