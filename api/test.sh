#!/usr/bin/sh

cmd="$*"

case $cmd in
    "flake8")
      flake8 rpserver
      ;;
    "isort")
      flake8 rpserver
      ;;
esac
