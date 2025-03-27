#!/bin/bash

rm -rf ./_build
jupyter-book build ./
echo Enter message for commit
read message 
git add -A
git commit -m '$message'
git push
ghp-import -n -p -f _build/html
