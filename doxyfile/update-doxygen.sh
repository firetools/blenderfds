#!/bin/bash
# Usage: bash /path/to/update-doxygen.sh

# System variables
DOXYGEN_PATHFILE="/opt/doxygen/doxygen"

# Generation of new doxygen
echo "Doxygen generation ..."
DOXYGEN_PATHFILE doxyfile > doxygen.log 2>&1

# Moving to branch gh-pages
echo "Pulling gh-pages branch ..."
git checkout gh-pages
git pull

# Replacing current doxygen
echo "Deploying doxygen ..."
rm -rf  ../docs/html
mv html ../docs/

# Commit
echo "Commit doxygen ..."
git add ../docs/html
COMMITMESSAGE="Doxygen update $(date)"
git commit -m "$COMMITMESSAGE"
git push

# Restoring master
git checkout master
