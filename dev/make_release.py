#!/usr/bin/env python3

# BlenderFDS, an open tool for the NIST Fire Dynamics Simulator
# Copyright (C) 2013  Emanuele Gissi, http://www.blenderfds.org
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTIBILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

"""!
Automatic creation of blenderfds release zip.
"""

import shutil, tempfile

# Config
ignore_patterns = (
    "__pycache__",
    "*.pyc",
    ".git",
    ".gitignore",
    ".vscode",
    "startup.blend1",
    "TODO.md",
    "dev",
    "docs",
    "verification",
)
output_filename = "blenderfds"

# Make
with tempfile.TemporaryDirectory() as tmpdirname:
    blenderfds_dir = tmpdirname + "/blenderfds"
    shutil.copytree(
        "..",
        blenderfds_dir,
        symlinks=False,
        ignore=shutil.ignore_patterns(*ignore_patterns),
    )
    shutil.make_archive(output_filename, "zip", blenderfds_dir)

print(f"Done: {output_filename}.zip")
