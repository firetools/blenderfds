#!python3

import shutil, tempfile

# Config
ignore_patterns = (
    "__pycache__",
    "*.pyc",
    "dev",
    "docs",
    "*.blend1",
    ".git*",
    ".vscode",
    "verification",
)
output_filename = "/home/egissi/blenderfds"

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
