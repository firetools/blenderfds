"""!
BlenderFDS, input/output routines.
"""

import os, bpy, logging
from pathlib import Path
from ..types import BFException

log = logging.getLogger(__name__)

# Read/write txt files
# no need to catch exceptions


def read_txt_file(filepath):
    """!
    Read text file from filepath.
    """
    encodings = [
        ("utf8", "ignore"),
        ("windows-1252", None),
        ("utf8", None),
    ]  # Last tested first
    while encodings:
        e = encodings.pop()
        try:
            with open(filepath, "r", encoding=e[0], errors=e[1]) as f:
                return f.read()
        except UnicodeDecodeError:
            pass
        except Exception as err:
            raise BFException(None, f"Error reading file <{filepath}>:\n{err}")
    raise UnicodeDecodeError(f"Unknown text encoding in file <{filepath}>")


def write_txt_file(filepath, text=None, force_dir=False):
    """!
    Write text file to filepath.
    """
    try:
        if force_dir:
            Path(os.path.dirname(filepath)).mkdir(parents=True, exist_ok=True)
        with open(filepath, "w", encoding="utf8", errors="ignore") as f:
            f.write(text or str())
    except Exception as err:
        raise BFException(None, f"Error writing file <{filepath}>:\n{err}")


# File operations # TODO UNUSED?


def get_filename(filepath, extension=False):
    # /home/egissi/test/text.txt > text
    if extension:
        return os.path.basename(filepath)
    else:
        return os.path.splitext(os.path.basename(filepath))[0]


def get_dir_name(filepath):
    # /home/egissi/test/text.txt > test
    return os.path.basename(os.path.dirname(filepath))


def get_path(filepath):
    # /home/egissi/test/text.txt > /home/egissi/test
    return os.path.dirname(filepath)


def is_file(filepath, endswith=None):
    """!
    Check if filepath exists, it is a file, and has an extension.
    """
    if endswith:
        return (
            os.path.exists(filepath)
            and os.path.isfile(filepath)
            and filepath.endswith(endswith)
        )
    else:
        return os.path.exists(filepath) and os.path.isfile(filepath)


def is_dir(path):
    return os.path.isdir(path)


# Transform paths

# Paths notes:
#
# / ... / .blend / ... / .fds / ... / .bingeom
# From Blender:
# -----------> bpy.data.filepath (if file saved, always abs?)
# · · · · · · -------------> sc.bf_config_directory
# · · · · · · ----------------------------> ob.bf_geom_binary_directory
# · · · · · · ----------------------------> same for .ge1
# From FDS:
# -------------------------> fds file abspath
# · · · · · · · · · · · · · --------------> BINARY_NAME


def bl_path_to_os(bl_path, name=None, extension=None, bl_start=None, start=None):
    """!
    Get the absolute or relative os path from a Blender path.
    @param bl_path: original path in Blender notation.
    @param name: add file name to returned path.
    @param extension: ensure file extension to returned path.
    @param bl_start: set returned os path relative to other Blender path.
    @param start: set returned os path relative to other os path.
    """
    path = bpy.path.abspath(bl_path)
    if not os.path.isabs(path):  # still relative?
        raise BFException(
            None, f"Relative path <{path}> cannot be resolved to absolute."
        )
    if name:
        path = os.path.join(path, name)
    if extension:
        path = bpy.path.ensure_ext(path, extension)
    if bl_start:
        bl_start = bpy.path.abspath(bl_start)
        start = bl_path_to_os(bl_path=bl_start)
    if start:
        path = os.path.relpath(path, start=start)
    return path


def os_filepath_to_bl(filepath, start=None):
    """!
    Get the name and the directory in Blender format from an os filepath.
    @param filepath: filepath in os format, eg. /path/filename.txt or path/filename.txt.
    @param start: if filepath is relative, use os start directory to make it absolute.
    """
    name = os.path.splitext(os.path.basename(filepath))[0]
    path = os.path.dirname(filepath)
    if not os.path.isabs(path):  # still relative?
        # Try to make it abs with start
        path = os.path.join(start or "", path)
        if not os.path.isabs(path):
            raise BFException(
                None,
                f"Relative path <{path}> cannot be resolved, while getting name and dir.",
            )
    if os.path.isabs(bpy.path.abspath(bpy.data.filepath)):
        # Make it rel to blend file
        path = bpy.path.relpath(path)
    return name, path
