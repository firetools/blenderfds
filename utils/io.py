"""!
BlenderFDS, input/output routines.
"""

import os, bpy, logging
from pathlib import Path
from ..types import BFException, BFNotImported

log = logging.getLogger(__name__)

# TODO see also:
# bpy_extras.io_utils.path_reference(filepath, base_src, base_dst, mode='AUTO', copy_subdir='', copy_set=None, library=None)
# bpy_extras.io_utils.unique_name(key, name, name_dict, name_max=- 1, clean_func=None, sep='.')


def shorten(string, max_len=80, start_part=0.1):
    """Shorten string to max_len length."""
    current_len = len(string)
    if current_len > max_len:
        return f"{string[:int(max_len*start_part)]}···{string[-int(max_len*(1-start_part)-3):]}"
    return string


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
            raise BFException(None, f"Error reading file: <{filepath}>\n{err}")
    raise UnicodeDecodeError(f"Unknown text encoding in file: <{filepath}>")


def write_txt_file(filepath, text=None, force_dir=False):
    """!
    Write text file to filepath.
    """
    try:
        if force_dir:
            make_dir(filepath)
        with open(filepath, "w", encoding="utf8", errors="ignore") as f:
            f.write(text or str())
    except Exception as err:
        raise BFException(None, f"Error writing file: <{filepath}>\n{err}")


# Transform paths

# Paths notes:
#
# / ... / .blend / ... / .fds / ... / .bingeom

# From Blender to FDS:
# -----------> bpy.data.filepath (if file saved, always abs?)
# · · · · · · -------------> sc.bf_config_directory
# · · · · · · ----------------------------> ob.bf_geom_binary_directory
# · · · · · · ----------------------------> same for .ge1

# From FDS to Blender:
# -------------------------> fds file abspath
# · · · · · · · · · · · · · --------------> BINARY_NAME

# From relative to blender file (rbl)


def transform_rbl_to_abs(context, filepath_rbl, name="", extension="") -> str:
    """!
    Transform filepath relative to blender file to absolute.
    """
    filepath = bpy.path.abspath(filepath_rbl or "//.")
    if not is_abs(filepath):
        msg = f"transform_rbl_to_abs(context, filepath_rbl={filepath_rbl}, name={name}, extension={extension})"
        log.debug(msg)
        raise BFException(None, f"Save the Blender file first!")
    if name or extension:
        filepath = append_filename(filepath, name, extension)
    return filepath


def transform_rbl_to_abs_and_rfds(context, filepath_rbl, name="", extension=""):
    """!
    Transform filepath relative to blender file to absolute and relative to fds case file.
    """
    # Get abs filepath
    filepath = transform_rbl_to_abs(
        context=context,
        filepath_rbl=filepath_rbl or get_abs_fds_path(context.scene),
        name=name,
        extension=extension,
    )

    # Set filepath_rfds
    if not filepath_rbl:  # empty means same as fds case path
        filepath_rfds = os.path.basename(filepath)
    elif not is_abs(filepath_rbl):  # try to leave it relative
        filepath_rfds = transform_abs_to_rfds(filepath, context.scene)
    else:  # keep the existing abs
        filepath_rfds = filepath

    return filepath, filepath_rfds


# From relative to fds case file (rfds)


def get_abs_fds_path(sc) -> str:
    """!
    Get absolute fds case path.
    @param sc: exported scene.
    @return: absolute fds path.
    """
    # Could be abs already, so no check on save but on result
    # If empty try the same as the Blender file
    fds_path = bpy.path.abspath(sc.bf_config_directory or "//.")  # always or "//."
    if not is_abs(fds_path):
        log.debug(f"get_abs_fds_path(bf_config_directory={sc.bf_config_directory})")
        raise BFException(None, f"Save the Blender file first!")
    return fds_path


def transform_rfds_to_abs(context, filepath_rfds) -> str:
    """!
    Transform filepath relative to fds file to absolute.
    """
    fds_path = get_abs_fds_path(context.scene)
    filepath = os.path.abspath(os.path.join(fds_path, filepath_rfds))
    return filepath


def transform_rfds_to_abs_and_rbl(context, filepath_rfds):
    """!
    Transform filepath relative to fds file to absolute and relative to blender file.
    """
    # Get abs filepath
    filepath = transform_rfds_to_abs(
        context=context, filepath_rfds=filepath_rfds or get_abs_fds_path(context.scene)
    )

    # Set filepath_rbl
    if not filepath_rfds:  # empty means same as fds case path
        filepath_rbl = ""
    elif not is_abs(filepath_rfds):  # try to leave it relative
        filepath_rbl = transform_abs_to_rbl(filepath)
    else:  # keep the existing abs
        filepath_rbl = filepath

    # Set other
    path_rbl, name = extract_path_name(filepath_rbl)

    return filepath, filepath_rbl, path_rbl, name


# From absolute to relative


def transform_abs_to_rbl(filepath) -> str:
    """!
    Transform absolute filepath to relative to blend file.
    """
    if bpy.data.is_saved:
        return bpy.path.relpath(filepath)  # try to make it relative
    else:
        return filepath  # keep the existing


def transform_abs_to_rfds(filepath, sc) -> str:
    """!
    Transform absolute filepath to relative to fds case file.
    """
    try:
        fds_path = get_abs_fds_path(sc)
    except BFException:
        return filepath  # keep the existing
    else:
        return os.path.relpath(filepath, start=fds_path)  # make it relative


# Others


def append_filename(path="", name="", extension="") -> str:
    filename = bpy.path.ensure_ext(name, extension)
    return os.path.join(path, filename)


def extract_path_basename(filepath) -> tuple:
    # Fix bug in Windows, never use os.path.dirname
    basename = bpy.path.basename(filepath)
    path = filepath[: -len(basename)]
    return path, basename


def extract_path_name(filepath) -> tuple:
    path, basename = extract_path_basename(filepath)
    name = os.path.splitext(basename)[0]
    return path, name


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


def is_abs(path):
    if path.startswith("//"):  # Blender notation for relative paths
        return False
    return os.path.isabs(path)


def is_clean(filename):
    return bpy.path.clean_name(filename) == filename


def make_dir(filepath):
    path, _ = extract_path_basename(filepath)
    Path(path).mkdir(parents=True, exist_ok=True)
