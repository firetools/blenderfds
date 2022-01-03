"""!
BlenderFDS, input/output routines.
"""

import os, struct, bpy
import numpy as np
from .types import BFException

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


def write_txt_file(filepath, text=None):
    """!
    Write text file to filepath.
    """
    try:
        with open(filepath, "w", encoding="utf8", errors="ignore") as f:
            f.write(text or str())
    except Exception as err:
        raise BFException(None, f"Error writing file <{filepath}>:\n{err}")


# Read/write fds bingeom files

# The FDS bingeom file is written from Fortran90 like this:
#      WRITE(731) INTEGER_ONE
#      WRITE(731) N_VERTS,N_FACES,N_SURF_ID,N_VOLUS
#      WRITE(731) VERTS(1:3*N_VERTS)
#      WRITE(731) FACES(1:3*N_FACES)
#      WRITE(731) SURFS(1:N_FACES)
#      WRITE(731) VOLUS(1:4*N_VOLUS)


def _read_record(f, req_dtype, req_dlen):
    """!
    Read a record from an open binary unformatted sequential Fortran90 file.
    @param f: open Python file object in 'rb' mode.
    @param req_dtype: requested type of data in 'int32' or 'float64'.
    @param req_dlen: requested length of the record.
    @return np.array of read data.
    """
    # The start tag is an int32 number (4 bytes) declaring the length of the record in bytes
    tag = struct.unpack("i", f.read(4))[0]
    # Calc the length of the record in numbers, using the byte size of the requested dtype
    dlen = int(tag / np.dtype(req_dtype).itemsize)
    if dlen != req_dlen:
        raise IOError(
            f"Different requested and declared record length: {req_dlen}, {dlen}"
        )
    # Read the record
    data = np.fromfile(f, dtype=req_dtype, count=req_dlen)
    # The end tag should be equal to the start tag
    end_tag = struct.unpack("i", f.read(4))[0]  # end tag, last 4 bytes, int32
    if tag != end_tag:  # check tags
        raise IOError(f"Different start and end record tags: {tag}, {end_tag}")
    # print(f"Read: record tag: {tag} dlen: {len(data)}\ndata: {data}")  # TODO log debug
    return data


def read_bingeom_file(filepath):
    """!
    Read FDS bingeom file
    @param filepath: filepath to be read from
    @return n_surf_id as integer, and fds_verts, fds_faces, fds_surfs, fds_volus as np.arrays in FDS flat format
    """
    try:
        with open(filepath, "rb") as f:
            geom_type = _read_record(f, req_dtype="int32", req_dlen=1)[0]
            if geom_type not in (1, 2):
                msg = f"Unknown GEOM type <{geom_type}> in bingeom file <{filepath}>"
                raise AssertionError(msg)
            n_verts, n_faces, n_surf_id, n_volus = _read_record(
                f, req_dtype="int32", req_dlen=4
            )
            fds_verts = _read_record(f, req_dtype="float64", req_dlen=3 * n_verts)
            fds_faces = _read_record(f, req_dtype="int32", req_dlen=3 * n_faces)
            fds_surfs = _read_record(f, req_dtype="int32", req_dlen=n_faces)
            fds_volus = _read_record(f, req_dtype="int32", req_dlen=4 * n_volus)
    except Exception as err:
        raise BFException(None, f"Error reading bingeom file <{filepath}>:\n{err}")
    return n_surf_id, fds_verts, fds_faces, fds_surfs, fds_volus


def _write_record(f, data):
    """!
    Write a record to a binary unformatted sequential Fortran90 file.
    @param f: open Python file object in 'wb' mode.
    @param data: np.array() of data.
    """
    # Calc start and end record tag
    tag = len(data) * data.dtype.itemsize
    # print(f"Write: record tag: {tag} dlen: {len(data)}\ndata: {data}")  # TODO log debug
    # Write start tag, data, and end tag
    f.write(struct.pack("i", tag))
    data.tofile(f)
    f.write(struct.pack("i", tag))


def write_bingeom_file(
    geom_type, n_surf_id, fds_verts, fds_faces, fds_surfs, fds_volus, filepath
):
    """!
    Write FDS bingeom file.
    @param geom_type: GEOM type (eg. 1 is manifold, 2 is terrain)
    @param n_surf_id: number of referred boundary conditions
    @param fds_verts: vertices coordinates in FDS flat format, eg. (x0, y0, z0, x1, y1, ...)
    @param fds_faces: faces connectivity in FDS flat format, eg. (i0, j0, k0, i1, ...)
    @param fds_surfs: boundary condition indexes, eg. (i0, i1, ...)
    @param fds_volus: volumes connectivity in FDS flat format, eg. (i0, j0, k0, w0, i1, ...)
    @param filepath: destination filepath
    """

    try:
        with open(filepath, "wb") as f:
            _write_record(f, np.array((geom_type,), dtype="int32"))  # was 1 only
            _write_record(
                f,
                np.array(
                    (
                        len(fds_verts) // 3,
                        len(fds_faces) // 3,
                        n_surf_id,
                        len(fds_volus) // 4,
                    ),
                    dtype="int32",
                ),
            )
            _write_record(f, np.array(fds_verts, dtype="float64"))
            _write_record(f, np.array(fds_faces, dtype="int32"))
            _write_record(f, np.array(fds_surfs, dtype="int32"))
            _write_record(f, np.array(fds_volus, dtype="int32"))
    except Exception as err:
        raise BFException(None, f"Error writing bingeom file <{filepath}>:\n{err}")


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


def calc_path(bl_path, name=None, extension=None, bl_start=None, start=None):
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
        start = calc_path(bl_path=bl_start)
    if start:
        path = os.path.relpath(path, start=start)
    return path


def calc_bl_name_and_dir(filepath, start=None):
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
