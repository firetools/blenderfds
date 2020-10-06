"""!
BlenderFDS, FDS MOVE tools.
"""

# FDS MOVE parameter to Blender transformation matrix and back

from ..types import FDSNamelist, FDSParam
from ..utils import BFException, BFNotImported, is_iterable

from mathutils import Matrix, Vector
from math import radians


def to_matrix(fds_namelist):
    """!
    Transform MOVE namelist to Blender transformation matrix.
    @param fds_namelist: MOVE namelist in FDSNamelist format.
    @return: ID string, Blender transformation matrix.
    """
    # CHeck input
    if not fds_namelist.fds_label == "MOVE":
        raise AssertionError(f"MOVE namelist required, not <{fds_namelist}>")
    # Get ID
    p_id = fds_namelist.get_by_label(fds_label="ID", remove=True)
    if not p_id:
        raise BFNotImported(None, "MOVE namelist requires an ID in <{fds_namelist}>")
    hid = p_id.value
    # Get T34, that has priority
    p_t34 = fds_namelist.get_by_label(fds_label="T34", remove=True)
    if p_t34:
        t34 = p_t34.value
        m = Matrix()
        (
            m[0][0],
            m[1][0],
            m[2][0],
            m[0][1],
            m[1][1],
            m[2][1],
            m[0][2],
            m[1][2],
            m[2][2],
            m[0][3],
            m[1][3],
            m[2][3],
        ) = t34
        return hid, m
    # Get other params
    p_x0 = fds_namelist.get_by_label(fds_label="X0", remove=True)
    p_y0 = fds_namelist.get_by_label(fds_label="Y0", remove=True)
    p_z0 = fds_namelist.get_by_label(fds_label="Z0", remove=True)
    p_axis = fds_namelist.get_by_label(fds_label="AXIS", remove=True)
    p_rot_angle = fds_namelist.get_by_label(fds_label="ROTATION_ANGLE", remove=True)
    p_scale = fds_namelist.get_by_label(fds_label="SCALE", remove=True)
    p_scalex = fds_namelist.get_by_label(fds_label="SCALEX", remove=True)
    p_scaley = fds_namelist.get_by_label(fds_label="SCALEY", remove=True)
    p_scalez = fds_namelist.get_by_label(fds_label="SCALEZ", remove=True)
    p_dx = fds_namelist.get_by_label(fds_label="DX", remove=True)
    p_dy = fds_namelist.get_by_label(fds_label="DY", remove=True)
    p_dz = fds_namelist.get_by_label(fds_label="DZ", remove=True)
    if fds_namelist.fds_params:
        msg = f"MOVE namelist has unknown parameters: <{fds_namelist.fds_params}>"
        raise AssertionError(msg)
    # Get fds_param value
    x0 = p_x0 and p_x0.value or 0.0
    y0 = p_y0 and p_y0.value or 0.0
    z0 = p_z0 and p_z0.value or 0.0
    axis = p_axis and p_axis.value or (0.0, 0.0, 1.0)
    rot_angle = p_rot_angle and p_rot_angle.value or 0.0
    scale = p_scale and p_scale.value or 1.0
    scalex = p_scalex and p_scalex.value
    scaley = p_scaley and p_scaley.value
    scalez = p_scalez and p_scalez.value
    dx = p_dx and p_dx.value or 0.0
    dy = p_dy and p_dy.value or 0.0
    dz = p_dz and p_dz.value or 0.0
    # Set scale
    if scalex is None:
        scalex = scale
    if scaley is None:
        scaley = scale
    if scalez is None:
        scalez = scale
    # Build matrices as FDS does, then multiply them
    mt0 = Matrix().Translation((x0, y0, z0))
    mr = Matrix().Rotation(radians(rot_angle), 4, Vector(axis))
    msx = Matrix().Scale(scalex, 4, (1, 0, 0))
    msy = Matrix().Scale(scaley, 4, (0, 1, 0))
    msz = Matrix().Scale(scalez, 4, (0, 0, 1))
    mt1 = Matrix().Translation((dx, dy, dz))
    mt2 = Matrix().Translation((-x0, -y0, -z0))
    matrix = mt0 @ mr @ msx @ msy @ msz @ mt1 @ mt2
    return hid, matrix


def from_matrix(hid, matrix):
    """!
    Transform Blender transformation matrix to FDS MOVE namelist in FDSNamelist format.
    @param hid: Identifier string.
    @param matrix: Blender 4x4 transformation matrix.
    @return: MOVE FDSNamelist
    """

    """!
    Transform 4x4 Blender transformation matrix to FDS MOVE T34 param.
    """
    t34 = (
        matrix[0][0],
        matrix[1][0],
        matrix[2][0],
        matrix[0][1],
        matrix[1][1],
        matrix[2][1],
        matrix[0][2],
        matrix[1][2],
        matrix[2][2],
        matrix[0][3],
        matrix[1][3],
        matrix[2][3],
    )
    return FDSNamelist(
        fds_label="MOVE",
        fds_params=(
            FDSParam(fds_label="ID", value=hid),
            FDSParam(fds_label="T34", value=t34, precision=6),
        ),
    )
