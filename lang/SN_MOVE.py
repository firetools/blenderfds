import logging
from math import radians
from mathutils import Matrix, Vector
from ..types import BFNamelistSc, BFNamelistOb, FDSNamelist, FDSParam, BFNotImported

log = logging.getLogger(__name__)


# As a reminder:
# context.object.matrix_world =
#     Matrix(((1.0, 0.0, 0.0, 1.0),
#             (0.0, 1.0, 0.0, 2.0),
#             (0.0, 0.0, 1.0, 3.0),
#             (0.0, 0.0, 0.0, 1.0)))
# context.object.matrix_world[1][3] = 2.
# t34 = 1.0, 0.0, 0.0,   0.0, 1.0, 0.0,   0.0, 0.0, 1.0,   1.0, 2.0, 3.0

# m = mathutils.Matrix.Rotation(0.7, 4, "Z")
# bpy.data.objects["Cube"].matrix_world @= m  # apply rotation
# bpy.data.objects["Cube"].matrix_world @= m.copy().invert() # apply inverse rotation


# Export functions


def bl_matrix_to_t34(m) -> tuple:
    """!
    Transform the Blender transformation matrix in the FDS MOVE T34 notation.
    """
    return tuple(m[i][j] for j in range(4) for i in range(3))


# Import functions


def fds_move_to_bl_matrix(
    t34=None,
    dx=0.0,
    dy=0.0,
    dz=0.0,
    scale=None,
    scalex=1.0,
    scaley=1.0,
    scalez=1.0,
    x0=0.0,
    y0=0.0,
    z0=0.0,
    rotation_angle=0.0,
    axis=(0.0, 0.0, 1.0),
) -> Matrix:
    """!
    Transform the FDS MOVE notation in a Blender 4x4 transformation matrix.
    """
    if t34:
        m = list(tuple(t34[j * 3 + i] for j in range(4)) for i in range(3))
        m.append((0.0, 0.0, 0.0, 1.0))  # add last row
        m = Matrix(m)
    else:
        if scale:
            scalex = scaley = scalez = scale
        m = (
            Matrix().Translation((dx, dy, dz))  # last applied
            @ Matrix().Scale(scalex, 4, (1, 0, 0))
            @ Matrix().Scale(scaley, 4, (0, 1, 0))
            @ Matrix().Scale(scalez, 4, (0, 0, 1))
            @ Matrix().Translation((x0, y0, z0))
            @ Matrix().Rotation(radians(rotation_angle), 4, Vector(axis))
            @ Matrix().Translation((-x0, -y0, -z0))  # first applied
        )
    return m


# Class


class SN_MOVE(BFNamelistSc):
    # This namelist is not displayed in the bf_namelist_cls menu,
    # and has no panel. Used for importing only, it translates
    # fds transformations in Blender matrix of a scene dict
    label = "MOVE"
    description = "Geometric transformations"
    enum_id = False  # no bf_namelist_cls menu, no automatic export
    fds_label = "MOVE"

    def to_fds_namelist(self, context):
        pass

    def from_fds(self, context, fds_namelist):
        # Read fds_params
        ps = {  # label: default value
            "ID": None,
            "T34": None,
            "X0": 0.0,
            "Y0": 0.0,
            "Z0": 0.0,
            "AXIS": (0.0, 0.0, 1.0),
            "ROTATION_ANGLE": 0.0,
            "SCALE": None,
            "SCALEX": 1.0,
            "SCALEY": 1.0,
            "SCALEZ": 1.0,
            "DX": 0.0,
            "DY": 0.0,
            "DZ": 0.0,
        }
        for fds_label in ps:
            fds_param = fds_namelist.get_fds_param(fds_label=fds_label, remove=True)
            if fds_param:
                ps[fds_label] = fds_param.get_value(context)  # assign value
        # Check ID
        hid = ps["ID"]
        if not hid:
            raise BFNotImported(self, f"Missing ID in: {fds_namelist}")
        # Prepare Scene dict
        if "bf_move_coll" not in context.scene:
            context.scene["bf_move_coll"] = dict()
        bf_move_coll = context.scene["bf_move_coll"]
        # Treat T34
        if ps["T34"]:
            m = fds_move_to_bl_matrix(t34=ps["T34"])
        # Treat other cases
        else:
            m = fds_move_to_bl_matrix(
                x0=ps["X0"],
                y0=ps["Y0"],
                z0=ps["Z0"],
                axis=ps["AXIS"],
                rotation_angle=ps["ROTATION_ANGLE"],
                scale=ps["SCALE"],
                scalex=ps["SCALEX"],
                scaley=ps["SCALEY"],
                scalez=ps["SCALEZ"],
                dx=ps["DX"],
                dy=ps["DY"],
                dz=ps["DZ"],
            )
        # Set Scene dict
        bf_move_coll[hid] = m


class ON_MOVE(BFNamelistOb):
    # This namelist is not displayed in the bf_namelist_cls menu,
    # and has no panel. Used for exporting only.
    label = "MOVE"
    description = "Geometric transformations"
    enum_id = False  # no bf_namelist_cls menu, no automatic export
    fds_label = "MOVE"

    def to_fds_namelist(self, context):
        t34 = bl_matrix_to_t34(m=self.element.matrix_world)
        return FDSNamelist(
            fds_label=self.fds_label,
            fds_params=(
                FDSParam(fds_label="ID", value=f"{self.element.name}_move"),
                FDSParam(fds_label="T34", value=t34, precision=6),
            ),
        )

    def from_fds(self, context, fds_namelist):
        raise Exception("The Object ON_MOVE cannot export!")