import logging
from math import radians
from mathutils import Matrix, Vector
from ...types import BFNamelistOb, FDSNamelist, FDSParam

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


def bl_matrix_to_t34(m) -> list:
    return tuple(m[i][j] for j in range(4) for i in range(3))


def ob_to_t34(ob) -> list:
    return bl_matrix_to_t34(ob.matrix_world)


def ob_to_other():  # TODO
    return  # dx, dy, dz, scale, scalex, scaley, scalez, x0, y0, z0, rotation_angle, axis, ob


# Import functions


def t34_to_bl_matrix(t34) -> Matrix:
    m = list(tuple(t34[j * 3 + i] for j in range(4)) for i in range(3))
    m.append((0.0, 0.0, 0.0, 1.0))  # add last row
    return Matrix(m)


def t34_to_ob(t34, ob):
    m = t34_to_bl_matrix(t34)
    if m.is_orthogonal:  # no shearing and skewing
        ob.matrix_world = m
    else:
        # Get translation, rotation and scaling matrices
        loc, rot, sca = m.decompose()
        mloc = Matrix.Translation(loc)
        mrot = rot.to_matrix().to_4x4()
        msca = Matrix.Identity(4)
        msca[0][0], msca[1][1], msca[2][2] = sca
        # Get the matrix that can be used for matrix_world
        mout = mloc @ mrot @ msca  # do not forget, the order is important
        # Get the shearing and skewing in another matrix
        mh = mout.inverted_safe() @ m
        # Apply to Object and Mesh
        ob.matrix_world = mout
        ob.data.transform(mh)


def other_to_ob(
    dx, dy, dz, scale, scalex, scaley, scalez, x0, y0, z0, rotation_angle, axis, ob
):
    if scale:
        scalex = scaley = scalez = scale
    # Apply to Object
    ob.matrix_world = (
        Matrix().Translation((dx, dy, dz))  # last applied
        @ Matrix().Scale(scalex, 4, (1, 0, 0))
        @ Matrix().Scale(scaley, 4, (0, 1, 0))
        @ Matrix().Scale(scalez, 4, (0, 0, 1))
        @ Matrix().Translation((x0, y0, z0))
        @ Matrix().Rotation(radians(rotation_angle), 4, Vector(axis))
        @ Matrix().Translation((-x0, -y0, -z0))  # first applied
        @ ob.matrix_world
    )


# Class


class ON_MOVE(BFNamelistOb):
    # This namelist is not displayed in the bf_namelist_cls menu,
    # and has no panel. When importing, it is used to translate
    # fds transformations in Blender matrix_world (and Mesh shearing and skewing)
    label = "MOVE"
    description = "Geometric transformations"
    enum_id = False  # no bf_namelist_cls menu, no automatic export
    fds_label = "MOVE"

    def to_fds_namelist(self, context):
        return FDSNamelist(
            fds_label=self.fds_label,
            fds_params=(
                FDSParam(fds_label="ID", value=f"{self.element.name}_move"),
                FDSParam(fds_label="T34", value=ob_to_t34(self.element), precision=6),
            ),
        )

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
        # Treat T34
        if ps["T34"]:
            t34_to_ob(
                t34=ps["T34"],
                ob=self.element,
            )
        # Treat other cases
        else:
            other_to_ob(
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
                ob=self.element,
            )
