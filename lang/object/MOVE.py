import logging
from math import radians
from mathutils import Matrix, Vector
from bpy.types import Object
from ...types import BFParam, BFParamStr, BFNamelistOb

log = logging.getLogger(__name__)


class OP_MOVE_ID(BFParamStr):
    label = "MOVE_ID"
    description = "Export this namelist with a corresponding MOVE namelist"
    fds_label = "ID"
    bpy_type = Object
    bpy_idname = "bf_move_id"
    bpy_export = "bf_move_id_export"
    bpy_export_default = False

    @property
    def exported(self):
        return True

    @property
    def value(self):  # never empty
        return self.element.bf_move_id or f"{self.element.name}_move"


# As a reminder:
# context.object.matrix_world =
#     Matrix(((1.0, 0.0, 0.0, 1.0),
#             (0.0, 1.0, 0.0, 2.0),
#             (0.0, 0.0, 1.0, 3.0),
#             (0.0, 0.0, 0.0, 1.0)))
# context.object.matrix_world[1][3] = 2.
# t34 = 1.0, 0.0, 0.0,   0.0, 1.0, 0.0,   0.0, 0.0, 1.0,   1.0, 2.0, 3.0

# And:
# m = mathutils.Matrix.Rotation(0.7, 4, "Z")
# bpy.data.objects["Cube"].matrix_world *= m  # apply rotation
# bpy.data.objects["Cube"].matrix_world *= m.copy().invert() # apply inverse rotation


def t34_to_bl_matrix(t34) -> Matrix:
    m = list(tuple(t34[j * 3 + i] for j in range(4)) for i in range(3))
    m.append((0.0, 0.0, 0.0, 1.0))  # add last row
    return Matrix(m)


def t34_to_ob(t34, ob):
    m = t34_to_bl_matrix(t34)
    if m.is_orthogonal:  # no shearing and skewing
        ob.matrix_world = m
    else:
        # get translation, rotation and scaling matrices
        loc, rot, sca = m.decompose()
        mloc = Matrix.Translation(loc)
        mrot = rot.to_matrix().to_4x4()
        msca = Matrix.Identity(4)
        msca[0][0], msca[1][1], msca[2][2] = sca
        # get the matrix that can be used for matrix_world
        mout = mloc @ mrot @ msca
        # get the shearing and skewing in another matrix
        mh = mout.inverted_safe() @ m
        # apply to object and mesh
        ob.matrix_world = mout
        ob.data.transform(mh)


def bl_matrix_to_t34(m) -> list:
    return tuple(m[i][j] for j in range(4) for i in range(3))


def ob_to_t34(ob) -> list:
    return bl_matrix_to_t34(ob.matrix_world)


class OP_MOVE_T34(BFParam):
    label = "T34"
    description = "Geometric transformation matrix"
    fds_label = "T34"
    bpy_type = Object
    bpy_other = {"precision": 6}

    @property
    def value(self):
        return ob_to_t34(self.element)

    def set_value(self, context, value):
        t34_to_ob(value, self.element)


class ON_MOVE(BFNamelistOb):
    label = "MOVE"
    description = "Transformation FIXME"
    enum_id = False  # not displayed in namelist type menu, not auto called for export
    fds_label = "MOVE"

    bf_params = (
        OP_MOVE_ID,
        OP_MOVE_T34,
    )

    def from_fds(self, context, fds_namelist, free_text=None):
        # T34
        if fds_namelist.get_by_label(fds_label="T34"):
            super().from_fds(context, fds_namelist, free_text=free_text)  # get the rest
            return
        # Other fds_params
        ps = {  # label, default value
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
        for key in ps:  # read
            fds_param = fds_namelist.get_by_label(fds_label=key, remove=True)
            if fds_param:  # assign value
                ps[key] = fds_param.value
        if ps["SCALE"]:
            ps["SCALEX"], ps["SCALEY"], ps["SCALEZ"] = (ps["SCALE"],) * 3
        # Assign transformation matrix, as FDS does
        self.element.matrix_world @= (
            Matrix().Translation((ps["DX"], ps["DY"], ps["DZ"]))  # last applied
            @ Matrix().Scale(ps["SCALEX"], 4, (1, 0, 0))
            @ Matrix().Scale(ps["SCALEY"], 4, (0, 1, 0))
            @ Matrix().Scale(ps["SCALEZ"], 4, (0, 0, 1))
            @ Matrix().Translation((ps["X0"], ps["Y0"], ps["Z0"]))
            @ Matrix().Rotation(radians(ps["ROTATION_ANGLE"]), 4, Vector(ps["AXIS"]))
            @ Matrix().Translation((-ps["X0"], -ps["Y0"], -ps["Z0"]))
        )
