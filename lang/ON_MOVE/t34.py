# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from math import radians
from mathutils import Matrix, Vector

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


def calc_t34(m) -> tuple:
    """!
    Transform the Blender transformation matrix into the FDS MOVE T34 notation.
    """
    return tuple(m[i][j] for j in range(4) for i in range(3))


def calc_bl_matrix(
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
    Transform the FDS MOVE notation into a Blender 4x4 transformation matrix.
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
