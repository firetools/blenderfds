# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, translate geometry from FDS XYZ notation to a Blender mesh.
"""

import bmesh, logging
from mathutils import Matrix, Vector

log = logging.getLogger(__name__)


def _xyzs_to_bm(bm, xyzs, scale_length):
    for xyz in xyzs:
        bm.verts.new(c / scale_length for c in xyz)


def xyzs_to_ob(context, ob, xyzs, add=False, set_origin=False) -> str():
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Object.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param xyzs: the xyzs vertices.
    @param add: add to existing Mesh.
    @param set_origin: set reasonable origin.
    @return: the new bf_xyz in CENTER or VERTICES
    """
    if not xyzs:
        return "VERTICES"
    # Generate the new bmesh
    bm = bmesh.new()
    if add:  # get existing data in world coo
        bm.from_mesh(ob.data)
        bm.transform(ob.matrix_world)
    scale_length = context.scene.unit_settings.scale_length
    # Inject geometry
    if len(xyzs) > 1:
        _xyzs_to_bm(bm, xyzs, scale_length)
    # Transform to local coo
    if set_origin:
        origin = Vector(xyzs[0])
        ma = Matrix.Translation(-origin)
        ob.matrix_world = Matrix.Translation(+origin)
    else:
        ma = ob.matrix_world.inverted()
        # same ob.matrix_world
    bm.transform(ma)
    bm.to_mesh(ob.data)
    bm.free()
    if len(xyzs) == 1:
        return "CENTER"
    else:
        return "VERTICES"
