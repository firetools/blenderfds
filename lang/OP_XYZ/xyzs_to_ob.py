"""!
BlenderFDS, translate geometry from FDS XYZ notation to a Blender mesh.
"""

import bmesh, logging
from mathutils import Matrix, Vector
from ...types import BFException

log = logging.getLogger(__name__)

# FIXME matrix, extend


def xyzs_vertices_to_mesh(context, me, xyzs) -> None:
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xyzs: the xyzs vertices.
    """
    bm = bmesh.new()
    bm.from_mesh(me)  # add to current mesh
    scale_length = context.scene.unit_settings.scale_length
    for xyz in xyzs:
        bm.verts.new(
            (xyz[0] / scale_length, xyz[1] / scale_length, xyz[2] / scale_length)
        )
    bm.to_mesh(me)
    bm.free()


def xyzs_to_ob(context, ob, xyzs) -> str():
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Object.
    @param xyzs: the xyzs vertices.
    @param context: the Blender context.
    @param ob: the Blender object.
    @return: the new bf_xyz in CENTER or VERTICES
    """
    # log.debug(f"Importing xyzs to Object <{ob.name}>")
    xyzs_vertices_to_mesh(context=context, me=ob.data, xyzs=xyzs)
    # Set origin
    try:  # protect from empty
        origin = Vector(xyzs[0])
    except IndexError:
        pass
    else:
        ob.data.transform(Matrix.Translation(-origin))
        ob.matrix_world = Matrix.Translation(origin) @ ob.matrix_world
    if len(xyzs) == 1:
        return "CENTER"
    else:
        return "VERTICES"
