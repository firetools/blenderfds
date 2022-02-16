"""!
BlenderFDS, translate geometry from FDS XYZ notation to a Blender mesh.
"""

import bmesh, logging
from mathutils import Matrix, Vector

log = logging.getLogger(__name__)


def xyzs_vertices_to_mesh(context, me, xyzs, matrix=None, add=False) -> None:
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xyzs: the xyzs vertices.
    @param matrix: transform bmesh by matrix before importing.
    @param add: if set, add to existing Mesh.
    """
    bm = bmesh.new()
    if add:
        bm.from_mesh(me)
    scale_length = context.scene.unit_settings.scale_length
    for xyz in xyzs:
        bm.verts.new(
            (xyz[0] / scale_length, xyz[1] / scale_length, xyz[2] / scale_length)
        )
    if matrix:
        bm.transform(matrix)
    bm.to_mesh(me)
    bm.free()


def xyzs_to_ob(context, ob, xyzs, is_world=True, add=False, set_origin=False) -> str():
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Object.
    @param xyzs: the xyzs vertices.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param is_world: coordinates are in world ref.
    @param add: if set, add to existing Mesh.
    @param set_origin: if set, set reasonable origin.
    @return: the new bf_xyz in CENTER or VERTICES
    """
    if not xyzs:
        return "VERTICES"
    if not add and set_origin:
        origin = Vector(xyzs[0])
        matrix = Matrix.Translation(-origin)
        ob.matrix_world = Matrix.Translation(+origin)
    else:
        matrix = ob.matrix_world.inverted()
    xyzs_vertices_to_mesh(
        context=context, me=ob.data, xyzs=xyzs, matrix=matrix, add=add
    )
    if len(xyzs) == 1:
        return "CENTER"
    else:
        return "VERTICES"
