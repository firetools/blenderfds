"""!
BlenderFDS, translate geometry from FDS XB notation to a Blender mesh.
"""

import bmesh, logging
from mathutils import Matrix, Vector
from ...types import BFException

log = logging.getLogger(__name__)

epsilon = 1e-5  # TODO unify epsilon mgmt


def xbs_edges_to_mesh(context, me, xbs, matrix=None, add=False):
    """!
    Set xbs edges ((x0,x1,y0,y1,z0,z1,), ...) to Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xbs: the xbs edges.
    @param matrix: transform bmesh by matrix before importing.
    @param add: if set, add to existing Mesh.
    """
    bm = bmesh.new()
    if add:
        bm.from_mesh(me)
    scale_length = context.scene.unit_settings.scale_length
    for xb in xbs:
        x0, x1, y0, y1, z0, z1 = (coo / scale_length for coo in xb)
        v0 = bm.verts.new((x0, y0, z0))
        v1 = bm.verts.new((x1, y1, z1))
        bm.edges.new((v0, v1))
    if matrix:
        bm.transform(matrix)
    bm.to_mesh(me)
    bm.free()


def xbs_faces_to_mesh(context, me, xbs, matrix=None, add=False):
    """!
    Set xbs faces ((x0,x1,y0,y1,z0,z1,), ...) to Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xbs: the xbs edges.
    @param matrix: transform bmesh by matrix before importing.
    @param add: if set, add to existing Mesh.
    """
    bm = bmesh.new()
    if add:
        bm.from_mesh(me)
    scale_length = context.scene.unit_settings.scale_length
    for xb in xbs:
        x0, x1, y0, y1, z0, z1 = (coo / scale_length for coo in xb)
        if abs(x1 - x0) <= epsilon:
            v0 = bm.verts.new((x0, y0, z0))
            v1 = bm.verts.new((x0, y1, z0))
            v2 = bm.verts.new((x0, y1, z1))
            v3 = bm.verts.new((x0, y0, z1))
        elif abs(y1 - y0) <= epsilon:
            v0 = bm.verts.new((x0, y0, z0))
            v1 = bm.verts.new((x1, y0, z0))
            v2 = bm.verts.new((x1, y0, z1))
            v3 = bm.verts.new((x0, y0, z1))
        elif abs(z1 - z0) <= epsilon:
            v0 = bm.verts.new((x0, y0, z0))
            v1 = bm.verts.new((x0, y1, z0))
            v2 = bm.verts.new((x1, y1, z0))
            v3 = bm.verts.new((x1, y0, z0))
        else:
            raise BFException(me, f"Unrecognized face <{xb}> in XB")
        bm.faces.new((v0, v1, v2, v3))
    if matrix:
        bm.transform(matrix)
    bm.to_mesh(me)
    bm.free()


def xbs_bbox_to_mesh(context, me, xbs, matrix=None, add=False, set_materials=False):
    """!
    Set xbs bboxes ((x0,x1,y0,y1,z0,z1,), ...) to Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xbs: the xbs edges.
    @param set_materials: if True set material_slots to faces
    @param matrix: transform bmesh by matrix before importing.
    @param add: if set, add to existing Mesh.
    """
    bm = bmesh.new()
    if add:
        bm.from_mesh(me)
    scale_length = context.scene.unit_settings.scale_length
    for xb in xbs:
        x0, x1, y0, y1, z0, z1 = (coo / scale_length for coo in xb)
        v000 = bm.verts.new((x0, y0, z0))
        v100 = bm.verts.new((x1, y0, z0))
        v110 = bm.verts.new((x1, y1, z0))
        v010 = bm.verts.new((x0, y1, z0))
        v001 = bm.verts.new((x0, y0, z1))
        v101 = bm.verts.new((x1, y0, z1))
        v111 = bm.verts.new((x1, y1, z1))
        v011 = bm.verts.new((x0, y1, z1))
        bm.faces.new((v000, v001, v011, v010))  # -x 0
        bm.faces.new((v111, v101, v100, v110))  # +x 1
        bm.faces.new((v000, v100, v101, v001))  # -y 2
        bm.faces.new((v111, v110, v010, v011))  # +y 3
        bm.faces.new((v000, v010, v110, v100))  # -z 4 bottom
        bm.faces.new((v111, v011, v001, v101))  # +z 5 top
    if matrix:
        bm.transform(matrix)
    bm.to_mesh(me)
    bm.free()
    # Assign material_slots to faces
    if set_materials:
        match len(me.materials):
            case 0:  # no SURF_ID
                return
            case 1:  # SURF_ID = 'A'
                for face in me.polygons:
                    face.material_index = 0
            case 3:  # SURF_IDS = 'A', 'B', 'C' (top, sides, bottom)
                for iface, face in enumerate(me.polygons):
                    if iface % 6 == 5:
                        face.material_index = 0  # top
                    elif iface % 6 == 4:
                        face.material_index = 2  # bottom
                    else:
                        face.material_index = 1  # sides
            case 6:  # SURF_ID6 = ... (x0, x1, y0, y1, z0, z1)
                for iface, face in enumerate(me.polygons):
                    face.material_index = iface % 6
            case _:
                raise BFException(me, "Bad GEOM XB: Wrong SURF_ID/IDS/ID6 len")


xbs_to_mesh = {
    "BBOX": xbs_bbox_to_mesh,
    "VOXELS": xbs_bbox_to_mesh,
    "PIXELS": xbs_bbox_to_mesh,
    "EDGES": xbs_edges_to_mesh,
    "FACES": xbs_faces_to_mesh,
}


def xbs_to_ob(context, ob, xbs, bf_xb=None, add=False, set_origin=False):
    """!
    Set xbs geometry ((x0,x1,y0,y1,z0,z1,), ...) to Blender Object.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param xbs: the xbs edges.
    @param bf_xb: the xb parameter between BBOX, VOXELS, FACES, PIXELS, EDGES.
    @param add: if set, add to existing Mesh.
    @param set_origin: if set, set reasonable origin.
    @return the new xb parameter between BBOX, VOXELS, FACES, PIXELS, EDGES.
    """
    if not xbs:
        return "BBOX"
    if not add and set_origin:
        origin = Vector((xbs[0][0],xbs[0][2],xbs[0][4]))
        matrix = Matrix.Translation(-origin)
        ob.matrix_world = Matrix.Translation(+origin)
    else:
        matrix = ob.matrix_world.inverted()
    if not bf_xb:
        bf_xb = "FACES"
    try:
        xbs_to_mesh[bf_xb](
                context=context,
                me=ob.data,
                xbs=xbs,
                matrix=matrix,
                add=add,
            )
    except BFException:
        bf_xb = "BBOX"
        xbs_to_mesh[bf_xb](
                context=context,
                me=ob.data,
                xbs=xbs,
                matrix=matrix,
                add=add,
            )
    return bf_xb
