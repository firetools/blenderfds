"""!
BlenderFDS, translate geometry from FDS XB notation to a Blender mesh.
"""

import bmesh, logging
from mathutils import Matrix, Vector
from ...types import BFException

log = logging.getLogger(__name__)

EPSILON = 1e-5  # TODO unify epsilon mgmt


def _xbs_edges_to_bm(bm, xbs, scale_length):
    for xb in xbs:
        x0, x1, y0, y1, z0, z1 = (c / scale_length for c in xb)
        v0 = bm.verts.new((x0, y0, z0))
        v1 = bm.verts.new((x1, y1, z1))
        bm.edges.new((v0, v1))

def _xbs_faces_to_bm(bm, xbs, scale_length):
    for xb in xbs:
        x0, x1, y0, y1, z0, z1 = (c / scale_length for c in xb)
        if abs(x1 - x0) <= EPSILON:  # i face
            v0 = bm.verts.new((x0, y0, z0))
            v1 = bm.verts.new((x0, y1, z0))
            v2 = bm.verts.new((x0, y1, z1))
            v3 = bm.verts.new((x0, y0, z1))
        elif abs(y1 - y0) <= EPSILON:  # j face
            v0 = bm.verts.new((x0, y0, z0))
            v1 = bm.verts.new((x1, y0, z0))
            v2 = bm.verts.new((x1, y0, z1))
            v3 = bm.verts.new((x0, y0, z1))
        elif abs(z1 - z0) <= EPSILON:  # k face
            v0 = bm.verts.new((x0, y0, z0))
            v1 = bm.verts.new((x0, y1, z0))
            v2 = bm.verts.new((x1, y1, z0))
            v3 = bm.verts.new((x1, y0, z0))
        else:
            raise AssertionError(f"Unrecognized face <{xb}> in XB")
        bm.faces.new((v0, v1, v2, v3))

def _xbs_bbox_to_bm(bm, xbs, scale_length):
    for xb in xbs:
        x0, x1, y0, y1, z0, z1 = (c / scale_length for c in xb)
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

def _is_faces(xbs, scale_length):
    """!
    Check if xbs are only faces.
    """
    for xb in xbs:
        x0, x1, y0, y1, z0, z1 = (c / scale_length for c in xb)
        if abs(x1 - x0) > EPSILON and abs(y1 - y0) > EPSILON and abs(z1 - z0) > EPSILON:
            return False
    return True

_xbs_to_bm = {
    "BBOX": _xbs_bbox_to_bm,
    "VOXELS": _xbs_bbox_to_bm,
    "PIXELS": _xbs_bbox_to_bm,
    "EDGES": _xbs_edges_to_bm,
    "FACES": _xbs_faces_to_bm,  
}

def xbs_to_ob(context, ob, xbs, bf_xb=None, add=False, set_origin=False):
    """!
    Set xbs geometry ((x0,x1,y0,y1,z0,z1,), ...) to Blender Object.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param xbs: the xbs values.
    @param bf_xb: the xb parameter between BBOX, VOXELS, FACES, PIXELS, EDGES.
    @param add: add to existing Mesh.
    @param set_origin: set reasonable origin.
    @return the new xb parameter between BBOX, VOXELS, FACES, PIXELS, EDGES.
    """
    if not xbs:
        return "BBOX"
    # Generate the new bmesh
    bm = bmesh.new()
    if add:  # get existing data in world coo
        bm.from_mesh(ob.data)
        bm.transform(ob.matrix_world)
    scale_length = context.scene.unit_settings.scale_length
    if not bf_xb:
        if _is_faces(xbs=xbs, scale_length=scale_length):
            bf_xb = "FACES"
        else:
            bf_xb = "BBOX"
    # Inject geometry
    _xbs_to_bm[bf_xb](bm=bm, xbs=xbs, scale_length=scale_length)
    # Transform to local coo
    if set_origin:
        origin = Vector((xbs[0][0],xbs[0][2],xbs[0][4]))
        ma = Matrix.Translation(-origin)
        ob.matrix_world = Matrix.Translation(+origin)
    else:
        ma = ob.matrix_world.inverted()
    bm.transform(ma)
    bm.to_mesh(ob.data)
    bm.free()
    return bf_xb

def set_materials(ob) -> None:
    """!
    Set Blender Materials from Material Slots to Object Mesh faces.
    @param ob: the Blender object.
    """
    me = ob.data
    match len(me.materials):
        case 0:  # no SURF_ID
            return
        case 1:  # SURF_ID = 'A'
            for face in me.polygons:
                face.material_index = 0
        case 3:  # SURF_IDS = 'A', 'B', 'C' (top, sides, bottom)
            if len(me.polygons) > 6:
                raise AssertionError(f"Too many polygons in Object {ob.name}: {len(me.polygons)}")
            for iface, face in enumerate(me.polygons):
                if iface % 6 == 5:
                    face.material_index = 0  # top
                elif iface % 6 == 4:
                    face.material_index = 2  # bottom
                else:
                    face.material_index = 1  # sides
        case 6:  # SURF_ID6 = ... (x0, x1, y0, y1, z0, z1)
            if len(me.polygons) > 6:
                raise AssertionError(f"Too many polygons in Object {ob.name}: {len(me.polygons)}")
            for iface, face in enumerate(me.polygons):
                face.material_index = iface % 6
        case _:
            raise BFException(ob, f"Wrong number of Material Slots")
