"""!
BlenderFDS, translate Blender object geometry to FDS XB notation.
"""

import logging
from ...types import BFException
from ... import utils
from ..ON_MULT import multiply_xbs
from .calc_voxels import get_voxels
from .calc_pixels import get_pixels


log = logging.getLogger(__name__)

# TODO world not used
def _ob_to_xbs_voxels(context, ob, world) -> tuple((list, list)):
    """!
    Transform Object solid geometry to xbs notation (voxelization).
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return xbs notation and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    xbs, voxel_size = get_voxels(context=context, ob=ob)
    if not xbs:
        raise BFException(ob, "XB: No exported voxels!")
    msgs = list((f"XB Voxels: {len(xbs)} | Resolution: {voxel_size:.3f} m",))
    return xbs, msgs


# TODO world not used
def _ob_to_xbs_pixels(context, ob, world) -> tuple((list, list)):
    """!
    Transform Object flat geometry to xbs notation (flat voxelization).
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return xbs notation (flat voxelization) and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    xbs, voxel_size = get_pixels(context=context, ob=ob)
    if not xbs:
        raise BFException(ob, "XB: No exported pixels!")
    scale_length = context.scene.unit_settings.scale_length
    res = voxel_size * scale_length
    msgs = list((f"XB Pixels: {len(xbs)} | Resolution: {res:.3f} m",))
    return xbs, msgs


def _ob_to_xbs_bbox(context, ob, world) -> tuple((list, list)):
    """!
    Transform Object solid geometry to xbs notation (bounding box).
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return xbs notation (bounding box) and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    xb = utils.geometry.get_bbox_xb(context, ob, world=world)
    xbs, msgs = list((xb,)), list()
    return xbs, msgs


def _ob_to_xbs_faces(context, ob, world) -> tuple((list, list)):
    """!
    Transform Object flat faces to xbs notation (faces).
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return xbs notation (faces) and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    xbs = list()
    bm = utils.geometry.get_object_bmesh(context, ob, world=world)
    bm.faces.ensure_lookup_table()
    scale_length = context.scene.unit_settings.scale_length
    for face in bm.faces:
        verts = face.verts
        xs, ys, zs = tuple(zip(*(v.co for v in verts)))
        x0, x1, y0, y1, z0, z1 = (min(xs), max(xs), min(ys), max(ys), min(zs), max(zs))
        deltas = [(x1 - x0, 2), (y1 - y0, 1), (z1 - z0, 0)]
        deltas.sort()
        if deltas[0][1] == 2:
            x1 = x0 = (x0 + x1) / 2.0
        if deltas[0][1] == 1:
            y1 = y0 = (y0 + y1) / 2.0
        if deltas[0][1] == 0:
            z1 = z0 = (z0 + z1) / 2.0
        xbs.append(tuple(c * scale_length for c in (x0, x1, y0, y1, z0, z1)))
    bm.free()
    xbs.sort()
    if not xbs:
        raise BFException(ob, "XB: No exported faces!")
    msgs = list((f"XB Faces: {len(xbs)}",))
    return xbs, msgs


def _ob_to_xbs_edges(context, ob, world) -> tuple((list, list)):
    """!
    Transform Object edges in xbs notation (edges).
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return xbs notation (edges) and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    xbs = list()
    bm = utils.geometry.get_object_bmesh(context, ob, world=world)
    bm.edges.ensure_lookup_table()
    scale_length = context.scene.unit_settings.scale_length
    for edge in bm.edges:
        x0, y0, z0 = edge.verts[0].co
        x1, y1, z1 = edge.verts[1].co
        xbs.append(tuple(c * scale_length for c in (x0, x1, y0, y1, z0, z1)))
    bm.free()
    xbs.sort()
    if not xbs:
        raise BFException(ob, "XB: No exported edges!")
    msgs = list((f"XB Edges: {len(xbs)}",))
    return xbs, msgs


_choice_to_xbs = {
    "BBOX": _ob_to_xbs_bbox,
    "VOXELS": _ob_to_xbs_voxels,
    "FACES": _ob_to_xbs_faces,
    "PIXELS": _ob_to_xbs_pixels,
    "EDGES": _ob_to_xbs_edges,
}


def ob_to_xbs(context, ob, bf_xb, world=True) -> tuple((list, list, list)):
    """!
    Transform Object geometry according to bf_xb to FDS notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bf_xb: string in (BBOX, VOXELS, FACES, PIXELS, EDGES).
    @param world: True to return the object in world coordinates.
    @return the FDS notation and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    # Calc xbs and msgs
    xbs, msgs = tuple(), tuple()
    if ob.bf_xb_export:
        if ob.get("ob_to_xbs_cache") is None:  # no cache?
            ob["ob_to_xbs_cache"] = _choice_to_xbs[bf_xb](context, ob, world)
        xbs, msgs = ob["ob_to_xbs_cache"]

    # Calc hids
    n = ob.name
    match ob.bf_id_suffix:
        case "IDI":
            hids = (f"{n}_{i}" for i, _ in enumerate(xbs))
        case "IDX":
            hids = (f"{n}_x{xb[0]:+.3f}" for xb in xbs)
        case "IDY":
            hids = (f"{n}_y{xb[2]:+.3f}" for xb in xbs)
        case "IDZ":
            hids = (f"{n}_z{xb[4]:+.3f}" for xb in xbs)
        case "IDXY":
            hids = (f"{n}_x{xb[0]:+.3f}_y{xb[2]:+.3f}" for xb in xbs)
        case "IDXZ":
            hids = (f"{n}_x{xb[0]:+.3f}_z{xb[4]:+.3f}" for xb in xbs)
        case "IDYZ":
            hids = (f"{n}_y{xb[2]:+.3f}_z{xb[4]:+.3f}" for xb in xbs)
        case "IDXYZ":
            hids = (f"{n}_x{xb[0]:+.3f}_y{xb[2]:+.3f}_z{xb[4]:+.3f}" for xb in xbs)
        case _:
            raise AssertionError(f"Unknown suffix <{ob.bf_id_suffix}>")

    # Multiply
    hids, xbs, msgs, _ = multiply_xbs(context=context, ob=ob, hids=hids, xbs=xbs, msgs=msgs)

    return tuple(hids), tuple(xbs), tuple(msgs)
