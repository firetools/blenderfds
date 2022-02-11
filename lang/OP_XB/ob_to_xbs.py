"""!
BlenderFDS, translate Blender object geometry to FDS XB notation.
"""

import logging
from ...types import BFException
from ... import utils
from .calc_voxels import get_voxels
from .calc_pixels import get_pixels


log = logging.getLogger(__name__)


def _ob_to_xbs_voxels(context, ob, world):
    """!
    Transform Object solid geometry to xbs notation (voxelization).
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return xbs notation and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    xbs, voxel_size = get_voxels(context, ob, world)
    msgs = list((f"XB Voxels: {len(xbs)} | Resolution: {voxel_size:.3f} m",))
    return xbs, msgs


def _ob_to_xbs_pixels(context, ob, world):
    """!
    Transform Object flat geometry to xbs notation (flat voxelization).
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return xbs notation (flat voxelization) and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    xbs, voxel_size = get_pixels(context, ob, world)
    scale_length = context.scene.unit_settings.scale_length
    res = voxel_size * scale_length
    msgs = list((f"XB Pixels: {len(xbs)} | Resolution: {res:.3f} m",))
    return xbs, msgs


def _ob_to_xbs_bbox(context, ob, world):
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


def _ob_to_xbs_faces(context, ob, world):
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
        xbs.append(
            (
                x0 * scale_length,
                x1 * scale_length,
                y0 * scale_length,
                y1 * scale_length,
                z0 * scale_length,
                z1 * scale_length,
            )
        )
    bm.free()
    xbs.sort()
    if not xbs:
        raise BFException(ob, "XB: No exported faces!")
    msgs = list((f"XB Faces: {len(xbs)}",))
    return xbs, msgs


def _ob_to_xbs_edges(context, ob, world):
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
        pt0x, pt0y, pt0z = edge.verts[0].co
        pt1x, pt1y, pt1z = edge.verts[1].co
        xbs.append(
            (
                pt0x * scale_length,
                pt1x * scale_length,
                pt0y * scale_length,
                pt1y * scale_length,
                pt0z * scale_length,
                pt1z * scale_length,
            )
        )
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


def ob_to_xbs(context, ob, bf_xb, world=True):
    """!
    Transform Object geometry according to bf_xb to FDS notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bf_xb: string in (BBOX, VOXELS, FACES, PIXELS, EDGES).
    @param world: True to return the object in world coordinates.
    @return the FDS notation and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    if ob.get("ob_to_xbs_cache") is None:
        # Not available in cache, recalc
        ob["ob_to_xbs_cache"] = _choice_to_xbs[bf_xb](context, ob, world)
    xbs, msgs = ob["ob_to_xbs_cache"]
    return list(xbs), list(msgs)
