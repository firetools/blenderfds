"""!
BlenderFDS, translate Blender object geometry to FDS notation.
"""

import bpy, logging
from time import time
from ..types import BFException
from . import calc_voxels, calc_trisurfaces, utils


log = logging.getLogger(__name__)


# to GEOM in Blender units (no cache)


def ob_to_geom(context, ob, check=True, check_open=True, world=True):
    """!
    Transform Object geometry to FDS notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param check: True to check the bmesh sanity.
    @param check_open: True to check if bmesh is open.
    @param world: True to return the object in world coordinates.
    @return FDS GEOM notation as lists and message.
    """
    (
        fds_verts,
        fds_faces,
        fds_surfs,
        fds_faces_surfs,
    ) = calc_trisurfaces.get_fds_trisurface(
        context=context,
        ob=ob,
        check=check,
        check_open=check_open,
        world=world,
    )
    msg = f"GEOM Vertices: {len(fds_verts)} | Faces: {len(fds_faces)}"
    return fds_verts, fds_faces, fds_surfs, fds_faces_surfs, msg


# to XB in Blender units (cached)


def _ob_to_xbs_voxels(context, ob, world):
    """!
    Transform Object solid geometry to xbs notation (voxelization).
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return xbs notation and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    xbs, voxel_size = calc_voxels.get_voxels(context, ob, world)
    msg = f"XB Voxels: {len(xbs)} | Resolution: {voxel_size:.3f} m"
    return xbs, msg


def _ob_to_xbs_pixels(context, ob, world):
    """!
    Transform Object flat geometry to xbs notation (flat voxelization).
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return xbs notation (flat voxelization) and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    xbs, voxel_size = calc_voxels.get_pixels(context, ob, world)
    scale_length = context.scene.unit_settings.scale_length
    res = voxel_size * scale_length
    msg = f"XB Pixels: {len(xbs)} | Resolution: {res:.3f} m"
    return xbs, msg


def _ob_to_xbs_bbox(context, ob, world):
    """!
    Transform Object solid geometry to xbs notation (bounding box).
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return xbs notation (bounding box) and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    xb = utils.get_bbox_xb(context, ob, world=world)
    xbs, msg = list((xb,)), str()
    return xbs, msg


def _ob_to_xbs_faces(context, ob, world):
    """!
    Transform Object flat faces to xbs notation (faces).
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return xbs notation (faces) and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    xbs = list()
    bm = utils.get_object_bmesh(context, ob, world=world)
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
    msg = f"XB Faces: {len(xbs)}"
    return xbs, msg


def _ob_to_xbs_edges(context, ob, world):
    """!
    Transform Object edges in xbs notation (edges).
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return xbs notation (edges) and any error message: ((x0,x1,y0,y1,z0,z1,), ...), 'Msg'.
    """
    xbs = list()
    bm = utils.get_object_bmesh(context, ob, world=world)
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
    msg = f"XB Edges: {len(xbs)}"
    return xbs, msg


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
    if ob.get("ob_to_xbs_cache") is None:  # recalc
        ob["ob_to_xbs_cache"] = _choice_to_xbs[bf_xb](context, ob, world)
    return ob["ob_to_xbs_cache"]


# to XYZ in Blender units (no cache)


def _ob_to_xyzs_vertices(context, ob, world):
    """!
    Transform Object vertices to xyzs notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return the xyzs notation and any error message: ((x0,y0,z0,), ...), 'Msg'.
    """
    xyzs = list()
    bm = utils.get_object_bmesh(context, ob, world=world)
    # For each vertex...
    bm.verts.ensure_lookup_table()
    scale_length = context.scene.unit_settings.scale_length
    for v in bm.verts:
        pt0x, pt0y, pt0z = v.co
        xyzs.append((pt0x * scale_length, pt0y * scale_length, pt0z * scale_length))
    bm.free()
    xyzs.sort()
    if not xyzs:
        raise BFException(ob, "XYZ: No exported vertices!")
    msg = f"XYZ Vertices: {len(xyzs)}"
    return xyzs, msg


def _ob_to_xyzs_center(context, ob, world):
    """!
    Transform Object center to xyzs notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return the xyzs notation and any error message: ((x0,y0,z0,), ...), 'Msg'.
    """
    scale_length = context.scene.unit_settings.scale_length
    xyzs = [  # FIXME world
        (
            ob.location[0] * scale_length,
            ob.location[1] * scale_length,
            ob.location[2] * scale_length,
        )
    ]
    msg = str()
    return xyzs, msg


_choice_to_xyzs = {"CENTER": _ob_to_xyzs_center, "VERTICES": _ob_to_xyzs_vertices}


def ob_to_xyzs(context, ob, bf_xyz, world=True):
    """!
    Transform Object geometry according to bf_xyz to xyzs notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bf_xyz: string in (CENTER, VERTICES).
    @param world: True to return the object in world coordinates.
    @return the xyzs notation and any error message: ((x0,y0,z0,), ...), 'Msg'.
    """
    return _choice_to_xyzs[bf_xyz](context, ob, world)


# to PB in Blender units (no cache)


def _ob_to_pbs_planes(context, ob, world):
    """!
    Transform Object faces to pbs notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return the pbs notation and any error message: ((0,x3,), (1,x7,), (1,y9,), ...), 'Msg'.
    """
    pbs = list()
    xbs, msg = _ob_to_xbs_faces(context, ob, world)
    epsilon = 1e-5
    # For each face build a plane...
    for xb in xbs:
        if abs(xb[1] - xb[0]) < epsilon:
            pbs.append((0, xb[0]))  # PBX is 0
        elif abs(xb[3] - xb[2]) < epsilon:
            pbs.append((1, xb[2]))  # PBY is 1
        elif abs(xb[5] - xb[4]) < epsilon:
            pbs.append((2, xb[4]))  # PBZ is 2
        else:
            raise ValueError(
                "BFDS: Building planes impossible, problem in ob_to_xbs_faces."
            )
    pbs.sort()
    if not pbs:
        raise BFException(ob, "PB*: No exported planes!")
    msg = f"PB* Planes: {len(pbs)}"
    return pbs, msg


def ob_to_pbs(context, ob, bf_pb, world=True):
    """!
    Transform Object geometry according to bf_pb to pbs notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bf_pb: string in (PLANES,)
    @param world: True to return the object in world coordinates.
    @return the pbs notation and any error message.
    """
    return _ob_to_pbs_planes(context, ob, world=world)
