"""!
BlenderFDS, translate Blender object geometry to FDS XYZ notation.
"""

import logging
from ...types import BFException
from ... import utils

log = logging.getLogger(__name__)


def _ob_to_xyzs_vertices(context, ob, world) -> tuple((list, list)):
    """!
    Transform Object vertices to xyzs notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return the xyzs notation and any error message: ((x0,y0,z0,), ...), 'Msg'.
    """
    bm = utils.geometry.get_object_bmesh(context, ob, world=world)
    bm.verts.ensure_lookup_table()
    scale_length = context.scene.unit_settings.scale_length
    xyzs = list(tuple(c * scale_length for c in v.co) for v in bm.verts)
    bm.free()
    xyzs.sort()
    if not xyzs:
        raise BFException(ob, "XYZ: No exported vertices!")
    msgs = list((f"XYZ Vertices: {len(xyzs)}",))
    return xyzs, msgs


def _ob_to_xyzs_center(context, ob, world) -> tuple((list, list)):
    """!
    Transform Object center to xyzs notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return the xyzs notation and any error message: ((x0,y0,z0,), ...), 'Msg'.
    """
    scale_length = context.scene.unit_settings.scale_length
    if world:
        xyzs = list((tuple(l * scale_length for l in ob.location),))
    else:
        xyzs = list(((0.0, 0.0, 0.0),))
    msgs = list()
    return xyzs, msgs


_choice_to_xyzs = {"CENTER": _ob_to_xyzs_center, "VERTICES": _ob_to_xyzs_vertices}


def ob_to_xyzs(context, ob, bf_xyz, world=True) -> tuple((list, list)):
    """!
    Transform Object geometry according to bf_xyz to xyzs notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bf_xyz: string in (CENTER, VERTICES).
    @param world: True to return the object in world coordinates.
    @return the xyzs notation and any error message: ((x0,y0,z0,), ...), 'Msg'.
    """
    return _choice_to_xyzs[bf_xyz](context, ob, world)  # xyzs, msgs
