"""!
BlenderFDS, translate Blender object geometry to FDS XYZ notation.
"""

import logging
from ...types import BFException
from ... import utils

log = logging.getLogger(__name__)


def _ob_to_xyzs_vertices(context, ob, world):
    """!
    Transform Object vertices to xyzs notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return the xyzs notation and any error message: ((x0,y0,z0,), ...), 'Msg'.
    """
    xyzs = list()
    bm = utils.geometry.get_object_bmesh(context, ob, world=world)
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
