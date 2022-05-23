# SPDX-License-Identifier: GPL-3.0-or-later

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


def ob_to_xyzs(context, ob, bf_xyz, world=True) -> tuple((list, list, list)):
    """!
    Transform Object geometry according to bf_xyz to xyzs notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bf_xyz: string in (CENTER, VERTICES).
    @param world: True to return the object in world coordinates.
    @return the xyzs notation and any error message: ((x0,y0,z0,), ...), 'Msg'.
    """
    # Calc xyzs and msgs
    xyzs, msgs = tuple(), tuple()
    if ob.bf_xyz_export:
        xyzs, msgs = _choice_to_xyzs[bf_xyz](context, ob, world)

    # Calc hids
    n = ob.name
    match ob.bf_id_suffix:
        case "IDI":
            hids = (f"{n}_{i}" for i, _ in enumerate(xyzs))
        case "IDX":
            hids = (f"{n}_x{xyz[0]:+.3f}" for xyz in xyzs)
        case "IDY":
            hids = (f"{n}_y{xyz[1]:+.3f}" for xyz in xyzs)
        case "IDZ":
            hids = (f"{n}_z{xyz[2]:+.3f}" for xyz in xyzs)
        case "IDXY":
            hids = (f"{n}_x{xyz[0]:+.3f}_y{xyz[1]:+.3f}" for xyz in xyzs)
        case "IDXZ":
            hids = (f"{n}_x{xyz[0]:+.3f}_z{xyz[2]:+.3f}" for xyz in xyzs)
        case "IDYZ":
            hids = (f"{n}_y{xyz[1]:+.3f}_z{xyz[2]:+.3f}" for xyz in xyzs)
        case "IDXYZ":
            hids = (f"{n}_x{xyz[0]:+.3f}_y{xyz[1]:+.3f}_z{xyz[2]:+.3f}" for xyz in xyzs)
        case _:
            raise AssertionError(f"Unknown suffix <{ob.bf_id_suffix}>")

    return tuple(hids), tuple(xyzs), tuple(msgs)
