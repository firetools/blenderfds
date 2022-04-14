"""!
BlenderFDS, translate Blender object geometry to FDS PB notation.
"""

import logging
from ...types import BFException
from ..OP_XB.ob_to_xbs import _ob_to_xbs_faces

log = logging.getLogger(__name__)


def _ob_to_pbs_planes(context, ob, world) -> tuple((list, list)):
    """!
    Transform Object faces to pbs notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return the object in world coordinates.
    @return the pbs notation and messages.
    """
    # pbs is: ("PBX", 3.5), ("PBX", 4.), ("PBY", .5) ...
    pbs = list()
    xbs, _ = _ob_to_xbs_faces(context, ob, world)
    epsilon = 1e-5
    # For each face build a plane...
    for xb in xbs:
        if abs(xb[1] - xb[0]) < epsilon:
            pbs.append(("PBX", xb[0]))
        elif abs(xb[3] - xb[2]) < epsilon:
            pbs.append(("PBY", xb[2]))
        elif abs(xb[5] - xb[4]) < epsilon:
            pbs.append(("PBZ", xb[4]))
        else:
            raise ValueError(
                "BFDS: Building planes impossible, problem in ob_to_xbs_faces."
            )
    pbs.sort()
    if not pbs:
        raise BFException(ob, "PB*: No exported planes!")
    msgs = list((f"PB* Planes: {len(pbs)}",))
    return pbs, msgs


def ob_to_pbs(context, ob, bf_pb, world=True) -> tuple((list, list)):
    """!
    Transform Object geometry according to bf_pb to pbs notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bf_pb: string in (PLANES,)
    @param world: True to return the object in world coordinates.
    @return the pbs notation and messages.
    """
    # pbs is: ("PBX", 3.5), ("PBX", 4.), ("PBY", .5) ...
    pbs, msgs = _ob_to_pbs_planes(context, ob, world=world)
    # Calc hids
    n = ob.name
    match ob.bf_id_suffix:
        case "IDI":
            hids = (f"{n}_{i}" for i, _ in enumerate(pbs))
        case _:
            hids = (
                {"PBX": f"{n}_x{pb:+.3f}", "PBY": f"{n}_y{pb:+.3f}", "PBZ": f"{n}_z{pb:+.3f}"}[axis]
                for axis, pb in pbs
            )
    return tuple(hids), tuple(pbs), tuple(msgs)
