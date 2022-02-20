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
    pbs = list()
    xbs, _ = _ob_to_xbs_faces(context, ob, world)
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
    return _ob_to_pbs_planes(context, ob, world=world)  # pbs, msgs
