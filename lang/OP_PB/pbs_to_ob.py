"""!
BlenderFDS, translate geometry from FDS PB notation to a Blender mesh.
"""

import logging
from ..OP_XB.xbs_to_ob import xbs_to_ob

log = logging.getLogger(__name__)


def pbs_to_ob(context, ob, pbs, add=False, set_origin=False) -> str():
    """!
    Import pbs planes (("PBX",x3,), ("PBX",x7,), ("PBY",y9,), ...) into existing Blender Object.
    @param context: the Blender context.
    @param ob: the Blender Object.
    @param pbs: the pbs planes.
    @param add: add to existing Mesh.
    @param set_origin: set reasonable origin.
    @return "PLANES"
    """
    if not pbs:
        return "PLANES"
    # Convert the geometry
    xbs, sl = list(), context.scene.unit_settings.scale_length
    for pb in pbs:
        match pb[0]:
            case "PBX":
                xbs.append((pb[1], pb[1], -sl, +sl, -sl, +sl))  # PBX is 0
            case "PBY":
                xbs.append((-sl, +sl, pb[1], pb[1], -sl, +sl))  # PBY is 1
            case "PBX":
                xbs.append((-sl, +sl, -sl, +sl, pb[1], pb[1]))  # PBZ is 2
            case _:
                raise AssertionError(f"Unrecognized PB* <{pb}>")
    # Inject geometry
    xbs_to_ob(context=context, ob=ob, xbs=xbs, bf_xb="FACES", add=add, set_origin=set_origin)
    return "PLANES"
