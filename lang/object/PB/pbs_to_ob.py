"""!
BlenderFDS, translate geometry from FDS PB notation to a Blender mesh.
"""

import logging
from ....types import BFException
from ..XB.xbs_to_ob import xbs_faces_to_mesh

log = logging.getLogger(__name__)


def pbs_planes_to_mesh(context, me, pbs):
    """!
    Import pbs planes ((0,x3,), (0,x7,), (1,y9,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param pbs: the pbs planes.
    """
    xbs = list()
    sl = context.scene.unit_settings.scale_length
    for pb in pbs:
        match pb[0]:
            case 0:
                xbs.append((pb[1], pb[1], -sl, +sl, -sl, +sl))  # PBX is 0
            case 1:
                xbs.append((-sl, +sl, pb[1], pb[1], -sl, +sl))  # PBY is 1
            case 2:
                xbs.append((-sl, +sl, -sl, +sl, pb[1], pb[1]))  # PBZ is 2
            case _:
                raise AssertionError(f"Unrecognized PB* <{pb}>")
    xbs_faces_to_mesh(context=context, me=me, xbs=xbs, matrix=None)


def pbs_to_ob(context, ob, pbs):
    """!
    Import pbs planes ((0,x3,), (0,x7,), (1,y9,), ...) into existing Blender Object.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param pbs: the pbs planes.
    @return "PLANES"
    """
    pbs_planes_to_mesh(context=context, me=ob.data, pbs=pbs)
    return "PLANES"
