"""!
BlenderFDS, translate geometry from FDS PB notation to a Blender mesh.
"""

import logging, bpy
from ...types import BFException
from ..OP_XB.xbs_to_ob import xbs_faces_to_mesh

log = logging.getLogger(__name__)


def pbs_planes_to_mesh(context, me, pbs, matrix=None, new=False) -> None:
    """!
    Import pbs planes ((0,x3,), (0,x7,), (1,y9,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param pbs: the pbs planes.
    @param matrix: transform bmesh by matrix before importing.
    @param new: set a new Mesh.
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
    xbs_faces_to_mesh(context=context, me=me, xbs=xbs, matrix=matrix, new=new)


def pbs_to_ob(context, ob, pbs, is_world=True, new=False) -> str:
    """!
    Import pbs planes ((0,x3,), (0,x7,), (1,y9,), ...) into existing Blender Object.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param pbs: the pbs planes.
    @param is_world: coordinates are in world ref. 
    @param new: set a new Mesh.
    @return "PLANES"
    """
    matrix = is_world and ob.matrix_world.inverted()
    pbs_planes_to_mesh(context=context, me=ob.data, pbs=pbs, matrix=matrix, new=new)
    return "PLANES"
