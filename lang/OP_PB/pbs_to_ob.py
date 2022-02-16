"""!
BlenderFDS, translate geometry from FDS PB notation to a Blender mesh.
"""

import logging
from mathutils import Matrix
from ...types import BFException
from ..OP_XB.xbs_to_ob import xbs_faces_to_mesh

log = logging.getLogger(__name__)


def pbs_planes_to_mesh(context, me, pbs, matrix=None, add=False) -> None:
    """!
    Import pbs planes ((0,x3,), (0,x7,), (1,y9,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param pbs: the pbs planes.
    @param matrix: transform bmesh by matrix before importing.
    @param add: if set, add to existing Mesh.
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
    xbs_faces_to_mesh(context=context, me=me, xbs=xbs, matrix=matrix, add=add)


def pbs_to_ob(context, ob, pbs, add=False, set_origin=False) -> str:
    """!
    Import pbs planes ((0,x3,), (0,x7,), (1,y9,), ...) into existing Blender Object.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param pbs: the pbs planes.
    @param is_world: coordinates are in world ref. 
    @param add: if set, add to existing Mesh.
    @param set_origin: if set, set reasonable origin.
    @return "PLANES"
    """
    if not pbs:
        return "PLANES"
    if not add and set_origin:
        matrix = None
        ob.matrix_world = Matrix()
    else:
        matrix = ob.matrix_world.inverted()
    pbs_planes_to_mesh(context=context, me=ob.data, pbs=pbs, matrix=matrix, add=add)
    return "PLANES"
