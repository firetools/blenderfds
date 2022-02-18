"""!
BlenderFDS, pixelization algorithms.
"""

import bpy, logging
from ...types import BFException
from ... import utils
from .calc_voxels import _get_voxel_size, get_voxels

log = logging.getLogger(__name__)


def get_pixels(context, ob):
    """!
    Get pixels from flat object in xbs format.
    @param context: the Blender context.
    @param ob: the Blender object.
    @return the xbs and the voxel size.
    """
    # log.debug(f"Get pixels in Object <{ob.name}>")
    # Check object and init
    if ob.type not in {"MESH", "CURVE", "SURFACE", "FONT", "META"}:
        raise BFException(ob, "Object can not be converted to mesh.")
    if not ob.data.vertices:
        raise BFException(ob, "Empty object, no available geometry")
    voxel_size = _get_voxel_size(context, ob)
    # Get evaluated ob (eg. modifiers applied) and its Mesh
    dg = context.evaluated_depsgraph_get()
    ob_eval = ob.evaluated_get(dg)  # no need to clean up, it is tmp
    me_eval = bpy.data.meshes.new_from_object(ob_eval)  # static
    # Create a new Object, in world coo
    ob_copy = bpy.data.objects.new(f"{ob.name}_voxels_tmp", me_eval)
    ob_copy.data.transform(ob.matrix_world)
    context.collection.objects.link(ob_copy)
    # Set data for ob_copy
    ob_copy.bf_xb_custom_voxel = ob.bf_xb_custom_voxel
    ob_copy.bf_xb_center_voxels = ob.bf_xb_center_voxels
    ob_copy.bf_xb_voxel_size = ob.bf_xb_voxel_size
    # Get flat axis of evaluated ob
    flat_axis = _get_flat_axis(ob_copy, voxel_size)
    # Check how flat it is
    if ob_copy.dimensions[flat_axis] > voxel_size / 2.0:
        bpy.data.meshes.remove(ob_copy.data, do_unlink=True)
        raise BFException(ob, "Object is not flat enough.")
    # Get origin for flat xbs
    xb = utils.geometry.get_bbox_xb(context, ob_copy, world=world)
    flat_origin = (
        (xb[1] + xb[0]) / 2.0,
        (xb[3] + xb[2]) / 2.0,
        (xb[5] + xb[4]) / 2.0,
    )
    # Add solidify modifier
    _add_solidify_mod(context, ob_copy, voxel_size)
    # Voxelize (already corrected for unit_settings)
    try:
        xbs, voxel_size = get_voxels(context, ob_copy)
    except BFException as err:
        raise BFException(ob, f"No pixel created!\n{err}")
    finally:
        bpy.data.meshes.remove(ob_copy.data, do_unlink=True)  # clean up
    # Flatten the solidified object xbs
    choice = (_x_flatten_xbs, _y_flatten_xbs, _z_flatten_xbs)[flat_axis]
    xbs = choice(xbs, flat_origin)
    return xbs, voxel_size


def _add_solidify_mod(context, ob, voxel_size):
    """!
    Add new solidify modifier.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param voxel_size: the voxel size of the object.
    @return the modifier.
    """
    mo = ob.modifiers.new("solidify_tmp", "SOLIDIFY")
    mo.thickness = voxel_size * 3
    mo.offset = 0.0  # centered
    return mo


def _get_flat_axis(ob, voxel_size):
    """!
    Get object flat axis.
    @param ob: the Blender object.
    @param voxel_size: the voxel size of the object.
    @return the object flat axis.
    """
    dimensions = ob.dimensions
    choices = [
        (dimensions[0], 0),  # object faces are normal to x axis
        (dimensions[1], 1),  # ... to y axis
        (dimensions[2], 2),  # ... to z axis
    ]
    choices.sort(key=lambda k: k[0])  # sort by dimension
    return choices[0][1]


def _x_flatten_xbs(xbs, flat_origin):
    """!
    Flatten voxels to obtain pixels (normal to x axis) at flat_origin height.
    @param xbs: voxelized xbs.
    @param flat_origin: local origin of flat object.
    @return xbs.
    """
    return [[flat_origin[0], flat_origin[0], xb[2], xb[3], xb[4], xb[5]] for xb in xbs]


def _y_flatten_xbs(xbs, flat_origin):
    """!
    Flatten voxels to obtain pixels (normal to y axis) at flat_origin height.
    @param xbs: voxelized xbs.
    @param flat_origin: local origin of flat object.
    @return xbs.
    """
    return [[xb[0], xb[1], flat_origin[1], flat_origin[1], xb[4], xb[5]] for xb in xbs]


def _z_flatten_xbs(xbs, flat_origin):
    """!
    Flatten voxels to obtain pixels (normal to z axis) at flat_origin height.
    @param xbs: voxelized xbs.
    @param flat_origin: local origin of flat object.
    @return xbs.
    """
    return [[xb[0], xb[1], xb[2], xb[3], flat_origin[2], flat_origin[2]] for xb in xbs]
