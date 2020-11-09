"""!
BlenderFDS, voxelization algorithms.
"""

import bpy, bmesh, logging
from math import floor, ceil

from mathutils import Matrix

from ..utils import BFException, BFNotImported, is_iterable
from . import utils

log = logging.getLogger(__name__)

# "world" coordinates are absolute coordinate referring to Blender main origin of axes,
# that are directly transformed to FDS coordinates (that refers its coordinates to the
# one and only origin of axes)


def get_voxels(context, ob, world=True):  # FIXME world
    """!
    Get voxels from object in xbs format.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return in world coordinates.
    @return the voxels in xbs format.
    """
    # log.debug(f"Get voxels in Object <{ob.name}>")
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
    ob_tmp = bpy.data.objects.new(f"{ob.name}_voxels_tmp", me_eval)
    ob_tmp.data.transform(ob.matrix_world)
    context.collection.objects.link(ob_tmp)
    # Align voxels to world origin and add remesh modifier
    _align_remesh_bbox(context, ob_tmp, voxel_size, centered=ob.bf_xb_center_voxels)
    _add_remesh_mod(context, ob_tmp, voxel_size)
    # Get evaluated bmesh from ob_tmp
    bm = utils.get_object_bmesh(context, ob_tmp, world=True)
    # Clean up
    bpy.data.meshes.remove(ob_tmp.data, do_unlink=True)  # no mem leaks
    # Check
    if len(bm.faces) == 0:  # no faces
        raise BFException(ob, "No voxel created!")
    # Get faces and sort them according to normals
    x_faces, y_faces, z_faces = _sort_faces_by_normal(bm)
    # Choose shorter list of faces, relative functions, and parameters
    choices = [
        (len(x_faces), _get_boxes_along_x, x_faces, _grow_boxes_along_x, 0),
        (len(y_faces), _get_boxes_along_y, y_faces, _grow_boxes_along_y, 2),
        (len(z_faces), _get_boxes_along_z, z_faces, _grow_boxes_along_z, 4),
    ]
    choices.sort(key=lambda choice: choice[0])
    get_boxes = choices[0][1]  # get boxes by fastest orientation
    faces = choices[0][2]
    grow_boxes_along_first_axis = choices[1][3]  # 1st axis growing direction
    first_sort_by = choices[2][4]
    grow_boxes_along_second_axis = choices[2][3]  # 2nd axis growing direction
    second_sort_by = choices[1][4]
    # For each face find other sides and build boxes data structure
    boxes, origin = get_boxes(faces, voxel_size)
    # Join boxes along other axis and return their world coordinates
    boxes = grow_boxes_along_first_axis(boxes, first_sort_by)
    boxes = grow_boxes_along_second_axis(boxes, second_sort_by)
    # Transform boxes to xbs in world coordinates and correct for unit_settings
    xbs = list(_get_box_xbs(context, boxes, origin, voxel_size))
    # Clean up
    bm.free()
    if not xbs:
        raise BFException(ob, "No voxel created!")
    scale_length = context.scene.unit_settings.scale_length
    return xbs, voxel_size * scale_length


def _sort_faces_by_normal(bm):
    """!
    Sort bmesh faces according to normal.
    @param bm: the bmesh to handle.
    @return the x, y and z faces lists.
    """
    x_faces, y_faces, z_faces = list(), list(), list()
    for face in bm.faces:
        normal = face.normal
        if abs(normal[0]) > 0.9:
            x_faces.append(face)  # face is normal to x axis
        elif abs(normal[1]) > 0.9:
            y_faces.append(face)  # ... to y axis
        elif abs(normal[2]) > 0.9:
            z_faces.append(face)  # ... to z axis
        else:
            raise ValueError("Abnormal face")
    if len(x_faces) < 2 or len(y_faces) < 2 or len(z_faces) < 2:
        raise ValueError("Not enough faces")
    return x_faces, y_faces, z_faces


# When appling a remesh modifier to a Blender Object in BLOCKS mode,
# the object max dimension is scaled up and divided in
# (2 ** octree_depth voxels - 1) cubic voxels
# Example: dimension = 4.2, voxel_size = 0.2,
# octree_depth = 5, number of voxels = 2^5-1 = 31,
# scale = 3/4 = 0.75
# The next function reverses the procedures and calculate octree_depth
# and scale that generate the desired voxel_size.


def _init_remesh_mod(context, ob, voxel_size):
    """!
    Calc remesh modifier parameters from voxel_size.
    When appling a remesh modifier to a Blender Object in BLOCKS mode,
    the object max dimension is scaled up and divided in
    (2 ** octree_depth voxels - 1) cubic voxels.
    This function reverses the procedures and calculate octree_depth
    and scale that generate the desired voxel_size.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param voxel_size: the voxel size of the object.
    @return the octree_depth and scale that generate the desired voxel_size.
    """
    dimension, octree_depth = max(ob.dimensions), 0.0
    while True:
        octree_depth += 1.0
        scale = dimension / voxel_size / 2 ** octree_depth
        if 0.010 < scale < 0.990:
            break
        if octree_depth > 9:
            raise BFException(
                ob, "Object too large for its voxel size, split in parts."
            )
    return octree_depth, scale


def _add_remesh_mod(context, ob, voxel_size):
    """!
    Add new blocks remesh modifier.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param voxel_size: the voxel size of the object.
    @return the modifier.
    """
    octree_depth, scale = _init_remesh_mod(context, ob, voxel_size)
    mo = ob.modifiers.new("remesh_tmp", "REMESH")
    mo.mode, mo.use_remove_disconnected = "BLOCKS", False
    mo.octree_depth, mo.scale = octree_depth, scale
    return mo


def _get_voxel_size(context, ob):
    """!
    Get voxel_size of an object.
    @param context: the Blender context.
    @param ob: the Blender object.
    @return the voxel size.
    """
    if ob.bf_xb_custom_voxel:
        return ob.bf_xb_voxel_size
    else:
        return context.scene.bf_default_voxel_size


# When appling a remesh modifier to a Blender Object, the octree is aligned with
# the max dimension of the object. By inserting some loose vertices to the
# temporary object, we can align the voxelization to FDS world origin


def _align_remesh_bbox(context, ob, voxel_size, centered=False):  # FIXME world
    """!
    Modify object mesh for remesh voxel safe alignment to world origin or to ob center by inserting 8 vertices.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param voxel_size: the voxel size of the object.
    """
    xb = utils.get_bbox_xb(context, ob, blender_units=True, world=True)  # in world coo
    # Calc new xbox (in Blender units)
    #           +----+ xb1, pv1
    #           |    |
    #  xb0, pv0 +----+  + origin
    # Calc voxel grid origin
    if centered:
        hvs = voxel_size / 2.0
        origin = (
            (xb[1] + xb[0]) / 2.0 - hvs,
            (xb[3] + xb[2]) / 2.0 - hvs,
            (xb[5] + xb[4]) / 2.0 - hvs,
        )
    else:
        origin = 0.0, 0.0, 0.0
    # Calc adimensional coordinates and align to the voxel grid
    pv0 = (
        floor((xb[0] - origin[0]) / voxel_size),
        floor((xb[2] - origin[1]) / voxel_size),
        floor((xb[4] - origin[2]) / voxel_size),
    )
    pv1 = (
        ceil((xb[1] - origin[0]) / voxel_size),
        ceil((xb[3] - origin[1]) / voxel_size),
        ceil((xb[5] - origin[2]) / voxel_size),
    )
    # Set odd number of voxels for better shapes
    pv1 = (
        pv0[0] + (pv1[0] - pv0[0]) // 2 * 2 + 1,
        pv0[1] + (pv1[1] - pv0[1]) // 2 * 2 + 1,
        pv0[2] + (pv1[2] - pv0[2]) // 2 * 2 + 1,
    )
    # Calc new bounding box
    xb = (
        pv0[0] * voxel_size + origin[0],
        pv1[0] * voxel_size + origin[0],
        pv0[1] * voxel_size + origin[1],
        pv1[1] * voxel_size + origin[1],
        pv0[2] * voxel_size + origin[2],
        pv1[2] * voxel_size + origin[2],
    )
    # Prepare new vertices and insert them
    verts = (
        (xb[0], xb[2], xb[4]),
        (xb[0], xb[2], xb[5]),
        (xb[0], xb[3], xb[4]),
        (xb[0], xb[3], xb[5]),
        (xb[1], xb[2], xb[4]),
        (xb[1], xb[2], xb[5]),
        (xb[1], xb[3], xb[4]),
        (xb[1], xb[3], xb[5]),
    )
    _insert_verts_into_mesh(ob.data, verts)


def _insert_verts_into_mesh(me, verts):
    """!
    Insert vertices into mesh.
    @param me: the Blender mesh.
    @param verts: the vertices.
    """
    bm = bmesh.new()
    bm.from_mesh(me)
    for v in verts:
        bm.verts.new(v)
    bm.to_mesh(me)
    bm.free()
    bpy.context.view_layer.update()  # push update


# The following functions transform remesh modifier faces into boxes,
# by raytracing along the requested axis. Each face is transformed into
# integer coordinates according to a local origin (the first face center).
# The faces are piled up, sorted, and transformed into solids:
# Eg.: z axis --> pile 0|==solid==1| void 2|==solid==3| void ...
# If solid is manifold, len(izs) is an even number:
# go into solid at izs[0], get at last out of it at izs[-1].
# In fact this piles can be easily transformed in boxes:
# (ix0, ix1, iy0, iy1, iz0, iz1)
# boxes are very alike XBs, but in integer coordinates.

# For example, with y faces:
#  y ^
#    |     B
#  3 +---+=x=+---+ F = (.5, 0): face origin (integer coordinates)
#    |   H   H   | face A = (1, 0, 0)
#  2 +---+---+---+ face B = (1, 3, 0)
#    |   H   H   | O = (0, 0): box origin (integer coordinate)
#  1 +---+---+---+ box AB = (1, 2, 0, 3, 0, 0)
#    |   H   H   |
#  0 +-F-+=x=+---+->
#    0   1 A 2   3 x


def _get_face_center(face):
    """!
    Get bmesh face center point.
    @param face: the modifier face.
    @param verts: the center coordinates.
    """
    xs, ys, zs = zip(*(v.co for v in face.verts))
    return sum(xs) / len(xs), sum(ys) / len(ys), sum(zs) / len(zs)


def _get_boxes_along_x(faces, voxel_size):
    """!
    Get minimal boxes from faces by raytracing along x axis.
    @param faces: the modifier faces.
    @param voxel_size: the voxel size of the object.
    @return the minimal boxes and their origins.
    """
    # First face center becomes origin of the integer grid for faces
    f_origin = _get_face_center(faces[0])
    hvs = voxel_size / 2.0  # half voxel size
    origin = (f_origin[0], f_origin[1] - hvs, f_origin[2] - hvs)
    # Get integer coordinates of faces and
    # classify faces in integer piles along z
    # piles = {(3,4):(3,4,15,25,), (3,5):(3,4,15,25), ...}
    piles = dict()
    for face in faces:
        center = _get_face_center(face)
        ix, iy, iz = (
            round((center[0] - f_origin[0]) / voxel_size),
            round((center[1] - f_origin[1]) / voxel_size),
            round((center[2] - f_origin[2]) / voxel_size),
        )
        try:
            piles[(iy, iz)].append(ix)
        except KeyError:
            piles[(iy, iz)] = [ix]
    # Create boxes by raytracing piles along axis
    # boxes = [[ix0, ix1, iy0, iy1, iz0, iz1], ...]
    boxes = list()
    for (iy, iz), ixs in piles.items():
        ixs.sort()  # sort in +x direction
        while ixs:
            ix1, ix0 = ixs.pop(), ixs.pop()  # pop solid volumes from top to bottom
            boxes.append([ix0, ix1, iy, iy + 1, iz, iz + 1])
    return boxes, origin


def _get_boxes_along_y(faces, voxel_size):
    """!
    Get minimal boxes from faces by raytracing along y axis.
    @param faces: the modifier faces.
    @param voxel_size: the voxel size of the object.
    @return the minimal boxes and their origins.
    """
    # First face center becomes origin of the integer grid for faces
    f_origin = _get_face_center(faces[0])
    hvs = voxel_size / 2.0  # half voxel size
    origin = (f_origin[0] - hvs, f_origin[1], f_origin[2] - hvs)
    # Get integer coordinates of faces and
    # classify faces in integer piles along z
    # piles = {(3,4):(3,4,15,25,), (3,5):(3,4,15,25), ...}
    piles = dict()
    for face in faces:
        center = _get_face_center(face)
        ix, iy, iz = (
            round((center[0] - f_origin[0]) / voxel_size),
            round((center[1] - f_origin[1]) / voxel_size),
            round((center[2] - f_origin[2]) / voxel_size),
        )
        try:
            piles[(iz, ix)].append(iy)
        except KeyError:
            piles[(iz, ix)] = [iy]
    # Create boxes by raytracing piles along axis
    # boxes = [[ix0, ix1, iy0, iy1, iz0, iz1], ...]
    boxes = list()
    for (iz, ix), iys in piles.items():
        iys.sort()  # sort in +y direction
        while iys:
            iy1, iy0 = iys.pop(), iys.pop()  # pop solid volumes from top to bottom
            boxes.append([ix, ix + 1, iy0, iy1, iz, iz + 1])
    return boxes, origin


def _get_boxes_along_z(faces, voxel_size):
    """!
    Get minimal boxes from faces by raytracing along z axis.
    @param faces: the modifier faces.
    @param voxel_size: the voxel size of the object.
    @return the minimal boxes and their origins.
    """
    # First face center becomes origin of the integer grid for faces
    f_origin = _get_face_center(faces[0])
    hvs = voxel_size / 2.0  # half voxel size
    origin = (f_origin[0] - hvs, f_origin[1] - hvs, f_origin[2])
    # Get integer coordinates of faces and
    # classify faces in integer piles along z
    # piles = {(3,4):(3,4,15,25,), (3,5):(3,4,15,25), ...}
    piles = dict()
    for face in faces:
        center = _get_face_center(face)
        ix, iy, iz = (
            round((center[0] - f_origin[0]) / voxel_size),
            round((center[1] - f_origin[1]) / voxel_size),
            round((center[2] - f_origin[2]) / voxel_size),
        )
        try:
            piles[(ix, iy)].append(iz)
        except KeyError:
            piles[(ix, iy)] = [iz]
    # Create boxes by raytracing piles along axis
    # boxes = [[ix0, ix1, iy0, iy1, iz0, iz1], ...]
    boxes = list()
    for (ix, iy), izs in piles.items():
        izs.sort()  # sort in +z direction
        while izs:
            iz1, iz0 = izs.pop(), izs.pop()  # pop solid volumes from top to bottom
            boxes.append([ix, ix + 1, iy, iy + 1, iz0, iz1])
    return boxes, origin


# The following functions reduce the number of boxes in xbs format,
# used to describe the geometry, by merging them


def _grow_boxes_along_x(boxes, sort_by):
    """!
    Grow boxes by merging neighbours along x axis.
    @param boxes: the boxes to handle.
    @param sort_by: sorting criteria.
    @return the grown boxes.
    """
    # Sort boxes
    boxes.sort(key=lambda box: (box[sort_by], box[0]))
    # Grow boxes in -x direction, starting from last one
    boxes_grown = list()
    box = boxes.pop()
    while boxes:
        abox = boxes.pop()
        # Check same iz0, iz1, iy0, iy1, and touching abox ix1 with box ix0
        if (
            abox[4] == box[4]
            and abox[5] == box[5]
            and abox[2] == box[2]
            and abox[3] == box[3]
            and abox[1] == box[0]
        ):
            box[0] = abox[0]  # grow box along -x
        else:
            boxes_grown.append(box)  # stash the resulting box
            box = abox  # init next cycle
    # Stash the last one
    boxes_grown.append(box)
    return boxes_grown


def _grow_boxes_along_y(boxes, sort_by):
    """!
    Grow boxes by merging neighbours along y axis.
    @param boxes: the boxes to handle.
    @param sort_by: sorting criteria.
    @return the grown boxes.
    """
    # Sort boxes
    boxes.sort(key=lambda box: (box[sort_by], box[2]))
    # Grow boxes in -y direction, starting from last one
    boxes_grown = list()
    box = boxes.pop()
    while boxes:
        abox = boxes.pop()
        # Check same iz0, iz1, ix0, ix1, and touching abox iy1 with box iy0
        if (
            abox[4] == box[4]
            and abox[5] == box[5]
            and abox[0] == box[0]
            and abox[1] == box[1]
            and abox[3] == box[2]
        ):
            box[2] = abox[2]  # grow box along -y
        else:
            boxes_grown.append(box)  # stash the resulting box
            box = abox  # init next cycle
    # Stash the last one
    boxes_grown.append(box)
    return boxes_grown


def _grow_boxes_along_z(boxes, sort_by):
    """!
    Grow boxes by merging neighbours along z axis.
    @param boxes: the boxes to handle.
    @param sort_by: sorting criteria.
    @return the grown boxes.
    """
    # Sort boxes
    boxes.sort(key=lambda box: (box[sort_by], box[4]))
    # Grow boxes in -z direction, starting from last one
    boxes_grown = list()
    box = boxes.pop()
    while boxes:
        abox = boxes.pop()
        # Check same iy0, iy1, ix0, ix1, and touching abox iz1 with box iz0
        if (
            abox[2] == box[2]
            and abox[3] == box[3]
            and abox[0] == box[0]
            and abox[1] == box[1]
            and abox[5] == box[4]
        ):
            box[4] = abox[4]  # grow box along -z
        else:
            boxes_grown.append(box)  # stash the resulting box
            box = abox  # init next cycle
    # Stash the last one
    boxes_grown.append(box)
    return boxes_grown


# Transform boxes in integer coordinates, back to world coordinates


def _get_box_xbs(context, boxes, origin, voxel_size):
    """!
    Transform boxes to xbs in world coordinates.
    @param boxes: the boxes to handle.
    @param origin: local origin.
    @param voxel_size: the voxel size of the object.
    @return the xbs.
    """
    epsilon = 1e-5
    scale_length = context.scene.unit_settings.scale_length
    return (
        (
            (origin[0] + box[0] * voxel_size - epsilon) * scale_length,
            (origin[0] + box[1] * voxel_size + epsilon) * scale_length,
            (origin[1] + box[2] * voxel_size - epsilon) * scale_length,
            (origin[1] + box[3] * voxel_size + epsilon) * scale_length,
            (origin[2] + box[4] * voxel_size - epsilon) * scale_length,
            (origin[2] + box[5] * voxel_size + epsilon) * scale_length,
        )
        for box in boxes
    )


# Pixelization


def get_pixels(context, ob, world=True):  # FIXME world
    """!
    Get pixels from flat object in xbs format.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param world: True to return in world coordinates.
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
    xb = utils.get_bbox_xb(context, ob_copy, world=world)
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
