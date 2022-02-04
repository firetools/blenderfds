"""!
BlenderFDS, geometric utilities.
"""

# TODO change file name

import bpy, bmesh
from mathutils import Matrix
from ..types import BFException

# Working on Blender objects


def get_object_bmesh(
    context, ob, matrix=None, world=True, triangulate=False, lookup=False
):
    """!
    Return evaluated object bmesh.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param matrix: transformation matrix.
    @param world: if True, set bmesh in world coordinates.
    @param triangulate: triangulate faces.
    @param lookup: prepare lookup tables for faces, verts, and edges.
    @return the evaluated bmesh.
    """
    # Check object and init
    if ob.type not in {"MESH", "CURVE", "SURFACE", "FONT", "META"}:
        raise BFException(ob, "Object cannnot be converted into mesh")
    # if context.object:
    #    bpy.ops.object.mode_set(mode="OBJECT")  # actualize TODO not allowed in panel calls
    # Get evaluated bmesh from ob
    bm = bmesh.new()
    depsgraph = context.evaluated_depsgraph_get()
    bm.from_object(ob, depsgraph=depsgraph, cage=False, face_normals=True)
    if matrix is not None:
        bm.transform(matrix)  # transform
    if world:
        bm.transform(ob.matrix_world)  # set in world coordinates
    if triangulate:  # triangulate faces
        bmesh.ops.triangulate(bm, faces=bm.faces)
    if lookup:  # update bm indexes for reference
        bm.faces.ensure_lookup_table()
        bm.verts.ensure_lookup_table()
        bm.edges.ensure_lookup_table()
    return bm


# FIXME
# def get_new_object(context, sc=None, ob=None, name="New", set_tmp=False):
#     if not sc:
#         sc = context.scene
#     me_new = bpy.data.meshes.new(name)
#     ob_new = bpy.data.objects.new(name, object_data=me_new)
#     sc.collection.objects.link(ob_new)
#     if set_tmp:
#         ob_new.bf_is_tmp = True
#         if ob:
#             ob.bf_has_tmp = True
#             ob.hide_set(True)
#     return ob_new


def get_tmp_object(context, ob, name="tmp"):
    """!
    Get a new tmp Object from ob.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param name: the new object name.
    @return the temp object.
    """
    # Create new tmp Object
    me_tmp = bpy.data.meshes.new(name)
    ob_tmp = bpy.data.objects.new(name, me_tmp)
    ob_tmp.bf_is_tmp = True
    co = ob.users_collection[0]
    co.objects.link(ob_tmp)
    # Set original
    ob.bf_has_tmp = True
    ob.hide_set(True)
    return ob_tmp


def rm_tmp_objects():
    """!
    Remove all tmp objects from bpy.data
    """
    mes = bpy.data.meshes
    for ob in bpy.data.objects:
        if ob.bf_is_tmp:
            mes.remove(
                ob.data, do_unlink=True
            )  # best way to remove an Object wo mem leaks
        elif ob.bf_has_tmp:
            ob.bf_has_tmp = False
            ob.hide_set(False)
            ob.select_set(True)


def rm_geometric_cache(ob):
    """!
    Remove geometric caches for XB, XYZ, PB*, GEOM from object
    @param ob: Blender Object.
    """
    # ob["ob_to_geom_cache"] = None
    ob["ob_to_xbs_cache"] = None  # TODO or del?
    # ob["ob_to_xyzs_cache"] = None
    # ob["ob_to_pbs_cache"] = None


def rm_geometric_caches():
    """!
    Remove geometric caches for XB, XYZ, PB*, GEOM from all objects in bpy.data
    """
    for ob in bpy.data.objects:
        # ob["ob_to_geom_cache"] = None
        ob["ob_to_xbs_cache"] = None  # TODO or del?
        # ob["ob_to_xyzs_cache"] = None
        # ob["ob_to_pbs_cache"] = None


def transform_ob(ob, m, force_othogonal=False):
    """!
    Trasform Object with matrix.
    """
    m = Matrix(m)
    if force_othogonal or m.is_orthogonal:
        # No shearing and skewing
        ob.matrix_world = m @ ob.matrix_world
    else:
        # Get translation, rotation and scaling matrices
        loc, rot, sca = m.decompose()
        mloc = Matrix.Translation(loc)
        mrot = rot.to_matrix().to_4x4()
        msca = Matrix.Identity(4)
        msca[0][0], msca[1][1], msca[2][2] = sca
        # Get the matrix that can be used for matrix_world
        mout = mloc @ mrot @ msca  # do not forget, the order is important
        # Get the shearing and skewing in another matrix
        mh = mout.inverted_safe() @ m
        # Apply to Object and Mesh
        ob.matrix_world = mout @ ob.matrix_world
        ob.data.transform(mh)


# Working on Blender materials


def get_new_material(context, name):
    """!
    Create new material, named name.
    @param context: the Blender context.
    @param name: the new material name.
    @return the new material.
    """
    return bpy.data.materials.new(name)


def get_material_by_name(context, name):
    """!
    Get a material by name and return it.
    @param context: the Blender context.
    @param name: the material name.
    @return the material if exist. None otherwise.
    """
    if name and name in bpy.data.materials:
        return bpy.data.materials[name]


def get_material(context, name):
    """!
    Get an existing material by name or create a new one, and return it.
    @param context: the Blender context.
    @param name: the material name.
    @return the material.
    """
    if name and name in bpy.data.materials:
        return bpy.data.materials[name]
    return bpy.data.materials.new(name)


# Working on bounding box and size


def get_bbox_xb(context, ob, blender_units=False, world=True):
    """!
    Get object’s bounding box in xb format.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param blender_units: return in Blender units.
    @param world: if True, set bmesh in world coordinates.
    @return the object’s bounding box.
    """
    bm = get_object_bmesh(context, ob, world=world)
    bm.verts.ensure_lookup_table()
    if not bm.verts:
        raise BFException(ob, "Empty object, no available geometry")
    xs, ys, zs = tuple(zip(*(v.co for v in bm.verts)))
    bm.free()
    scale_length = blender_units and 1.0 or context.scene.unit_settings.scale_length
    return (
        min(xs) * scale_length,
        max(xs) * scale_length,
        min(ys) * scale_length,
        max(ys) * scale_length,
        min(zs) * scale_length,
        max(zs) * scale_length,
    )
