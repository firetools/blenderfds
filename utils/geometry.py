# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, geometric utilities.
"""

# TODO change file name

import bpy, bmesh
from mathutils import Matrix
from ..types import BFException

# Working on Blender collections


def get_collection(context, name=None):
    """
    Get existing Collection or create a new Collection.
    If no name is set, return Scene Collection.
    """
    if name:
        co = bpy.data.collections.get(name)
        if not co:
            co = bpy.data.collections.new(name=name)
            context.scene.collection.children.link(co)
    else:
        co = context.scene.collection
    return co


def get_sc_collections(context, sc):
    """
    Get all Collection from Scene.
    """
    return tuple(c for c in bpy.data.collections if sc.user_of_id(c))


# Working on Blender objects


def get_exported_obs(context, obs):
    """!
    Get generator of all exported Objects in context.
    @param context: the Blender Context.
    @param obs: the pool of Blender Objects to examine (eg. sc.objects)
    @return generator of exported Objects
    """
    return (
        ob
        for ob in context.scene.objects
        if ob.type == "MESH"  # no lights, cameras, ...
        and not ob.hide_render  # Object is exported
        and not ob.get_layer_collection(context).exclude  # visible in the View Layer
        and not ob.bf_is_tmp  # not tmp geometry
    )


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


def get_new_object(context, name="New", co=None):
    """!
    Create a new Object
    @param context: the Blender context.
    @param name: the new Object name.
    @param co: the Blender Collection.
    @return the created Object.
    """
    ob = bpy.data.objects.new(name, object_data=bpy.data.meshes.new(name))
    if not co:
        co = context.scene.collection
    co.objects.link(ob)
    return ob


def set_is_tmp(context, ob) -> None:
    """!
    Set an Object that is temporary geometry
    @param context: the Blender context.
    @param ob: the Object
    """
    ob.bf_is_tmp = True
    ob.show_wire = True
    ob.select_set(True)


def set_has_tmp(context, ob) -> None:
    """!
    Set an Object that has temporary geometry
    @param context: the Blender context.
    @param ob: the Object
    """
    ob.bf_has_tmp = True
    ob.hide_set(True)


def rm_tmp_objects():
    """!
    Remove all tmp objects from bpy.data
    """
    mes = bpy.data.meshes
    for ob in bpy.data.objects:
        if ob.bf_is_tmp:
            # best way to remove an Object wo mem leaks
            mes.remove(ob.data, do_unlink=True)
        elif ob.bf_has_tmp:
            ob.bf_has_tmp = False
            ob.hide_set(False)
            ob.select_set(True)


def rm_geometric_cache(ob):
    """!
    Remove geometric caches for XB from object
    @param ob: Blender Object.
    """
    ob["ob_to_xbs_cache"] = None


def rm_geometric_caches():
    """!
    Remove geometric caches for XB from all objects in bpy.data
    """
    for ob in bpy.data.objects:
        ob["ob_to_xbs_cache"] = None


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
