"""!
BlenderFDS, translate Blender object geometry to FDS GEOM notation.
"""

import bpy, bmesh, mathutils, logging
from ... import utils, config
from ...types import BFException
from . import bingeom

log = logging.getLogger(__name__)


def ob_to_geom(context, ob, check, is_open, world, filepath=None):
    """!
    Transform Object geometry to FDS notation.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param check: True to check the bmesh sanity.
    @param is_open: set if bmesh should be open.
    @param world: set to return the object in world coordinates.
    @param filepath: if set, write to bingeom file.
    @return FDS GEOM notation and messages.
    """
    fds_verts, fds_faces, fds_surfs, fds_faces_surfs = get_fds_trisurface(
        context=context,
        ob=ob,
        check=check,
        is_open=is_open,
        world=world,
    )
    msgs = list((f"GEOM Vertices: {len(fds_verts)} | Faces: {len(fds_faces)}",))
    if filepath:
        bingeom.write_bingeom_file(
            geom_type=ob.data.bf_geom_is_terrain and 2 or 1,
            n_surf_id=len(ob.data.materials),
            fds_verts=fds_verts,
            fds_faces=fds_faces,
            fds_surfs=fds_surfs,
            fds_volus=list(),
            filepath=filepath,
        )
    return fds_verts, fds_faces, fds_surfs, fds_faces_surfs, msgs


def get_fds_trisurface(context, ob, check, is_open, world):
    """!
    Get triangulated surface from object in FDS format.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param check: set to check the bmesh sanity.
    @param is_open: set if bmesh should be open.
    @param world: set to return the object in world coordinates.
    @return FDS GEOM notation.
    """
    # Get bmesh and check it, if requested
    bm = utils.geometry.get_object_bmesh(
        context=context, ob=ob, world=world, triangulate=True, lookup=False
    )
    if check:
        _is_bm_sane(context, ob, bm, protect=True, is_open=is_open)
    # Get geometric data from bmesh
    fds_verts, fds_faces, fds_surfs = list(), list(), list()
    fds_faces_surfs = list()
    scale_length = context.scene.unit_settings.scale_length
    for v in bm.verts:
        co = v.co
        fds_verts.extend(
            (co.x * scale_length, co.y * scale_length, co.z * scale_length)
        )
    if ob.material_slots:
        for f in bm.faces:
            v = f.verts
            fds_faces.extend((v[0].index + 1, v[1].index + 1, v[2].index + 1))
            fds_surfs.append(f.material_index + 1)  # FDS index start from 1, not 0
            fds_faces_surfs.extend(  # this is for GEOM ASCII notation
                (v[0].index + 1, v[1].index + 1, v[2].index + 1, f.material_index + 1)
            )
    else:
        for f in bm.faces:
            v = f.verts
            fds_faces.extend((v[0].index + 1, v[1].index + 1, v[2].index + 1))
            fds_surfs.append(0)  # no material_slots
            fds_faces_surfs.extend(  # this is for GEOM ASCII notation
                (v[0].index + 1, v[1].index + 1, v[2].index + 1, 0)
            )
    bm.free()  # clean up bmesh
    if not fds_verts or not fds_faces:
        raise BFException(ob, "The object is empty")
    return fds_verts, fds_faces, fds_surfs, fds_faces_surfs


def get_boundary_condition_ids(context, ob):  # used by GEOM
    hids = list()
    material_slots = ob.material_slots
    for ms in material_slots:
        ma = ms.material
        if not ma:
            raise BFException(
                ob,
                f"Empty reference to boundary condition, fill all Material slots",
            )
        if not ma.bf_surf_export:
            raise BFException(ob, f"Referenced SURF <{ma.name}> is not exported")
        hids.append(ma.name)
    return tuple(hids)


# Check sanity


def check_geom_sanity(context, ob, protect, is_open):
    """!
    Check that Object is a closed orientable manifold, with no degenerate geometry.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param protect: if set raise BFException without modifications.
    @param is_open: set if the bmesh should be open.
    """
    # log.debug(f"Check geom sanity in Object <{ob.name}>")
    bm = utils.geometry.get_object_bmesh(
        context=context, ob=ob, world=False, triangulate=True, lookup=True
    )
    _is_bm_sane(context, ob, bm, protect, is_open=is_open)
    bm.free()


def _is_bm_sane(context, ob, bm, protect, is_open):
    """!
    Check that bmesh is a closed orientable manifold, with no degenerate geometry.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bm: the object's bmesh.
    @param protect: if True raise BFException without context modifications.
    @param is_open: True if bmesh should be open.
    """
    _has_manifold_verts(context, ob, bm, protect)
    if not is_open:
        _has_manifold_edges(context, ob, bm, protect)
    _has_no_degenerate_edges(context, ob, bm, protect)
    _has_no_degenerate_faces(context, ob, bm, protect)
    _has_loose_vertices(context, ob, bm, protect)
    _has_duplicate_vertices(context, ob, bm, protect)
    _has_inconsistent_normals(context, ob, bm, protect)
    if not is_open:
        _has_inverted_normals(context, ob, bm, protect)
    return True


def _has_manifold_verts(context, ob, bm, protect):
    """!
    Check manifold vertices.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bm: the object's bmesh.
    @param protect: if True raise BFException without context modifications.
    """
    bad_verts = list()
    for vert in bm.verts:
        if not vert.is_manifold:
            bad_verts.append(vert)
    if bad_verts:
        msg = (
            f"Bad geometry: Non manifold vertices detected ({len(bad_verts)} vertices)."
        )
        _raise_bad_geometry(context, ob, bm, msg, protect, bad_verts=bad_verts)


def _has_manifold_edges(context, ob, bm, protect):
    """!
    Check manifold edges, each edge should join two faces, no more no less.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bm: the object's bmesh.
    @param protect: if True raise BFException without context modifications.
    """
    bad_edges = list()
    for edge in bm.edges:
        if not edge.is_manifold:
            bad_edges.append(edge)
    if bad_edges:
        msg = f"Bad geometry: Non manifold or open geometry detected ({len(bad_edges)} edges)."
        _raise_bad_geometry(context, ob, bm, msg, protect, bad_edges=bad_edges)


def _has_inconsistent_normals(context, ob, bm, protect):
    """!
    Check normals, adjoining faces should have normals in the same directions.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bm: the object's bmesh.
    @param protect: if True raise BFException without context modifications.
    """
    bad_edges = list()
    for edge in bm.edges:
        if edge.is_manifold and not edge.is_contiguous:
            # manifold because open boundaries are not contiguous
            bad_edges.append(edge)
    if bad_edges:
        msg = f"Bad geometry: Inconsistent face normals detected ({len(bad_edges)} edges)."
        _raise_bad_geometry(context, ob, bm, msg, protect, bad_edges=bad_edges)


def _has_inverted_normals(context, ob, bm, protect):
    """!
    Check inverted normals, inside/outside.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bm: the object's bmesh.
    @param protect: if True raise BFException without context modifications.
    """
    bm.faces.ensure_lookup_table()
    bm_tmp = bm.copy()
    bmesh.ops.recalc_face_normals(bm_tmp, faces=bm_tmp.faces)
    if (
        bm.faces
        and bm.faces[0].normal.normalized().dot(bm_tmp.faces[0].normal.normalized())
        < -0.9  # anti parallel!
    ):
        msg = f"Bad geometry: Inverted face normals detected."
        _raise_bad_geometry(context, ob, bm, msg, protect)


def _has_no_degenerate_edges(context, ob, bm, protect):
    """!
    Check no degenerate edges, zero lenght edges.
    """
    bad_edges = list()
    epsilon_len = config.MIN_EDGE_LENGTH
    for edge in bm.edges:
        if edge.calc_length() <= epsilon_len:
            bad_edges.append(edge)
    if bad_edges:
        msg = f"Bad geometry: Too short edges detected ({len(bad_edges)} edges)."
        _raise_bad_geometry(context, ob, bm, msg, protect, bad_edges=bad_edges)


def _has_no_degenerate_faces(context, ob, bm, protect):
    """!
    Check degenerate faces, zero area faces.
    """
    bad_faces = list()
    epsilon_area = config.MIN_FACE_AREA
    for face in bm.faces:
        if face.calc_area() <= epsilon_area:
            bad_faces.append(face)
    if bad_faces:
        msg = f"Bad geometry: Too small area faces detected ({len(bad_faces)} faces)."
        _raise_bad_geometry(context, ob, bm, msg, protect, bad_faces=bad_faces)


def _has_loose_vertices(context, ob, bm, protect):
    """!
    Check loose vertices, vertices that have no connectivity.
    """
    bad_verts = list()
    for vert in bm.verts:
        if not bool(vert.link_edges):
            bad_verts.append(vert)
    if bad_verts:
        msg = f"Bad geometry: Loose vertices detected ({len(bad_verts)} vertices)."
        _raise_bad_geometry(context, ob, bm, msg, protect, bad_verts=bad_verts)


def _has_duplicate_vertices(context, ob, bm, protect):
    """!
    Check duplicate vertices.
    """
    bad_verts = list()
    epsilon_len = config.MIN_EDGE_LENGTH
    size = len(bm.verts)
    kd = mathutils.kdtree.KDTree(size)  # create a kd-tree from a mesh
    for i, vert in enumerate(bm.verts):
        kd.insert(vert.co, i)
    kd.balance()
    for vert in bm.verts:
        vert_group = list()
        for (_, i, _) in kd.find_range(vert.co, epsilon_len):
            vert_group.append(i)
        if len(vert_group) > 1:
            for i in vert_group:
                bad_verts.append(bm.verts[i])
    if bad_verts:
        msg = f"Bad geometry: Duplicate vertices detected ({len(bad_verts)} vertices)."
        _raise_bad_geometry(context, ob, bm, msg, protect, bad_verts=bad_verts)


# Check intersections


def check_intersections(context, ob, other_obs=None, protect=True):
    """!
    Check ob self-intersection and intersection with other_obs.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param other_obs: the list of objects to evaluate the intersection with ob.
    @param protect: if True raise BFException without context modifications.
    """
    # log.debug(f"Check intersections in Object <{ob.name}>")
    if context.object:
        bpy.ops.object.mode_set(mode="OBJECT")
    epsilon_int = config.MIN_INTERSECTION_LENGTH
    bad_faces = list()
    bm = utils.geometry.get_object_bmesh(
        context=context, ob=ob, world=False, lookup=True
    )
    tree = mathutils.bvhtree.BVHTree.FromBMesh(bm, epsilon=epsilon_int)
    # Get self-intersections
    bad_faces.extend(_get_bm_intersected_faces(bm, tree, tree))
    # Get intersections
    for other_ob in other_obs or tuple():
        matrix = ob.matrix_world.inverted() @ other_ob.matrix_world
        other_bm = utils.geometry.get_object_bmesh(
            context=context, ob=ob, world=False, matrix=matrix, lookup=True
        )
        other_tree = mathutils.bvhtree.BVHTree.FromBMesh(other_bm, epsilon=epsilon_int)
        other_bm.free()
        bad_faces.extend(_get_bm_intersected_faces(bm, tree, other_tree))
    # Raise
    if bad_faces:
        msg = "Intersection detected."
        _raise_bad_geometry(context, ob, bm, msg, protect, bad_faces=bad_faces)


def _get_bm_intersected_faces(bm, tree, other_tree):
    """!
    Get intersected faces between trees.
    @param bm: the object's bmesh.
    @param tree: BVHTree of first compared Object
    @param other_tree: BVHTree of second compared Object
    """
    overlap = tree.overlap(other_tree)
    if overlap:
        ifaces = {i_pair[0] for i_pair in overlap}
        return [bm.faces[iface] for iface in ifaces]
    return list()


# Raise bad geometry


def _raise_bad_geometry(
    context, ob, bm, msg, protect, bad_verts=None, bad_edges=None, bad_faces=None
):
    """!
    Select bad elements, show them, raise BFException.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param bm: the object's bmesh.
    @param msg: the BFException message.
    @param protect: if True raise BFException without context modifications.
    @param bad_verts: list of offending vertices to be shown
    @param bad_edges: list of offending edges to be shown
    @param bad_faces: list of offending faces to be shown
    """
    if protect:
        raise BFException(ob, msg)
    # Deselect all in bmesh
    for vert in bm.verts:
        vert.select = False
    bm.select_flush(False)
    # Select bad elements
    select_type = None
    if bad_faces:
        select_type = "FACE"
        for b in bad_faces:
            b.select = True
    if bad_edges:
        select_type = "EDGE"
        for b in bad_edges:
            b.select = True
    if bad_verts:
        select_type = "VERT"
        for b in bad_verts:
            b.select = True
    # Remove applied modifiers, but keep it relative
    ob.modifiers.clear()
    # Apply bm to ob
    bm.to_mesh(ob.data)
    bm.free()
    # Select object and go to edit mode
    if context.object:
        bpy.ops.object.mode_set(mode="OBJECT")
    bpy.ops.object.select_all(action="DESELECT")
    ob.select_set(True)  # Blender 2.80
    context.view_layer.objects.active = ob  # Blender 2.80
    bpy.ops.object.mode_set(mode="EDIT")
    bpy.ops.mesh.select_mode(use_extend=False, use_expand=False, type=select_type)
    raise BFException(ob, msg)
