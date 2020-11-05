"""!
BlenderFDS, translate geometry from FDS notation to a Blender mesh.
"""

import bpy, bmesh, logging
from time import time
from mathutils import Matrix, Vector
from ..utils import BFException, BFNotImported, is_iterable
from . import utils

log = logging.getLogger(__name__)

epsilon = 1e-5  # TODO unify epsilon mgmt

# From GEOM


def geom_verts_to_mesh(context, me, vs):
    """!
    Import GEOM VERTS into Blender Mesh.
    @param context: the blender context.
    @param me: the Blender Mesh.
    @param vs: the FDS GEOM VERTS vertices.
    """
    # Check input length
    if len(vs) % 3:
        raise BFException(me, f"Bad GEOM: len of VERTS is not multiple of 3")
    # Create a new bmesh, never add to existing
    bm = bmesh.new()
    # Fill the bm.verts
    scale_length = context.scene.unit_settings.scale_length
    for i in range(0, len(vs), 3):
        bm.verts.new(
            (vs[i] / scale_length, vs[i + 1] / scale_length, vs[i + 2] / scale_length,)
        )
    bm.to_mesh(me)
    bm.free()


def geom_faces_to_mesh(context, me, fs=None, ss=None, fss=None):
    """!
    Import GEOM FACES into Blender Mesh.
    @param context: the blender context.
    @param me: the Blender Mesh.
    @param fs: the FDS GEOM faces vector.
    @param ss: the FDS GEOM surfs vector.
    @param fss: the FDS GEOM FACES faces indexes and boundary condition indexes.
    """
    # Transform fss to fs and ss
    if fss:
        if fs or ss:
            raise AssertionError("Set faces and surfs or faces_surfs, not both")
        else:
            fs, ss = list(), list()
            for i in range(0, len(fss), 4):
                fs.extend(fss[i : i + 3])
                ss.append(fss[i + 3])
    # Check input length
    if len(fs) % 3:
        raise BFException(me, f"Bad GEOM: len of FACES is not multiple of 3")
    if len(ss) != len(fs) // 3:
        raise BFException(
            me, f"Bad GEOM: len of FACE SURFS is not equal to len of FACES"
        )
    # Get the bmesh from existing
    bm = bmesh.new()
    bm.from_mesh(me)
    # Fill the bm.faces
    bm.verts.ensure_lookup_table()
    for i in range(0, len(fs), 3):
        bm.faces.new(
            (
                bm.verts[fs[i] - 1],  # -1 from F90 to py indexes
                bm.verts[fs[i + 1] - 1],
                bm.verts[fs[i + 2] - 1],
            )
        )
    bm.to_mesh(me)
    bm.free()
    # Check and assign materials to faces
    if max(ss) > len(me.materials):  # from F90 to py indexes
        raise BFException(
            me,
            f"Bad GEOM: FACE SURF index is higher that available SURF_ID number: {max(ss)}",
        )
    for iface, face in enumerate(me.polygons):
        face.material_index = ss[iface] - 1  # -1 from F90 to py indexes


def geom_to_ob(context, ob, vs, fs=None, ss=None, fss=None):
    """!
    Import GEOM into Blender Object.
    @param context: the blender context.
    @param ob: the Blender Object.
    @param vs: the FDS GEOM VERTS vertices.
    @param fs: the FDS GEOM faces vector.
    @param ss: the FDS GEOM surfs vector.
    @param fss: the FDS GEOM FACES faces indexes and boundary condition indexes.
    """
    geom_verts_to_mesh(context, me=ob.data, vs=vs)
    geom_faces_to_mesh(context, me=ob.data, fs=fs, ss=ss, fss=fss)


# Special GEOMs


def geom_sphere_to_ob(context, ob, n_levels=2, radius=0.5, origin=(0.0, 0.0, 0.0)):
    """!
    Import GEOM SPHERE into Blender Object.
    @param context: the blender context.
    @param ob: the Blender Object.
    @param n_levels: number of subdivisions.
    @param radius: sphere radius.
    @param origin: sphere location.
    """
    # Create new object
    scale_length = context.scene.unit_settings.scale_length
    bpy.ops.mesh.primitive_ico_sphere_add(
        subdivisions=n_levels, radius=radius / scale_length
    )
    ob_tmp = context.object
    # Attach materials before copying Mesh
    for ma in ob.data.materials:
        ob_tmp.data.materials.append(ma)
    # Attach new mesh to original object and rm new object
    ob.data = ob_tmp.data
    bpy.data.objects.remove(ob_tmp, do_unlink=True)
    # Set location and rotation for original Object
    matrix_loc = Matrix.Translation(Vector(origin) / scale_length)
    ob.matrix_world = matrix_loc @ ob.matrix_world


def geom_cylinder_to_ob(
    context,
    ob,
    origin=(0.0, 0.0, 0.0),
    axis=(0.0, 0.0, 1.0),
    radius=0.5,
    length=2.0,
    nseg_theta=8,
    nseg_axis=1,
    set_materials=True,
):
    """!
    Import GEOM CYLINDER into Blender Object.
    @param context: the blender context.
    @param ob: the Blender Object.
    @param origin: cylinder location.
    @param axis: cylinder axis.
    @param radius: cylinder radius.
    @param length: cylinder length.
    @param nseg_theta: cylinder number of segments on base.
    @param nseg_axis: cylinder number of segments along axis.
    @param set_materials: set materials to faces.
    """
    # Create tmp object
    scale_length = context.scene.unit_settings.scale_length
    bpy.ops.mesh.primitive_cylinder_add(
        vertices=nseg_theta, radius=radius / scale_length, depth=length / scale_length,
    )
    ob_tmp = context.object
    # Attach materials before copying Mesh
    for ma in ob.data.materials:
        ob_tmp.data.materials.append(ma)
    # Attach new mesh to original Object, rm tmp
    ob.data = ob_tmp.data
    bpy.data.objects.remove(ob_tmp, do_unlink=True)
    # Set location and rotation for original Object
    matrix_loc = Matrix.Translation(Vector(origin) / scale_length)
    matrix_rot = (
        Vector((0.0, 0.0, 1.0))
        .rotation_difference(Vector(axis).normalized())
        .to_matrix()
        .to_4x4()
    )
    ob.matrix_world = matrix_loc @ matrix_rot @ ob.matrix_world
    # Assign material_slots to faces
    me = ob.data
    n = len(me.materials)
    if not set_materials or n == 0:
        return
    elif n == 1:  # SURF_ID = 'A'
        for face in me.polygons:
            face.material_index = 0
    elif n == 3:  # SURF_IDS = 'A', 'B', 'C'
        for face in me.polygons:
            face.material_index = 1
        if len(me.polygons) > 5:
            me.polygons[-1].material_index = 0
            me.polygons[-4].material_index = 2
    else:
        raise BFException(ob, "Bad GEOM CYLINDER: Wrong SURF_ID/IDS len")


def geom_poly_to_ob(context, ob, ps, extrude):
    """!
    Import GEOM POLY into Blender Mesh.
    @param context: the blender context.
    @param ob: the Blender Object.
    @param ps: the FDS GEOM POLY vector.
    @param ps: the FDS GEOM EXTRUDE quantity.
    """
    # Get the bmesh from existing
    bm = bmesh.new()
    bm = bm.from_mesh(ob.data)
    # Fill the bm.faces
    bm.verts.ensure_lookup_table()
    bm.faces.new(bm.verts[p - 1] for p in ps)  # -1 from F90 to py indexes
    bm.to_mesh(ob.data)
    bm.free()
    # Extrude
    # FIXME


# from XB in Blender units


def xbs_edges_to_mesh(context, me, xbs, matrix):
    """!
    Import xbs edges ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xbs: the xbs edges.
    @param matrix: transform bmesh by matrix before importing.
    """
    bm = bmesh.new()
    bm.from_mesh(me)  # add to current mesh
    scale_length = context.scene.unit_settings.scale_length
    for xb in xbs:
        x0, x1, y0, y1, z0, z1 = (coo / scale_length for coo in xb)
        v0 = bm.verts.new((x0, y0, z0))
        v1 = bm.verts.new((x1, y1, z1))
        bm.edges.new((v0, v1))
    if matrix:
        bm.transform(matrix)
    bm.to_mesh(me)
    bm.free()


def xbs_faces_to_mesh(context, me, xbs, matrix):
    """!
    Import xbs faces ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xbs: the xbs edges.
    @param matrix: transform bmesh by matrix before importing.
    """
    bm = bmesh.new()
    bm.from_mesh(me)  # add to current mesh
    scale_length = context.scene.unit_settings.scale_length
    for xb in xbs:
        x0, x1, y0, y1, z0, z1 = (coo / scale_length for coo in xb)
        if abs(x1 - x0) <= epsilon:
            v0 = bm.verts.new((x0, y0, z0))
            v1 = bm.verts.new((x0, y1, z0))
            v2 = bm.verts.new((x0, y1, z1))
            v3 = bm.verts.new((x0, y0, z1))
        elif abs(y1 - y0) <= epsilon:
            v0 = bm.verts.new((x0, y0, z0))
            v1 = bm.verts.new((x1, y0, z0))
            v2 = bm.verts.new((x1, y0, z1))
            v3 = bm.verts.new((x0, y0, z1))
        elif abs(z1 - z0) <= epsilon:
            v0 = bm.verts.new((x0, y0, z0))
            v1 = bm.verts.new((x0, y1, z0))
            v2 = bm.verts.new((x1, y1, z0))
            v3 = bm.verts.new((x1, y0, z0))
        else:
            raise BFException(me, f"Unrecognized face <{xb}> in XB")
        bm.faces.new((v0, v1, v2, v3))
    if matrix:
        bm.transform(matrix)
    bm.to_mesh(me)
    bm.free()


def xbs_bbox_to_mesh(context, me, xbs, set_materials=False, matrix=None):
    """!
    Import xbs bboxes ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xbs: the xbs edges.
    @param set_materials: if True set material_slots to faces
    @param matrix: transform bmesh by matrix before importing.
    """
    # Set Mesh
    bm = bmesh.new()
    # bm.from_mesh(me)  # add to current mesh FIXME parameter "new_mesh"?
    scale_length = context.scene.unit_settings.scale_length
    for xb in xbs:
        x0, x1, y0, y1, z0, z1 = (coo / scale_length for coo in xb)
        v000 = bm.verts.new((x0, y0, z0))
        v100 = bm.verts.new((x1, y0, z0))
        v110 = bm.verts.new((x1, y1, z0))
        v010 = bm.verts.new((x0, y1, z0))
        v001 = bm.verts.new((x0, y0, z1))
        v101 = bm.verts.new((x1, y0, z1))
        v111 = bm.verts.new((x1, y1, z1))
        v011 = bm.verts.new((x0, y1, z1))
        bm.faces.new((v000, v001, v011, v010))  # -x 0
        bm.faces.new((v111, v101, v100, v110))  # +x 1
        bm.faces.new((v000, v100, v101, v001))  # -y 2
        bm.faces.new((v111, v110, v010, v011))  # +y 3
        bm.faces.new((v000, v010, v110, v100))  # -z 4
        bm.faces.new((v111, v011, v001, v101))  # +z 5
    if matrix:
        bm.transform(matrix)
    bm.to_mesh(me)
    bm.free()
    # Assign material_slots to faces
    n = len(me.materials)
    if not set_materials or n == 0:
        return
    elif n == 1:  # SURF_ID = 'A'
        for face in me.polygons:
            face.material_index = 0
    elif n == 3:  # SURF_IDS = 'A', 'B', 'C'
        for iface, face in enumerate(me.polygons):
            if iface % 6 == 4:
                face.material_index = 0
            elif iface % 6 == 5:
                face.material_index = 2
            else:
                face.material_index = 1
    elif n == 6:  # SURF_ID6 = ...
        for iface, face in enumerate(me.polygons):
            face.material_index = iface % 6
    else:
        raise BFException(me, "Bad GEOM XB: Wrong SURF_ID/IDS/ID6 len")


xbs_to_mesh = {
    "BBOX": xbs_bbox_to_mesh,
    "VOXELS": xbs_bbox_to_mesh,
    "PIXELS": xbs_bbox_to_mesh,
    "EDGES": xbs_edges_to_mesh,
    "FACES": xbs_faces_to_mesh,
}

# FIXME remove bf_xb
def xbs_to_ob(context, ob, xbs, bf_xb=None, matrix=None):
    """!
    Import xbs geometry ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Object.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param xbs: the xbs edges.
    @param bf_xb: the xb parameter between BBOX, VOXELS, FACES, PIXELS, EDGES.
    @param matrix: transform bmesh by matrix before importing.
    @return the new xb parameter between BBOX, VOXELS, FACES, PIXELS, EDGES.
    """
    # log.debug(f"Importing xbs to Object <{ob.name}>")
    if bf_xb:  # force bf_xb
        xbs_to_mesh[bf_xb](
            context=context, me=ob.data, xbs=xbs, matrix=matrix,
        )
    else:  # auto choose
        try:
            bf_xb = "FACES"
            xbs_to_mesh[bf_xb](
                context=context, me=ob.data, xbs=xbs, matrix=matrix,
            )
        except BFException:
            bf_xb = "BBOX"
            xbs_to_mesh[bf_xb](
                context=context, me=ob.data, xbs=xbs, matrix=matrix,
            )
    return bf_xb


# From XYZ in Blender units  # FIXME matrix, extend


def xyzs_vertices_to_mesh(context, me, xyzs):
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xyzs: the xyzs vertices.
    """
    bm = bmesh.new()
    bm.from_mesh(me)  # add to current mesh
    scale_length = context.scene.unit_settings.scale_length
    for xyz in xyzs:
        bm.verts.new(
            (xyz[0] / scale_length, xyz[1] / scale_length, xyz[2] / scale_length)
        )
    bm.to_mesh(me)
    bm.free()


def xyzs_to_ob(context, ob, xyzs):
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Object.
    @param xyzs: the xyzs vertices.
    @param context: the Blender context.
    @param ob: the Blender object.
    @return: the new bf_xyz in CENTER or VERTICES
    """
    # log.debug(f"Importing xyzs to Object <{ob.name}>")
    xyzs_vertices_to_mesh(context=context, me=ob.data, xyzs=xyzs)
    # Set center to the first xyz
    try:  # protect from empty
        xyz = Vector(xyzs[0])
    except IndexError:
        pass
    else:
        ob.data.transform(Matrix.Translation(-xyz))
        ob.matrix_world = Matrix.Translation(xyz) @ ob.matrix_world
    if len(xyzs) == 1:
        return "CENTER"
    else:
        return "VERTICES"


# From PB


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
        if pb[0] == 0:
            xbs.append((pb[1], pb[1], -sl, +sl, -sl, +sl))  # PBX is 0
        elif pb[0] == 1:
            xbs.append((-sl, +sl, pb[1], pb[1], -sl, +sl))  # PBY is 1
        elif pb[0] == 2:
            xbs.append((-sl, +sl, -sl, +sl, pb[1], pb[1]))  # PBZ is 2
        else:
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
