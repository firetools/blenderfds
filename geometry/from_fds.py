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


def geom_to_mesh(context, me, vs, fs=None, ss=None, fss=None, scale_length=None):
    """!
    Import GEOM into Blender Mesh.
    @param context: the blender context.
    @param me: the Blender Mesh.
    @param vs: the FDS GEOM VERTS vertices.
    @param fs: the FDS GEOM faces vector.
    @param ss: the FDS GEOM surfs vector.
    @param fss: the FDS GEOM FACES faces indexes and boundary condition indexes.
    @param scale_length: the scale to use.
    """
    # log.debug(f"Importing geom to Mesh <{me.name}>")
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
    if len(vs) % 3:
        raise BFException(me, f"Bad GEOM: len(vs) is not multiple of 3")
    if len(fs) % 3:
        raise BFException(me, f"Bad GEOM: len(fs) is not multiple of 3")
    if len(ss) != len(fs) // 3:
        raise BFException(me, f"Bad GEOM: len(ss) is not equal to len(faces)")
    # Create new mesh, no addition to existing
    bm = bmesh.new()
    for i in range(0, len(vs), 3):
        bm.verts.new(
            (vs[i] / scale_length, vs[i + 1] / scale_length, vs[i + 2] / scale_length,)
        )
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
        raise BFException(me, f"Bad GEOM: Wrong len(SURF_ID)")
    for iface, face in enumerate(me.polygons):
        face.material_index = ss[iface] - 1  # -1 from F90 to py indexes


def geom_to_ob(context, ob, vs, fs=None, ss=None, fss=None, scale_length=None):
    """!
    Import GEOM into Blender Object.
    @param context: the blender context.
    @param ob: the Blender Object.
    @param vs: the FDS GEOM VERTS vertices.
    @param fs: the FDS GEOM faces vector.
    @param ss: the FDS GEOM surfs vector.
    @param fss: the FDS GEOM FACES faces indexes and boundary condition indexes.
    @param scale_length: the scale to use.
    """
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
    geom_to_mesh(
        context, me=ob.data, vs=vs, fs=fs, ss=ss, fss=fss, scale_length=scale_length
    )


# from XB in Blender units


def xbs_edges_to_mesh(context, me, xbs, scale_length, matrix):
    """!
    Import xbs edges ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xbs: the xbs edges.
    @param scale_length: the scale to use.
    @param matrix: transform bmesh by matrix before importing.
    """
    bm = bmesh.new()
    bm.from_mesh(me)  # add to current mesh
    for xb in xbs:
        x0, x1, y0, y1, z0, z1 = (coo / scale_length for coo in xb)
        v0 = bm.verts.new((x0, y0, z0))
        v1 = bm.verts.new((x1, y1, z1))
        bm.edges.new((v0, v1))
    if matrix:
        bm.transform(matrix)
    bm.to_mesh(me)
    bm.free()


def xbs_faces_to_mesh(context, me, xbs, scale_length, matrix):
    """!
    Import xbs faces ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xbs: the xbs edges.
    @param scale_length: the scale to use.
    @param matrix: transform bmesh by matrix before importing.
    """
    bm = bmesh.new()
    bm.from_mesh(me)  # add to current mesh
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


def xbs_bbox_to_mesh(context, me, xbs, scale_length, set_materials=False, matrix=None):
    """!
    Import xbs bboxes ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xbs: the xbs edges.
    @param scale_length: the scale to use.
    @param set_materials: if True set material_slots to faces
    @param matrix: transform bmesh by matrix before importing.
    """
    # Set Mesh
    bm = bmesh.new()
    # bm.from_mesh(me)  # add to current mesh FIXME parameter "new_mesh"?
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
def xbs_to_ob(context, ob, xbs, scale_length=None, bf_xb=None, matrix=None):
    """!
    Import xbs geometry ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Object.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param xbs: the xbs edges.
    @param scale_length: the scale to use.
    @param bf_xb: the xb parameter between BBOX, VOXELS, FACES, PIXELS, EDGES.
    @param matrix: transform bmesh by matrix before importing.
    @return the new xb parameter between BBOX, VOXELS, FACES, PIXELS, EDGES.
    """
    # log.debug(f"Importing xbs to Object <{ob.name}>")
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
    if bf_xb:  # force bf_xb
        xbs_to_mesh[bf_xb](
            context=context,
            me=ob.data,
            xbs=xbs,
            scale_length=scale_length,
            matrix=matrix,
        )
    else:  # auto choose
        try:
            bf_xb = "FACES"
            xbs_to_mesh[bf_xb](
                context=context,
                me=ob.data,
                xbs=xbs,
                scale_length=scale_length,
                matrix=matrix,
            )
        except BFException:
            bf_xb = "BBOX"
            xbs_to_mesh[bf_xb](
                context=context,
                me=ob.data,
                xbs=xbs,
                scale_length=scale_length,
                matrix=matrix,
            )
    return bf_xb


# From XYZ in Blender units  # FIXME matrix, extend


def xyzs_vertices_to_mesh(context, me, xyzs, scale_length=None):
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param xyzs: the xyzs vertices.
    @param scale_length: the scale to use.
    """
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
    bm = bmesh.new()
    bm.from_mesh(me)  # add to current mesh
    for xyz in xyzs:
        bm.verts.new(
            (xyz[0] / scale_length, xyz[1] / scale_length, xyz[2] / scale_length)
        )
    bm.to_mesh(me)
    bm.free()


def xyzs_to_ob(context, ob, xyzs, scale_length=None):
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Object.
    @param xyzs: the xyzs vertices.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param scale_length: the scale to use.
    @return: the new bf_xyz in CENTER or VERTICES
    """
    # log.debug(f"Importing xyzs to Object <{ob.name}>")
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
    xyzs_vertices_to_mesh(
        context=context, me=ob.data, xyzs=xyzs, scale_length=scale_length
    )
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


def pbs_planes_to_mesh(context, me, pbs, scale_length=None):
    """!
    Import pbs planes ((0,x3,), (0,x7,), (1,y9,), ...) into existing Blender Mesh.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param pbs: the pbs planes.
    @param scale_length: the scale to use.
    """
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
    xbs = list()
    for pb in pbs:
        sl = scale_length
        if pb[0] == 0:
            xbs.append((pb[1], pb[1], -sl, +sl, -sl, +sl))  # PBX is 0
        elif pb[0] == 1:
            xbs.append((-sl, +sl, pb[1], pb[1], -sl, +sl))  # PBY is 1
        elif pb[0] == 2:
            xbs.append((-sl, +sl, -sl, +sl, pb[1], pb[1]))  # PBZ is 2
        else:
            raise AssertionError(f"Unrecognized PB* <{pb}>")
    xbs_faces_to_mesh(context=context, me=me, xbs=xbs, scale_length=scale_length)


def pbs_to_ob(context, ob, pbs, scale_length=None):
    """!
    Import pbs planes ((0,x3,), (0,x7,), (1,y9,), ...) into existing Blender Object.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param pbs: the pbs planes.
    @param scale_length: the scale to use.
    @return "PLANES"
    """
    # log.debug(f"Importing pbs to Object <{ob.name}>")
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
    pbs_planes_to_mesh(context=context, me=ob.data, pbs=pbs, scale_length=scale_length)
    return "PLANES"
