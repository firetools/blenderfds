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


def geom_to_mesh(
    context,
    me,
    fds_verts,
    fds_faces=None,
    fds_surfs=None,
    fds_faces_surfs=None,
    scale_length=None,
):
    """!
    Import GEOM into existing Blender Mesh.
    @param context: the blender context.
    @param me: the Blender Mesh.
    @param fds_verts: the FDS GEOM VERTS vertices.
    @param fds_faces: the FDS GEOM faces vector.
    @param fds_surfs: the FDS GEOM surfs vector.
    @param fds_faces_surfs: the FDS GEOM FACES faces indexes and boundary condition indexes.
    @param scale_length: the scale to use.
    """
    # log.debug(f"Importing geom to Mesh <{me.name}>")
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
    # Transform fds_faces_surfs to fds_faces and fds_surfs
    if fds_faces_surfs:
        if fds_faces or fds_surfs:
            raise AssertionError("Set faces and surfs or faces_surfs, not both")
        else:
            fds_faces, fds_surfs = list(), list()
            for i in range(0, len(fds_faces_surfs), 4):
                fds_faces.extend(fds_faces_surfs[i : i + 3])
                fds_surfs.append(fds_faces_surfs[i + 3])
    # Check input length
    if len(fds_verts) % 3:
        raise BFException(me, f"Bad GEOM: len(fds_verts) is not multiple of 3")
    if len(fds_faces) % 3:
        raise BFException(me, f"Bad GEOM: len(fds_faces) is not multiple of 3")
    if len(fds_surfs) != len(fds_faces) // 3:
        raise BFException(me, f"Bad GEOM: len(fds_surfs) is not equal to len(faces)")
    # Create mesh
    bm = bmesh.new()
    # No: bm.from_mesh(me), mesh is replaced, not added
    for i in range(0, len(fds_verts), 3):
        bm.verts.new(
            (
                fds_verts[i] / scale_length,
                fds_verts[i + 1] / scale_length,
                fds_verts[i + 2] / scale_length,
            )
        )
    bm.verts.ensure_lookup_table()
    for i in range(0, len(fds_faces), 3):
        bm.faces.new(
            (
                bm.verts[fds_faces[i] - 1],  # -1 from F90 to py indexes
                bm.verts[fds_faces[i + 1] - 1],
                bm.verts[fds_faces[i + 2] - 1],
            )
        )
    bm.to_mesh(me)
    bm.free()
    # Check and assign materials to faces
    if max(fds_surfs) > len(me.materials):  # from F90 to py indexes
        raise BFException(me, f"Bad GEOM: Wrong len(SURF_ID)")
    for iface, face in enumerate(me.polygons):
        face.material_index = fds_surfs[iface] - 1  # -1 from F90 to py indexes


# from XB in Blender units


def xbs_edges_to_mesh(xbs, context, me, scale_length=None):
    """!
    Import xbs edges ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Mesh.
    @param xbs: the xbs edges.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param scale_length: the scale to use.
    """
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
    bm = bmesh.new()
    bm.from_mesh(me)  # add to current mesh
    for xb in xbs:
        x0, x1, y0, y1, z0, z1 = (coo / scale_length for coo in xb)
        v0 = bm.verts.new((x0, y0, z0))
        v1 = bm.verts.new((x1, y1, z1))
        bm.edges.new((v0, v1))
    bm.to_mesh(me)
    bm.free()


def xbs_faces_to_mesh(xbs, context, me, scale_length=None):
    """!
    Import xbs faces ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Mesh.
    @param xbs: the xbs edges.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param scale_length: the scale to use.
    """
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
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
    bm.to_mesh(me)
    bm.free()


def xbs_bbox_to_mesh(xbs, context, me, scale_length=None, surf_id=None):
    """!
    Import xbs bboxes ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Mesh.
    @param xbs: the xbs edges.
    @param context: the Blender context.
    @param me: the Blender Mesh.
    @param scale_length: the scale to use.
    @param surf_ids: if True set material_slots to faces
    """
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
    # Set Mesh
    bm = bmesh.new()
    # bm.from_mesh(me)  # add to current mesh FIXME parameter "add"?
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
    bm.to_mesh(me)
    bm.free()
    # Assign material_slots to faces
    n = len(me.materials)
    if not surf_id or n == 0:
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


def xbs_to_ob(xbs, context, ob, scale_length=None, ma=None, bf_xb=None):
    """!
    Import xbs geometry ((x0,x1,y0,y1,z0,z1,), ...) into existing Blender Object.
    @param xbs: the xbs edges.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param scale_length: the scale to use.
    @param ma: the active material.
    @param bf_xb: the xb parameter between BBOX, VOXELS, FACES, PIXELS, EDGES
    @return the new xb parameter between BBOX, VOXELS, FACES, PIXELS, EDGES
    """
    # log.debug(f"Importing xbs to Object <{ob.name}>")
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
    if bf_xb:  # force bf_xb
        xbs_to_mesh[bf_xb](xbs, context, ob.data, scale_length)
    else:  # auto choose
        try:
            bf_xb = "FACES"
            xbs_to_mesh[bf_xb](xbs, context, ob.data, scale_length)
        except:
            bf_xb = "BBOX"
            xbs_to_mesh[bf_xb](xbs, context, ob.data, scale_length)
    if ma:
        ob.active_material = ma
    return bf_xb


# From XYZ in Blender units


def xyzs_vertices_to_mesh(xyzs, context, me, scale_length=None):
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Mesh.
    @param xyzs: the xyzs vertices.
    @param context: the Blender context.
    @param me: the Blender Mesh.
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


def xyzs_to_ob(xyzs, context, ob, scale_length=None, ma=None):
    """!
    Import xyzs vertices ((x0,y0,z0,), ...) into existing Blender Object.
    @param xyzs: the xyzs vertices.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param scale_length: the scale to use.
    @param ma: the active material.
    @return: the new bf_xyz in CENTER or VERTICES
    """
    # log.debug(f"Importing xyzs to Object <{ob.name}>")
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
    xyzs_vertices_to_mesh(xyzs, context, ob.data, scale_length)
    # Set center to the first xyz
    try:  # protect from empty
        xyz = Vector(xyzs[0])
    except IndexError:
        pass
    else:
        ob.data.transform(Matrix.Translation(-xyz))
        ob.matrix_world = Matrix.Translation(xyz) @ ob.matrix_world
    if ma:
        ob.active_material = ma
    if len(xyzs) == 1:
        return "CENTER"
    else:
        return "VERTICES"


# From PB


def pbs_planes_to_mesh(pbs, context, me, scale_length=None):
    """!
    Import pbs planes ((0,x3,), (0,x7,), (1,y9,), ...) into existing Blender Mesh.
    @param pbs: the pbs planes.
    @param context: the Blender context.
    @param me: the Blender Mesh.
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
    xbs_faces_to_mesh(xbs, context, me, scale_length)


def pbs_to_ob(pbs, context, ob, scale_length=None, ma=None):
    """!
    Import pbs planes ((0,x3,), (0,x7,), (1,y9,), ...) into existing Blender Object.
    @param pbs: the pbs planes.
    @param context: the Blender context.
    @param ob: the Blender object.
    @param scale_length: the scale to use.
    @param ma: the active material.
    @return "PLANES"
    """
    # log.debug(f"Importing pbs to Object <{ob.name}>")
    if not scale_length:
        scale_length = context.scene.unit_settings.scale_length
    pbs_planes_to_mesh(pbs, context, ob.data, scale_length)
    if ma:
        ob.active_material = ma
    return "PLANES"
