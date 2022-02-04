import bpy, bmesh, logging
from mathutils import Matrix, Vector
from ...types import BFException
from . import bingeom

log = logging.getLogger(__name__)

epsilon = 1e-5  # TODO unify epsilon mgmt


def geom_to_ob(context, ob, fds_verts=None, fds_faces=None, fds_surfs=None, fds_faces_surfs=None, filepath=None):
    """!
    Import GEOM into Blender Object.
    @param context: the blender context.
    @param ob: the Blender Object.
    @param fds_verts: vertices coordinates in FDS flat format, eg. (x0, y0, z0, x1, y1, ...)
    @param fds_faces: faces connectivity in FDS flat format, eg. (i0, j0, k0, i1, ...)
    @param fds_surfs: boundary condition indexes in FDS flat format, eg. (b0, b1, ...)
    @param fds_faces_surfs: faces connectivity and boundary condition indexes faces connectivity, eg. (i0, j0, k0, b0, i1, ...)
    @param filepath: if set, read from bingeom file.
    """
    if filepath:
        _, fds_verts, fds_faces, fds_surfs, _ = bingeom.read_bingeom_file(filepath)
        fds_faces_surfs = None
    geom_to_mesh(context, me=ob.data, fds_verts=fds_verts, fds_faces=fds_faces, fds_surfs=fds_surfs, fds_faces_surfs=fds_faces_surfs)


def geom_to_mesh(context, me, fds_verts, fds_faces=None, fds_surfs=None, fds_faces_surfs=None):
    """!
    Import GEOM VERTS and FACES into Blender Mesh.
    @param context: the blender context.
    @param me: the Blender Mesh.
    @param fds_verts: vertices coordinates in FDS flat format, eg. (x0, y0, z0, x1, y1, ...)
    @param fds_faces: faces connectivity in FDS flat format, eg. (i0, j0, k0, i1, ...)
    @param fds_surfs: boundary condition indexes in FDS flat format, eg. (b0, b1, ...)
    @param fds_faces_surfs: faces connectivity and boundary condition indexes faces connectivity, eg. (i0, j0, k0, b0, i1, ...)
    """
    # Transform fss to fs and ss
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
        raise BFException(me, f"Bad GEOM: VERTS vector length not multiple of 3")
    if len(fds_faces) % 3:
        raise BFException(me, f"Bad GEOM: FACES vector length not multiple of 3")
    if len(fds_surfs) != len(fds_faces) // 3:
        raise BFException(
            me, f"Bad GEOM: FACE SURFS vector length different from FACES vector"
        )
    if max(fds_surfs) > len(me.materials):
        raise BFException(
            me, f"Bad GEOM: Max FACE SURF index ({max(fds_surfs)}) > SURF_ID {len(me.materials)}",
        )        
    # Create a new bmesh, 
    bm = bmesh.new()
    bm.from_mesh(me)  # add to current mesh
    # Fill the bm.verts
    scale_length = context.scene.unit_settings.scale_length
    for i in range(0, len(fds_verts), 3):
        bm.verts.new(
            (
                fds_verts[i] / scale_length,
                fds_verts[i + 1] / scale_length,
                fds_verts[i + 2] / scale_length,
            )
        )
    # Fill the bm.faces
    bm.verts.ensure_lookup_table()
    for i in range(0, len(fds_faces), 3):
        bm.faces.new(
            (
                bm.verts[fds_faces[i] - 1],  # -1 from F90 to py indexes
                bm.verts[fds_faces[i + 1] - 1],
                bm.verts[fds_faces[i + 2] - 1],
            )
        )
    # Update mesh
    bm.to_mesh(me)
    bm.free()
    # Assign materials to faces
    for iface, face in enumerate(me.polygons):
        material_index = fds_surfs[iface] - 1  # -1 from F90 to py indexes
        face.material_index = material_index

# Special GEOMs


def geom_sphere_to_ob(context, ob, origin, n_levels=2, radius=0.5):
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
        subdivisions=n_levels, radius=radius/scale_length
    )
    ob_tmp = context.object
    # Attach materials before copying Mesh
    for ma in ob.data.materials:
        ob_tmp.data.materials.append(ma)
    # Attach new mesh to original object and rm new object
    ob.data = ob_tmp.data
    bpy.data.objects.remove(ob_tmp, do_unlink=True)
    # Set location and rotation for original Object
    matrix_loc = Matrix.Translation(Vector(origin)/scale_length)
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
        vertices=nseg_theta,
        radius=radius / scale_length,
        depth=length / scale_length,
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
    if set_materials:
        me = ob.data
        match len(me.materials):
            case 0:  # no SURF_ID
                pass
            case 1:  # SURF_ID = 'A'
                for face in me.polygons:
                    face.material_index = 0
            case 3:  # SURF_IDS = 'A', 'B', 'C' (top, sides, bottom)
                for face in me.polygons:
                    face.material_index = 1  # sides
                if len(me.polygons) > 5:
                    me.polygons[-4].material_index = 0  # top
                    me.polygons[-1].material_index = 2  # bottom
            case _:
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
