import bpy, bmesh, logging
from mathutils import Matrix, Vector
from ....types import BFException

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
            (
                vs[i] / scale_length,
                vs[i + 1] / scale_length,
                vs[i + 2] / scale_length,
            )
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
    nseg_axis=1,  # FIXME unused
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
