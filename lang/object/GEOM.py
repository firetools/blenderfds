import logging, os, bpy
from bpy.types import Object
from bpy.props import StringProperty, BoolProperty
from ...types import BFParam, BFNamelistOb, BFException, BFNotImported, FDSParam
from ... import geometry, utils
from .object import OP_ID, OP_FYI, OP_other
from .MOVE import ON_MOVE, OP_MOVE_ID


log = logging.getLogger(__name__)


class OP_GEOM_check_sanity(BFParam):
    label = "Check Sanity While Exporting"
    description = "Check if closed orientable manifold, with no degenerate geometry while exporting"
    bpy_type = Object
    bpy_idname = "bf_geom_check_sanity"
    bpy_prop = BoolProperty
    bpy_default = True

    def draw(self, context, layout):
        ob = self.element
        layout.prop(ob, "bf_geom_check_sanity")


class OP_GEOM_protect(BFParam):  # TODO put in operator? several ops depend on it
    label = "Protect Original"
    description = "Protect original Object geometry while checking its sanity"
    bpy_type = Object
    bpy_idname = "bf_geom_protect"
    bpy_prop = BoolProperty
    bpy_default = True


class OP_GEOM_SURF_ID(BFParam):
    label = "SURF_ID"
    fds_label = "SURF_ID"
    bpy_type = Object

    @property
    def value(self):
        value = geometry.calc_trisurfaces.get_boundary_condition_ids(
            context=None, ob=self.element
        )
        return value or None  # if value is empty, no SURF_ID

    def set_value(self, context, value):
        # A single material is a string
        if isinstance(value, str):
            value = (value,)
        # Delete and refill material_slots
        ob = self.element
        while ob.material_slots:
            ob.active_material_index = 0  # select the top material
            bpy.ops.object.material_slot_remove()  # delete it
        for name in value:
            ma = bpy.data.materials.get(name)
            if not ma:
                raise BFException(self, f"Unknown SURF_ID <{name}>")
            ob.data.materials.append(ma)

    def draw(self, context, layout):
        pass


class OP_GEOM_SURF_IDS(OP_GEOM_SURF_ID):
    label = "SURF_IDS"
    fds_label = "SURF_IDS"
    bpy_type = Object

    def to_fds_param(self, context):
        pass


class OP_GEOM_SURF_ID6(OP_GEOM_SURF_ID):
    label = "SURF_ID6"
    fds_label = "SURF_ID6"
    bpy_type = Object

    def to_fds_param(self, context):
        pass


class OP_GEOM_VERTS(BFParam):
    label = "VERTS"
    fds_label = "VERTS"
    bpy_type = Object

    def set_value(self, context, value):
        geometry.from_fds.geom_verts_to_mesh(context, me=self.element.data, vs=value)

    def to_fds_param(self, context):  # export in ON_GEOM, integrated with VERTS
        pass


class OP_GEOM_FACES(BFParam):
    label = "FACES"
    fds_label = "FACES"
    bpy_type = Object

    def set_value(self, context, value):
        geometry.from_fds.geom_faces_to_mesh(context, me=self.element.data, fss=value)

    def to_fds_param(self, context):  # export in ON_GEOM, integrated with VERTS
        pass


class OP_GEOM_XB(BFParam):
    label = "XB"
    fds_label = "XB"
    bpy_type = Object

    def set_value(self, context, value):
        xbs = (value,)
        if len(xbs[0]) != 6:
            raise BFNotImported(self, f"Unsupported XB value <{value}>")
        ob = self.element
        me = ob.data
        geometry.from_fds.xbs_bbox_to_mesh(
            context=context,
            me=me,
            xbs=xbs,
            set_materials=True,
        )

    def to_fds_param(self, context):
        pass


class OP_GEOM_binary_directory(BFParam):
    # Used in conjuction with OP_GEOM_BINARY_FILE
    # Contains abs path or path relative to saved blend file
    label = "Binary Directory"
    description = "Destination directory for the binary bingeom file"
    bpy_type = Object
    bpy_idname = "bf_geom_binary_directory"
    bpy_prop = StringProperty
    bpy_other = {"subtype": "DIR_PATH", "maxlen": 1024}

    # Taken care at OP_GEOM_BINARY_FILE
    def draw(self, context, layout):
        pass


class OP_GEOM_BINARY_FILE(BFParam):
    label = "BINARY_FILE"
    description = "Name of the binary bingeom file"
    fds_label = "BINARY_FILE"
    bpy_type = Object
    bpy_idname = "data"
    bpy_export = "bf_geom_binary_file_export"
    bpy_export_default = False

    def _get_bingeom_filepath(self, sc, ob):
        return utils.bl_path_to_os(
            bl_path=ob.bf_geom_binary_directory or sc.bf_config_directory or "//",
            name=ob.data.name,
            extension=".bingeom",
        )

    def _get_fds_binary_file(self, sc, ob):
        return utils.bl_path_to_os(
            bl_path=ob.bf_geom_binary_directory or sc.bf_config_directory or "//",
            name=ob.data.name,
            extension=".bingeom",
            bl_start=sc.bf_config_directory,
        )

    def _get_blend_name_and_directory(self, fds_binary_file, sc):
        fds_path = utils.bl_path_to_os(
            bl_path=sc.bf_config_directory or "//",
        )
        return utils.os_filepath_to_bl(
            filepath=fds_binary_file,
            start=fds_path,
        )

    def check(self, context):
        filepath = self._get_bingeom_filepath(sc=context.scene, ob=self.element)
        path = os.path.dirname(filepath)
        if self.element.bf_geom_binary_directory and not os.path.exists(path):
            raise BFException(self, f"Bingeom directory <{path}> not existing")

    def draw(self, context, layout):
        ob, mesh, space = context.object, context.mesh, context.space_data
        active = self.element.bf_geom_binary_file_export
        col = layout.column(align=True, heading=" ")
        col.active = active
        col.prop(ob, "bf_geom_binary_file_export", text="BINARY_FILE")
        if active:
            if ob:  # mesh name, from properties_data_mesh.py
                col.template_ID(ob, "data", text="Name")
            elif mesh:
                col.template_ID(space, "pin_id", text="Name")
            row = col.row()
            row_active = bool(self.element.bf_geom_binary_directory)
            row.active = row_active
            if row_active:
                try:
                    self.check(context)
                except BFException:
                    row.alert = True
            row.prop(ob, "bf_geom_binary_directory", text="Directory")

    def to_fds_param(self, context):
        if not self.exported:
            return
        self.check(context)
        # Calc geometry
        ob = self.element
        vs, fs, ss, _, msg = geometry.to_fds.ob_to_geom(
            context=context,
            ob=self.element,
            check=self.element.bf_geom_check_sanity,
            check_open=not self.element.bf_geom_is_terrain,
            world=not (ob.data.users > 1 or ob.bf_move_id_export),
        )
        # Save .bingeom file
        filepath = self._get_bingeom_filepath(sc=context.scene, ob=self.element)
        utils.write_bingeom_file(
            geom_type=self.element.bf_geom_is_terrain and 2 or 1,
            n_surf_id=len(self.element.data.materials),
            fds_verts=vs,
            fds_faces=fs,
            fds_surfs=ss,
            fds_volus=list(),
            filepath=filepath,
        )
        # Prepare fds_param
        value = self._get_fds_binary_file(sc=context.scene, ob=self.element)
        return FDSParam(fds_label="BINARY_FILE", value=value, msg=msg)

    def from_fds(self, context, value):
        if not value:
            return
        # Calc and assign value
        name, path = self._get_blend_name_and_directory(
            fds_binary_file=value, sc=context.scene
        )
        # Blender bug, assign twice to have it right  # TODO
        self.element.data.name = name
        self.element.data.name = name
        if self.element.data.name != name:
            raise Exception(f"Blender bug")
        self.element.bf_geom_binary_directory = path
        self.set_exported(context, value=True)
        # Load .bingeom file and import it
        filepath = self._get_bingeom_filepath(sc=context.scene, ob=self.element)
        _, vs, fs, ss, _ = utils.read_bingeom_file(filepath)
        geometry.from_fds.geom_to_ob(
            context=context, ob=self.element, vs=vs, fs=fs, ss=ss
        )


class OP_GEOM_MOVE_ID(OP_MOVE_ID):
    fds_label = "MOVE_ID"
    bpy_export_default = None

    @property
    def exported(self):
        ob = self.element
        # export if a bingeom is exported and my data is used by other Objects
        # or if requested
        return (
            ob.bf_geom_binary_file_export and ob.data.users > 1
        ) or ob.bf_move_id_export

    def to_fds_param(self, context):
        # return MOVE_ID="ob_move" and its MOVE namelist
        p = super().to_fds_param(context)
        return p and (p, ON_MOVE(self.element).to_fds_namelist(context))


class OP_GEOM_IS_TERRAIN(BFParam):
    label = "IS_TERRAIN"
    description = "Set if it represents a terrain"
    fds_label = "IS_TERRAIN"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_geom_is_terrain"


class OP_GEOM_EXTEND_TERRAIN(BFParam):
    label = "EXTEND_TERRAIN"
    description = "Set if this terrain needs extension to fully cover the domain"
    fds_label = "EXTEND_TERRAIN"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_geom_extend_terrain"

    @property
    def exported(self):
        ob = self.element
        return ob.bf_geom_is_terrain


class ON_GEOM(BFNamelistOb):
    label = "GEOM"
    description = "Geometry"
    enum_id = 1021
    fds_label = "GEOM"
    bf_params = (
        OP_ID,
        OP_FYI,
        OP_GEOM_check_sanity,
        OP_GEOM_SURF_ID,
        OP_GEOM_SURF_IDS,
        OP_GEOM_SURF_ID6,
        OP_GEOM_VERTS,
        OP_GEOM_FACES,
        OP_GEOM_XB,
        OP_GEOM_binary_directory,
        OP_GEOM_BINARY_FILE,
        OP_GEOM_MOVE_ID,
        OP_GEOM_IS_TERRAIN,
        OP_GEOM_EXTEND_TERRAIN,
        OP_other,
    )
    bf_other = {"appearance": "TEXTURED"}

    def to_fds_namelist(self, context):  # VERTS and FACES are integrated here
        # First populate the Object
        fds_namelist = super().to_fds_namelist(context)
        # Treat VERTS and FACES
        if not self.element.bf_geom_binary_file_export:  # no bingeom?
            # Prepare geometry
            vs, _, _, fss, msg = geometry.to_fds.ob_to_geom(
                context=context,
                ob=self.element,
                check=self.element.bf_geom_check_sanity,
                check_open=not self.element.bf_geom_is_terrain,
                world=not self.element.bf_move_id_export,
            )
            # Send VERTS and FACES
            fds_namelist.fds_params.extend(
                (
                    FDSParam(fds_label="VERTS", value=vs, precision=6),
                    FDSParam(fds_label="FACES", value=fss),
                )
            )
            fds_namelist.msg = msg
        return fds_namelist

    def from_fds(self, context, fds_namelist, free_text=None):
        if fds_namelist.get_by_label("ZVALS") or fds_namelist.get_by_label(
            "POLY"
        ):  # FIXME rm poly when ready
            raise BFNotImported(self, "Not implemented")
        # Get SPHERE
        p_sphere_origin = fds_namelist.get_by_label("SPHERE_ORIGIN", remove=True)
        p_sphere_radius = fds_namelist.get_by_label("SPHERE_RADIUS", remove=True)
        p_n_levels = fds_namelist.get_by_label("N_LEVELS", remove=True)
        # Get CYLINDER
        p_cyl_length = fds_namelist.get_by_label("CYLINDER_LENGTH", remove=True)
        p_cyl_radius = fds_namelist.get_by_label("CYLINDER_RADIUS", remove=True)
        p_cyl_origin = fds_namelist.get_by_label("CYLINDER_ORIGIN", remove=True)
        p_cyl_axis = fds_namelist.get_by_label("CYLINDER_AXIS", remove=True)
        p_cyl_nseg_axis = fds_namelist.get_by_label("CYLINDER_NSEG_AXIS", remove=True)
        p_cyl_nseg_theta = fds_namelist.get_by_label("CYLINDER_NSEG_THETA", remove=True)
        # Get POLY
        p_poly = fds_namelist.get_by_label("POLY", remove=True)
        p_extrude = fds_namelist.get_by_label("EXTRUDE", remove=True)
        # Populate the Object
        super().from_fds(context, fds_namelist)
        # Fill the Mesh
        if len(self.element.data.vertices):
            # Mesh already filled, no special treatment for bingeom
            return
        elif p_sphere_origin:
            # Treat SPHERE
            geometry.from_fds.geom_sphere_to_ob(
                context=context,
                ob=self.element,
                n_levels=3,  # FIXME p_n_levels and p_n_levels.value or 2,
                radius=p_sphere_radius and p_sphere_radius.value or 0.5,
                origin=p_sphere_origin.value,
            )
        elif p_cyl_origin:
            # Treat CYLINDER
            geometry.from_fds.geom_cylinder_to_ob(
                context=context,
                ob=self.element,
                origin=p_cyl_origin.value,
                axis=p_cyl_axis and p_cyl_axis.value or (0.0, 0.0, 1.0),
                radius=p_cyl_radius and p_cyl_radius.value or 1.0,
                length=p_cyl_length and p_cyl_length.value or 2.0,
                nseg_theta=p_cyl_nseg_theta and p_cyl_nseg_theta.value or 8,
                nseg_axis=p_cyl_nseg_axis and p_cyl_nseg_axis.value or 1,
            )
        elif p_poly and p_extrude:
            # Treat POLY
            geometry.from_fds.geom_poly_to_ob(
                context=context,
                ob=self.element,
                ps=p_poly.value,
                extrude=p_extrude.value,
            )
        else:
            raise BFException(self, f"Unknown GEOM type <{fds_namelist}>")

    def draw_operators(self, context, layout):
        ob = context.object
        col = layout.column()
        col.prop(ob, "bf_geom_protect")
        col.operator("object.bf_geom_check_sanity")
        col.operator("object.bf_geom_check_intersections")
