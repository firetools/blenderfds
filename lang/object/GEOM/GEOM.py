import logging, os, bpy
from bpy.types import Object
from bpy.props import StringProperty, BoolProperty
from ....types import BFParam, BFNamelistOb, BFException, BFNotImported, FDSParam
from .... import utils
from ..object import OP_ID, OP_FYI, OP_other
from ..MOVE import ON_MOVE, OP_MOVE_ID
from . import bingeom
from .ob_to_geom import ob_to_geom, get_boundary_condition_ids
from .geom_to_ob import (
    geom_to_ob,
    geom_sphere_to_ob,
    geom_cylinder_to_ob,
    geom_faces_to_mesh,
    geom_verts_to_mesh,
)
from ..XB.xbs_to_ob import xbs_bbox_to_mesh


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

    def get_value(self):
        value = get_boundary_condition_ids(context=None, ob=self.element)
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
        geom_verts_to_mesh(context, me=self.element.data, vs=value)

    def to_fds_param(self, context):  # export in ON_GEOM, integrated with VERTS
        pass


class OP_GEOM_FACES(BFParam):
    label = "FACES"
    fds_label = "FACES"
    bpy_type = Object

    def set_value(self, context, value):
        geom_faces_to_mesh(context, me=self.element.data, fss=value)

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
        xbs_bbox_to_mesh(
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
    bpy_default = ""  # current fds dir
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

    def _get_bingeom_path_rbl(self, context):
        return (
            self.element.bf_geom_binary_directory or context.scene.bf_config_directory
        )

    def check(self, context):
        # Get absolute path
        path_rbl = self._get_bingeom_path_rbl(context)
        path = utils.io.transform_rbl_to_abs(filepath_rbl=path_rbl)
        # Check existance
        if self.element.bf_geom_binary_directory and not os.path.exists(path):
            raise BFException(self, f"Bingeom directory not existing: <{path}>")

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
        if not self.get_exported():
            return
        self.check(context)
        sc, ob = context.scene, self.element

        # Get filepaths
        filepath, filepath_rfds = utils.io.transform_rbl_to_abs_and_rfds(
            context,
            filepath_rbl=self._get_bingeom_path_rbl(context),
            name=ob.data.name,
            extension=".bingeom",
        )

        # Calc geometry
        ob = self.element
        vs, fs, ss, _, msg = ob_to_geom(
            context=context,
            ob=self.element,
            check=self.element.bf_geom_check_sanity,
            check_open=not self.element.bf_geom_is_terrain,
            world=not (ob.data.users > 1 or ob.bf_move_id_export),
        )

        # Save the .bingeom file  # FIXME one only command
        bingeom.write_bingeom_file(
            geom_type=self.element.bf_geom_is_terrain and 2 or 1,
            n_surf_id=len(self.element.data.materials),
            fds_verts=vs,
            fds_faces=fs,
            fds_surfs=ss,
            fds_volus=list(),
            filepath=filepath,
        )

        # Prepare fds_param, relative to the fds_path
        return FDSParam(fds_label="BINARY_FILE", value=filepath_rfds, msg=msg)

    def from_fds(self, context, value):
        # Get filepaths
        filepath, _, path_rbl, name = utils.io.transform_rfds_to_abs_and_rbl(
            context, filepath_rfds=value
        )

        # Import bingeom geometry to Object
        _, vs, fs, ss, _ = bingeom.read_bingeom_file(filepath)
        geom_to_ob(context=context, ob=self.element, vs=vs, fs=fs, ss=ss)

        # Set Object Data and path
        self.element.data.name = name
        self.element.bf_geom_binary_directory = ""  # disconnect from path_rbl
        self.set_exported(context, value=True)


class OP_GEOM_MOVE_ID(OP_MOVE_ID):
    fds_label = "MOVE_ID"
    bpy_export_default = None

    def get_exported(self):
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

    def get_exported(self):
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
            vs, _, _, fss, msg = ob_to_geom(
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
            fds_namelist.msgs.append(msg)
        return fds_namelist

    def from_fds(self, context, fds_namelist, free_text=None):
        if not (
            "VERTS" in fds_namelist
            or "BINARY_FILE" in fds_namelist
            or "XB" in fds_namelist
        ):
            ps = {  # label, default value
                "SPHERE_ORIGIN": None,
                "SPHERE_RADIUS": 0.5,
                "N_LEVELS": 2,
                "CYLINDER_ORIGIN": None,
                "CYLINDER_LENGTH": 2.0,
                "CYLINDER_RADIUS": 1.0,
                "CYLINDER_AXIS": (0.0, 0.0, 1.0),
                "CYLINDER_NSEG_AXIS": 1,
                "CYLINDER_NSEG_THETA": 8,
                "POLY": None,
                "EXTRUDE": None,  # FIXME default
                "ZVALS": None,
            }
            for key in ps:  # read
                fds_param = fds_namelist.get_fds_param(fds_label=key, remove=True)
                if fds_param:  # assign value
                    ps[key] = fds_param.get_value()
            # Fill the Mesh
            if len(self.element.data.vertices):
                # Mesh already filled, no special treatment for bingeom
                return
            elif ps["SPHERE_ORIGIN"] is not None:
                geom_sphere_to_ob(
                    context=context,
                    ob=self.element,
                    n_levels=ps["N_LEVELS"],
                    radius=ps["SPHERE_RADIUS"],
                    origin=ps["SPHERE_ORIGIN"],
                )
            elif ps["CYLINDER_ORIGIN"] is not None:
                geom_cylinder_to_ob(
                    context=context,
                    ob=self.element,
                    origin=ps["CYLINDER_ORIGIN"],
                    length=ps["CYLINDER_LENGTH"],
                    radius=ps["CYLINDER_RADIUS"],
                    axis=ps["CYLINDER_AXIS"],
                    nseg_axis=ps["CYLINDER_NSEG_AXIS"],
                    nseg_theta=ps["CYLINDER_NSEG_THETA"],
                )
            elif ps["POLY"] is not None:
                raise BFNotImported(self, "POLY not implemented")
            elif ps["ZVALS"] is not None:
                raise BFNotImported(self, "ZVALS not implemented")
            else:
                raise BFException(self, f"Unknown GEOM type <{fds_namelist}>")
        super().from_fds(context, fds_namelist=fds_namelist, free_text=free_text)

    def draw_operators(self, context, layout):
        ob = context.object
        col = layout.column()
        col.prop(ob, "bf_geom_protect")
        col.operator("object.bf_geom_check_sanity")
        col.operator("object.bf_geom_check_intersections")
