import logging, os, bpy
from bpy.types import Object, Mesh
from bpy.props import StringProperty, BoolProperty
from ....types import BFParam, BFNamelistOb, BFException, BFNotImported, FDSParam
from .... import utils
from ..object import OP_ID, OP_FYI, OP_other
from ..MOVE import ON_MOVE
from .ob_to_geom import ob_to_geom, get_boundary_condition_ids
from .geom_to_ob import (
    geom_to_ob,
    geom_to_mesh,
    geom_sphere_to_ob,
    geom_cylinder_to_ob,
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


class OP_GEOM_protect(BFParam):
    label = "Protect Original Geometry"
    description = "Protect original Object geometry while checking its sanity\n(eg. do not triangulate)"
    bpy_type = Object
    bpy_idname = "bf_geom_protect"
    bpy_prop = BoolProperty
    bpy_default = True


class OP_GEOM_SURF_ID(BFParam):
    label = "SURF_ID"
    fds_label = "SURF_ID"
    bpy_type = Object

    def get_value(self, context):
        value = get_boundary_condition_ids(context=context, ob=self.element)
        return value

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

    def draw(self, context, layout):  # FIXME
        ma_names = ", ".join(self.get_value(context)) or "Empty"
        layout.label(text=f"SURF_ID: {ma_names}")  # FIXME


class OP_GEOM_SURF_IDS(OP_GEOM_SURF_ID):  # importing only
    label = "SURF_IDS"
    fds_label = "SURF_IDS"
    bpy_type = Object

    def to_fds_param(self, context):
        pass

    def draw(self, context, layout):
        pass


class OP_GEOM_SURF_ID6(OP_GEOM_SURF_ID):  # importing only
    label = "SURF_ID6"
    fds_label = "SURF_ID6"
    bpy_type = Object

    def to_fds_param(self, context):
        pass

    def draw(self, context, layout):
        pass


class OP_GEOM_BINARY_FILE(BFParam):
    label = "BINARY_FILE"
    description = "Name of the binary bingeom file"
    fds_label = "BINARY_FILE"
    bpy_type = Object
    bpy_idname = "data"

    def draw(self, context, layout):
        ob, me, space = self.element, self.element.data, context.space_data
        if ob:  # mesh name, from properties_data_mesh.py
            layout.template_ID(ob, "data", text=self.label)
        elif me:
            layout.template_ID(space, "pin_id", text=self.label)

    def to_fds_param(self, context):
        # Write bingeom file
        ob = self.element
        filepath, filepath_rfds = utils.io.transform_rbl_to_abs_and_rfds(
            context=context,
            filepath_rbl=self.element.data.bf_geom_binary_directory,
            name=self.element.data.name,
            extension=".bingeom",
        )
        move_id = ob.data.users > 1 and f"{ob.name}_move"  # shared bingeom
        _, _, _, _, msg = ob_to_geom(
            context=context,
            ob=ob,
            check=ob.bf_geom_check_sanity,
            check_open=not ob.bf_geom_is_terrain,
            world=not move_id,
            filepath=filepath,
        )
        # Prepare FDSParam
        fds_params = list()
        fds_params.append(
            FDSParam(fds_label=self.fds_label, value=filepath_rfds, msg=msg)
        )
        if move_id:  # shared bingeom needs a MOVE namelist
            fds_params.extend(
                (
                    FDSParam(
                        fds_label="MOVE_ID", value=move_id, msg="BINARY_FILE is shared"
                    ),
                    ON_MOVE(ob).to_fds_namelist(context),
                )
            )
        return fds_params  # many

    def set_value(self, context, value):
        # Read bingeom
        filepath, _, _, name = utils.io.transform_rfds_to_abs_and_rbl(
            context=context, filepath_rfds=value
        )
        geom_to_ob(context=context, ob=self.element, filepath=filepath)
        # Set properties
        self.element.data.name = name
        self.element.data.bf_geom_binary_directory = ""  # disconnect from original


class OP_GEOM_binary_directory(BFParam):  # This is a Mesh property
    # Used in conjuction with OP_GEOM_BINARY_FILE
    # Contains abs path or path relative to saved blend file
    # When empty is the same as the fds case file (as in FDS)
    label = "Directory"
    description = "Destination directory for the binary bingeom file"
    bpy_type = Mesh
    bpy_idname = "bf_geom_binary_directory"
    bpy_prop = StringProperty
    bpy_default = ""  # current fds dir
    bpy_other = {"subtype": "DIR_PATH", "maxlen": 1024}

    def check(self, context):
        d = self.element.bf_geom_binary_directory
        if d and not os.path.exists(utils.io.transform_rbl_to_abs(filepath_rbl=d)):
            raise BFException(self, f"Binary directory not existing: <{d}>")


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

    def get_exported(self, context):
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
        OP_GEOM_protect,
        OP_GEOM_BINARY_FILE,
        OP_GEOM_binary_directory,
        OP_GEOM_IS_TERRAIN,
        OP_GEOM_EXTEND_TERRAIN,
        OP_GEOM_SURF_ID,
        OP_GEOM_SURF_IDS,
        OP_GEOM_SURF_ID6,
        OP_other,
    )
    bf_other = {"appearance": "TEXTURED"}

    def from_fds(self, context, fds_namelist):
        # Read fds_params
        ps = {  # label: default value
            "MOVE_ID": None,
            # Traditional
            "VERTS": None,
            "FACES": None,
            # Box
            "XB": None,
            # Sphere
            "SPHERE_ORIGIN": None,
            "SPHERE_RADIUS": 0.5,
            "SPHERE_TYPE": None,
            "N_LEVELS": 2,
            "N_LAT": None,
            "N_LONG": None,
            # Cylinder
            "CYLINDER_ORIGIN": None,
            "CYLINDER_LENGTH": 2.0,
            "CYLINDER_RADIUS": 1.0,
            "CYLINDER_AXIS": (0.0, 0.0, 1.0),
            "CYLINDER_NSEG_AXIS": 1,
            "CYLINDER_NSEG_THETA": 8,
            # Poly
            "POLY": None,
            "EXTRUDE": None,
            # Zval terrain
            "ZVALS": None,
        }
        for fds_label in ps:
            fds_param = fds_namelist.get_fds_param(fds_label=fds_label, remove=True)
            if fds_param:
                ps[fds_label] = fds_param.get_value(context)  # assign value
        # All other params (eg. ID, SURF_ID, BINARY_FILE, ...)
        super().from_fds(context, fds_namelist=fds_namelist)
        # Treat MOVE
        if ps["MOVE_ID"]:
            self.element["MOVE_ID"] = ps[
                "MOVE_ID"
            ]  # scene will apply the transformation
        # Treat alternative geometries
        if ps["VERTS"] and ps["FACES"]:
            geom_to_mesh(
                context,
                me=self.element.data,
                fds_verts=ps["VERTS"],
                fds_faces_surfs=ps["FACES"],
            )
        elif ps["XB"]:
            xbs = (ps["XB"],)  # xbs, not xb
            if len(xbs[0]) != 6:
                raise BFNotImported(self, f"Unsupported XB value: <{ps['XB']}>")
            ob = self.element
            me = ob.data
            xbs_bbox_to_mesh(
                context=context,
                me=me,
                xbs=xbs,
                set_materials=True,
            )
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

    def draw_operators(self, context, layout):
        ob = context.object
        col = layout.column()
        col.operator("object.bf_geom_check_sanity")
        col.operator("object.bf_geom_check_intersections")
