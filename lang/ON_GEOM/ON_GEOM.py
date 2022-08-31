# SPDX-License-Identifier: GPL-3.0-or-later

import logging, os, bpy
from bpy.types import Object, Mesh
from bpy.props import StringProperty, BoolProperty
from ...types import (
    BFParam,
    BFNamelistOb,
    FDSParam,
    BFException,
    BFNotImported,
    FDSList,
)
from ... import utils, config
from ..bf_object import OP_namelist_cls, OP_ID, OP_FYI, OP_other
from ..ON_MOVE import OP_other_MOVE_ID
from .ob_to_geom import ob_to_geom, get_boundary_condition_ids
from .geom_to_ob import geom_to_ob, geom_to_mesh, geom_sphere_to_ob, geom_cylinder_to_ob
from ..OP_XB.xbs_to_ob import set_materials, xbs_to_ob


log = logging.getLogger(__name__)


class OP_GEOM_SURF_ID(BFParam):
    label = "SURF_ID"
    fds_label = "SURF_ID"
    bpy_type = Object

    def get_value(self, context):
        # When empty send a None, and it is not exported
        return get_boundary_condition_ids(context=context, ob=self.element) or None

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
                raise BFException(self, f"Unknown SURF_ID: <{name}>")
            ob.data.materials.append(ma)

    def draw(self, context, layout):  # only label
        row = layout.split(factor=0.4)
        row.alignment = "RIGHT"
        row.label(text="SURF_ID")
        row.alignment = "EXPAND"
        row.prop(self.element, "material_slots", icon="MATERIAL_DATA", text="")
        row = row.row()
        row.alignment = "RIGHT"
        row.operator("object.material_slot_remove_unused", icon="BRUSH_DATA", text="")


class OP_GEOM_SURF_IDS(OP_GEOM_SURF_ID):  # importing only
    label = "SURF_IDS"
    fds_label = "SURF_IDS"
    bpy_type = Object

    def get_exported(self, context):
        return False

    def draw(self, context, layout):
        pass


class OP_GEOM_SURF_ID6(OP_GEOM_SURF_ID):  # importing only
    label = "SURF_ID6"
    fds_label = "SURF_ID6"
    bpy_type = Object

    def get_exported(self, context):
        return False

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

    def to_fds_list(self, context) -> FDSList:
        ob = self.element
        # Get bingeom filepath
        filepath, filepath_rfds = utils.io.transform_rbl_to_abs_and_rfds(
            context=context,
            filepath_rbl=ob.data.bf_geom_binary_directory,
            name=ob.data.name,
            extension=".bingeom",
        )
        # Check if shared bingeom
        has_move_id = OP_GEOM_MOVE_ID(self.element).get_active(context)
        # Write
        fds_verts, _, _, fds_faces_surfs, msgs = ob_to_geom(
            context=context,
            ob=ob,
            check=ob.data.bf_geom_check_sanity,
            is_open=ob.data.bf_geom_is_terrain,
            world=not has_move_id,
            filepath=filepath,
        )
        if has_move_id:
            msgs.append("Shared BINARY_FILE")
        # Export ASCII version of GEOM, if requested in config
        if config.EXPORT_ASCII_GEOM:  # as comment
            return FDSList(
                iterable=(
                    FDSParam(fds_label="VERTS", value=fds_verts),
                    FDSParam(fds_label="FACES", value=fds_faces_surfs),
                ),
                msgs=msgs,
            )
        else:
            return FDSParam(fds_label=self.fds_label, value=filepath_rfds, msgs=msgs)

    def set_value(self, context, value):
        # Read bingeom
        filepath, _, path_rbl, name = utils.io.transform_rfds_to_abs_and_rbl(
            context=context, filepath_rfds=value
        )
        geom_to_ob(context=context, ob=self.element, filepath=filepath)
        # Set properties
        self.element.data.name = name
        self.element.data.bf_geom_binary_directory = ""  # unlink from original path_rbl


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
        if d and not os.path.exists(
            utils.io.transform_rbl_to_abs(context=context, filepath_rbl=d)
        ):
            raise BFException(self, f"Binary directory not existing: <{d}>")


class OP_GEOM_MOVE_ID(OP_other_MOVE_ID):
    def get_active(self, context):
        # Check if shared bingeom
        return self.element.data.users > 1


class OP_GEOM_check_sanity(BFParam):  # This is a Mesh property
    label = "Check Sanity While Exporting"
    description = "Check if closed orientable manifold, with no degenerate geometry while exporting"
    bpy_type = Mesh
    bpy_idname = "bf_geom_check_sanity"
    bpy_prop = BoolProperty
    bpy_default = True


class OP_GEOM_protect(BFParam):  # This is a Mesh property
    label = "Protect Original Geometry"
    description = "Protect original Object geometry while checking its sanity\n(eg. do not triangulate)"
    bpy_type = Mesh
    bpy_idname = "bf_geom_protect"
    bpy_prop = BoolProperty
    bpy_default = True

    def get_active(self, context):
        return self.element.bf_geom_check_sanity


class OP_GEOM_IS_TERRAIN(BFParam):  # This is a Mesh property
    label = "IS_TERRAIN"
    description = "Set if it represents a terrain"
    fds_label = "IS_TERRAIN"
    fds_default = False
    bpy_type = Mesh
    bpy_prop = BoolProperty
    bpy_idname = "bf_geom_is_terrain"


class OP_GEOM_EXTEND_TERRAIN(BFParam):  # This is a Mesh property
    label = "EXTEND_TERRAIN"
    description = "Set if this terrain needs extension to fully cover the domain"
    fds_label = "EXTEND_TERRAIN"
    fds_default = False
    bpy_type = Mesh
    bpy_prop = BoolProperty
    bpy_idname = "bf_geom_extend_terrain"

    def get_active(self, context):
        return self.element.bf_geom_is_terrain


class ON_GEOM(BFNamelistOb):
    label = "GEOM"
    description = "Geometry"
    collection = "Obstacles"
    enum_id = 1021
    fds_label = "GEOM"
    bf_params = (
        OP_namelist_cls,
        OP_ID,
        OP_FYI,
        OP_GEOM_SURF_ID,  # before bingeom
        OP_GEOM_SURF_IDS,
        OP_GEOM_SURF_ID6,
        OP_GEOM_BINARY_FILE,  # after SURF_ID*
        OP_GEOM_binary_directory,
        OP_GEOM_MOVE_ID,
        OP_GEOM_check_sanity,
        OP_GEOM_protect,
        OP_GEOM_IS_TERRAIN,
        OP_GEOM_EXTEND_TERRAIN,
        OP_other,
    )
    bf_import_order = 200

    def draw_operators(self, context, layout):
        col = layout.column(align=True)
        col.prop(self.element.data, "bf_geom_protect", text="Protect Original")
        col.operator("object.bf_geom_check_sanity")
        col.operator("object.bf_geom_check_intersections")

    def from_fds_list(self, context, fds_list):
        # Read fds_params
        ps = {  # label: default value
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
            fds_param = fds_list.get_fds_param(fds_label=fds_label, remove=True)
            if fds_param:
                ps[fds_label] = fds_param.get_value()  # assign value
        # Read SURF_ID params, to prepare Material slots
        super().from_fds_list(context, fds_list=fds_list, fds_label="SURF_ID")
        super().from_fds_list(context, fds_list=fds_list, fds_label="SURF_IDS")
        super().from_fds_list(context, fds_list=fds_list, fds_label="SURF_ID6")
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
            xbs_to_ob(context=context, ob=self.element, xbs=xbs, bf_xb="BBOX")
            set_materials(self.element)
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
        # Read all other remaining params (eg. BINARY_FILE, MOVE_ID)
        super().from_fds_list(context, fds_list=fds_list)
