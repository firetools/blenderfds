# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operators to choose IDs for MATL_ID, PROP_ID in free text.
"""

import logging, csv
from bpy.types import Operator
from bpy.props import EnumProperty
from ...types import FDSList
from ... import config, utils

log = logging.getLogger(__name__)

# Helper function


def get_referenced_ids(context, fds_label="SURF"):
    """!
    Get fds_label IDs referenced in Free Text.
    """
    fds_list = FDSList()
    sc = context.scene

    # Get namelists from Free Text
    if sc.bf_config_text:
        fds_list.from_fds(f90_namelists=sc.bf_config_text.as_string())

    # # Get namelists from available CATF files
    # if sc.bf_catf_export:
    #     for item in sc.bf_catf_files:
    #         if not item.bf_export:
    #             continue
    #         filepath = item.name
    #         try:
    #             f90_namelists = utils.io.read_txt_file(filepath)
    #         except IOError:
    #             pass
    #         else:
    #             fds_list.from_fds(f90_namelists=f90_namelists)

    # Prepare list of IDs
    items = list()
    for fds_namelist in fds_list:
        if fds_namelist.fds_label != fds_label:
            continue
        fds_param = fds_namelist.get_fds_label(fds_label="ID")
        if fds_param:
            hid = fds_param.get_value()
            items.append((hid, hid, ""))  # (identifier, name, description)
    items.sort(key=lambda k: k[0])

    return items


# MATL_ID


def _get_matl_items(self, context):
    return get_referenced_ids(context, fds_label="MATL")


class MATERIAL_OT_bf_choose_matl_id(Operator):
    bl_label = "Choose MATL_ID"
    bl_idname = "material.bf_choose_matl_id"
    bl_description = "Choose MATL_ID from MATLs available in Free Text"
    bl_property = "bf_matl_id"

    bf_matl_id: EnumProperty(
        name="MATL_ID",
        description="MATL_ID parameter",
        items=_get_matl_items,  # Updating function
    )

    @classmethod
    def poll(cls, context):
        return context.object and context.object.active_material

    def execute(self, context):
        if self.bf_matl_id:
            context.object.active_material.bf_matl_id = self.bf_matl_id
            self.report({"INFO"}, "MATL_ID set")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, "MATL_ID not set")
            return {"CANCELLED"}

    def invoke(self, context, event):
        context.window_manager.invoke_search_popup(self)
        return {"FINISHED"}


def _get_devc_prop_items(self, context):
    return get_referenced_ids(context, fds_label="PROP")


# PROP_ID


class OBJECT_OT_bf_choose_devc_prop_id(Operator):
    bl_label = "Choose PROP_ID"
    bl_idname = "object.bf_choose_devc_prop_id"
    bl_description = "Choose PROP_ID from PROP namelists available in Free Text"
    bl_property = "bf_devc_prop_id"

    bf_devc_prop_id: EnumProperty(
        name="PROP_ID",
        description="PROP_ID parameter",
        items=_get_devc_prop_items,  # Updating function
    )

    @classmethod
    def poll(cls, context):
        return context.object

    def execute(self, context):
        if self.bf_devc_prop_id:
            context.object.bf_devc_prop_id = self.bf_devc_prop_id
            self.report({"INFO"}, "PROP_ID set")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, "PROP_ID not set")
            return {"CANCELLED"}

    def invoke(self, context, event):
        context.window_manager.invoke_search_popup(self)
        return {"FINISHED"}


# CTRL_ID


def _get_devc_ctrl_items(self, context):
    return get_referenced_ids(context, fds_label="CTRL")


class OBJECT_OT_bf_choose_devc_ctrl_id(Operator):
    bl_label = "Choose CTRL_ID"
    bl_idname = "object.bf_choose_devc_ctrl_id"
    bl_description = "Choose CTRL_ID from CTRL namelists available in Free Text"
    bl_property = "bf_devc_ctrl_id"

    bf_devc_ctrl_id: EnumProperty(
        name="CTRL_ID",
        description="CTRL_ID parameter",
        items=_get_devc_ctrl_items,  # Updating function
    )

    @classmethod
    def poll(cls, context):
        return context.object

    def execute(self, context):
        if self.bf_devc_ctrl_id:
            context.object.bf_devc_ctrl_id = self.bf_devc_ctrl_id
            self.report({"INFO"}, "CTRL_ID set")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, "CTRL_ID not set")
            return {"CANCELLED"}

    def invoke(self, context, event):
        context.window_manager.invoke_search_popup(self)
        return {"FINISHED"}


# DEVC QUANTITY from config.FDS_QUANTITIES


def _get_devc_quantity_items(self, context):
    return (
        (q[0], q[0], q[0])
        for q in csv.reader(config.FDS_QUANTITIES.splitlines())
        if "D" in q[2]
    )


class OBJECT_OT_bf_choose_devc_quantity(Operator):
    bl_label = "Choose QUANTITY for DEVC"
    bl_idname = "object.bf_choose_devc_quantity"
    bl_description = "Choose QUANTITY parameter for DEVC namelist"
    bl_property = "bf_quantity"

    bf_quantity: EnumProperty(
        name="QUANTITY",
        description="QUANTITY parameter for DEVC namelist",
        items=_get_devc_quantity_items,
    )

    @classmethod
    def poll(cls, context):
        return context.object

    def execute(self, context):
        if self.bf_quantity:
            context.object.bf_quantity = self.bf_quantity
            self.report({"INFO"}, "QUANTITY set")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, "QUANTITY not set")
            return {"CANCELLED"}

    def invoke(self, context, event):
        context.window_manager.invoke_search_popup(self)
        return {"FINISHED"}


# Register/unregister

bl_classes = [
    MATERIAL_OT_bf_choose_matl_id,
    OBJECT_OT_bf_choose_devc_prop_id,
    OBJECT_OT_bf_choose_devc_ctrl_id,
    OBJECT_OT_bf_choose_devc_quantity,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
