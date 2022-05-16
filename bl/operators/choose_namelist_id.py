"""!
BlenderFDS, operators to choose IDs for MATL_ID, PROP_ID in free text.
"""

import logging, csv, bpy
from bpy.types import Operator
from bpy.props import EnumProperty
from ...types import FDSList
from ... import config

log = logging.getLogger(__name__)


def _get_namelist_items(self, context, fds_label):
    """!
    Get fds_label namelist IDs available in Free Text.
    """
    fds_list = FDSList()
    sc = context.scene
    # Get namelists from Free Text
    if sc.bf_config_text:
        fds_list.from_fds(f90_namelists=sc.bf_config_text.as_string())
    # Prepare list of IDs
    items = list()
    while True:
        fds_namelist = fds_list.get_fds_label(fds_label=fds_label, remove=True)
        if not fds_namelist:
            break
        fds_param = fds_namelist.get_fds_label(fds_label="ID", remove=True)
        if fds_param:
            hid = fds_param.get_value()
            items.append((hid, hid, ""))
    items.sort(key=lambda k: k[0])
    return items


def _get_matl_items(self, context):
    return _get_namelist_items(self, context, fds_label="MATL")


class MATERIAL_OT_bf_choose_matl_id(Operator):
    """!
    Choose MATL_ID from MATLs available in Free Text.
    """

    bl_label = "Choose MATL_ID"
    bl_idname = "material.bf_choose_matl_id"
    bl_description = "Choose MATL_ID from MATLs available in Free Text"

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
            ma = context.object.active_material
            ma.bf_matl_id = self.bf_matl_id
            self.report({"INFO"}, "MATL_ID parameter set")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, "MATL_ID parameter not set")
            return {"CANCELLED"}

    def invoke(self, context, event):
        ma = context.object.active_material
        try:
            self.bf_matl_id = ma.bf_matl_id
        except TypeError:
            pass
        wm = context.window_manager
        return wm.invoke_props_dialog(self, width=300)

    def draw(self, context):
        self.layout.prop(self, "bf_matl_id", text="")


def _get_devc_prop_items(self, context):
    return _get_namelist_items(self, context, fds_label="PROP")


class OBJECT_OT_bf_choose_devc_prop_id(Operator):
    bl_label = "Choose PROP_ID"
    bl_idname = "object.bf_choose_devc_prop_id"
    bl_description = "Choose PROP_ID from PROPs available in Free Text"
    bl_property = "bf_devc_prop_id"

    bf_devc_prop_id: EnumProperty(
        name="PROP_ID",
        description="PROP_ID parameter",
        items=_get_devc_prop_items,
    )

    @classmethod
    def poll(cls, context):
        return context.object

    def execute(self, context):
        if self.bf_devc_prop_id:
            context.object.bf_devc_prop_id = self.bf_devc_prop_id
            self.report({"INFO"}, "PROP_ID parameter set")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, "PROP_ID parameter not set")
            return {"CANCELLED"}

    def invoke(self, context, event):
        context.window_manager.invoke_search_popup(self)
        return {"FINISHED"}


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
            self.report({"INFO"}, "QUANTITY parameter set")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, "QUANTITY parameter not set")
            return {"CANCELLED"}

    def invoke(self, context, event):
        context.window_manager.invoke_search_popup(self)
        return {"FINISHED"}


bl_classes = [
    MATERIAL_OT_bf_choose_matl_id,
    OBJECT_OT_bf_choose_devc_prop_id,
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
