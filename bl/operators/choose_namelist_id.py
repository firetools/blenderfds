#
"""!
BlenderFDS, operators to choose IDs for MATL_ID, PROP_ID in free text and CATF files.
"""

import logging
from bpy.types import Operator
from bpy.props import EnumProperty
from ...types import FDSCase
from ... import utils, config

log = logging.getLogger(__name__)


def _get_namelist_items(self, context, fds_label):
    """!
    Get namelist IDs available in Free Text and CATF files.
    """
    fds_case = FDSCase()
    sc = context.scene
    # Get namelists from Free Text
    if sc.bf_config_text:
        fds_case.from_fds(f90=sc.bf_config_text.as_string())
    # Get namelists from available CATF files
    if sc.bf_catf_export:
        for filepath in tuple(item.name for item in sc.bf_catf_files if item.bf_export):
            try:
                f90 = utils.io.read_txt_file(filepath)
            except IOError:
                pass
            else:
                fds_case.from_fds(f90=f90)
    # Prepare list of IDs
    items = list()
    while True:
        fds_namelist = fds_case.get_fds_namelist(fds_label=fds_label, remove=True)
        if not fds_namelist:
            break
        fds_param = fds_namelist.get_fds_param(fds_label="ID", remove=True)
        if fds_param:
            hid = fds_param.get_value()
            items.append((hid, hid, ""))
    items.sort(key=lambda k: k[0])
    return items


def _get_matl_items(self, context):
    return _get_namelist_items(self, context, fds_label="MATL")


class MATERIAL_OT_bf_choose_matl_id(Operator):
    """!
    Choose MATL_ID from MATLs available in Free Text and CATF files.
    """

    bl_label = "Choose MATL_ID"
    bl_idname = "material.bf_choose_matl_id"
    bl_description = "Choose MATL_ID from MATLs available in Free Text and CATF files"

    bf_matl_id: EnumProperty(
        name="MATL_ID",
        description="MATL_ID parameter",
        items=_get_matl_items,  # Updating function
    )

    @classmethod
    def poll(cls, context):
        return context.active_object and context.active_object.active_material

    def execute(self, context):
        if self.bf_matl_id:
            ma = context.active_object.active_material
            ma.bf_matl_id = self.bf_matl_id
            self.report({"INFO"}, "MATL_ID parameter set")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, "MATL_ID parameter not set")
            return {"CANCELLED"}

    def invoke(self, context, event):
        ma = context.active_object.active_material
        try:
            self.bf_matl_id = ma.bf_matl_id
        except TypeError:
            pass
        wm = context.window_manager
        return wm.invoke_props_dialog(self, width=300)

    def draw(self, context):
        self.layout.prop(self, "bf_matl_id", text="")


def _get_prop_items(self, context):
    return _get_namelist_items(self, context, fds_label="PROP")


class OBJECT_OT_bf_choose_devc_prop_id(Operator):
    """!
    Choose PROP_ID from PROPs available in Free Text and CATF files.
    """

    bl_label = "Choose PROP_ID"
    bl_idname = "object.bf_choose_devc_prop_id"
    bl_description = "Choose PROP_ID from PROPs available in Free Text and CATF files"

    bf_devc_prop_id: EnumProperty(
        name="PROP_ID",
        description="PROP_ID parameter",
        items=_get_prop_items,  # Updating function
    )

    @classmethod
    def poll(cls, context):
        return context.active_object

    def execute(self, context):
        if self.bf_devc_prop_id:
            ob = context.active_object
            ob.bf_devc_prop_id = self.bf_devc_prop_id
            self.report({"INFO"}, "PROP_ID parameter set")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, "PROP_ID parameter not set")
            return {"CANCELLED"}

    def invoke(self, context, event):
        ob = context.active_object
        try:
            self.bf_devc_prop_id = ob.bf_devc_prop_id
        except TypeError:
            pass
        wm = context.window_manager
        return wm.invoke_props_dialog(self, width=300)

    def draw(self, context):
        self.layout.prop(self, "bf_devc_prop_id", text="")


class OBJECT_OT_bf_choose_devc_quantity(Operator):
    bl_label = "Choose QUANTITY for DEVC"
    bl_idname = "object.bf_choose_devc_quantity"
    bl_description = "Choose QUANTITY parameter for DEVC namelist"

    bf_quantity: EnumProperty(
        name="QUANTITY",
        description="QUANTITY parameter for DEVC namelist",
        items=config.get_quantity_items(qtype="D"),
    )

    def execute(self, context):
        ob = context.active_object
        ob.bf_quantity = self.bf_quantity
        self.report({"INFO"}, "QUANTITY parameter set")
        return {"FINISHED"}

    def invoke(self, context, event):
        ob = context.active_object
        try:
            self.bf_quantity = ob.bf_quantity  # Manage None
        except TypeError:
            ob.bf_quantity = ""
        # Call dialog
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def draw(self, context):
        self.layout.prop(self, "bf_quantity", text="")


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
