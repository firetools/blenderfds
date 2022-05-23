from bpy.types import PropertyGroup, UIList
from bpy.props import BoolProperty, StringProperty
from bpy.utils import register_class, unregister_class
import logging

log = logging.getLogger(__name__)

# Prepare for "Other" parameters
# PropertyGroup and UIList
# The PG properties should always be: bf_export, name


class WM_PG_bf_other(PropertyGroup):
    """!
    Blender PropertyGroup for items of 'other' FDS parameters.
    """

    bf_export: BoolProperty(name="Export", default=True)
    name: StringProperty(name="Name")


class WM_UL_bf_other_items(UIList):
    """!
    Blender UIList for items of 'other' FDS parameters.
    """

    def draw_item(self, context, layout, data, item, icon, active_data):
        row = layout.row()
        row.active = item.bf_export
        row = row.split(factor=0.08)
        row.prop(item, "bf_export", text="")
        row.prop(item, "name", text="", emboss=False, icon_value=icon)


bl_classes = [
    WM_PG_bf_other,
    WM_UL_bf_other_items,
]


def register():
    log.info("Register ui_lists...")
    for c in bl_classes:
        register_class(c)


def unregister():
    log.info("Unregister ui_lists...")
    for c in reversed(bl_classes):
        unregister_class(c)
