"""!
BlenderFDS, custom UIlist operators.
"""

import bpy

from bpy.types import Operator, Object, Material, Scene
from bpy.props import IntProperty, CollectionProperty


# Operator helpers


class _OPSlotAdd:
    """!
    Operator helper to add slot to custom list operator.
    """

    bl_label = "Add"
    bl_description = "Add slot"

    bpy_type = Object  # or Material, Scene
    bpy_idx_idname = "bf_other_idx"
    bpy_idname = "bf_other"

    def set_item(self, context, item):
        """!
        Set item in the new slot.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        @param item: TODO
        """
        pass

    def execute(self, context):
        celem = getattr(context, self.bpy_type.__name__.lower())
        ccollection = getattr(celem, self.bpy_idname)
        item = ccollection.add()
        self.set_item(context, item)
        setattr(celem, self.bpy_idx_idname, len(ccollection) - 1)
        return {"FINISHED"}


class _OPSlotRm:
    """!
    Operator helper to remove slot from custom list operator.
    """

    bl_label = "Remove"
    bl_description = "Remove slot"

    bpy_type = Object  # or Material, Scene
    bpy_idx_idname = "bf_other_idx"
    bpy_idname = "bf_other"

    @classmethod
    def poll(cls, context):
        celem = getattr(context, cls.bpy_type.__name__.lower())
        return getattr(celem, cls.bpy_idname)

    def invoke(self, context, event):
        celem = getattr(context, self.bpy_type.__name__.lower())
        cidx = getattr(celem, self.bpy_idx_idname)
        ccollection = getattr(celem, self.bpy_idname)
        if cidx < 0:  # available item?
            return {"FINISHED"}
        ccollection.remove(cidx)
        setattr(celem, self.bpy_idx_idname, max(0, cidx - 1))
        return {"FINISHED"}


class _OPSlotMv:
    """!
    Operator helper to move slot from custom list operator.
    """

    bl_label = "Move"
    bl_description = "Move slot"

    bpy_type = Object  # or Material, Scene
    bpy_idx_idname = "bf_other_idx"
    bpy_idname = "bf_other"

    direction: bpy.props.EnumProperty(items=(("UP", "Up", ""), ("DOWN", "Down", "")))

    @classmethod
    def poll(cls, context):
        celem = getattr(context, cls.bpy_type.__name__.lower())
        return getattr(celem, cls.bpy_idname)

    def execute(self, context):
        celem = getattr(context, self.bpy_type.__name__.lower())
        cidx = getattr(celem, self.bpy_idx_idname)
        ccollection = getattr(celem, self.bpy_idname)
        delta = -1 if self.direction == "UP" else 1
        neighbor = cidx + delta
        ccollection.move(neighbor, cidx)
        setattr(celem, self.bpy_idx_idname, max(0, min(neighbor, len(ccollection) - 1)))
        return {"FINISHED"}


# Collection helper


def register_collection(bpy_type, bpy_idname, name, bpy_pg, description=""):
    """!
    Helper to register the Blender properties of a collection.
    @param bpy_type: type in bpy.types for Blender property, eg. Object
    @param bpy_idname: prop in bpy.props of Blender property, eg. StringProperty
    @param name: name
    @param bpy_pg: related PropertyGroup for items
    @param description: description
    @return operators for adding, moving and removing
    """
    # Register index bpy_idx_idname
    bpy_idx_idname = f"{bpy_idname}_idx"
    setattr(bpy_type, bpy_idx_idname, IntProperty(name="Index", default=0))
    # Register collection bpy_idname
    setattr(
        bpy_type,
        bpy_idname,
        CollectionProperty(name=name, description=description, type=bpy_pg),
    )
    # Register operators: add, rm, mv
    op_name = f"{bpy_type.__name__.upper()}_OT_{bpy_idname}"
    op_idname = f"{bpy_type.__name__.lower()}.{bpy_idname}"
    op_add = type(
        op_name + "_slot_add",
        (_OPSlotAdd, Operator),
        {
            "bl_idname": op_idname + "_slot_add",
            "bpy_type": bpy_type,
            "bpy_idx_idname": bpy_idx_idname,
            "bpy_idname": bpy_idname,
        },
    )
    op_rm = type(
        op_name + "_slot_rm",
        (_OPSlotRm, Operator),
        {
            "bl_idname": op_idname + "_slot_rm",
            "bpy_type": bpy_type,
            "bpy_idx_idname": bpy_idx_idname,
            "bpy_idname": bpy_idname,
        },
    )
    op_mv = type(
        op_name + "_slot_mv",
        (_OPSlotMv, Operator),
        {
            "bl_idname": op_idname + "_slot_mv",
            "bpy_type": bpy_type,
            "bpy_idx_idname": bpy_idx_idname,
            "bpy_idname": bpy_idname,
        },
    )
    ops = (op_add, op_rm, op_mv)
    for op in ops:
        bpy.utils.register_class(op)
    # Return
    return ops


def unregister_collection(bpy_type, bpy_idname, ops):
    """!
    Helper to unregister the Blender properties of a collection. 
    @param bpy_type: type in bpy.types for Blender property, eg. Object
    @param bpy_idname: prop in bpy.props of Blender property, eg. StringProperty
    @param ops: operators for adding, moving and removing
    """
    bpy_idx_idname = f"{bpy_idname}_idx"
    # Unregister operators
    for op in ops:
        bpy.utils.unregister_class(op)
    # Unregister collection and index
    delattr(bpy_type, bpy_idname)
    delattr(bpy_type, bpy_idx_idname)


def draw_collection(element, context, layout, bpy_type, bpy_idname, name, bpy_ul):
    """!
    Helper to draw the UI of the Blender collection.
    """
    # Init
    bpy_idx_idname = f"{bpy_idname}_idx"
    op_idname = f"{bpy_type.__name__.lower()}.{bpy_idname}"
    # Draw
    layout.label(text=name)
    row = layout.row()
    row.template_list(
        bpy_ul.__name__, "", element, bpy_idname, element, bpy_idx_idname, rows=3
    )
    col = row.column(align=True)
    col.operator(f"{op_idname}_slot_add", icon="ADD", text="")
    col.operator(f"{op_idname}_slot_rm", icon="REMOVE", text="")
    col.separator()
    col.operator(f"{op_idname}_slot_mv", icon="TRIA_UP", text="").direction = "UP"
    col.operator(f"{op_idname}_slot_mv", icon="TRIA_DOWN", text="").direction = "DOWN"
