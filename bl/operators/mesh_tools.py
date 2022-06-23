# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operators for the MESH namelist.
"""

from math import sqrt
import logging, bpy
from bpy.types import Operator
from bpy.props import (
    BoolProperty,
    FloatVectorProperty,
    FloatProperty,
    IntProperty,
    EnumProperty,
)
from ...config import DENSITY_P, GRAVITY_P, HEAT_CAPACITY_P, LP, HRR_P, TEMP_P
from ... import utils, lang
from ...types import BFException

log = logging.getLogger(__name__)


class OBJECT_OT_bf_set_suggested_mesh_cell_size(Operator):
    """!
    Suggest MESH cell size according to NUREG 1824, US NRC.
    """

    bl_label = "Set Suggested Cell Size"
    bl_idname = "object.bf_set_suggested_mesh_cell_size"
    bl_description = (
        "Set suggested MESH cell size according to\n"
        "'Verification and Validation of Selected Fire Models\n"
        "for Nuclear power Plant Applications.\n"
        "NUREG 1824, US NRC, 2007'"
    )
    bl_options = {"REGISTER", "UNDO"}

    bf_max_hrr: FloatProperty(
        name="Max HRR [kW]",
        description="Maximum heat release rate during the simulation",
        default=1000.0,
        min=1.0,
        precision=HRR_P,
    )

    bf_ncell: IntProperty(
        name="Cell Number",
        description=(
            "Desired number of cells spanning the characteristic fire diameter D*."
            "The NUREG 1824, US NRC (2007) used a D*/dx ratio between 4 and 16 to"
            "accurately resolve fires in various scenarios."
        ),
        default=16,
        min=4,
    )

    bf_poisson_restriction: BoolProperty(
        name="Poisson Restriction",
        description="Respect FDS Poisson solver restriction on the IJK value.",
        default=False,
    )

    bf_density: FloatProperty(
        name="Density (ρ) [kg/m³]",
        description="Density",
        default=1.204,
        min=0.1,
        precision=DENSITY_P,
    )

    bf_cp: FloatProperty(
        name="Specific Heat (Cp) [KJ/(kg·K)]",
        description="Specific heat at constant pressure",
        default=1.005,
        min=0.1,
        precision=HEAT_CAPACITY_P,
    )

    bf_t: FloatProperty(
        name="Temperature (T∞) [°C]",
        description="Ambient temperature",
        default=20.0,
        min=-273.0,
        precision=TEMP_P,
    )

    bf_g: FloatProperty(
        name="Gravity (g) [m/s²]",
        description="Gravity acceleration",
        default=9.81,
        min=1.0,
        precision=GRAVITY_P,
    )

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.bf_namelist_cls == "ON_MESH"

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "bf_max_hrr")
        layout.prop(self, "bf_ncell")
        layout.prop(self, "bf_density")
        layout.prop(self, "bf_cp")
        layout.prop(self, "bf_t")
        layout.prop(self, "bf_g")
        layout.label(text="Result saved to Free Text and MESH IJK")

    def invoke(self, context, event):
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def execute(self, context):
        # Calculation
        d_star = (
            self.bf_max_hrr
            / (self.bf_density * self.bf_cp * (self.bf_t + 273.15) * sqrt(self.bf_g))
        ) ** (2 / 5)
        d_star_4, d_star_16 = d_star / 4.0, d_star / 16.0
        d_star_d = d_star / self.bf_ncell

        # Set cell size
        ob = context.object
        desired_cs = [
            d_star_d,
        ] * 3
        ob.bf_mesh_ijk = lang.ON_MESH.get_ijk_from_desired_cs(
            context, ob, desired_cs=desired_cs, poisson=self.bf_poisson_restriction
        )

        # Prepare free text
        sc = context.scene
        if not sc.bf_config_text:
            sc.bf_config_text = utils.ui.show_bl_text(
                context=context, bl_text=None, name="New Text"
            )
        texts = (
            f"According to the NUREG 1824, US NRC (2007), given the max HRR: {self.bf_max_hrr:.{HRR_P}f} kW,",
            f"density: {self.bf_density:.{DENSITY_P}f} kg/m³, Cp: {self.bf_cp:.{HEAT_CAPACITY_P}f} KJ/(kg·K), temperature: {self.bf_t:.{TEMP_P}f}°C, gravity: {self.bf_g:.{GRAVITY_P}f} m/s²,",
            f"then the characteristic fire diameter D* is calculated as: {d_star:.{LP}f} m",
            f"and the cell size range for D*/dx ratio between 4 and 16 is: {d_star_16:.{LP}f} ÷ {d_star_4:.{LP}f} m",
        )
        utils.ui.write_bl_text(
            context,
            bl_text=sc.bf_config_text,
            header="-- Suggested cell size calculation",
            texts=texts,
        )
        utils.ui.show_bl_text(context=context, bl_text=sc.bf_config_text)  # refresh

        self.report({"INFO"}, "MESH IJK set")
        return {"FINISHED"}


class OBJECT_OT_bf_set_mesh_cell_size(Operator):
    """!
    Set current MESH cell size.
    """

    bl_label = "Set Cell Size"
    bl_idname = "object.bf_set_mesh_cell_size"
    bl_description = "Set current MESH cell size"
    bl_options = {"REGISTER", "UNDO"}

    bf_cell_sizes: FloatVectorProperty(
        name="Desired Cell Sizes [m]",
        description="Desired MESH cell sizes",
        default=(0.3, 0.3, 0.3),
        min=0.001,
        precision=LP,
        size=3,
    )
    bf_poisson_restriction: BoolProperty(
        name="Poisson Restriction",
        description="Respect FDS Poisson solver restriction on the IJK value.",
        default=True,
    )

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.bf_namelist_cls == "ON_MESH"

    def draw(self, context):
        layout = self.layout
        layout.prop(self, "bf_cell_sizes", text="")
        layout.prop(self, "bf_poisson_restriction")
        layout.label(text="Result saved to MESH IJK")

    def invoke(self, context, event):
        ob = context.object
        # Set default
        self.bf_cell_sizes = lang.ON_MESH.get_cell_sizes(context, ob)
        # Call dialog
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def execute(self, context):
        ob = context.object
        ob.bf_xb, ob.bf_xb_export = "BBOX", True
        ob.bf_mesh_ijk = lang.ON_MESH.get_ijk_from_desired_cs(
            context, ob, self.bf_cell_sizes, self.bf_poisson_restriction
        )
        self.report({"INFO"}, "MESH IJK set")
        return {"FINISHED"}


def _get_mesh_items(self, context):
    obs = utils.geometry.get_exported_obs(context, obs=context.scene.objects)
    sel_ob = context.object
    items = list(
        (ob.name, ob.name, "")
        for ob in obs
        if ob.bf_namelist_cls == "ON_MESH" and ob != sel_ob
    )
    items.sort(key=lambda k: k[0])
    return items


class OBJECT_OT_bf_align_to_mesh(Operator):
    bl_label = "Align To"
    bl_idname = "object.bf_align_to_mesh"
    bl_description = "Align current to a reference MESH"
    bl_options = {"REGISTER", "UNDO"}

    bf_dest_element: EnumProperty(
        name="Reference MESH",
        description="Reference MESH",
        items=_get_mesh_items,  # Updating function
    )

    bf_protect_ref_xb: BoolProperty(
        name="Protect Ref XB",
        description="Protect reference MESH size.",
        default=True,
    )

    bf_protect_ref_cs: BoolProperty(
        name="Protect Ref Cell Size",
        description="Protect reference cell size.",
        default=True,
    )

    bf_poisson_restriction: BoolProperty(
        name="Poisson Restriction",
        description="Respect FDS Poisson solver restriction on the IJK value.",
        default=False,
    )

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.bf_namelist_cls == "ON_MESH"

    def invoke(self, context, event):
        # Call dialog
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def execute(self, context):
        bpy.ops.object.mode_set(mode="OBJECT")

        # Get source and destination objects
        source_element = context.object
        dest_element = context.scene.objects.get(self.bf_dest_element, None)

        if source_element == dest_element:
            self.report({"WARNING"}, "Reference MESH same as currently selected MESH")
            return {"CANCELLED"}
        if not dest_element:
            self.report({"ERROR"}, "No reference MESH selected")
            return {"CANCELLED"}

        # Align
        rijk = dest_element.bf_mesh_ijk  # ref ijk
        rxb = utils.geometry.get_bbox_xb(context, ob=dest_element, world=True)
        mijk = source_element.bf_mesh_ijk
        mxb = utils.geometry.get_bbox_xb(context, ob=source_element, world=True)
        try:
            rijk, rxb, mijk, mxb, msg = lang.ON_MESH.align_meshes(
                rijk,
                rxb,
                mijk,
                mxb,
                poisson=self.bf_poisson_restriction,
                protect_rxb=self.bf_protect_ref_xb,
                protect_rcs=self.bf_protect_ref_cs,
            )
        except BFException as err:
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}

        # Set reference MESH (dest_element)
        dest_element.bf_mesh_ijk = rijk
        if dest_element.data.users > 1:  # only local change
            dest_element.data = bpy.data.meshes.new(dest_element.data.name)
        lang.OP_XB.xbs_to_ob(
            context=context,
            ob=dest_element,
            xbs=(rxb,),
            bf_xb="BBOX",
        )

        # Set aligned MESH (source_element)
        source_element.bf_mesh_ijk = mijk
        if source_element.data.users > 1:  # only local change
            source_element.data = bpy.data.meshes.new(source_element.data.name)
        lang.OP_XB.xbs_to_ob(
            context=context,
            ob=source_element,
            xbs=(mxb,),
            bf_xb="BBOX",
        )
        log.debug(f"<{source_element.name}> to <{dest_element.name}>: {msg}")

        # Update 3dview
        context.view_layer.update()
        self.report({"INFO"}, msg and f"Aligned: {msg}." or "Aligned.")
        return {"FINISHED"}


# currently used only for testing FIXME rm and update test
class OBJECT_OT_bf_align_selected_meshes(Operator):
    bl_label = "Align Selected"
    bl_idname = "object.bf_align_selected_meshes"
    bl_description = "Align selected MESHes to the current Object MESH"
    bl_options = {"REGISTER", "UNDO"}

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.bf_namelist_cls == "ON_MESH"

    def invoke(self, context, event):  # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        bpy.ops.object.mode_set(mode="OBJECT")

        # Get source and destination objects
        source_element = context.object
        dest_elements = set(
            ob
            for ob in context.selected_objects
            if ob.type == "MESH"
            and ob != source_element
            and ob.bf_namelist_cls == "ON_MESH"
        )
        if not dest_elements:
            self.report({"WARNING"}, "No destination Object")
            return {"CANCELLED"}
        if not source_element:
            self.report({"WARNING"}, "No source Object")
            return {"CANCELLED"}

        # Align
        rijk = source_element.bf_mesh_ijk  # ref ijk
        rxb = utils.geometry.get_bbox_xb(context, ob=source_element, world=True)
        for de in dest_elements:
            mijk = de.bf_mesh_ijk
            mxb = utils.geometry.get_bbox_xb(context, ob=de, world=True)
            try:
                rijk, rxb, mijk, mxb, msg = lang.ON_MESH.align_meshes(
                    rijk, rxb, mijk, mxb, poisson=False, protect_rxb=False
                )
            except BFException as err:
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}

            # Set source element
            source_element.bf_mesh_ijk = rijk
            if source_element.data.users > 1:
                source_element.data = bpy.data.meshes.new(source_element.data.name)
            lang.OP_XB.xbs_to_ob(
                context=context,
                ob=source_element,
                xbs=(rxb,),
                bf_xb="BBOX",
            )

            # Set destination element
            de.bf_mesh_ijk = mijk
            if de.data.users > 1:
                de.data = bpy.data.meshes.new(de.data.name)
            lang.OP_XB.xbs_to_ob(
                context=context,
                ob=de,
                xbs=(mxb,),
                bf_xb="BBOX",
            )
            msg = f"<{de.name}> to <{source_element.name}>: {msg}"
            log.debug(msg)

        # Update 3dview
        context.view_layer.update()
        if len(dest_elements) == 1:
            self.report({"INFO"}, f"Alignment: {msg}")
            return {"FINISHED"}
        else:
            self.report({"INFO"}, "Alignment completed (see console log)")
            return {"FINISHED"}


bl_classes = [
    OBJECT_OT_bf_set_suggested_mesh_cell_size,
    OBJECT_OT_bf_set_mesh_cell_size,
    OBJECT_OT_bf_align_selected_meshes,
    OBJECT_OT_bf_align_to_mesh,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
