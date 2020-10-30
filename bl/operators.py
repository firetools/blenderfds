"""!
BlenderFDS, Operator class extensions.
"""

import os, sys, subprocess

import bpy, logging
from bpy.types import (
    bpy_struct,
    PropertyGroup,
    UIList,
    Operator,
    Object,
    Scene,
    Material,
    Collection,
)
from bpy.props import (
    BoolProperty,
    FloatProperty,
    IntProperty,
    IntVectorProperty,
    FloatVectorProperty,
    StringProperty,
    PointerProperty,
    EnumProperty,
    CollectionProperty,
)

from ..types import FDSCase
from ..utils import BFException, BFNotImported, is_iterable
from .. import config
from .. import geometry, gis, io, fds
from ..lang import OP_XB, OP_XYZ, OP_PB

log = logging.getLogger(__name__)


# Collections

bl_classes = list()


def subscribe(cls):
    """!
    Subscribe class to related collections for registration.
    @param cls: class to be registered.
    """
    bl_classes.append(cls)
    return cls


# Load BlenderFDS Settings


@subscribe
class WM_OT_bf_load_blenderfds_settings(Operator):
    """!
    Load default BlenderFDS settings, deleting current data.
    """

    bl_label = "Load Default BlenderFDS Settings"
    bl_idname = "wm.bf_load_blenderfds_settings"
    bl_description = "Load default BlenderFDS settings, deleting current data!"

    def invoke(self, context, event):
        # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        # Set default startup.blend
        filepath = (
            os.path.dirname(sys.modules[__package__].__file__) + "/../startup.blend"
        )
        bpy.ops.wm.open_mainfile(filepath=filepath, load_ui=True, use_scripts=True)
        bpy.ops.wm.save_homefile()
        # Save user preferences
        bpy.ops.wm.save_userpref()
        # Report
        self.report({"INFO"}, "Default BlenderFDS settings loaded")
        return {"FINISHED"}


# GEOM, check geometry sanity and intersections


@subscribe
class OBJECT_OT_bf_check_intersections(Operator):
    """!
    Check self-intersections or intersections with other selected objects.
    """

    bl_label = "Check Intersections"
    bl_idname = "object.bf_geom_check_intersections"
    bl_description = (
        "Check self-intersections or intersections with other selected objects"
    )

    @classmethod
    def poll(cls, context):
        """!
        Test if the operator can be called or not.
        @param context: the Blender context.
        @return True if operator can be called, False otherwise.
        """
        return context.active_object

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        ob = context.active_object
        obs = context.selected_objects
        if obs:
            obs.remove(ob)
        try:
            geometry.calc_trisurfaces.check_intersections(
                context, ob, obs, protect=ob.bf_geom_protect
            )
        except BFException as err:
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        else:
            self.report({"INFO"}, "No intersection")
            return {"FINISHED"}
        finally:
            w.cursor_modal_restore()


@subscribe
class SCENE_OT_bf_check_sanity(Operator):
    """!
    Check if closed orientable manifold, with no degenerate geometry.
    """

    bl_label = "Check Sanity"
    bl_idname = "object.bf_geom_check_sanity"
    bl_description = "Check if closed orientable manifold, with no degenerate geometry"

    @classmethod
    def poll(cls, context):
        """!
        Test if the operator can be called or not.
        @param context: the Blender context.
        @return True if operator can be called, False otherwise.
        """
        return context.active_object

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        ob = context.active_object
        try:
            geometry.calc_trisurfaces.check_geom_sanity(
                context,
                ob,
                protect=ob.bf_geom_protect,
                check_open=not ob.bf_geom_is_terrain,
            )
        except BFException as err:
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        else:
            self.report({"INFO"}, "Geometry sanity ok")
            return {"FINISHED"}
        finally:
            w.cursor_modal_restore()


# Show FDS code


class _show_fds_code:
    """!
    Helper for showing fds code operators
    """

    def draw(self, context):
        """!
        Draw function for the operator.
        @param context: the Blender context.
        """
        if self.lines:
            lines = self.lines.split("\n")
        else:
            lines = ("No FDS code is exported",)
        if len(lines) > 60:
            lines = lines[:55] + ["..."] + lines[-4:]
        layout = self.layout
        for line in lines:
            layout.label(text=line)

    def execute(self, context):
        self.report({"INFO"}, "FDS Code Shown")
        return {"FINISHED"}

    def _get_lines(self, context):
        """!
        Placeholder method to get text lines.
        @param context: the Blender context.
        @return text
        """
        return str()

    def invoke(self, context, event):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        try:
            self.lines = self._get_lines(context)  # get FDS code
        except BFException as err:
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        else:
            wm = context.window_manager
            return wm.invoke_props_dialog(self, width=600)
        finally:
            w.cursor_modal_restore()


@subscribe
class OBJECT_OT_bf_show_fds_code(_show_fds_code, Operator):
    """!
    Show FDS code exported from current Object.
    """

    bl_label = "FDS Code"
    bl_idname = "object.bf_show_fds_code"
    bl_description = "Show FDS code exported from current Object"

    @classmethod
    def poll(cls, context):
        """!
        Test if the operator can be called or not.
        @param context: the Blender context.
        @return True if operator can be called, False otherwise.
        """
        return context.active_object

    def _get_lines(self, context):
        """!
        Get Object related FDS code.
        @param context: the Blender context.
        @return text
        """
        return context.active_object.to_fds(context)


@subscribe
class MATERIAL_OT_bf_show_fds_code(_show_fds_code, Operator):
    """!
    Show FDS code exported from current Material.
    """

    bl_label = "FDS Code"
    bl_idname = "material.bf_show_fds_code"
    bl_description = "Show FDS code exported from current Material"

    @classmethod
    def poll(cls, context):
        """!
        Test if the operator can be called or not.
        @param context: the Blender context.
        @return True if operator can be called, False otherwise.
        """
        return context.active_object and context.active_object.active_material

    def _get_lines(self, context):
        """!
        Get Material related FDS code.
        @param context: the Blender context.
        @return text
        """
        return context.active_object.active_material.to_fds(context)


@subscribe
class SCENE_OT_bf_show_fds_code(_show_fds_code, Operator):
    """!
    Show FDS code exported from Scene.
    """

    bl_label = "FDS Code"
    bl_idname = "scene.bf_show_fds_code"
    bl_description = "Show FDS code exported from Scene"

    @classmethod
    def poll(cls, context):
        return context.scene

    def _get_lines(self, context):
        """!
        Get Scene related FDS code.
        @param context: the Blender context.
        @return text
        """
        return context.scene.to_fds(context)


# Show exported geometry


@subscribe
class OBJECT_OT_bf_show_fds_geometry(Operator):
    """!
    Show geometry of Object as exported to FDS.
    """

    bl_label = "FDS Geometry"
    bl_idname = "object.bf_show_fds_geometry"
    bl_description = "Show/Hide geometry as exported to FDS"

    @classmethod
    def poll(cls, context):
        return context.object

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        ob = context.object
        # Hide
        if ob.bf_is_tmp or ob.bf_has_tmp:
            geometry.utils.rm_tmp_objects()
            try:
                context.view_layer.objects.active = ob
            except ReferenceError:
                try:
                    context.view_layer.objects.active = context.selected_objects[0]
                except IndexError:
                    pass
            w.cursor_modal_restore()
            self.report({"INFO"}, "Temporary geometry hidden")
            return {"FINISHED"}
        # Show
        bf_namelist = ob.bf_namelist
        if not bf_namelist.exported:
            w.cursor_modal_restore()
            self.report({"WARNING"}, "Not exported, nothing to show!")
            return {"CANCELLED"}
        # Manage GEOM
        if ob.bf_namelist_cls == "ON_GEOM" and not ob.hide_render:  # was bf_export
            try:
                vs, fs, ss, _, msg = geometry.to_fds.ob_to_geom(
                    context=context,
                    ob=ob,
                    check=ob.bf_geom_check_sanity,
                    check_open=not ob.bf_geom_is_terrain,
                    world=True,
                )
            except BFException as err:
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}
            else:
                ob_tmp = geometry.utils.get_tmp_object(
                    context, ob, f"{ob.name}_GEOM_tmp"
                )
                geometry.from_fds.geom_to_ob(
                    context=context, ob=ob_tmp, vs=vs, fs=fs, ss=ss
                )
                ob_tmp.show_wire = True
                self.report({"INFO"}, msg)
                return {"FINISHED"}
            finally:
                w.cursor_modal_restore()
        # Manage XB, XYZ, PB*
        msgs, ob_tmp = list(), None
        # XB
        if ob.bf_xb_export and ob.bf_namelist.bf_param_xb:
            try:
                xbs, msg = geometry.to_fds.ob_to_xbs(context=context, ob=ob)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}
            else:
                msgs.append(msg)
                ob_tmp = geometry.utils.get_tmp_object(
                    context=context, ob=ob, name=f"{ob.name}_XB_tmp"
                )
                geometry.from_fds.xbs_to_ob(
                    context=context, ob=ob_tmp, xbs=xbs, bf_xb=ob.bf_xb,
                )
                ob_tmp.active_material = ob.active_material
                ob_tmp.show_wire = True
        # XYZ
        if ob.bf_xyz_export and ob.bf_namelist.bf_param_xyz:
            try:
                xyzs, msg = geometry.to_fds.ob_to_xyzs(context=context, ob=ob)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}
            else:
                msgs.append(msg)
                ob_tmp = geometry.utils.get_tmp_object(
                    context=context, ob=ob, name=f"{ob.name}_XYZ_tmp"
                )
                geometry.from_fds.xyzs_to_ob(context=context, ob=ob_tmp, xyzs=xyzs)
                ob_tmp.active_material = ob.active_material
                ob_tmp.show_wire = True
        # PB
        if ob.bf_pb_export and ob.bf_namelist.bf_param_pb:
            try:
                pbs, msg = geometry.to_fds.ob_to_pbs(context=context, ob=ob)
            except BFException as err:
                w.cursor_modal_restore()
                self.report({"ERROR"}, str(err))
                return {"CANCELLED"}
            else:
                msgs.append(msg)
                ob_tmp = geometry.utils.get_tmp_object(
                    context=context, ob=ob, name=f"{ob.name}_PB*_tmp"
                )
                geometry.from_fds.pbs_to_ob(context=context, ob=ob_tmp, pbs=pbs)
                ob_tmp.active_material = ob.active_material
                ob_tmp.show_wire = True
        # Close
        w.cursor_modal_restore()
        msg = "; ".join(msg for msg in msgs if msg)
        if ob_tmp:
            self.report({"INFO"}, msg or "Geometry exported.")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, msg or "No geometry exported.")
            return {"CANCELLED"}


# Show text in Blender text editor


@subscribe
class SCENE_OT_bf_show_text(Operator):
    """!
    Show free text in the editor.
    """

    bl_label = "Show Free Text"
    bl_idname = "scene.bf_show_text"
    bl_description = "Show free text in the editor"

    def execute(self, context):
        te = context.scene.bf_config_text
        # If not existing, create one
        if not te:
            bpy.ops.text.new()
            te = bpy.data.texts[-1]
            context.scene.bf_config_text = te
        # Show text in text editor
        done = False
        for w in context.window_manager.windows:
            for area in w.screen.areas:
                if area.type == "TEXT_EDITOR":
                    space = area.spaces[0]
                    space.text = te
                    space.show_line_numbers = True
                    space.show_line_highlight = True
                    space.show_word_wrap = True
                    space.show_margin = True
                    space.margin_column = 130
                    space.show_syntax_highlight = True
                    done = True
                    break
        if done:
            self.report({"INFO"}, f"See <{te.name}> in Blender text editor")
            return {"FINISHED"}
        else:
            self.report({"WARNING"}, f"Open a Blender text editor first")
            return {"CANCELLED"}


# Dialog box operator


@subscribe
class WM_OT_bf_dialog(Operator):
    """!
    BlenderFDS Dialog.
    """

    bl_label = "BlenderFDS"
    bl_idname = "wm.bf_dialog"
    bl_description = "BlenderFDS Dialog"

    type: EnumProperty(
        name="Type",
        items=(("INFO", "Information", "Information"), ("ERROR", "Error", "Error")),
        description="Dialog type",
        default="INFO",
    )

    msg: StringProperty(
        name="Message", description="Dialog message", default="No message"
    )

    description: StringProperty(name="Description", description="Dialog description")

    def execute(self, context):
        return {"FINISHED"}

    def invoke(self, context, event):
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def draw(self, context):
        """!
        Draw function for the operator.
        @param context: the Blender context.
        """
        layout = self.layout
        col = layout.column()
        col.label(text=self.msg, icon=self.type)
        if self.description:
            col.separator()
            descriptions = self.description.splitlines()
            for description in descriptions:
                row = col.row()
                row.label(text=description)


# Copy FDS parameters between Blender elements

from .. import lang


def _bf_props_copy(context, source_element, dest_elements):
    """!
    Copy all parameters from source_element to dest_elements.
    """
    # Get bf_namelists
    if isinstance(source_element, Scene):
        bf_namelists = source_element.bf_namelists
    else:
        bf_namelists = tuple((source_element.bf_namelist,))
        for dest_element in dest_elements:
            dest_element.bf_namelist_cls = source_element.bf_namelist_cls
    # Copy them
    for bf_namelist in bf_namelists:
        for dest_element in dest_elements:
            bf_namelist.copy_to(dest_element)


@subscribe
class SCENE_OT_bf_copy_props_to_scene(Operator):
    """!
    Copy all current scene FDS parameters to another Scene.
    """

    bl_label = "Copy To"
    bl_idname = "scene.bf_props_to_scene"
    bl_description = "Copy all current scene FDS parameters to another Scene"
    bl_options = {"REGISTER", "UNDO"}

    bf_destination_element: StringProperty(name="Destination Scene")

    def draw(self, context):
        """!
        Draw function for the operator.
        @param context: the Blender context.
        """
        layout = self.layout
        row = layout.row()
        row.prop_search(
            self, "bf_destination_element", bpy.data, "scenes", text="Scene"
        )

    def execute(self, context):
        # Get source and destination scenes
        source_element = context.scene
        destination_elements = (
            bpy.data.scenes.get(self.bf_destination_element, None),
        )  # a tuple!
        if not destination_elements[0]:
            self.report({"WARNING"}, "No destination Scene")
            return {"CANCELLED"}
        if not source_element:
            self.report({"WARNING"}, "No source Scene")
            return {"CANCELLED"}
        # Copy
        _bf_props_copy(context, source_element, destination_elements)
        self.report(
            {"INFO"}, f"Copied to destination Scene <{destination_elements[0].name}>"
        )
        return {"FINISHED"}

    def invoke(self, context, event):
        # Call dialog
        wm = context.window_manager
        return wm.invoke_props_dialog(self)


@subscribe
class OBJECT_OT_bf_copy_FDS_properties_to_sel_obs(Operator):
    """!
    Copy current object FDS parameters to selected Objects.
    """

    bl_label = "Copy To"
    bl_idname = "object.bf_props_to_sel_obs"
    bl_description = "Copy current object FDS parameters to selected Objects"
    bl_options = {"REGISTER", "UNDO"}

    @classmethod
    def poll(cls, context):
        """!
        Test if the operator can be called or not.
        @param context: the Blender context.
        @return True if operator can be called, False otherwise.
        """
        return context.active_object

    def invoke(self, context, event):
        # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        bpy.ops.object.mode_set(mode="OBJECT")
        # Get source and destination objects
        source_element = context.active_object
        destination_elements = set(
            ob
            for ob in context.selected_objects
            if ob.type == "MESH" and ob != source_element
        )
        if not destination_elements:
            self.report({"WARNING"}, "No destination Object")
            return {"CANCELLED"}
        if not source_element:
            self.report({"WARNING"}, "No source Object")
            return {"CANCELLED"}
        # Copy
        _bf_props_copy(context, source_element, destination_elements)
        self.report(
            {"INFO"}, f"Copied to {len(destination_elements)} selected Object(s)",
        )
        return {"FINISHED"}


@subscribe
class MATERIAL_OT_bf_assign_BC_to_sel_obs(Operator):
    """!
    Assign current boundary condition to selected Objects.
    """

    bl_label = "Assign To"
    bl_idname = "material.bf_surf_to_sel_obs"
    bl_description = "Assign current boundary condition to selected Objects"
    bl_options = {"REGISTER", "UNDO"}

    @classmethod
    def poll(cls, context):
        """!
        Test if the operator can be called or not.
        @param context: the Blender context.
        @return True if operator can be called, False otherwise.
        """
        source_element = context.active_object
        active_material = source_element.active_material
        return source_element and active_material

    def invoke(self, context, event):
        # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        bpy.ops.object.mode_set(mode="OBJECT", toggle=False)
        # Get source and destination materials
        source_element = context.active_object
        active_material = source_element.active_material
        destination_elements = set(
            ob
            for ob in context.selected_objects
            if ob.type == "MESH" and ob != source_element
        )
        if not destination_elements:
            self.report({"WARNING"}, "No destination Object")
            return {"CANCELLED"}
        if not source_element:
            self.report({"WARNING"}, "No source Object")
            return {"CANCELLED"}
        if not active_material:
            self.report({"WARNING"}, "No boundary condition to assign")
            return {"CANCELLED"}
        # Loop on objects
        for ob in destination_elements:
            ob.active_material = active_material
            log.debug(f"Assign Material <{active_material.name}> -> <{ob.name}>")
        # Set myself as exported
        active_material.bf_surf_export = True
        # Return
        self.report(
            {"INFO"},
            f"Assigned Material <{active_material.name}> to {len(destination_elements)} selected Object(s)",
        )
        return {"FINISHED"}


# Choose IDs for MATL_ID, PROP_ID in free text and CATF files


def _get_namelist_items(self, context, label) -> "items":
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
                f90 = io.read_txt_file(filepath)
            except IOError:
                pass
            else:
                fds_case.from_fds(f90)
    # Prepare list of IDs
    items = list()
    for fds_namelist in fds_case.fds_namelists:
        if fds_namelist.label == label:
            fds_param = fds_namelist.get_by_label("ID")
            if fds_param:
                hid = fds_param.value
                items.append((hid, hid, ""))
    items.sort(key=lambda k: k[0])
    return items


def _get_matl_items(self, context):
    return _get_namelist_items(self, context, "MATL")


@subscribe
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
        """!
        Test if the operator can be called or not.
        @param context: the Blender context.
        @return True if operator can be called, False otherwise.
        """
        return context.active_object and context.active_object.active_material

    def execute(self, context):
        ma = context.active_object.active_material
        ma.bf_matl_id = self.bf_matl_id
        self.report({"INFO"}, "MATL_ID parameter set")
        return {"FINISHED"}

    def invoke(self, context, event):
        ma = context.active_object.active_material
        try:
            self.bf_matl_id = ma.bf_matl_id
        except TypeError:
            pass
        wm = context.window_manager
        return wm.invoke_props_dialog(self, width=300)

    def draw(self, context):
        """!
        Draw function for the operator.
        @param context: the Blender context.
        """
        self.layout.prop(self, "bf_matl_id", text="")


def _get_prop_items(self, context):
    return _get_namelist_items(self, context, "PROP")


@subscribe
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
        ob = context.active_object
        ob.bf_devc_prop_id = self.bf_devc_prop_id
        self.report({"INFO"}, "PROP_ID parameter set")
        return {"FINISHED"}

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


# Choose DEVC QUANTITY


@subscribe
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


# MESH Tools


@subscribe
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
        precision=3,
        size=3,
    )
    bf_poisson_restriction: BoolProperty(
        name="Poisson Restriction",
        description="Respect FDS Poisson solver restriction on IJK value while setting desired cell sizes.",
        default=True,
    )

    @classmethod
    def poll(cls, context):
        """!
        Test if the operator can be called or not.
        @param context: the Blender context.
        @return True if operator can be called, False otherwise.
        """
        ob = context.active_object
        return ob and ob.bf_namelist_cls == "ON_MESH"

    def draw(self, context):
        """!
        Draw function for the operator.
        @param context: the Blender context.
        """
        layout = self.layout
        layout.prop(self, "bf_cell_sizes", text="")
        layout.prop(self, "bf_poisson_restriction")

    def invoke(self, context, event):
        ob = context.active_object
        # Set default
        xb = geometry.utils.get_bbox_xb(context=context, ob=ob, world=True)
        self.bf_cell_sizes = fds.mesh_tools.calc_cell_sizes(ijk=ob.bf_mesh_ijk, xb=xb,)
        # Call dialog
        wm = context.window_manager
        return wm.invoke_props_dialog(self)

    def execute(self, context):
        ob = context.active_object
        ob.bf_xb, ob.bf_xb_export = "BBOX", True
        xb = geometry.utils.get_bbox_xb(context=context, ob=ob, world=True)
        ob.bf_mesh_ijk = fds.mesh_tools.calc_ijk(
            xb=xb, desired_cs=self.bf_cell_sizes, poisson=self.bf_poisson_restriction
        )
        self.report({"INFO"}, "MESH cell size set")
        return {"FINISHED"}


# FIXME FIXME FIXME  align meshes


@subscribe
class OBJECT_OT_bf_align_selected_meshes(Operator):
    bl_label = "Align Selected"
    bl_idname = "object.bf_align_selected_meshes"
    bl_description = "Align selected MESHes to the current Object MESH"
    bl_options = {"REGISTER", "UNDO"}

    @classmethod
    def poll(cls, context):
        ob = context.active_object
        return ob and ob.bf_namelist_cls == "ON_MESH"

    def invoke(self, context, event):  # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        bpy.ops.object.mode_set(mode="OBJECT")
        # Get source and destination objects
        source_element = context.active_object
        destination_elements = set(
            ob
            for ob in context.selected_objects
            if ob.type == "MESH"
            and ob != source_element
            and ob.bf_namelist_cls == "ON_MESH"
        )
        if not destination_elements:
            self.report({"WARNING"}, "No destination Object")
            return {"CANCELLED"}
        if not source_element:
            self.report({"WARNING"}, "No source Object")
            return {"CANCELLED"}
        # Align
        rijk = source_element.bf_mesh_ijk  # ref ijk
        rxb = geometry.utils.get_bbox_xb(context, ob=source_element, world=True)
        for de in destination_elements:
            mijk = de.bf_mesh_ijk
            mxb = geometry.utils.get_bbox_xb(context, ob=de, world=True)
            rijk, rxb, mijk, mxb, msgs = fds.mesh_tools.align_meshes(
                rijk, rxb, mijk, mxb, poisson=False, protect_rl=False
            )
            source_element.bf_mesh_ijk = rijk
            matrix = source_element.matrix_world.invert()
            geometry.from_fds.xbs_to_ob(
                context=context,
                ob=source_element,
                xbs=(rxb,),
                bf_xb="BBOX",
                matrix=matrix,
            )
            de.bf_mesh_ijk = mijk
            matrix = de.matrix_world.invert()
            geometry.from_fds.xbs_to_ob(
                context=context, ob=de, xbs=(mxb,), bf_xb="BBOX", matrix=matrix,
            )
            log.debug("\n".join(msgs))
        # Update 3dview
        context.view_layer.update()
        self.report({"INFO"}, "MESH Objects aligned")
        return {"FINISHED"}


# GIS


class _bf_set_geoloc:
    """!
    Set geographic location (WGS84).
    """

    bl_label = "Set Geolocation"
    # bl_idname = "scene.bf_set_geoloc"
    bl_description = "Set geographic location (WGS84)"
    bl_options = {"REGISTER", "UNDO"}

    show: BoolProperty(name="Show Geolocation", default=False)

    bf_lon: FloatProperty(
        name="Longitude",
        description="Longitude (WGS84, EPSG:4326) in decimal degrees",
        min=-180,
        max=+180,
        precision=6,
    )

    bf_lat: FloatProperty(
        name="Latitude",
        description="Latitude (WGS84, EPSG:4326) in decimal degrees",
        min=-80,
        max=+84,
        precision=9,
    )

    def draw(self, context):
        """!
        Draw function for the operator.
        @param context: the Blender context.
        """
        col = self.layout.column(align=True)
        col.label(text="Error on horizontal geoposition < 1 m")
        col.prop(self, "bf_lon", text="Longitude")
        col.prop(self, "bf_lat", text="Latitude")

    def _get_loc(self, context):
        """!
        Placeholder method to get the XYZ location
        @param context: the Blender context.
        @return xyz location
        """
        return 0.0, 0.0, 0.0

    def _set_loc(self, context, xyz):
        """!
        Placeholder method to set the XYZ location
        @param context: the Blender context.
        @param xyz location
        """
        pass

    def execute(self, context):
        # Get loc, use only unmodified z
        _, _, z = self._get_loc(context)
        sc = context.scene
        utm = gis.LonLat(lon=self.bf_lon, lat=self.bf_lat,).to_UTM(
            force_zn=sc.bf_origin_utm_zn, force_ne=sc.bf_origin_utm_ne
        )
        # Compute loc relative to scene origin
        scale_length = sc.unit_settings.scale_length
        x = (utm.easting - sc.bf_origin_utm_easting) / scale_length
        y = (utm.northing - sc.bf_origin_utm_northing) / scale_length
        self._set_loc(context, (x, y, z))
        self.report({"INFO"}, "Geolocation set")
        return {"FINISHED"}

    def invoke(self, context, event):
        sc = context.scene
        # Check origin geolocation
        if not sc.bf_misc_export or not sc.bf_origin_export:
            self.report({"ERROR"}, "Set origin geolocation in MISC namelist.")
            return {"FINISHED"}
        # Get loc, convert to set default
        x, y, _ = self._get_loc(context)
        scale_length = sc.unit_settings.scale_length
        utm = gis.UTM(
            zn=sc.bf_origin_utm_zn,
            ne=sc.bf_origin_utm_ne,
            easting=sc.bf_origin_utm_easting + x * scale_length,
            northing=sc.bf_origin_utm_northing + y * scale_length,
        )
        lonlat = utm.to_LonLat()
        # Show
        if self.show:
            url = lonlat.to_url()
            bpy.ops.wm.url_open(url=url)
            self.report({"INFO"}, "Geolocation shown")
            return {"FINISHED"}
        # Set defaults
        self.bf_lon = lonlat.lon
        self.bf_lat = lonlat.lat
        # Call dialog
        wm = context.window_manager
        return wm.invoke_props_dialog(self)


@subscribe
class SCENE_OT_bf_set_cursor_geoloc(Operator, _bf_set_geoloc):
    """!
    Set geographic location (WGS84).
    """

    bl_idname = "scene.bf_set_cursor_geoloc"

    @classmethod
    def poll(cls, context):
        return context.scene.cursor

    def _get_loc(self, context):
        return context.scene.cursor.location

    def _set_loc(self, context, xyz):
        context.scene.cursor.location = xyz


@subscribe
class SCENE_OT_bf_set_ob_geoloc(Operator, _bf_set_geoloc):
    """!
    Set geographic location (WGS84).
    """

    bl_idname = "scene.bf_set_ob_geoloc"

    @classmethod
    def poll(cls, context):
        return context.active_object

    def _get_loc(self, context):
        return context.active_object.location

    def _set_loc(self, context, xyz):
        context.active_object.location = xyz


# Register


def register():
    """!
    Load the Python classes and functions to blender.
    """
    from bpy.utils import register_class

    for cls in bl_classes:
        register_class(cls)


def unregister():
    """!
    Unload the Python classes and functions from blender.
    """
    from bpy.utils import unregister_class

    for cls in reversed(bl_classes):
        unregister_class(cls)
