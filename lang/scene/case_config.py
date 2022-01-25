import logging, os, bpy
from bpy.types import Scene
from bpy.props import BoolProperty, FloatProperty, StringProperty, PointerProperty
from ...types import BFParam, BFNamelistSc, BFException

log = logging.getLogger(__name__)


class SP_config_directory(BFParam):
    label = "Case Directory"
    description = "Destination directory for exported fds case"
    bpy_type = Scene
    bpy_idname = "bf_config_directory"
    bpy_prop = StringProperty
    # no bpy_default, user choice when saving
    bpy_other = {"subtype": "DIR_PATH", "maxlen": 1024}

    def get_exported(self):
        return True

    def check(self, context):
        if self.get_exported():
            value = self.element.bf_config_directory
            if not os.path.exists(bpy.path.abspath(value)):
                raise BFException(self, f"Case directory <{value}> not existing")


class SP_config_text(BFParam):
    label = "Free Text"
    description = "Internal free text, included verbatim"
    bpy_type = Scene
    bpy_idname = "bf_config_text"
    bpy_prop = PointerProperty
    bpy_other = {"type": bpy.types.Text}

    def draw_operators(self, context, layout):
        layout.operator("scene.bf_show_text", text="", icon="GREASEPENCIL")


class SN_config(BFNamelistSc):
    label = "FDS Case Config"
    bf_params = (SP_config_directory, SP_config_text)


class SP_config_min_edge_length_export(BFParam):
    label = "Use Custom Min Edge Length"
    description = "Use custom min allowed edge length for current case"
    bpy_type = Scene
    bpy_idname = "bf_config_min_edge_length_export"
    bpy_prop = BoolProperty
    bpy_default = False


class SP_config_min_edge_length(BFParam):
    label = "Min Edge Length"
    description = "Min allowed edge length for current case"
    bpy_type = Scene
    bpy_idname = "bf_config_min_edge_length"
    bpy_prop = FloatProperty
    bpy_default = 1e-05
    bpy_other = {"unit": "LENGTH"}
    bpy_export = "bf_config_min_edge_length_export"


class SP_config_min_face_area_export(BFParam):
    label = "Use Custom Min Face Area"
    description = "Use custom min allowed face area for current case"
    bpy_type = Scene
    bpy_idname = "bf_config_min_face_area_export"
    bpy_prop = BoolProperty
    bpy_default = False


class SP_config_min_face_area(BFParam):
    label = "Min Face Area"
    description = "Min allowed face area for current case"
    bpy_type = Scene
    bpy_idname = "bf_config_min_face_area"
    bpy_prop = FloatProperty
    bpy_default = 1e-08
    bpy_other = {"unit": "AREA"}
    bpy_export = "bf_config_min_face_area_export"


class SP_config_default_voxel_size(BFParam):
    label = "Voxel/Pixel Size"
    description = "Default voxel/pixel resolution"
    bpy_type = Scene
    bpy_idname = "bf_default_voxel_size"
    bpy_prop = FloatProperty
    bpy_default = 0.1
    bpy_other = {"unit": "LENGTH", "step": 1.0, "precision": 3}


class SN_config_sizes(BFNamelistSc):
    label = "Default Sizes and Thresholds"
    bf_params = (
        SP_config_min_edge_length,
        SP_config_min_face_area,
        SP_config_default_voxel_size,
    )
