import logging
from bpy.types import Material
from bpy.props import FloatProperty, EnumProperty, StringProperty
from ..types import (
    BFParam,
    BFNamelistMa,
    BFNotImported,
)
from .bf_material import MP_ID, MP_FYI, MP_RGB, MP_COLOR, MP_TRANSPARENCY, MP_other

log = logging.getLogger(__name__)


class MN_SURF(BFNamelistMa):
    label = "SURF"
    description = "Boundary condition"
    enum_id = 2000
    fds_label = "SURF"
    bpy_export = "bf_surf_export"
    bpy_export_default = True
    bf_params = (
        MP_ID,
        MP_FYI,
        MP_RGB,
        MP_COLOR,
        MP_TRANSPARENCY,
        MP_other,
    )


class MP_THICKNESS(BFParam):
    label = "THICKNESS [m]"
    description = "Surface thickness for heat transfer calculation"
    fds_label = "THICKNESS"
    fds_default = 0.0
    bpy_type = Material
    bpy_idname = "bf_thickness"
    bpy_prop = FloatProperty
    bpy_other = {"step": 1.0, "precision": 6, "min": 0.000001}
    bpy_export = "bf_thickness_export"
    bpy_export_default = False

    def from_fds(self, context, value):
        if isinstance(value, float):
            return super().from_fds(context, value)
        else:
            raise BFNotImported(self, "Thickness list not handled")


class MP_HRRPUA(BFParam):
    label = "HRRPUA [kW/m²]"
    description = "Heat release rate per unit area"
    fds_label = "HRRPUA"
    fds_default = 0.0
    bpy_type = Material
    bpy_idname = "bf_hrrpua"
    bpy_prop = FloatProperty
    bpy_other = {"precision": 3, "min": 0.0}


class MP_TAU_Q(BFParam):
    label = "TAU_Q [s]"
    description = "Ramp time for heat release rate"
    fds_label = "TAU_Q"
    fds_default = 1.0
    bpy_type = Material
    bpy_idname = "bf_tau_q"
    bpy_prop = FloatProperty
    bpy_other = {"step": 10.0, "precision": 1}


class MP_MATL_ID(BFParam):
    label = "MATL_ID"
    description = "Reference to a MATL (Material) line for self properties"
    fds_label = "MATL_ID"
    bpy_type = Material
    bpy_prop = StringProperty
    bpy_idname = "bf_matl_id"
    bpy_export = "bf_matl_id_export"
    bpy_export_default = False

    def draw_operators(self, context, layout):
        layout.operator("material.bf_choose_matl_id", icon="VIEWZOOM", text="")

    def from_fds(self, context, value):
        if isinstance(value, str):
            return super().from_fds(context, value)
        else:
            raise BFNotImported(self, "Material list not handled")


class MP_IGNITION_TEMPERATURE(BFParam):
    label = "IGNITION_TEMPERATURE [°C]"
    description = "Ignition temperature"
    fds_label = "IGNITION_TEMPERATURE"
    fds_default = 5000.0
    bpy_type = Material
    bpy_idname = "bf_ignition_temperature"
    bpy_prop = FloatProperty
    bpy_other = {"step": 100.0, "precision": 1, "min": -273.0}
    bpy_export = "bf_ignition_temperature_export"
    bpy_export_default = False


class MP_BACKING(BFParam):
    label = "BACKING"
    description = "Exposition of back side surface"
    fds_label = "BACKING"
    fds_default = "EXPOSED"
    bpy_type = Material
    bpy_idname = "bf_backing"
    bpy_prop = EnumProperty
    bpy_prop_export = "bf_backing_export"
    bpy_export_default = False
    bpy_other = {
        "items": (
            (
                "VOID",
                "VOID",
                "The wall is assumed to back up to the ambient temperature",
                100,
            ),
            (
                "INSULATED",
                "INSULATED",
                "The back side of the material is perfectly insulated",
                200,
            ),
            (
                "EXPOSED",
                "EXPOSED",
                "The heat transfer into the space behind the wall is calculated (only if wall is one cell thick)",
                300,
            ),
        )
    }


# FIXME FIXME FIXME remove? set generic with wizards for specific?
class MN_SURF_burner(MN_SURF):
    label = "SURF burner"
    description = "Burner boundary condition"
    enum_id = 2001
    bpy_export_default = None  # already defined
    bf_params = (
        MP_ID,
        MP_FYI,
        MP_RGB,
        MP_COLOR,
        MP_TRANSPARENCY,
        MP_MATL_ID,
        MP_THICKNESS,
        MP_BACKING,
        MP_HRRPUA,
        MP_TAU_Q,
        MP_IGNITION_TEMPERATURE,
        MP_other,
    )
