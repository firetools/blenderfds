import logging
from bpy.types import Material
from bpy.props import FloatProperty, EnumProperty
from ...types import (
    BFParam,
    BFParamOther,
    BFParamFYI,
    BFParamStr,
    BFNamelistMa,
    BFException,
    BFNotImported,
    FDSParam,
)
from ...bl.ui_lists import (
    WM_PG_bf_other,
    WM_UL_bf_other_items,
)
from ... import config

log = logging.getLogger(__name__)


def update_MP_namelist_cls(self, context):
    self.bf_namelist.set_appearance(context)


class MP_namelist_cls(BFParam):
    label = "Namelist"
    description = "Identification of FDS namelist"
    bpy_type = Material
    bpy_idname = "bf_namelist_cls"
    bpy_prop = EnumProperty
    bpy_other = {
        "items": (("MN_SURF", "SURF", "Generic boundary condition", 2000),),
        "update": update_MP_namelist_cls,
    }
    bpy_default = "MN_SURF"

    @property
    def exported(self):
        if self.element.name in {"INERT", "HVAC", "MIRROR", "OPEN", "PERIODIC"}:
            return False
        return super().exported


class MP_ID(BFParamStr):
    label = "ID"
    description = "Material identification name"
    fds_label = "ID"
    bpy_type = Material
    bpy_prop = None  # to avoid creation
    bpy_idname = "name"

    def copy_to(self, dest_element):
        pass


class MP_FYI(BFParamFYI):
    bpy_type = Material
    bpy_idname = "bf_fyi"


class MP_RGB(BFParam):
    label = "RGB"
    description = "Red, green, blue components of color"
    fds_label = "RGB"
    bpy_type = Material
    bpy_prop = None  # Do not register
    bpy_idname = "diffuse_color"

    def set_value(self, context, value):
        c = self.element.diffuse_color
        c[0], c[1], c[2] = value[0] / 255.0, value[1] / 255.0, value[2] / 255.0

    def to_fds_param(self, context):
        c = self.element.diffuse_color
        rgb = (int(c[0] * 255), int(c[1] * 255), int(c[2] * 255))
        if c[3] == 1.0:  # do not send TRANSPARENCY if it is 1
            return FDSParam(
                fds_label="RGB",
                value=rgb,
            )
        else:
            return (
                FDSParam(
                    fds_label="RGB",
                    value=rgb,
                ),
                FDSParam(fds_label="TRANSPARENCY", value=c[3], precision=2),
            )


class MP_COLOR(BFParam):
    label = "COLOR"
    description = "Color"
    fds_label = "COLOR"
    bpy_type = Material
    bpy_prop = None  # Do not register

    def set_value(self, context, value):
        c = self.element.diffuse_color
        rgb = config.fds_colors.get(value)
        if not rgb:
            raise BFException(self, f"Unknown color <{value}>")
        c[0], c[1], c[2] = rgb[0] / 255.0, rgb[1] / 255.0, rgb[2] / 255.0

    def to_fds_param(self, context):
        pass


class MP_TRANSPARENCY(BFParam):
    label = "TRANSPARENCY"
    description = "Red, green, blue components of color and transparency"
    fds_label = "TRANSPARENCY"
    bpy_type = Material
    bpy_prop = None  # Do not register

    def set_value(self, context, value):
        c = self.element.diffuse_color
        c[3] = value

    def to_fds_param(self, context):
        pass


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


class MP_MATL_ID(BFParamStr):
    label = "MATL_ID"
    description = "Reference to a MATL (Material) line for self properties"
    fds_label = "MATL_ID"
    bpy_type = Material
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


class MP_other(BFParamOther):
    bpy_type = Material
    bpy_idname = "bf_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


class MN_SURF(BFNamelistMa):
    label = "SURF"
    description = "Generic boundary condition"
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
        MP_MATL_ID,
        MP_THICKNESS,
        MP_BACKING,
        MP_HRRPUA,
        MP_TAU_Q,
        MP_IGNITION_TEMPERATURE,
        MP_other,
    )

    @property
    def exported(self):
        return (
            self.element.bf_surf_export and self.element.name not in config.default_mas
        )


# items = [ FIXME FIXME FIXME
#     (cls.__name__, cls.label, cls.description, cls.enum_id)
#     for _, cls in bf_namelists_by_cls.items()
#     if cls.bpy_type == Material and cls.enum_id
# ]
# items.sort(key=lambda k: k[1])
# MP_namelist_cls.bpy_other["items"] = items
