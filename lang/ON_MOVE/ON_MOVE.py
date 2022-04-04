import logging
from bpy.types import Object
from bpy.props import FloatProperty, IntProperty, FloatVectorProperty
from ...types import BFNamelist, BFParam, BFNotImported, FDSList, FDSNamelist, FDSParam
from ... import utils

log = logging.getLogger(__name__)

class OP_other_MOVE_ID(BFParam):  # FIXME FIXME FIXME
    label = "MOVE_ID"
    description = "Reference to geometric transformation"
    fds_label = "MOVE_ID"
    bpy_type = Object

    def to_fds_list(self, context) -> FDSList:
        if not self.get_exported(context):
            return FDSList()
        ob = self.element
        t34 = bl_matrix_to_t34(m=ob.matrix_world)
        return FDSList(
            iterable=(
                FDSParam(fds_label="MOVE_ID", value=f"{ob.name}_move"),
                FDSNamelist(
                    iterable=(
                        FDSParam(fds_label="ID", value=f"{ob.name}_move"),
                        FDSParam(fds_label="T34", value=t34, precision=6),
                    ),
                    fds_label="MOVE",
                ),
            )
        )

    def from_fds(self, context, value):
        try:
            m = context.scene["bf_move_coll"][value]
        except KeyError as err:
            raise BFNotImported(self, f"Missing MOVE ID={value}")
        utils.geometry.transform_ob(ob=self.element, m=m, force_othogonal=False)

    def draw(self, context, layout):
        pass
