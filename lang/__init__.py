# SPDX-License-Identifier: GPL-3.0-or-later

#  Import all py file to have them loaded
# the order of scene namelists is also the exporting order

from . import (
    bf_scene,
    bf_material,
    bf_collection,
    bf_object,
    SN_MOVE,
    SN_config,
    SN_HEAD,
    SN_TIME,
    SN_MISC,
    SN_PRES,
    SN_RADI,
    SN_REAC,
    SN_CATF,
    SN_DUMP,
    SN_MOVE,
    SN_MULT,
    MN_SURF,
    OP_XB,
    OP_XYZ,
    OP_PB,
    OP_SURF_ID,
    ON_DEVC,
    ON_GEOM,
    ON_HOLE,
    ON_INIT,
    ON_MESH,
    ON_MOVE,
    ON_MULT,
    ON_OBST,
    ON_other,
    ON_PROF,
    ON_SLCF,
    ON_VENT,
    ON_ZONE,
)


def register():
    import logging
    from ..types import BFParam, BFNamelist

    log = logging.getLogger(__name__)
    log.info("Register lang...")

    # Update namelist_cls items (after importing all namelists)
    bf_object.update_OP_namelist_cls_items()
    bf_material.update_MP_namelist_cls_items()

    # Register Blender entities extensions
    bf_object.BFObject.register()
    bf_material.BFMaterial.register()
    bf_scene.BFScene.register()
    bf_collection.BFCollection.register()

    # Register all lang classes, as recorded in BFParam and BFNamelist
    for bf_param in BFParam.subclasses:
        bf_param.register()
    for bf_namelist in BFNamelist.subclasses:
        bf_namelist.register()


def unregister():
    import logging
    from ..types import BFParam, BFNamelist

    log = logging.getLogger(__name__)
    log.info("Unregister lang...")

    # Unregister Blender entities extensions
    bf_object.BFObject.unregister()
    bf_material.BFMaterial.unregister()
    bf_scene.BFScene.unregister()
    bf_collection.BFCollection.unregister()

    # Unregister all lang classes, as recorded in BFParam and BFNamelist
    for bf_namelist in BFNamelist.subclasses:
        bf_namelist.unregister()
    for bf_param in BFParam.subclasses:
        bf_param.unregister()
