# Import all py file to have them loaded
from .collection import collection

# the order of scene namelists is also the exporting order
from .scene import scene, case_config, HEAD, TIME, MISC, PRES, RADI, REAC, CATF, DUMP
from .material import material, SURF
from .object import (
    object,
    DEVC,
    GEOM,
    HOLE,
    HVAC,
    INIT,
    MESH,
    MOVE,
    OBST,
    other_namelist,
    PB,
    PROF,
    SLCF,
    SURF_ID,
    VENT,
    XYZ,
    ZONE,
)


def register():
    import logging
    from ..types import BFParam, BFNamelist

    log = logging.getLogger(__name__)
    log.debug("Register lang...")

    # Update namelist_cls items (after importing all namelists)
    object.update_OP_namelist_cls_items()
    material.update_MP_namelist_cls_items()

    # Register Blender entities extensions
    object.BFObject.register()
    material.BFMaterial.register()
    scene.BFScene.register()
    collection.BFCollection.register()

    # Register all lang classes, as recorded in BFParam and BFNamelist
    for bf_param in BFParam.subclasses:
        bf_param.register()
    for bf_namelist in BFNamelist.subclasses:
        bf_namelist.register()


def unregister():
    import logging
    from ..types import BFParam, BFNamelist

    log = logging.getLogger(__name__)
    log.debug("Unregister lang...")

    # Unregister Blender entities extensions
    object.BFObject.unregister()
    material.BFMaterial.unregister()
    scene.BFScene.unregister()
    collection.BFCollection.unregister()

    # Unregister all lang classes, as recorded in BFParam and BFNamelist
    for bf_namelist in BFNamelist.subclasses:
        bf_namelist.unregister()
    for bf_param in BFParam.subclasses:
        bf_param.unregister()
