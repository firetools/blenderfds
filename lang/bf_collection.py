import logging

from numpy import iterable
from bpy.types import Collection
from ..types import FDSList

log = logging.getLogger(__name__)


class BFCollection:
    """!
    Extension of Blender Collection.
    """

    def get_layer_collection(self, context, _layer_collection=None):
        """!
        Return related layer_collection in current context.
        @param context: the Blender context.
        @param _layer_collection: internal use for recursivity.
        @return layer_collection related to self in current context.
        """
        if not _layer_collection:
            _layer_collection = context.view_layer.layer_collection
        found = None
        if _layer_collection.name == self.name:
            return _layer_collection
        for c in _layer_collection.children:
            found = self.get_layer_collection(context, _layer_collection=c)
            if found:
                return found

    def to_fds_list(self, context) -> FDSList:
        """!
        Return the FDSList instance from self, never None.
        """
        layer_collection = self.get_layer_collection(context)
        if self.hide_render or layer_collection.exclude:
            return FDSList()  # exclude from exporting
        header = f"\n--- Blender Collection: <{self.name}>"
        obs = list(self.objects)
        obs.sort(key=lambda k: k.name)  # alphabetic by name
        iterable = (ob.to_fds_list(context=context) for ob in obs)
        fds_list = FDSList(header=header, iterable=iterable)
        fds_list.extend(child.to_fds_list(context=context) for child in self.children)
        return fds_list

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        Collection.to_fds_list = cls.to_fds_list
        Collection.get_layer_collection = cls.get_layer_collection

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be unregistered.
        """
        del Collection.get_layer_collection
        del Collection.to_fds_list
