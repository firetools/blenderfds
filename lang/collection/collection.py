import logging
from bpy.types import Collection

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

    def to_fds(self, context):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return FDS formatted string, eg. "&OBST ID='Test' /".
        """
        layer_collection = self.get_layer_collection(context)
        if self.hide_render or layer_collection.exclude:
            return  # exclude from exporting
        obs = list(self.objects)
        obs.sort(key=lambda k: k.name)  # alphabetic by name
        lines = list(ob.to_fds(context) for ob in obs)
        lines.extend(child.to_fds(context) for child in self.children)
        if lines and self != context.scene.collection:
            header = f"\n! --- {self.name}"
            lines.insert(0, header)
        return "\n".join(l for l in lines if l)

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        Collection.to_fds = cls.to_fds
        Collection.get_layer_collection = cls.get_layer_collection

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be unregistered.
        """
        del Collection.to_fds
        del Collection.get_layer_collection
