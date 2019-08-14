(ns raytracer.grouping.shared)

(defprotocol CoordinatesConverter
  (local-to-world-coordinates [this shape svector])
  (world-to-local-coordinates [this shape point]))

(defprotocol ShapesContainer
  (get-root [this])
  (get-all-objects [this])) ;;; TODO/FIXME not needed outside tests probably

(defprotocol HierarchyEditor
  (add-root-object [this object])
  (update-objects [this update-f]))
