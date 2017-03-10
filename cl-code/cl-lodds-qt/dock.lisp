(in-package #:lodds-qt)
(in-readtable :qtools)

(define-widget dock (QDockWidget)
  ((main-window :initarg :main-window
                :initform (error "speify dock main-window"))
   (widget :initarg :widget
           :initform (error "specify dock widget"))
   (title :initarg :title
          :initform (error "specify dock title"))
   (menu :initarg :menu
         :initform nil)
   (finalize-widget :initarg :finalize-widget
                    :initform t)
   (side :initarg :side
         :initform :left
         :documentation "Where the dock should be added. One of :left
         :right :top :bottom or another dock widget, when another dock
         widget is given the dock is moved ontop of the given dock
         widget (using QMainWindow::tabifyDockWidget).")))

(define-initializer (dock setup-widget)
  (q+:set-window-title dock title)
  (q+:set-widget dock widget)
  (ctypecase side
    (keyword
     (q+:add-dock-widget main-window
                         (ccase side
                           (:left (q+:qt.left-dock-widget-area))
                           (:right (q+:qt.right-dock-widget-area))
                           (:bottom (q+:qt.bottom-dock-widget-area))
                           (:top (q+:qt.top-dock-widget-area)))
                         dock))
    (dock
     (q+:tabify-dock-widget main-window
                            side
                            dock)))
  (when menu
    (q+:add-action menu (q+:toggle-view-action dock))))

(define-finalizer (dock cleanup)
  (when finalize-widget
    (finalize widget)))
