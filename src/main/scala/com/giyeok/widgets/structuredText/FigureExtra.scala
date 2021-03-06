package com.giyeok.widgets.structuredText

private class FigureExtra(figure: Figure) {
    // TODO parent 정보가 정말 필요한가?
    var parent: Option[Figure] = _
    var startPoint: (Line, Int) = _
    // FlatLabel이나 FlatDeferred에서는 endPoint == startPoint
    var endPoint: (Line, Int) = _

    implicit final class MyEnsuring[A](private val self: A) {
        def ensuringEquals(other: A): A = {
            assert(self == other)
            self
        }
    }

    def updateParent(parent: Option[Figure]): Unit = {
        this.parent = parent
        figure match {
            case _: Label => // nothing to do
            case container: Container =>
                container.children foreach { _.figureExtra.updateParent(Some(container)) }
            case indented @ Indented(content) =>
                content.figureExtra.updateParent(Some(indented))
            case deferred: Deferred =>
                deferred.deferredExtra.contentCache foreach {
                    _.figureExtra.updateParent(Some(deferred))
                }
            case actionable @ Actionable(content) =>
                content.figureExtra.updateParent(Some(actionable))
            case transformable: Transformable =>
                transformable.content.figureExtra.updateParent(Some(transformable))
            case mutable: Mutable =>
                mutable.content.figureExtra.updateParent(Some(mutable))
        }
    }

    def contentUpdated(): Unit = {
        startPoint._1.contentUpdated(figure)
    }
}

object DeferredExtra {
    // TODO LRU 형태로 메모리를 너무 많이 차지하게 되면 _contentCache 날리는 기능 추가
}

private class DeferredExtra(deferred: Deferred) {
    private var _contentCache = Option.empty[Figure]
    def contentCache: Option[Figure] = _contentCache
    def isContentSet: Boolean = _contentCache.isDefined
    def clearCache(): Unit = { _contentCache = None }
    def content: Figure = _contentCache match {
        case Some(content) => content
        case None =>
            val content = deferred.contentFunc
            content.figureExtra.updateParent(Some(deferred))
            deferred.figureExtra.contentUpdated()
            _contentCache = Some(content)
            _contentCache.get
    }
}
