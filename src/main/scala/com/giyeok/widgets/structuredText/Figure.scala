package com.giyeok.widgets.structuredText

import java.util.concurrent.atomic.AtomicLong
import org.eclipse.swt.graphics.Image

// TODO Figure에서 줄 수 정보 얻어낼 수 있어야 함
// Label은 반드시 직사각형의 영역을 차지한다
// TODO figureExtra 구조에 대해서 더 고민하기 -> 현재 구조로는 Figure 객체를 여러 view에서 재사용이 불가능

trait Tag

sealed trait Figure {
    val id: Long = Figure.newId()

    val tags: Set[Tag]

    private[structuredText] var figureExtra = new FigureExtra(this)
}
object Figure {
    private val counter = new AtomicLong()
    private def newId(): Long = counter.incrementAndGet()
}

sealed trait FigureNoTags extends Figure {
    val tags = Set()
}

sealed trait Label extends Figure {
    def estimateDimension(dc: DrawingContext): Dimension
    def measureDimension(dc: DrawingContext): Dimension
}
case class TextLabel(text: String, deco: TextDecoration, tags: Set[Tag]) extends Label {
    def estimateDimension(dc: DrawingContext): Dimension =
        text.foldLeft(Dimension.zero) { (dim, c) =>
            dim.addRight(dc.charSizeMap.getOrElse(c, dc.charSizeMap.head._2))
        }
    def measureDimension(dc: DrawingContext): Dimension =
        dc.textExtent(text, deco)
}
case class ImageLabel(image: Image, tags: Set[Tag]) extends Label {
    def estimateDimension(dc: DrawingContext): Dimension =
        measureDimension(dc)
    def measureDimension(dc: DrawingContext): Dimension =
        Dimension(image.getImageData.width, image.getImageData.height)
}
case class SpacingLabel(pixelWidth: Int, spaceCount: Int) extends Label {
    val tags = Set()

    def widthInPixel(dc: DrawingContext): Int = {
        val spaceDim = dc.charSizeMap(' ')
        pixelWidth + spaceDim.width.toInt * spaceCount
    }

    def estimateDimension(dc: DrawingContext): Dimension =
        measureDimension(dc)
    def measureDimension(dc: DrawingContext): Dimension =
        Dimension(widthInPixel(dc), dc.standardLineHeight)
}

case class NewLine() extends Label with FigureNoTags {
    def estimateDimension(dc: DrawingContext): Dimension =
        measureDimension(dc)
    def measureDimension(dc: DrawingContext): Dimension =
        Dimension(0, dc.standardLineHeight)
}

case class Container(children: Seq[Figure], tags: Set[Tag]) extends Figure

// Indented는 위아래 NewLine이 추가된다
case class Indented(content: Figure) extends FigureNoTags

trait Deferred extends FigureNoTags {
    private[structuredText] var deferredExtra = new DeferredExtra(this)

    def content(): Figure = deferredExtra.content

    def contentFunc: Figure
    def pixelsDimension(dc: DrawingContext): FigureDimension
    def lines: FigureLines
}
object Deferred {
    def apply(contentFunc: => Figure, figureDimension: FigureDimension, figureLines: FigureLines): Deferred = {
        val func: () => Figure = () => contentFunc

        new Deferred {
            override def contentFunc: Figure =
                func()
            override def pixelsDimension(dc: DrawingContext): FigureDimension =
                figureDimension
            override def lines: FigureLines =
                figureLines
        }
    }
}
case class Actionable(content: Figure) extends FigureNoTags

class Transformable(contents: Map[String, Figure], defaultState: String) extends FigureNoTags {
    private var currentState: String = defaultState

    def state: String = currentState
    def state_=(newState: String): Unit = {
        currentState = newState
        content.figureExtra.updateParent(Some(this))
        figureExtra.contentUpdated()
    }

    def content: Figure = contents(currentState)
}
case class Collapsible(expandedContent: Figure, defaultCollapsed: Boolean)
        extends Transformable(
            Map(
                "expanded" -> expandedContent,
                "collapsed" -> Container(Seq(), Set())
            ),
            if (defaultCollapsed) "collapsed" else "expanded"
        ) {
    def collapsed: Boolean = this.state == "collapsed"
    def setCollapsed(collapsed: Boolean): Unit = {
        this.state = if (collapsed) "collapsed" else "expanded"
    }
    def toggle(): Unit = {
        setCollapsed(collapsed)
    }
}

case class Mutable(defaultContent: Figure, tags: Set[Tag]) extends Figure {
    private var _content: Figure = defaultContent

    def content: Figure = _content
    def update(newContent: Figure): Unit = {
        _content = newContent
        newContent.figureExtra.updateParent(Some(this))
        figureExtra.contentUpdated()
    }
}