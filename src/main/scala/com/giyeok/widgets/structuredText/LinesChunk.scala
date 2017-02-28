package com.giyeok.widgets.structuredText

import com.giyeok.widgets.structuredText.FlatFigureStream._

// TODO 가로로 긴 데이터를 잘 처리하기 위해서 FlatFigure sequence도 chunk화
// line.context는 line의 시작점 기준으로 FlatPush들 목록. 이 목록은 다음 용도로 사용:
// 1. Tag 처리를 위해서
// 2. Indent 처리를 위해서

private trait LinesChunk {
    var _parent: Option[(LinesChunk, Int)] = None
    // TODO estimatedDimension과 measuredDimension을 구분해서 저장하도록 하자
    private var _dimension: Option[Dimension] = None
    private var _linesCount: Option[Int] = None

    // TODO estimatedDimension 개념 추가
    //   - Deferred가 아닌 figure가 많으면 처음 뜰 때 너무 느림
    //   - estimatedDimension은 이 라인이 화면에 보일지 안 보일지 결정할 때 사용. 실제 화면에 그려질 때는 실제로 measureDimension해서 사용
    // TODO Figure에 해당 Figure가 속한 Line 포인터 추가(Line에 대한 reference, Push/Pop 혹은 Label의 인덱스)
    // TODO LinesChunk에 해당 Chunk가 속한 Chunk 포인터 추가(parent chunk에 대한 reference, 해당 청크 내에서 인덱스)
    // TODO Figure의 크기나 내용이 변경되면 Line에 notify, Line은 상위 LinesChunk에 notify
    def dimension(dc: DrawingContext): Dimension = _dimension match {
        case Some(dimension) => dimension
        case None =>
            val dimension = calculateDimension(dc)
            _dimension = Some(dimension)
            dimension
    }
    def linesCount: Int = _linesCount match {
        case Some(linesCount) => linesCount
        case None =>
            val linesCount = calculateLinesCount()
            _linesCount = Some(linesCount)
            linesCount
    }

    def invalidateRenderingCaches(): Unit = {
        _dimension = None
        _linesCount = None
    }

    // TODO rebalance는 Chunk 크기가 너무 크면 chunk를 쪼개고, 너무 작으면 다음 것들과 합치는 기능을 함
    def rebalance(): Unit = {
        ???
    }

    def calculateDimension(dc: DrawingContext): Dimension
    def calculateLinesCount(): Int
}
private case class Line(line: FlatFigureLine) extends LinesChunk {
    lazy val indentDepth: Int = line.context count { _.figure.isInstanceOf[Indented] }

    line.seq.zipWithIndex foreach { c =>
        c._1 match {
            case FlatPush(figure) =>
                figure.figureExtra.startPoint = (this, c._2)
            case FlatPop(figure) =>
                figure.figureExtra.endPoint = (this, c._2)
            case FlatLabel(label) =>
                val p = (this, c._2)
                label.figureExtra.startPoint = p
                label.figureExtra.endPoint = p
            case FlatDeferred(deferred) =>
                val p = (this, c._2)
                deferred.figureExtra.startPoint = p
                deferred.figureExtra.endPoint = p
        }
    }

    // figure가 업데이트되면 startPoint의 line에 `contentUpdated` 메소드를 호출한다
    // figure가 업데이트되었다는 것은 figure.startPoint~endPoint 사이에 포함된 figure의 목록이 변경된 경우.
    // Deferred의 내용이 구체화될 때도 호출됨.
    // 크기도 당연히 변할 것으로 본다.
    def contentUpdated(figure: Figure): Unit = {
        assert(figure.figureExtra.startPoint._1 == this)
        // figure.figureExtra.startPoint~endPoint 사이의 내용을 figure.flatFigures(false) 로 치환한다
        println("TODO contentUpdated")
        val lines = figure.flatFigures(expandDeferred = false, /* line.seq.take(figure.figureExtra.startPoint._2)에 대해서 진행하고 난 context ++ */ figure.figureExtra.startPoint._1.line.context).lines
        // figure.figureExtra.startPoint._1 에서 root까지 path, figure.figureExtra.endPoint._1 에서 root까지 path를 구해서
        // 둘 사이에 가장 가까운 공통 ancestor를 찾고, 그 사이를 전부 replace
        // 그 다음 공통 ancestor에 rebalance()
        val firstLine = line.seq.take(figure.figureExtra.startPoint._2) ++ lines.head.seq
        // firstLine과 lastLine 사이는 lines.init.tail로 치환
        // TODO context 고려
        val lastLine = lines.last.seq ++ figure.figureExtra.endPoint._1.line.seq.drop(figure.figureExtra.endPoint._2)
        // figure의 내용이 치환되고 난 뒤의 context는 변화 없음
    }

    // figure의 크기가 변한 경우 startPoint의 line에 `sizeUpdated` 메소드를 호출한다
    // estimated되었던 dimension이 실제로 measure해보니 변경된 경우 호출한다.
    def dimensionUpdated(figure: Figure): Unit = {
        assert(figure.figureExtra.startPoint._1 == this)
    }

    def calculateDimension(dc: DrawingContext): Dimension =
        line.seq.foldLeft(Dimension.zero) { (cc, figure) =>
            figure match {
                case FlatLabel(label) =>
                    cc.addRight(label.measureDimension(dc))
                case FlatDeferred(deferred) =>
                    val dim = deferred.pixelsDimension(dc)
                    // TODO 수정
                    cc.addRight(dim.leading)
                case _ => cc
            }
        }
    def calculateLinesCount(): Int = {
        val lines = line.seq collect {
            case FlatDeferred(deferred) if deferred.lines.following.isDefined =>
                deferred.lines.following.get._1
        }
        1 + lines.length + lines.sum
    }
}
private case class Chunk(initialChildren: Seq[LinesChunk]) extends LinesChunk {
    private var _children = initialChildren
    _children.zipWithIndex foreach { c => c._1._parent = Some(this, c._2) }

    // TODO _children 변경 메소드 추가
    // _children이 변경되면 _parent 재설정

    def calculateDimension(dc: DrawingContext): Dimension =
        _children.foldLeft(Dimension.zero) { (cc, line) =>
            cc.addBottom(line.dimension(dc))
        }
    def calculateLinesCount(): Int = (_children map { _.linesCount }).sum
}
