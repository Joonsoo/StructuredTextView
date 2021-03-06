package com.giyeok.widgets.structuredText

import org.eclipse.swt.graphics.Font
import org.eclipse.swt.graphics.GC

case class DrawingConfig(
    indentWidth: SpacingLabel,
    defaultFont: Font
)

object DrawingContext {
    private var indentWidth = Option.empty[Int]
    private var charSizeMapCache = Option.empty[Map[Char, Dimension]]
    private def charSizeMap(gc: GC, conf: DrawingConfig): Map[Char, Dimension] = {
        charSizeMapCache match {
            case Some(map) => map
            case None =>
                val chars = " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890.~`!\\|!@#$%^&*()_+-=,./<>?"
                gc.setFont(conf.defaultFont)
                val map = (chars map { c =>
                    val d = gc.textExtent("" + c)
                    c -> Dimension(d.x, d.y)
                }).toMap
                charSizeMapCache = Some(map)
                map
        }
    }
}

case class DrawingContext(gc: GC, conf: DrawingConfig) {
    def indentWidth: Long =
        DrawingContext.indentWidth match {
            case Some(indent) => indent
            case None =>
                val indent = conf.indentWidth.widthInPixel(this)
                DrawingContext.indentWidth = Some(indent)
                indent
        }

    lazy val charSizeMap: Map[Char, Dimension] = DrawingContext.charSizeMap(gc, conf)
    lazy val standardLineHeight: Int = (charSizeMap map { _._2.height }).max.toInt

    def textExtent(text: String, deco: TextDecoration): Dimension = {
        deco.execute(gc) {
            val dim = gc.textExtent(text)
            Dimension(dim.x, dim.y)
        }
    }

    def indentedLeft(dc: DrawingContext, indentDepth: Int): Long =
        conf.indentWidth.widthInPixel(dc) * indentDepth
}
