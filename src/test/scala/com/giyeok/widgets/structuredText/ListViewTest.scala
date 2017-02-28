package com.giyeok.widgets.structuredText

import scala.util.Random
import org.eclipse.jface.resource.JFaceResources
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell

object ListViewTest {
    private lazy val systemFont = JFaceResources.getFont(JFaceResources.TEXT_FONT)

    def deferred(figure: => Figure, dim: FigureDimension, lines: FigureLines): Figure =
        figure //Deferred(figure, dim, lines)

    def main(args: Array[String]): Unit = {
        val display = new Display()
        val shell = new Shell(display)

        val figure = {
            val (_, strings) = (0 until 100000).foldLeft((Random.alphanumeric, List[String]())) { (cc, i) =>
                val (newString, nextStream) = cc._1.splitAt(100 + Random.nextInt(100))
                (nextStream, f"$i%05d ${newString.mkString}" +: cc._2)
            }
            val labels = strings.tail.foldLeft(List[Figure](TextLabel(strings.head, TextNoDecoration, Set()))) { (figs, str) =>
                TextLabel(str, TextNoDecoration, Set()) +: NewLine() +: figs
            }
            Container(labels, Set())
        }

        shell.setLayout(new FillLayout())
        shell.setBounds(100, 100, 800, 600)

        // println(figure.flatFigureStream.textRender)

        // new FigureTreeView(shell, SWT.NONE, TextLabel("hello", TextNoDecoration, Set()), Seq(), DrawingConfig(15, JFaceResources.getFont(JFaceResources.TEXT_FONT)))
        val myFont = systemFont //new Font(null, "Menlo", 30, SWT.NONE)
        new StructuredTextView(shell, SWT.NONE, figure, Seq(), DrawingConfig(SpacingLabel(pixelWidth = 0, spaceCount = 2), myFont))

        shell.open()

        while (!shell.isDisposed) {
            if (!display.readAndDispatch()) {
                display.sleep()
            }
        }
        display.dispose()
    }

}
