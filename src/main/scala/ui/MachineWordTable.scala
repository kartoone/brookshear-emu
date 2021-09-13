package ui

import scala.scalajs.js.Dynamic.{global => g}
import emulator.MachineWord

import scala.scalajs.js
import scala.util._


// UPDATE by Brian Toone (github username: kartoone)
// to take advantage of all the code here, we can reuse this for a highly pixelated tiny display
// by setting a flag variable (isDisplay) to true to indicate we want to display a grayscale square
// instead of the actual hex value
class MachineWordTable(cols: Int, showFirstHeaderRow: Boolean, customHeaderRow: String = null, isDisplay: Boolean = false) {
  
  def setData(d: Seq[MachineWord]) { _data = d; update() }

  def element = tableElement
  
  var onChangeHandler: ((MachineWord, MachineWord) => Unit) = null
  
  var onHoverHandler: (Option[MachineWord] => Unit) = null
  
  private var _data: Seq[MachineWord] = Seq.empty

  private lazy val tableElement = g.document.createElement("table")
  
  private def makeFirstRow = {
    val tr = g.document.createElement("tr")
    
    if (customHeaderRow != null) {
      val th = g.document.createElement("th")
      th.textContent = customHeaderRow
      th.colSpan = cols + 1
      tr.appendChild(th)
    } else {
      tr.appendChild(g.document.createElement("th"))
      
      (0 until cols) foreach { i =>
        val th = g.document.createElement("th")
		th.textContent = f"$i%X"
		th.style.textAlign = "center"
        tr.appendChild(th)
      }
    }
    
    tr
  }
  
  private var lastIndex: Option[Int] = None
  
  private def onInput(text: String, index: Int) {
    
    val hexValue = Try(Integer.parseInt(text, 16)) filter (v => v >=0 && v <= 0xff)
    
    hexValue match {
      case Success(h) if onChangeHandler != null => onChangeHandler(MachineWord(index), MachineWord(h))
      case _ => update()
    }
  }
  
  private def focusAndSelectText(e: js.Dynamic) {
    e.focus()
    val range = g.document.createRange()
    range.selectNodeContents(e)
    val sel = g.window.getSelection()
    sel.removeAllRanges()
    sel.addRange(range)
  }
  
  private def update() {
	import org.scalajs.dom
    
    var elementToFocus: Option[js.Dynamic] = None
    
    tableElement.innerHTML = ""
    tableElement.onmouseout = () => if (onHoverHandler != null) onHoverHandler(None)
    
    if (showFirstHeaderRow) 
      tableElement.appendChild(makeFirstRow)
    
    for ((row, rowIndex) <- _data.zipWithIndex.grouped(cols).zipWithIndex) {
   
      val tr = g.document.createElement("tr")
	  tr.style.padding = "0"
	  tr.style.margin = "0"
      tr.style.borderSpacing = "0"
      tableElement.appendChild(tr)
      
      val th = g.document.createElement("th")
      th.textContent = f"$rowIndex%X"
      tr.appendChild(th)
      
      for ((cell, cellIndex) <- row) {
        
        val td = g.document.createElement("td")
        tr.appendChild(td)
		if (isDisplay) {
			val canvas = g.document.createElement("canvas")
			td.appendChild(canvas)
			td.style.width = "34px"
			td.style.height = "30px"
			td.style.padding = "0 4px 0 0"
			td.style.margin= "0"
			canvas.width = "30"
			canvas.height = "30"
   			type Ctx2D = dom.CanvasRenderingContext2D
			val ctx = canvas.getContext("2d").asInstanceOf[Ctx2D]
			val amt = cell.toInt
			ctx.fillStyle = "rgb(" + amt + "," + amt + "," + amt + ")"
			ctx.fillRect(0,0,30,30)
     	} else {
        	td.textContent = f"${cell.toInt}%02X"
        	td.contentEditable = true
	  		if (rowIndex >= 8 && ((cellIndex&0x08) >= 8)) {
				td.style.backgroundColor = "#eee"
			}
	  		if (cellIndex == 240) {
				td.style.backgroundColor = "#bbb"
			}
		}
        
        td.onblur = (e: js.Dynamic) => onInput(td.textContent.toString, cellIndex)
        td.onkeypress = (e: js.Dynamic) => if (e.keyCode.toString == "13") {
          lastIndex = Some(cellIndex)
          td.blur()
          e.preventDefault()
          false 
        }
        td.onmouseover = () => if (onHoverHandler != null) onHoverHandler(Some(MachineWord(cellIndex))) 
        
        lastIndex filter (_ == cellIndex-1) foreach { _ => elementToFocus = Some(td); lastIndex = None }
        
   } 
    }
    elementToFocus foreach (focusAndSelectText(_)) 
  }
  
}
