package edu.colorado.csci3155.project2

/* A class to maintain a canvas */
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D}

/* A figure is a sealed trait. It can be a Polygon or a "MyCircle"*/
sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(x: Double, y: Double): Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
 */
case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    //TODO: Define the bounding box of the polygon
    // This function returns a 4-tuple (xmin, xmax, ymin, ymax)
    override def getBoundingBox: (Double, Double, Double, Double ) = {
        val xmin = cList.foldLeft[Double](Double.MaxValue)((acc,elem) => {
            //  println(("x",acc, elem._1))
            if (elem._1 < acc) {
                elem._1
            }
            else {
                acc
            }
        });

        //println(xmin)

        val ymin = cList.foldLeft[Double](Double.MaxValue)((acc,elem) => {
            // println(("y", acc, elem._1))
            if (elem._2 < acc) {
                elem._2
            }
            else {
                acc
            }
        });

        //println(ymin)

        val xmax = cList.foldLeft[Double](Double.MinValue)((acc,elem) => {
            if (elem._1 > acc) {
                elem._1
            }
            else {
                acc
            }
        });

        val ymax = cList.foldLeft[Double](Double.MinValue)((acc,elem) => {
            if (elem._2 > acc) {
                elem._2
            }
            else {
                acc
            }
        });

        //println(xmin, xmax, ymin, ymax)
        (xmin, xmax, ymin, ymax)

    }
    //TODO: Create a new polygon by shifting each vertex in cList by (x,y)
    //    Do not change the order in which the vertices appear
    override def translate(x: Double, y: Double): Polygon = {
        Polygon(cList.map((elem) => (elem._1 + x, elem._2 + y)))
    }
    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)

    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    //TODO: Define the bounding box for the circle
    override def getBoundingBox: (Double, Double, Double, Double) = {
        //xmin is center-rad, xmax is center+rad, same for y; ordering: (xmin, xmax, ymin, ymax)
        (c._1-r, c._1+r, c._2-r, c._2+r)
    }


    //TODO: Create a new circle by shifting the center
    override def translate(x: Double, y: Double): MyCircle = {
        MyCircle((c._1 + x, c._2 + y), r)
    }


    // Function: render -- draw the circle. Do not edit this function
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}

/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */

class MyCanvas (val listOfObjects: List[Figure]) {
    // TODO: Write a function to get the boundingbox for the entire canvas.
    // Hint: use existing boundingbox functions defined in each figure.
    def getBoundingBox: (Double, Double, Double, Double) = {
        listOfObjects.foldLeft[(Double, Double, Double, Double)]((Double.MaxValue, Double.MinValue, Double.MaxValue, Double.MinValue))((acc, elem) =>{
            val this_box: (Double, Double, Double, Double) = elem.getBoundingBox

            //https://alvinalexander.com/scala/scala-ternary-operator-syntax/
            (if (acc._1 < this_box._1) acc._1 else this_box._1, if (acc._2 > this_box._2) acc._2 else this_box._2,
              if (acc._3 < this_box._3) acc._3 else this_box._3, if (acc._4 > this_box._4) acc._4 else this_box._4)
        })
    }
    //TODO: Write a function to translate each figure in the canvas by shiftX, shiftY
    def translate(shiftX: Double, shiftY: Double): MyCanvas = {
        new MyCanvas(listOfObjects.map((elem) => elem.translate(shiftX, shiftY)))
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeRight(myc2: MyCanvas):MyCanvas = {
//        val this_bb = getBoundingBox
//        val myc2_bb = myc2.getBoundingBox
        val (x1, x2, y1, y2) = this.getBoundingBox
        val (cx1, cx2, cy1, cy2) = myc2.getBoundingBox
        val xshift = x2 - cx1
        val yshift = ((y1 + y2)/2) - ((cy1+cy2)/2)

//        val xshift: Double =  myc2_bb._1 - this_bb._2
//        val yshift:Double = (this_bb._4 - this_bb._3)/2 - (myc2_bb._4 - myc2_bb._3)/2

        this.overlap(myc2.translate(xshift, yshift))
//        val newList = myc2.listOfObjects.map((f: Figure) => f.translate(xshift, yshift))
//        new MyCanvas(newList++this.listOfObjects)
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the figures in myc2 on top of the figures in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeTop(myc2: MyCanvas): MyCanvas = {
//        val this_bb = getBoundingBox
//        val myc2_bb = myc2.getBoundingBox
//
//        val xshift: Double = (this_bb._2 - this_bb._1)/2 - (myc2_bb._2 - myc2_bb._1)/2
//        val yshift: Double = (this_bb._4 - myc2_bb._3)
        val (x1, x2, y1, y2) = this.getBoundingBox
        val (cx1, cx2, cy1, cy2) = myc2.getBoundingBox
        val xshift = (x1 + x2)/2 - (cx1 + cx2)/2
        val yshift = (y2 - cy1)

        overlap(myc2.translate(xshift, yshift))
    }

    //TODO: Write a function that will rotate each figure about the center of its bounding box in the canvas using
    // the angle `ang` defined in radians.
    // The writeup provided describes how to implement the rotation.
    // Hint: Write helper functions to rotate a Polygon and a circle. Then you can simply use
    // translation, followed by rotation of individual objects and translation back.
    def rotate(angRad: Double): MyCanvas = {
        val this_bb = getBoundingBox

        val rot_poly = (p: Polygon) => {
            val poly_bb = p.getBoundingBox

            //translate all objects by -center
            //val t = p.translate(-(this_bb._1 + this_bb._2)/2, -(this_bb._3 + this_bb._4)/2)

            //rotate as described by formula
            Polygon(p.cList.map((elem) => {(elem._1*math.cos(angRad) - elem._2*math.sin(angRad), elem._1*math.sin(angRad) + elem._2*math.cos(angRad))}))
              //.translate((this_bb._1 + this_bb._2)/2, (this_bb._3 + this_bb._4)/2)
        }

        val rot_circle = (c: MyCircle) => {
            val c_bb = c.getBoundingBox

            //translate all objects by -center
            //val t = c.translate(-(this_bb._1 + this_bb._2)/2, -(this_bb._3 + this_bb._4)/2)

            MyCircle((c.c._1 * math.cos(angRad) - c.c._2 * math.sin(angRad), c.c._1 * math.sin(angRad) + c.c._2 * math.cos(angRad)), c.r)
              //.translate((this_bb._1 + this_bb._2)/2, (this_bb._3 + this_bb._4)/2)
        }

        new MyCanvas(listOfObjects.map((fig) => {
            if (fig.isInstanceOf[MyCircle]) {
                rot_circle(fig.asInstanceOf[MyCircle])

            }
            else {
                rot_poly(fig.asInstanceOf[Polygon])
            }
        }))
    }


    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }

    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }

    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    // DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    // DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
