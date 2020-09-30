package com.gatis.homework.basics

object ClassesAndTraits extends App {

  sealed trait Shape extends Located with Bounded with Movable {
    def area: Double
  }

  // I kept the names of Shape, Located, Bounded as they were defined in the
  // ClassesAndTraits file in the exercise class, referring to 2D shapes
  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double): Shape
  }

  sealed trait Shape3D extends Located3D with Movable3D {
    def surfaceArea: Double
    def volume: Double
  }

  sealed trait Located3D extends Located {
    def z: Double
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override def area: Double = 0
    override def move(dx: Double, dy: Double) = Point(x + dx, y + dy)
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
  }

  object Origin extends Located {
    override def x: Double = 0
    override def y: Double = 0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def area: Double = Math.PI * Math.pow(radius, 2)
    override def move(dx: Double, dy: Double) = Circle(x + dx, y + dy, radius)
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
  }

  final case class Rectangle(minX: Double, minY: Double, lengthX: Double, lengthY: Double) extends Shape {
    // assumes positive lengths, otherwise could be abused. Same problem would be with e.g. minX and maxX - the user could specify minX>maxX
    // really a problem for other shapes as well e.g negative radius
    override def maxX: Double = minX + lengthX
    override def maxY: Double = minY + lengthY
    override def area: Double = lengthX * lengthY
    override def move(dx: Double, dy: Double) = Rectangle(minX + dx, minY + dy, lengthX, lengthY)
    override def x: Double = minX
    override def y: Double = minY
  }

  final case class Square(minX: Double, minY: Double, borderLength: Double) extends Shape {
    private val rectangle = Rectangle(minX, minY, borderLength, borderLength)
    override def area: Double = rectangle.area
    override def move(dx: Double, dy: Double): Square = Square(minX + dx, minY + dy, borderLength)
    override def x: Double = rectangle.x
    override def y: Double = rectangle.y
    override def maxX: Double = rectangle.maxX
    override def maxY: Double = rectangle.maxY
  }

  final case class Triangle(point1: Point, point2: Point, point3: Point) extends Shape {
    private val points = List(point1, point2, point3)
    // area method left unimplemented due to lengthy math
    override def area: Double = ???
    override def x: Double = point1.x
    override def y: Double = point1.y
    override def minX: Double = points.map(_.x).min
    override def maxX: Double = points.map(_.x).max
    override def minY: Double = points.map(_.y).min
    override def maxY: Double = points.map(_.y).max
    override def move(dx: Double, dy: Double): Triangle = Triangle(Point(point1.x + dx, point1.y + dy), Point(point2.x + dx, point2.y + dy), Point(point3.x + dx, point3.y + dy))
  }

  object Origin3D extends Located3D {
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def surfaceArea: Double = 0
    override def volume: Double = 0
    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3D {
    override def surfaceArea: Double = 4 * Math.PI * Math.pow(radius, 2)
    override def volume: Double = 4 / 3 * Math.PI * Math.pow(radius, 3)
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ
    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(x + dx, y + dy, z + dz, radius)
  }

  final case class Cube(minX: Double, minY: Double, minZ: Double, edgeLength: Double) extends Shape3D {
    private val cuboid = Cuboid(minX, minY, minZ, edgeLength, edgeLength)
    override def surfaceArea: Double = cuboid.surfaceArea
    override def volume: Double = cuboid.volume
    override def x: Double = cuboid.x
    override def y: Double = cuboid.y
    override def z: Double = cuboid.z
    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(x + dx, y + dy, z + dz, edgeLength)
  }

  final case class Cuboid(minX: Double, minY: Double, minZ: Double, squareEdgeLength: Double, height: Double) extends Shape3D {
    override def surfaceArea: Double = 2 * Math.pow(squareEdgeLength, 2) + 4 * squareEdgeLength * height
    override def volume: Double = Math.pow(squareEdgeLength, 2) * height
    override def x: Double = minX
    override def y: Double = minY
    override def z: Double = minZ
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(x + dx, y + dy, z + dz, squareEdgeLength, height)
  }

  final case class Tetrahedron(point1: Point3D, point2: Point3D, point3: Point3D, point4: Point3D) extends Shape3D {
    // surfaceArea and volume methods left unimplemented due to lengthy math formulas
    override def surfaceArea: Double = ???
    override def volume: Double = ???
    override def x: Double = point1.x
    override def y: Double = point1.y
    override def z: Double = point1.z
    override def move(dx: Double, dy: Double, dz: Double): Tetrahedron = Tetrahedron(
      Point3D(point1.x + dx, point1.y + dy, point1.z + dz),
      Point3D(point2.x + dx, point2.y + dy, point2.z + dz),
      Point3D(point3.x + dx, point3.y + dy, point3.z + dz),
      Point3D(point4.x + dx, point4.y + dy, point4.z + dz)
    )
  }

  def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = {
    new Bounded {
      implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering
      override def minX: Double = objects.map(_.minX).min
      override def maxX: Double = objects.map(_.maxX).max
      override def minY: Double = objects.map(_.minY).min
      override def maxY: Double = objects.map(_.maxY).max
    }
  }

  // Pattern matching and exhaustiveness checking
  def describe(x: Shape): String = x match {
    case Point(x, y)                             => s"Point(x = $x, y = $y)"
    case Circle(centerX, centerY, radius)        => s"Circle(centerX = $centerX, centerY = $centerY, radius = $radius)"
    case Rectangle(minX, minY, lengthX, lengthY) => s"Rectangle(minX= $minX, minY= $minY, lengthX= $lengthX, lengthY= $lengthY)"
    case Square(minX, minY, borderLength)        => s"Square(minX= $minX, minY= $minY, borderLength= $borderLength)"
    case Triangle(point1, point2, point3)        => s"Triangle(point1 x= $point1.x, y=$point1.y, point2 x= $point2.x, y=$point2.y, point3 x= $point3.x, y=$point3.y)"
  }

}
