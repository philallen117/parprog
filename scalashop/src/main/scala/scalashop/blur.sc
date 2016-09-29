import java.util.concurrent.ForkJoinTask

import common._
import scalashop._

val i15in4 = intervals(15, 4)
val i16in4 = intervals(16, 4)
val i4in1 = intervals(4, 1)

val tasks1 = for {
  (from, to) <- intervals(4, 1)
} yield task { (from, to) }
val tasks1res = tasks1 map { _.join() }

val tasks = for {
  (from, to) <- intervals(15, 4)
} yield task { (from, to) }
val tasksRes = tasks map { _.join() }

val w = 4
val h = 3
val src = new Img(w, h); val dst1 = new Img(w, h); val dst2 = new Img(w, h)
def setSrc(src: Img) = {
  src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
  src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
  src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11
}
setSrc(src)
boxBlurKernel(src, 0, 0, 2)
boxBlurKernel(src, w, h, 0)
boxBlurKernel(src, h, w, 0)
boxBlurKernel(src, w, h, 2)
VerticalBoxBlur.blur(src, dst1, 0, w, 2)
src == dst1
//HorizontalBoxBlur.blur(src, dst2, 0, h, 2)
//src == dst2

