package module1.futures

import HomeworksUtils.TaskSyntax

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future, Promise}


object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */

  def fullSequence2[A](futures: List[Future[A]])
                      (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
    task"Реализуйте метод `fullSequence`"()


  def fullSequence[A](futures: List[Future[A]]): Future[(List[A], List[Throwable])] = {
    val p = Promise[(List[A], List[Throwable])]
    val acc: Future[(List[A], List[Throwable])] = futures.reverse
      .foldLeft(Future.successful((List.empty[A], List.empty[Throwable]))) { (prevFut, fut) =>
        prevFut.flatMap { case (values, exceptions) =>
          fut.map(value => (value :: values, exceptions))
            .recover { case exception => (values, exception :: exceptions) }
        }
      }
    acc.onComplete(p.tryComplete)
    p.future
  }
}
