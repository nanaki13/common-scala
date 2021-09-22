package bon.jo.datamodeler.server


enum Response:
  case OK
  //case OKSave[T](r: T)
  case KO(reason: String)

//object Response:
  /*inline def unnapply[T](e: Response.OKSave[_]): Option[Response.OKSave[T]] =
    e match {
      case Response.OKSave(a) =>
        a match {
          case t: T => Some(Response.OKSave[T](t))
        }

    }*/
