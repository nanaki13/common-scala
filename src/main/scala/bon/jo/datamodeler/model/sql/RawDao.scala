package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.util.Pool

object RawDao:
  object Sync:
    inline def apply[E]()(using RawDaoInline.Sync[E]):RawDaoOps.Sync[E] =
      val inlneDao = summon[RawDaoInline.Sync[E]]
      RawDaoOps.Sync[E](e => inlneDao.insert(e) ,(es) => inlneDao.insertAll(es),() => inlneDao.selectAll(),(pa) => inlneDao.selectAll(pa),() => inlneDao.deleteAll())

    inline def fromPool[E]()(using Pool[java.sql.Connection]):RawDaoOps.Sync[E] =

      given inlneDao : RawDaoInline.Sync[E] = RawDaoInline()
      RawDaoOps.Sync[E](e => inlneDao.insert(e) ,(es) => inlneDao.insertAll(es),() => inlneDao.selectAll(),(pa) => inlneDao.selectAll(pa),() => inlneDao.deleteAll())