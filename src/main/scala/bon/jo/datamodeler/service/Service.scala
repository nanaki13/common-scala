package bon.jo.datamodeler.service

import bon.jo.datamodeler.model.Model.{Message, Room, RoomMessage, User, UserUserMessage}
import bon.jo.datamodeler.model.sql.DaoInline.IntDaoSyncInline
import bon.jo.datamodeler.model.sql.DaoInline.IntDaoSyncInline.dao
import bon.jo.datamodeler.model.Model.to
import bon.jo.datamodeler.model.Model
import bon.jo.datamodeler.model.sql.DaoInline.IntEntityMethods.*
import bon.jo.datamodeler.model.sql.DaoInline.EntityMethods.*
import bon.jo.datamodeler.model.sql.RawDaoInline
object Service {
  def apply()(using d1 : IntDaoSyncInline[Message], d2 :  RawDaoInline.Sync[UserUserMessage], d3 : RawDaoInline.Sync[RoomMessage]) : Service = new{}

}

trait Service(using d1 : IntDaoSyncInline[Message], d2 :  RawDaoInline.Sync[UserUserMessage], d3 : RawDaoInline.Sync[RoomMessage]) :
  extension (u : User)
    inline def sendToUser( m : Message, de : Int):Message =
      val mSave = m.save()
      //println(mSave)
      mSave.toUser(de).insertRaw()
      mSave
    inline def send( m : Message, de : User):Message =
      val mSave = m.save()
      mSave.to(de).insertRaw()
      mSave
    inline def send( m : Message, de : Room):Message =
      val mSave = m.save()
      mSave.to(de).insertRaw()
      mSave


    inline def readMessages():List[Message] =
      d2.joinWhereCustom[Message,Int](_.idMessage,_.idUserDest,u.id).map(_._2)

  extension (u : Room)
    inline def readMessages():List[Message] =
      d3.joinWhereCustom[Message,Int](_.idMessage,_.idRoom,u.id).map(_._2)
