package bon.jo.datamodeler.model

import java.awt.{Dimension, FlowLayout}
import javax.swing.{JButton, JFrame, JLabel, JPanel, JTextField, WindowConstants}
import java.awt.GridBagLayout
import java.awt.GridBagConstraints
import javax.swing.JComboBox
import javax.swing.DefaultComboBoxModel
import bon.jo.datamodeler.model.Dsl.Builder
import scala.collection.mutable.ListBuffer
import javax.swing.JTextArea
import javax.swing.JScrollPane
import java.awt.Insets
import java.awt.Component
import java.awt.TextArea
import ToScala.*
import ToSql.*
object SwingUi:
  def mainFrame(title: String): JFrame =
    val f = new JFrame(title)
    f.setContentPane(JPanel(FlowLayout()))
    f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    f.setMinimumSize(Dimension(700, 700))
    f

  def createGBL = 
    val ret = GridBagLayout()
    
  type JF[A] = JFrame ?=> A
  type JP[A] = JPanel ?=> A
  def thisFrame: JF[JFrame] = summon
  def contentRoot: JF[JPanel] =
    thisFrame.getContentPane.asInstanceOf[JPanel]
  def thisPanel: JP[JPanel] = summon
  def newJpanel: JF[JPanel] = contentRoot.add(JPanel(GridBagLayout())).asInstanceOf[JPanel]

  trait View[A]:
    def read: A
    def load(a: A): Unit
  object View:
    inline def apply[A](inline readF: () => A, inline loadF: A => Unit): View[A] =
      new View {
        def read: A = readF()
        def load(a: A): Unit = loadF(a)
      }
  def table: JP[GridBagLayout] =
    val l = GridBagLayout()
    thisPanel.setLayout(l)
    l
  def panel(trt: JP[JPanel]): JF[JPanel] =
    given JPanel = newJpanel
    trt
  def addProp(name: String, jp: JPanel)(using
      c: GridBagConstraints,
      b: Builder[Entity]
  ): JF[View[Prop]] =
    val txt: JTextField = JTextField(name)
    val nameLabel: JLabel = JLabel("name")
    nextRow
    jp.add(nameLabel, c)
    nextCol
    jp.add(txt, c)

    val dell = JButton("Del.")
    dell.addActionListener(a =>
      jp.remove(txt)
      jp.remove(dell)
      jp.invalidate
      jp.repaint()
    )
    val mdle = DefaultComboBoxModel[_TypeName](_TypeName.values)
    val _type = JComboBox(mdle)

    nextCol
    jp.add(_type, c)
    nextCol
    jp.add(dell, c)
    thisFrame.pack

    View(
      readF = () =>
        Prop(
          txt.getText,
          _Type.from(mdle.getSelectedItem.asInstanceOf[_TypeName])
        ),
      loadF = v => txt.setText(v.name)
    )

  inline def add(cp : Component)(using c: GridBagConstraints):JP[Unit] =  thisPanel.add(cp,c)
  inline def nextCol(using c: GridBagConstraints) = c.gridx += 1
  inline def nextCol(cp : Component)(using c: GridBagConstraints):JP[Unit] = 
    nextCol
    add(cp)
  inline def nextRow(cp : Component)(using c: GridBagConstraints):JP[Unit] = 
    nextRow
    add(cp)
  inline def init(cp : Component)(using c: GridBagConstraints):JP[Unit] =
    init
    add(cp)
  inline def init(using c: GridBagConstraints) =
    c.gridx = 0
    c.gridy = 0
  inline def nextRow(using c: GridBagConstraints) =
    c.gridx = 0
    c.gridy += 1

  def prepareEntity(
      nameTx: JTextField
  )(using c: GridBagConstraints, p: JPanel,out : JTextArea): JF[Builder[Entity]] =
    
    val name = nameTx.getText
    given e: Builder[Entity] = Builder(Entity(name, Nil))
    val currentName = JTextField(name)
    val txt: JTextField = JTextField("name")
    val nameLabel: JLabel = JLabel("name")
    val b: JButton = JButton("add prop")
    val read: JButton = JButton("read")
    val scalaB: JButton = JButton("scala")
    val sqlB: JButton = JButton("sql")
    val supp = JButton("Del.")
    given JPanel = p
    val currentPropsView = ListBuffer[View[Prop]]()
    init(nameLabel)

    nextCol(currentName)

    nextCol(read)

    nextCol(scalaB)

    nextCol(sqlB)
 
    nextCol(supp)

    nextRow(b)

    nextCol(txt)
  
    supp.addActionListener(a =>
      contentRoot.remove(p)
      contentRoot.invalidate
      contentRoot.repaint()
    )

    b.addActionListener(a => currentPropsView += addProp(txt.getText, p))
    read.addActionListener(a =>
      e.value = e.value.copy(currentName.getText,props = currentPropsView.toList.map(_.read))
     
      println(e.value)
    )
    scalaB.addActionListener(a =>
      e.value = e.value.copy(currentName.getText,props = currentPropsView.toList.map(_.read))
     
      out.setText(e.value.toScalaCaseClass())
    )
    sqlB.addActionListener(a =>
      e.value = e.value.copy(currentName.getText,props = currentPropsView.toList.map(_.read))
     
      out.setText(e.value.toSqlCreate())
    )
    thisFrame.pack
    e
  import Dsl.OnBuild
  import Dsl.buildingValue
  import Dsl.*
  def addToMdel(e: Entity): OnBuild[List[Entity]] =
    thisBuilder.value = thisBuilder.value :+ e
    thisBuilder
  def addEntity(name: JTextField)(using Builder[List[Entity]],JTextArea): JF[Unit] =

    given p: JPanel = newJpanel
    val gc: GridBagLayout = table

    given c: GridBagConstraints = GridBagConstraints()
    prepareEntity(name)

  @main def main() =
    given JFrame = mainFrame("test")

    val b: JButton = JButton("add entity")
    val txt: JTextField = JTextField("name")
    given JTextArea = JTextArea(10,10)
    given Builder[List[Entity]] = Builder(Nil)
    b.addActionListener(a => addEntity(txt))
   
    val jpp = JPanel()
    jpp.add(summon[JTextArea])
    val js = JScrollPane(jpp)
    panel {
      given GridBagConstraints = GridBagConstraints()
      init(txt)  
      nextCol(b)
      nextRow(js)
      thisPanel
    }
    thisFrame.setVisible(true)
