package systolic

object MeshMain extends App {
  chisel3.Driver.execute(args, () => new Mesh(8, 64, 64, true))
}
