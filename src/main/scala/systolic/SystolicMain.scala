package systolic

object SystolicMain extends App {
  chisel3.Driver.execute(args, () => new Mesh(8, 4, 4, pass_through = false))
}
