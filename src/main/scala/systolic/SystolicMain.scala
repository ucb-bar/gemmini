package systolic

object SystolicMain extends App {
  chisel3.Driver.execute(args, () => new Grid(16, 3, 2, 2, 2))
}
