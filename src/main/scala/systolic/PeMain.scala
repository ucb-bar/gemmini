package systolic

object PeMain extends App {
  chisel3.Driver.execute(args, () => new PE(8,true))
}
