package gemmini

import org.scalatest.FlatSpec

class HeaderGenerationUnitTest extends FlatSpec {
  it should "generate a header" in {
    println(GemminiConfigs.defaultConfig.generateHeader())
  }
}
