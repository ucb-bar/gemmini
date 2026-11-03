package gemmini

// MX mesh geometry: one acc row = 2 output rows x 2R bf16 cols (4 bf16 per 64-bit acc entry).
// Supported range (DIM 4..32, block 32) is checked in GemminiArrayConfig for MX configs.
case class MxGeom(dim: Int, mxBlock: Int = 32) {
  val R             = dim max (mxBlock / 2)   // acc row entries
  val W             = R / dim                 // col tiles packed per acc row
  val T             = (mxBlock / dim) max 1   // mesh tiles per MX block
  val numChunks     = MxGeom.numChunks
  val lanesPerBeat  = 2 * R                   // values per acc read beat
  val blocksPerBeat = lanesPerBeat / mxBlock  // MX blocks per acc read beat
  val codeBeats     = lanesPerBeat / dim      // spad rows per 8-bit requant beat
  val bf16Beats     = 2 * lanesPerBeat / dim  // spad rows per bf16 beat
}

object MxGeom {
  val numChunks = 2   // output rows per acc row
  // acc read chunks per row: MX always 2; non-MX rows narrower than 16 elems are read whole
  def accChunks(useMx: Boolean, accRowElems: Int): Int = if (useMx) numChunks else if (accRowElems < 16) 1 else 2
}
