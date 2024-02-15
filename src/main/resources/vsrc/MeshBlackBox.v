// This is the blackbox we are asking you to implement
// Assume TILEROWS=1, and TILECOLUMNS=1
module MeshBlackBox
    #(parameter MESHROWS, MESHCOLUMNS, INPUT_BITWIDTH, OUTPUT_BITWIDTH, MAX_SIM_MM_LOG, ACC_BITWIDTH_LOG, TILEROWS=1, TILECOLUMNS=1)
    (
        input                               clock,
        input                               reset,
        input signed [INPUT_BITWIDTH-1:0]   in_a[MESHROWS-1:0][TILEROWS-1:0],
        input signed [INPUT_BITWIDTH-1:0]   in_d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        input signed [INPUT_BITWIDTH-1:0]   in_b[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        input                               in_control_dataflow[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        input                               in_control_propagate[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        input [ACC_BITWIDTH_LOG-1:0]        in_control_shift[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        input                               in_valid[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        input [MAX_SIM_MM_LOG-1:0]          in_id[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        input                               in_last[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        output signed [OUTPUT_BITWIDTH-1:0] out_c[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        output signed [OUTPUT_BITWIDTH-1:0] out_b[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        output                              out_valid[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        output                              out_control_dataflow[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        output                              out_control_propagate[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        output [ACC_BITWIDTH_LOG-1:0]       out_control_shift[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        output [MAX_SIM_MM_LOG-1:0]         out_id[MESHCOLUMNS-1:0][TILECOLUMNS-1:0],
        output                              out_last[MESHCOLUMNS-1:0][TILECOLUMNS-1:0]
    );

    // ---------------------------------------------------------
    // ---------------------------------------------------------
    //           DO NOT MODIFY ANYTHING ABOVE THIS
    // ---------------------------------------------------------
    // ---------------------------------------------------------

    //**********************************************************
    //**********************************************************
    //**********************************************************
    //******************** FILL THIS ***************************
    //**********************************************************
    //**********************************************************
    //**********************************************************


endmodule // MeshBlackBox

//**********************************************************
//**********************************************************
//**********************************************************
//********** FEEL FREE TO ADD MODULES HERE *****************
//**********************************************************
//**********************************************************
//**********************************************************

// ---------------------------------------------------------
// ---------------------------------------------------------
//           DO NOT MODIFY ANYTHING BELOW THIS
// ---------------------------------------------------------
// ---------------------------------------------------------

// We are providing this adapter, due to the format of Chisel-generated Verilog
// and it's compatibility with a blackbox interface.
//
// This adapter converts the Gemmini multiplication function into something
// more amenable to teaching:
//
// Assumed that bias matrix is 0.
//
// Originally Gemmini does:
//   A*D + B => B
//   0 => C
//
// This adapter converts it to the following:
//   A*B + D => C
//   0 => B
module MeshBlackBoxAdapter
  #(parameter MESHROWS, MESHCOLUMNS, INPUT_BITWIDTH, OUTPUT_BITWIDTH, MAX_SIM_MM_LOG, ACC_BITWIDTH_LOG, TILEROWS=1, TILECOLUMNS=1)
    (
    input                                                 clock,
    input                                                 reset,
    input [MESHROWS*TILEROWS*INPUT_BITWIDTH-1:0]          in_a,
    input [MESHCOLUMNS*TILECOLUMNS*INPUT_BITWIDTH-1:0]    in_d,
    input [MESHCOLUMNS*TILECOLUMNS*INPUT_BITWIDTH-1:0]    in_b,
    input [MESHCOLUMNS*TILECOLUMNS-1:0]                   in_control_dataflow,
    input [MESHCOLUMNS*TILECOLUMNS-1:0]                   in_control_propagate,
    input [MESHCOLUMNS*TILECOLUMNS*ACC_BITWIDTH_LOG-1:0]  in_control_shift,
    input [MESHCOLUMNS*TILECOLUMNS-1:0]                   in_valid,
    input [MESHCOLUMNS*TILECOLUMNS*MAX_SIM_MM_LOG-1:0]    in_id,
    input [MESHCOLUMNS*TILECOLUMNS-1:0]                   in_last,
    output [MESHCOLUMNS*TILECOLUMNS*OUTPUT_BITWIDTH-1:0]  out_c,
    output [MESHCOLUMNS*TILECOLUMNS*OUTPUT_BITWIDTH-1:0]  out_b,
    output [MESHCOLUMNS*TILECOLUMNS-1:0]                  out_valid,
    output [MESHCOLUMNS*TILECOLUMNS-1:0]                  out_control_dataflow,
    output [MESHCOLUMNS*TILECOLUMNS-1:0]                  out_control_propagate,
    output [MESHCOLUMNS*TILECOLUMNS*ACC_BITWIDTH_LOG-1:0] out_control_shift,
    output [MESHCOLUMNS*TILECOLUMNS*MAX_SIM_MM_LOG-1:0]   out_id,
    output [MESHCOLUMNS*TILECOLUMNS-1:0]                  out_last
    );

  wire signed [INPUT_BITWIDTH-1:0]  in_a_2d[MESHROWS-1:0][TILEROWS-1:0];
  wire signed [INPUT_BITWIDTH-1:0]  in_d_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire signed [INPUT_BITWIDTH-1:0]  in_b_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire                              in_control_dataflow_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire                              in_control_propagate_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire [ACC_BITWIDTH_LOG-1:0]       in_control_shift_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire                              in_valid_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire [MAX_SIM_MM_LOG-1:0]         in_id_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire                              in_last_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire signed [OUTPUT_BITWIDTH-1:0] out_c_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire signed [OUTPUT_BITWIDTH-1:0] out_b_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire                              out_valid_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire                              out_control_dataflow_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire                              out_control_propagate_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire [ACC_BITWIDTH_LOG-1:0]       out_control_shift_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];  
  wire [MAX_SIM_MM_LOG-1:0]         out_id_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  wire                              out_last_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];  
  reg signed [OUTPUT_BITWIDTH-1:0] reg_out_c_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  reg signed [OUTPUT_BITWIDTH-1:0] reg_out_b_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  reg                              reg_out_valid_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  reg                              reg_out_control_dataflow_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  reg                              reg_out_control_propagate_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  reg [ACC_BITWIDTH_LOG-1:0]       reg_out_control_shift_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];  
  reg [MAX_SIM_MM_LOG-1:0]         reg_out_id_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0];
  reg                              reg_out_last_2d[MESHCOLUMNS-1:0][TILECOLUMNS-1:0]; 

  // Convert wide signals into "cleaner" 2D Verilog arrays
  genvar i;
  genvar j;
  generate
  for (i = 0; i < MESHROWS ; i++) begin
    for (j = 0; j < TILEROWS ; j++) begin
      assign in_a_2d[i][j] = in_a[i*(TILEROWS*INPUT_BITWIDTH)+(j+1)*(INPUT_BITWIDTH)-1:i*(TILEROWS*INPUT_BITWIDTH)+j*(INPUT_BITWIDTH)];
    end
  end
  endgenerate

  generate
  for (i = 0; i < MESHCOLUMNS ; i++) begin
    for (j = 0; j < TILECOLUMNS ; j++) begin
       assign in_d_2d[i][j] = in_d[i*(TILECOLUMNS*INPUT_BITWIDTH)+(j+1)*(INPUT_BITWIDTH)-1:i*(TILECOLUMNS*INPUT_BITWIDTH)+j*(INPUT_BITWIDTH)];
       assign in_b_2d[i][j] = in_b[i*(TILECOLUMNS*INPUT_BITWIDTH)+(j+1)*(INPUT_BITWIDTH)-1:i*(TILECOLUMNS*INPUT_BITWIDTH)+j*(INPUT_BITWIDTH)];
       assign in_control_dataflow_2d[i][j] = in_control_dataflow[i*(TILECOLUMNS)+(j+1)-1:i*(TILECOLUMNS)+j];
       assign in_control_propagate_2d[i][j] = in_control_propagate[i*(TILECOLUMNS)+(j+1)-1:i*(TILECOLUMNS)+j];
       assign in_control_shift_2d[i][j] = in_control_shift[i*(TILECOLUMNS*ACC_BITWIDTH_LOG)+(j+1)*(ACC_BITWIDTH_LOG)-1:i*(TILECOLUMNS*ACC_BITWIDTH_LOG)+j*(ACC_BITWIDTH_LOG)];
       assign in_valid_2d[i][j] = in_valid[i*(TILECOLUMNS)+(j+1)-1:i*(TILECOLUMNS)+j];
       assign in_id_2d[i][j] = in_id[i*(TILECOLUMNS*MAX_SIM_MM_LOG)+(j+1)*(MAX_SIM_MM_LOG)-1:i*(TILECOLUMNS*MAX_SIM_MM_LOG)+j*(MAX_SIM_MM_LOG)];
       assign in_last_2d[i][j] = in_last[i*(TILECOLUMNS)+(j+1)-1:i*(TILECOLUMNS)+j];

       assign out_c[i*(TILECOLUMNS*OUTPUT_BITWIDTH)+(j+1)*(OUTPUT_BITWIDTH)-1:i*(TILECOLUMNS*OUTPUT_BITWIDTH)+j*(OUTPUT_BITWIDTH)] = reg_out_c_2d[i][j];
       assign out_b[i*(TILECOLUMNS*OUTPUT_BITWIDTH)+(j+1)*(OUTPUT_BITWIDTH)-1:i*(TILECOLUMNS*OUTPUT_BITWIDTH)+j*(OUTPUT_BITWIDTH)] = reg_out_b_2d[i][j];
       assign out_valid[i*(TILECOLUMNS)+(j+1)-1:i*(TILECOLUMNS)+j] = reg_out_valid_2d[i][j];
       
       assign out_control_dataflow[i*(TILECOLUMNS)+(j+1)-1:i*(TILECOLUMNS)+j] = reg_out_control_dataflow_2d[i][j];
       assign out_control_propagate[i*(TILECOLUMNS)+(j+1)-1:i*(TILECOLUMNS)+j] = reg_out_control_propagate_2d[i][j];
       assign out_control_shift[i*(TILECOLUMNS*ACC_BITWIDTH_LOG)+(j+1)*(ACC_BITWIDTH_LOG)-1:i*(TILECOLUMNS*ACC_BITWIDTH_LOG)+j*(ACC_BITWIDTH_LOG)] = reg_out_control_shift_2d[i][j];

       assign out_id[i*(TILECOLUMNS*MAX_SIM_MM_LOG)+(j+1)*(MAX_SIM_MM_LOG)-1:i*(TILECOLUMNS*MAX_SIM_MM_LOG)+j*(MAX_SIM_MM_LOG)] = reg_out_id_2d[i][j];
       assign out_last[i*(TILECOLUMNS)+(j+1)-1:i*(TILECOLUMNS)+j] = reg_out_last_2d[i][j];

       always @(posedge clock) begin
           if (reset) begin
               // reset all values to 0
               reg_out_c_2d[i][j] <= '0;
               reg_out_b_2d[i][j] <= '0;
               reg_out_valid_2d[i][j] <= '0;
               reg_out_control_dataflow_2d[i][j] <= '0;
               reg_out_control_propagate_2d[i][j] <= '0;
               reg_out_control_shift_2d[i][j] <= '0;
               reg_out_id_2d[i][j] <= '0;
               reg_out_last_2d[i][j] <= '0;
           end
           else begin
               // regnext the values
               reg_out_c_2d[i][j] <= out_c_2d[i][j];
               reg_out_b_2d[i][j] <= out_b_2d[i][j];
               reg_out_valid_2d[i][j] <= out_valid_2d[i][j];
               reg_out_control_dataflow_2d[i][j] <= out_control_dataflow_2d[i][j];
               reg_out_control_propagate_2d[i][j] <= out_control_propagate_2d[i][j];
               reg_out_control_shift_2d[i][j] <= out_control_shift_2d[i][j];
               reg_out_id_2d[i][j] <= out_id_2d[i][j];
               reg_out_last_2d[i][j] <= out_last_2d[i][j];
           end
       end
    end
  end
  endgenerate

  // Instantiate the Mesh BlackBox implementation (the one you are writing in
  // this assignment)
  // Note: This swaps signals around a bit:
  //   in_b <-> in_d
  //   out_c <-> out_b
  MeshBlackBox #(.MESHROWS(MESHROWS),.TILEROWS(TILEROWS),.MESHCOLUMNS(MESHCOLUMNS),.MAX_SIM_MM_LOG(MAX_SIM_MM_LOG),.ACC_BITWIDTH_LOG(ACC_BITWIDTH_LOG), .TILECOLUMNS(TILECOLUMNS),.INPUT_BITWIDTH(INPUT_BITWIDTH),.OUTPUT_BITWIDTH(OUTPUT_BITWIDTH))
   mesh_blackbox_inst (
       .clock                (clock),
       .reset                (reset),
       .in_a                 (in_a_2d),
       .in_d                 (in_b_2d),
       .in_b                 (in_d_2d),
       .in_control_dataflow  (in_control_dataflow_2d),
       .in_control_propagate (in_control_propagate_2d),
       .in_control_shift     (in_control_shift_2d),
       .in_valid             (in_valid_2d),
       .in_id                (in_id_2d),
       .in_last              (in_last_2d),
       .out_c                (out_b_2d),
       .out_b                (out_c_2d),
       .out_valid            (out_valid_2d),
       .out_control_dataflow  (out_control_dataflow_2d),
       .out_control_propagate (out_control_propagate_2d),
       .out_control_shift     (out_control_shift_2d),
       .out_id               (out_id_2d),
       .out_last             (out_last_2d)
  );

endmodule  //MeshBlackBoxAdapter