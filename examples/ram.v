// RAM module with single input addr, input and output ports, a write enable and clock input.
// Data is clocked out of, and into, the RAM on positive clock edges.

module RAM #(parameter DATA_BITS = 8,  parameter ADDRESS_BITS = 4)

(input clk, input write, input[ADDRESS_BITS-1:0] addr, input[DATA_BITS-1:0] in_data, output[DATA_BITS-1:0] out_data);
   
   reg [DATA_BITS-1:0] memorySpace [0:2**ADDRESS_BITS-1];                          

   reg [DATA_BITS-1:0] data_out_reg;
   
   always @ (posedge clk) begin

       if (write) memorySpace[addr] <= in_data;

       data_out_reg <= memorySpace[addr];
       
    end

   assign out_data = data_out_reg;
   
endmodule
