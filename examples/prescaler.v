// Prescalar with default 16 bit division

module PRESCALER #(parameter BITS = 16) (input clk, output out);
   
    reg [BITS-1:0] counter = 0;
 
    always @ (posedge clk) begin      
    
        counter <= counter + 1;
    
    end

   assign out = counter[BITS-1];
   
endmodule
