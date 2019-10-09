// Take variable duration pulse which may be high at any time and generate a single cycle
// high pulse alligned with the positive edge of the clock pulse.

module SINGLE_TRIGGER (input clk, input trigger_in, output trigger_out);

   reg trigger = 0;

   reg last_trigger_in = 0;
   
   always @ (posedge clk) begin

      if (!last_trigger_in & trigger_in)
         trigger <= 1;
      else
         trigger <= 0;

      last_trigger_in <= trigger_in;
      
   end

   assign trigger_out = trigger;
   
endmodule
