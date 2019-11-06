module top (input CLK, output [7:0] OUT_C, output [2:0] OUT_R, output [3:0] IN_C, input [3:0] IN_R, output [3:0] IND);

    wire [23:0] led;
   
    // Prescaler on the clock
    reg [19:0] counter = 0;
    always @ (posedge CLK) begin
        counter <= counter + 1;
    end
   
    // Handle the inputs
    reg [3:0] shift_in = 4'b1110;
    always @ (negedge counter[13]) begin
        shift_in <= { shift_in[2:0], shift_in[3] };
    end
    assign IN_C = shift_in;

    reg [15:0] buttons = 0;

    always @ (posedge counter[13]) begin
        case (shift_in)
            4'b1110: begin
                buttons[0] <= !IN_R[0];
                buttons[4] <= !IN_R[1];
                buttons[8] <= !IN_R[2];
                buttons[12] <= !IN_R[3];    
            end
            4'b1101: begin
                buttons[1] <= !IN_R[0];
                buttons[5] <= !IN_R[1];
                buttons[9] <= !IN_R[2];
                buttons[13] <= !IN_R[3];      
            end
            4'b1011: begin
                buttons[2] <= !IN_R[0];
                buttons[6] <= !IN_R[1];
                buttons[10] <= !IN_R[2];
                buttons[14] <= !IN_R[3];
            end
            4'b0111: begin
                buttons[3] <= !IN_R[0];
                buttons[7] <= !IN_R[1];
                buttons[11] <= !IN_R[2];
                buttons[15] <= !IN_R[3];      
            end
        endcase
    end

    // Connect up the processor
    PROCESSOR cpu(
            .clk(counter[4]),
            .led(led),
            .indicators(IND),
            .buttons(buttons));

    // Handle output stuff
    reg [7:0] out;
    reg [2:0] shift_out = 3'b001;

    always @ (posedge counter[7]) begin
        if (shift_out[2] == 1)
            out <= led[7:0];
        if (shift_out[0] == 1)
            out <= led[15:8];
        if (shift_out[1] == 1)
            out <= led[23:16];
        shift_out <= { shift_out[1:0], shift_out[2] };
   end

   assign OUT_R = shift_out;
   assign OUT_C = out;

endmodule 
