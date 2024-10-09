`default_nettype none

module underscore(
    input  wire x,
    output wire y
);

wire _dummy = x;
assign y = _dummy;

endmodule
