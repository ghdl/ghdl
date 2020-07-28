entity repro1 is
port (
  in_valid   : in  bit;
  in_ready   : out bit);
end;

architecture rtl of repro1 is
  subtype ret_split_t is integer range 1 to 2;

  signal ready: bit_vector(0 to 3);
  signal valid: bit_vector(0 to 3);

  procedure split_stream (
    signal outcomb    : out bit_vector;
    signal incomb     : in bit_vector)
  is
  begin
    outcomb <= (outcomb'range => '1');
  end procedure split_stream;

begin
  split_stream (outcomb => valid(ret_split_t),
                incomb  => ready(ret_split_t));
end rtl;
