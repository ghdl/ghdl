
entity top3 is
  generic (
    ok : boolean := false
    );
    port (
        clk : in bit;
        inp : in bit;
        outp : out bit);
end;

architecture beh of top3 is
begin
  assert ok report "my assert message, value:" & bit'image(inp);
  outp <= inp;
end beh;
