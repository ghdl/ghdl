
entity top2 is
  generic (
    ok : boolean := false
    );
    port (
        clk : in bit;
        inp : in bit;
        outp : out bit);
end;

architecture beh of top2 is
begin
  assert ok report "my assert message";
  outp <= inp;
end beh;
