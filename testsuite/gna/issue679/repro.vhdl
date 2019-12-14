entity repro is
end repro;

architecture behav of repro is
   signal clk : bit;
   signal cyc: bit;
   signal wen : bit;
   signal lw : bit;
begin
   -- psl default clock is clk;

   -- psl c1: assert always lw -> cyc and (next not(wen))
   --  report "error";
end;
