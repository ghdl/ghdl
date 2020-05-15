library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue is
    port (sig_gt, sig_ge, sig_lt, sig_le : out boolean;
          uns_gt, uns_ge, uns_lt, uns_le : out boolean);
end issue;

architecture beh of issue is
begin
    -- all of those works
    uns_gt <= (unsigned'("1111") >  unsigned'("0111"));
    uns_ge <= (unsigned'("1111") >= unsigned'("0111"));
    uns_lt <= (unsigned'("1111") <  unsigned'("0111"));
    uns_le <= (unsigned'("1111") <= unsigned'("0111"));

    sig_gt <= (signed'("1111") >  signed'("0111"));
    sig_ge <= (signed'("1111") >= signed'("0111"));
    sig_lt <= (signed'("1111") <  signed'("0111"));
    sig_le <= (signed'("1111") <= signed'("0111"));
end architecture beh;
