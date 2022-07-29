library ieee;
  use ieee.std_logic_1164.all;

entity sequencer is
  generic (
    seq : string
  );
  port (
    clk  : in  std_logic;
    data : out std_logic
  );
end entity sequencer;

architecture rtl of sequencer is

  signal index : natural := seq'low;

  function to_bit (a : in character) return std_logic is
    variable ret : std_logic;
  begin
    case a is
      when '0' | '_' => ret := '0';
      when '1' | '-' => ret := '1';
      when others    => ret := 'X';
    end case;
    return ret;
  end function to_bit;

begin

  process (clk) is
  begin
    if rising_edge(clk) then
      if (index < seq'high) then
        index <= index + 1;
      end if;
    end if;
  end process;

  data <= to_bit(seq(index));

end architecture rtl;


library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity issue is
  port (
    clk : in std_logic
  );
end entity issue;

architecture psl of issue is

  signal a, b, c, d : std_logic;

begin

  --                                          01234567890
  SEQ_A : entity work.sequencer generic map ("_-____-____") port map (clk, a);
  SEQ_B : entity work.sequencer generic map ("___-_____-_") port map (clk, b);

  --                                          01234567890
  SEQ_C : entity work.sequencer generic map ("_-___-_____") port map (clk, c);
  SEQ_D : entity work.sequencer generic map ("_____-___-_") port map (clk, d);

  default clock is rising_edge(clk);

  -- Should hold, but fails at cycle 3
  -- Potential GHDL bug?
  -- This is similar to waveform 2.9(i) on page 16
  -- in the book "A practical introduction to PSL"
  BEFORE_0_a : assert always (a -> next (b before_ a))
    report "BEFORE_0_a failed";

  -- Should hold but does not at cycle 9
  -- Potential GHDL bug?
  -- This is similar to waveform 2.9(ii) on page 16
  -- in the book "A practical introduction to PSL"
  BEFORE_1_a : assert always (c -> next (d before_ c))
    report "BEFORE_1_a failed";

end architecture psl;

library ieee;
  use ieee.std_logic_1164.all;

use std.env.all;


entity test_issue is
end entity test_issue;


architecture sim of test_issue is

  signal clk   : std_logic := '1';

begin


  clk <= not clk after 500 ps;

  DUT : entity work.issue(psl) port map (clk);

  -- stop simulation after 30 cycles
  process
    variable index : natural := 10;
  begin
    loop
      wait until rising_edge(clk);
      index := index - 1;
      exit when index = 0;
    end loop;
    stop(0);
  end process;


end architecture sim;
