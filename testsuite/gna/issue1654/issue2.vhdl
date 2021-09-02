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


entity issue is
  port (
    clk : in std_logic
  );
end entity issue;


architecture psl of issue is

  signal a, b, c, d : std_logic;

begin

  -- async reset 100 ps after rising edge in cycle 1
  d <= '0', '1' after 1100 ps, '0' after 1400 ps;

  --                              0123456789
  SEQ_A : entity work.sequencer generic map ("-___-_____") port map (clk, a);
  SEQ_B : entity work.sequencer generic map ("_______-__") port map (clk, b);
  SEQ_C : entity work.sequencer generic map ("-_________") port map (clk, c);
  --  D :                                     _|________

  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- This assertion holds
  WITH_sync_ABORT_a : assert (always a -> next (b before a)) sync_abort c;

  -- This assertion should also hold, but it does not
  -- GHDL seems to implement abort as sync_abort instead of async_abort
  -- See 1850-2010 6.2.1.5.1 abort, async_abort, and sync_abort
  WITH_async_ABORT_a : assert (always a -> next (b before a)) async_abort d;

  --  According to IEEE 1850-2010 6.2.1.5.1, abort is identical to async_abort
  WITH_ABORT_a : assert (always a -> next (b before a)) abort d;

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

  -- stop simulation after 10 cycles
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
