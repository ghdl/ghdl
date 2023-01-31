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

entity issue_err1 is
  port (
    clk : in std_logic
  );
end entity;

architecture psl of issue_err1 is

  signal a, b, c, d, e, f : std_logic;

begin


  --                                          012345678901
  SEQ_A : entity work.sequencer generic map ("__-_________") port map (clk, a);

  -- Next 3 sequences should hold with next[3:5]
  --                                          012345678901
  SEQ_B : entity work.sequencer generic map ("_____-______") port map (clk, b);
  SEQ_C : entity work.sequencer generic map ("______-_____") port map (clk, c);
  SEQ_D : entity work.sequencer generic map ("_______-____") port map (clk, d);

  -- Next two sequences should not hold with next[3:5]
  --                                          012345678901
  SEQ_E : entity work.sequencer generic map ("____-_______") port map (clk, e);
  SEQ_F : entity work.sequencer generic map ("________-___") port map (clk, f);


  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

   -- This assertion should hold, but doesn't (GHDL BUG)
  NEXT_0_a : assert always (a -> next_e[3 to 5] (b))
    report "NEXT_0_a failed";

  -- This assertion should hold, but doesn't (GHDL BUG)
  NEXT_1_a : assert always (a -> next_e[3 to 5] (c))
    report "NEXT_1_a failed";

  -- This assertion holds (CORRECT)
  NEXT_2_a : assert always (a -> next_e[3 to 5] (d))
    report "NEXT_2_a failed";

  -- This assertion doesn't hold (CORRECT)
  NEXT_3_a : assert always (a -> next_e[3 to 5] (e))
    report "NEXT_3_a failed";

  -- This assertion doesn't hold (CORRECT)
--  NEXT_4_a : assert always (a -> next_e[3 to 5] (f))
--    report "NEXT_4_a failed";

end architecture psl;

library ieee;
  use ieee.std_logic_1164.all;

use std.env.all;


entity test_issue_err1 is
end entity test_issue_err1;


architecture sim of test_issue_err1 is

  signal clk   : std_logic := '1';

begin


  clk <= not clk after 500 ps;

  DUT : entity work.issue_err1(psl) port map (clk);

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
