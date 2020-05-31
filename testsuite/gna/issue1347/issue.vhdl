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


library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;


entity issue is
  port (
    clk : in std_logic
  );
end entity issue;


architecture psl of issue is

  component sequencer is
    generic (
      seq : string
    );
    port (
      clk  : in  std_logic;
      data : out std_logic
    );
  end component sequencer;

  signal a, b : std_logic;

begin


  --                              0123456789012345
  SEQ_A : sequencer generic map ("__-__-____-_____") port map (clk, a);
  SEQ_B : sequencer generic map ("_______-________") port map (clk, b);


  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- This assertion should fail
  EVENTUALLY_a : assert always (a -> eventually! b);


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
    variable index : natural := 29;
  begin
    loop
      wait until rising_edge(clk);
      index := index - 1;
      exit when index = 0;
    end loop;
    -- stop(0);
    finish(0);
  end process;


end architecture sim;
