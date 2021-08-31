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

use std.env.all;

entity tb_checker is
end;

architecture sim of tb_checker is
  signal clk   : std_logic := '1';
  signal a, b, c, d : std_logic;
begin
  clk <= not clk after 500 ps;

  -- async reset 100 ps after rising edge in cycle 1
  d <= '0', '1' after 1100 ps, '0' after 1400 ps;

  --                              0123456789
  SEQ_A : entity work.sequencer generic map ("-___-_____") port map (clk, a);
  SEQ_B : entity work.sequencer generic map ("_______-__") port map (clk, b);
  SEQ_C : entity work.sequencer generic map ("-_________") port map (clk, c);
  --  D :                                     _|________

  dut: entity work.checker port map
    (clk => clk, a => a, b => b, c => c, d => d);

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
