library ieee;
  use ieee.std_logic_1164.all;

entity hex_sequencer is
  generic (
    seq : string
  );
  port (
    clk  : in  std_logic;
    data : out std_logic_vector(3 downto 0)
  );
end entity hex_sequencer;

architecture rtl of hex_sequencer is

  signal index : natural := seq'low;

  function to_hex (a : in character) return std_logic_vector is
    variable ret : std_logic_vector(3 downto 0);
  begin
    case a is
      when '0' | '_' => ret := x"0";
      when '1'       => ret := x"1";
      when '2'       => ret := x"2";
      when '3'       => ret := x"3";
      when '4'       => ret := x"4";
      when '5'       => ret := x"5";
      when '6'       => ret := x"6";
      when '7'       => ret := x"7";
      when '8'       => ret := x"8";
      when '9'       => ret := x"9";
      when 'a' | 'A' => ret := x"A";
      when 'b' | 'B' => ret := x"B";
      when 'c' | 'C' => ret := x"C";
      when 'd' | 'D' => ret := x"D";
      when 'e' | 'E' => ret := x"E";
      when 'f' | 'F' | '-' => ret := x"F";
      when others => ret := x"X";
    end case;
    return ret;
  end function to_hex;

begin

  process (clk) is
  begin
    if rising_edge(clk) then
      if (index < seq'high) then
        index <= index + 1;
      end if;
    end if;
  end process;

  data <= to_hex(seq(index));

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

  signal a : std_logic_vector(3 downto 0);

begin

  SEQ_C : entity work.hex_sequencer generic map ("0123456789ABCDEF") port map (clk, a);

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
    stop(0);
  end process;

end architecture sim;


vunit issue_1850_vu (issue(psl)) {

  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- A simple check for counter increasing
  -- Nested generate leads to a crash
  test : if true generate
    counter_check : for i in 0 to 14 generate
      SERE_4_a : assert always
        {a = std_logic_vector(to_unsigned(i, 4))}
        |=>
        {a = std_logic_vector(to_unsigned(i + 1, 4))};
    end generate counter_check;
  end generate test;

}
