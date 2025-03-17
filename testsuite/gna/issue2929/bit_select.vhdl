library ieee;
use ieee.std_logic_1164.all;

entity bit_select_93vs08_sim is
  port (clk : out std_logic);
end bit_select_93vs08_sim;

architecture behaviour of bit_select_93vs08_sim
  is
  constant CONS1 : std_logic_vector(3 downto 0) := "0111"; -- strangely for 3 bit vectors (2 downto 0) works OK in VHDL2008.
  constant CONS2 : std_logic_vector(3 downto 0) := "0101";
begin

  bit_extract_process : process
    variable r : boolean;
  begin
    r := (CONS1 = CONS2);
    report "bool = " & boolean'image(r); -- should be always: bool = false
    assert r = false severity error;
    -- above works always OK

    r := (CONS1(1 downto 0) = CONS2(1 downto 0));
    report "bool = " & boolean'image(r); -- should be always: bool = false
    assert r = false severity error;
    
    -- above fails for vhdl 2008
    -- for --std=93 bool = false
    -- for --std=08 bool = true
    wait for 10 ns;
    wait;
  end process;

end behaviour;
