library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro is
  port (
    clk_i  : std_logic;
    addr_i : std_logic_vector(3 downto 0);
    data_i : std_logic_vector(7 downto 0);
    data_o : out std_logic_vector(7 downto 0);
    wen_i   : std_logic);
end repro;

architecture behav of repro is
begin
  process (clk_i, addr_i)
    variable mem : std_logic_vector(0 to 8*16 - 1);
    variable addr : natural range 0 to 15;

    impure function rd(ad : natural) return std_logic_vector is
    begin
      return mem(ad*8 to ad*8 + 7);
    end rd;
  begin
    if rising_edge(clk_i) then
      addr := to_integer (unsigned (addr_i));
      data_o <= rd (addr);
      if wen_i = '1' then
        mem (addr*8 to addr*8 + 7) := data_i;
      end if;
    end if;
  end process;
end behav;
