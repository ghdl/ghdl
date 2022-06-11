library ieee;
use ieee.std_logic_1164.all;

entity repro4 is
  port (
    rst  : std_logic;
    clk  : std_logic;
    de   : std_logic;
    vs_o : out std_logic);
end;

architecture synth of repro4 is
    type mem_t is array(0 to 15) of std_logic;

    signal mem : mem_t;
    signal addr : integer range mem_t'range;
begin
    process(rst, clk)
    begin
      if rst = '1' then
        addr    <= 0;
      elsif rising_edge(clk) then
        vs_o <= mem(addr);
        mem(addr) <= de;
        addr <= addr+1;
      end if;
    end process;
end;
