library ieee;
context ieee.ieee_std_context;
use work.uCPUtypes.all;

entity uCPU is
  port (
    rst, clk : in    logic;
    rom_addr : out   unsigned_byte;
    rom_data : in    code_word;
    ram_addr : out   unsigned_byte;
    ram_data : inout unsigned_byte;
    wr_en    : out   logic
  );
end entity uCPU;

architecture RTL of uCPU is
  signal IR : unsigned (3 downto 0);
  signal A, PC : unsigned_byte := x"00";
begin

rom_addr <= PC;

ram_addr <= A;

ram_data <= x"ZZ";

wr_en <= '0';

process (clk) is
begin
  if rising_edge(clk) then
    if rst then
      PC <= x"00";
      A  <= x"00";
    else
      IR <= rom_data(11 downto 8);
      A <= rom_data(7 downto 0);
      PC <= PC + 1;
    end if;
  end if;
end process;

end architecture RTL;
