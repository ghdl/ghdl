library ieee;
context ieee.ieee_std_context;
use work.uCPUtypes.all;

entity top is
  port (
    rst, clk : in logic;
    display : out unsigned_byte;
    stb : out logic
  );
end entity top;

architecture mixed of top is
  signal rom_addr, ram_addr, ram_data : unsigned_byte;
  signal rom_data : code_word;
  signal wr_en : logic;
begin

stb <= '1' when wr_en = '1' and ram_addr = x"00" else '0';

display <= ram_data;

CPU_instance: entity work.uCPU(RTL) port map (rst, clk, rom_addr, rom_data, ram_addr, ram_data, wr_en);

ROM_instance: entity work.ROM(RTL) port map (rom_addr, rom_data, '1');

RAM_instance: entity work.RAM(RTL) port map (clk, ram_addr, ram_data, wr_en);

end architecture mixed;
