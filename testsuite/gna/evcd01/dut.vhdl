library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--  Unit exercising in / out / inout ports, scalar and vector, for the
--  extended-VCD (--evcd) writer.
entity dut is
  port (
    clk     : in    std_logic;
    rst     : in    std_logic;
    data_in : in    std_logic_vector (3 downto 0);
    count   : out   std_logic_vector (3 downto 0);
    ready   : out   std_logic;
    bus_io  : inout std_logic
  );
end dut;

architecture rtl of dut is
  signal cnt : unsigned (3 downto 0) := (others => '0');
begin
  process (clk, rst) is
  begin
    if rst = '1' then
      cnt <= (others => '0');
    elsif rising_edge (clk) then
      cnt <= cnt + unsigned (data_in);
    end if;
  end process;

  count <= std_logic_vector (cnt);
  ready <= '1' when cnt /= 0 else '0';

  --  Drive the inout while running, release it (high-Z) during reset.
  bus_io <= cnt (0) when rst = '0' else 'Z';
end rtl;
