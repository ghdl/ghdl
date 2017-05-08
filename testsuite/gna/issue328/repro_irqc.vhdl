entity irqc_tb is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Test case architecture
architecture func of irqc_tb is

  type t_sbi_if is record
    cs      : std_logic;          -- to dut
    addr    : unsigned;           -- to dut
    rena    : std_logic;          -- to dut
    wena    : std_logic;          -- to dut
    wdata   : std_logic_vector;   -- to dut
    ready   : std_logic;          -- from dut
    rdata   : std_logic_vector;   -- from dut
  end record;

  function init_sbi_if_signals(
    addr_width : natural;
    data_width : natural
    ) return t_sbi_if is
    variable result : t_sbi_if( addr(addr_width - 1 downto 0), 
                                wdata(data_width - 1 downto 0), 
                                rdata(data_width - 1 downto 0));
  begin
    result.cs     := '0';
    result.rena   := '0';
    result.wena   := '0';
    result.addr   := (others => '0');
    result.wdata  := (others => '0');
    result.ready  := 'Z';
    result.rdata  := (others => 'Z');
    return result;
  end function;

  signal sbi_if : t_sbi_if(addr(2 downto 0), wdata(7 downto 0), rdata(7 downto 0)) := init_sbi_if_signals(3, 8);

  procedure write (signal s : inout t_sbi_if) is
  begin
    s.cs <= '1';
  end write;
begin
  process
  begin
    write(sbi_if);
    wait for 1 ns;
    assert sbi_if.rdata = (7 downto 0 => 'Z');
    assert sbi_if.addr = (2 downto 0 => '0');
    assert sbi_if.wdata = (7 downto 0 => '0');
    wait;
  end process;

  sbi_if.rdata <= (others => '0');
end func;
