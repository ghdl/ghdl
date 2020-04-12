library ieee;
use ieee.std_logic_1164.all;

entity repro1 is
	port (
		Clock   : in std_ulogic;
		Reset   : in std_ulogic;
		Sig_in  : in std_ulogic;
		Sig_out : out std_ulogic
	);
end entity;

architecture rtl of repro1 is
begin
  reg: process(Clock, Reset)
    variable pl_regs : std_ulogic;
  begin
    if Reset = '1' then
      pl_regs := '0';
    elsif rising_edge(Clock) then
      pl_regs := Sig_in;
    end if;

    Sig_out <= pl_regs;
  end process;
end architecture;
