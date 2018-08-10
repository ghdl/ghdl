library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.NUMERIC_STD.ALL;

library work;
use work.wishbone_pkg.all;

entity wb_demux is
    Port (
    	wbs_i	: in t_wishbone_slave_in
    );
end wb_demux;
architecture full_regs of wb_demux is
begin


end full_regs;
