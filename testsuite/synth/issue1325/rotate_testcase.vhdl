library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity rotate_testcase is
    Port (in_vec:  in  UNSIGNED(31 downto 0);
          out_vecl: out UNSIGNED(31 downto 0);
          out_vecr: out UNSIGNED(31 downto 0));
end entity;

architecture RTL of rotate_testcase is
begin
    out_vecl <= rotate_left(in_vec,1);
    out_vecr <= rotate_right(in_vec,1);
end RTL;
