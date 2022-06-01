library IEEE;use IEEE.numeric_std.all;

entity tb is
end;

architecture behavioral of tb is
  subtype int31 is integer range-0*(0)to 2**(31);
begin
  process
    variable tmp:int31;
  begin
    tmp:=0;
  end process;
end behavioral;
