library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro1 is
  port(mask    : in  std_logic_vector(1 to 3);
       vaddr   : in  std_logic_vector(41 downto 12);
       result  : out std_logic_vector(8 downto 0)
      );
end;

architecture rtl of repro1 is
begin
  process(vaddr, mask)
    variable pos    : integer;
  begin
    if mask = "000" then
      pos := 21;
    elsif mask = "010" then
      pos := 39;
    else
      pos := 30;
    end if;
    --  report "pos=" & integer'image(pos);

    if pos = 39 then
      result <= vaddr(pos - 1 downto pos - 9);
    else
      result <= vaddr(pos - 1 downto pos - 9);
    end if;
  end process;
end architecture;

