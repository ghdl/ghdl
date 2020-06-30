library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity tb_issue is
end entity tb_issue;

architecture dataflow of tb_issue is

  type arec is record
    member : std_logic_vector(31 downto 0);
  end record arec;

  signal a : arec;

begin

  process
  begin
    a.member <= (0 => '1', others => '0');
  end process;

end architecture dataflow;
