library ieee;
use ieee.std_logic_1164.all;

entity A2 is
  port (
    clk : in std_logic;
    b : out std_logic_vector(79 downto 0)
  );
end A2;

architecture rtl of A2 is
begin

  A_PROC : process(clk)

    procedure p(c : std_logic_vector) is
    begin
      b <= c;
    end procedure;

  begin
    if rising_edge(clk) then
            p(c => x"0B0E0000000000000000"); 
    end if;
  end process;

end architecture;
