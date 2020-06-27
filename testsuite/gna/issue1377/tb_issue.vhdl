library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity tb_issue is
end entity tb_issue;

architecture dataflow of tb_issue is

    type ia is
        array (integer range <>) of std_logic_vector(0 downto 0);

    signal ip : ia(0 to 0) := (others => (others => '0'));

    signal ins : std_logic_vector(1*1-1 downto 0) := (others => '0');

begin

    lbl : for i in 0 to 0 generate
      ins((i+1)*1-1 downto i*1) <= ip(i);
      --ins((0+1)*1-1 downto 0*1) <= ip(0); -- works                            
    end generate;

end architecture dataflow;
