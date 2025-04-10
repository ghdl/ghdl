library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test1 is
end entity;

architecture rtl of test1 is
  signal sig  : std_logic_vector(21 downto 0) := (others => '0');
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture rtl of test is
begin
  i_test1 : entity work.test1;
  p_proc : process
    function get_coded_width(
      constant data_width : natural
    ) return natural
    is
    begin
      case data_width is
        when 1        => return data_width + 1 + 1;
        when 2 to 4   => return data_width + 3 + 1;
        when 5 to 11  => return data_width + 4 + 1;
        when 12 to 26 => return data_width + 5 + 1;
        when 27 to 57 => return data_width + 6 + 1;
        when others =>
          return 0;
      end case;
    end function get_coded_width;
    
    constant C_DATA_WIDTH  : natural := 16;
    alias sig1 is <<signal i_test1.sig : std_logic_vector(21 downto 0)>>;
    alias sig2 is <<signal i_test1.sig : std_logic_vector(get_coded_width(C_DATA_WIDTH)-1 downto 0)>>;
    
    procedure force_edac_error(
    signal   ram_coded : inout std_logic_vector) is
      constant C_ERR_BIT_1 : natural := ram_coded'length - 4;
    begin 
      ram_coded(C_ERR_BIT_1) <= force not ram_coded(C_ERR_BIT_1);
    end procedure;
  begin
      force_edac_error(sig1);
      report "OK 1";
      force_edac_error(sig2);
      report "OK 2";
    wait;
  end process;
end architecture;
