library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;


entity test is
port (
    i_clk : in    std_logic;
    i_data: in std_logic_vector(10 downto 0);
    o_data: out unsigned(10 downto 0)
);
end entity test;

architecture rtl of test is

    function convert_bcd (
        bcd : std_logic_vector
      )
      return unsigned is
    
        variable v_ret_val : integer;
    
      begin
    
        case bcd'length is
    
          when 1 to 3 =>
    
            v_ret_val := to_integer(unsigned(bcd(bcd'high downto 0)));
    
          when 4 to 7 =>
    
            v_ret_val := to_integer(unsigned(bcd(3 downto 0)))
                         + to_integer(unsigned(bcd(bcd'high downto 4))) * 10;
    
          when 8 to 9 =>
    
            v_ret_val := to_integer(unsigned(bcd(3 downto 0)))
                         + to_integer(unsigned(bcd(7 downto 4))) * 10
                         + to_integer(unsigned(bcd(bcd'high downto 8))) * 100;

          --when others => -- Uncomment to make functional
            --v_ret_val := 0;
        end case;
    
        return to_unsigned(v_ret_val, bcd'length);
    
      end function;

  begin

    process (i_clk) is
    begin
        o_data <= convert_bcd(i_data);
    end process;

end architecture;
