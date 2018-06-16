library ieee;
use ieee.std_logic_1164.all  ;
entity foo is
  
  port (
    addr : in  std_logic_vector(3 downto 0);
    data_in  : out std_logic_vector(31 downto 0);
  data_out  : out std_logic_vector(31 downto 0)
  );

end entity foo;
architecture simple of foo is
  type mem_type is array (5 downto 0) of std_logic_vector(31 downto 0);
  signal mem : mem_type  ;

begin  -- architecture simple

  foop: process (all) is
  begin  -- process foop
    mem[address]<=data_in;
    -- The above code is erronous but instead of flaging an error the compiler
    -- crashes
    -- mem(address)<=data_in;
  end process foop;
end architecture simple;
