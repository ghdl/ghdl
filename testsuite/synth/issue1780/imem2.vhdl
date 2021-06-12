library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity imem2 is
  port (
    clk_i  : in  std_ulogic;
    rden_i : in  std_ulogic;
    wren_i : in  std_ulogic;
    ben_i  : in  std_ulogic_vector(03 downto 0);
    addr_i : in  std_ulogic_vector(31 downto 0);
    data_i : in  std_ulogic_vector(31 downto 0);
    data_o : out std_ulogic_vector(31 downto 0)
  );
end entity;

architecture imem_rtl of imem2 is

  signal addr : std_ulogic_vector(7 downto 0);

  type ram_t is array(0 to 2**8-1) of std_ulogic_vector(31 downto 0);

begin

  addr <= addr_i(addr'left+2 downto 2); -- word aligned

  verbose: block
  begin

    process(clk_i)
      variable memory : ram_t;
    begin
      if rising_edge(clk_i) then
        if wren_i then
          for x in 0 to 3 loop
            if ben_i(x) then
              memory(to_integer(unsigned(addr)))((x+1)*8-1 downto x*8) := data_i((x+1)*8-1 downto x*8);
            end if;
          end loop;
        end if;
      end if;
    end process;

    process(clk_i)
      variable memory : ram_t;
    begin
      if rising_edge(clk_i) then
        if rden_i then
          data_o <= memory(to_integer(unsigned(addr)));
        end if;
      end if;
    end process;
  end block;

end architecture;
