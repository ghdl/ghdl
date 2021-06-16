library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity imem2 is
  generic (
    IMEM_BASE : std_ulogic_vector(31 downto 0) := x"00000000"
  );
  port (
    clk_i  : in  std_ulogic;
    rden_i : in  std_ulogic;
    wren_i : in  std_ulogic;
    ben_i  : in  std_ulogic_vector(03 downto 0);
    addr_i : in  std_ulogic_vector(31 downto 0);
    data_i : in  std_ulogic_vector(31 downto 0);
    data_o : out std_ulogic_vector(31 downto 0);
    ack_o  : out std_ulogic
  );
end entity;

architecture ok of imem2 is
  signal addr : std_ulogic_vector(15 downto 0);
  type ram_t is array(0 to 2**15-1) of std_ulogic_vector(31 downto 0);
  signal acc_en : std_ulogic;
  constant abb_c : std_logic_vector(31 downto 16) := (others=>'0');
begin
  addr <= addr_i(addr'left+2 downto 2); -- word aligned
  acc_en <= addr_i(abb_c'range) ?= IMEM_BASE(abb_c'range);
  process(clk_i)
  variable memory : ram_t;
  begin
  if rising_edge(clk_i) then
    if acc_en then
      ack_o <= rden_i or wren_i;
    end if;
    if acc_en and wren_i then
      for x in 0 to 3 loop
        if ben_i(x) then
          memory(to_integer(unsigned(addr)))((x+1)*8-1 downto x*8) := data_i((x+1)*8-1 downto x*8);
        end if;
      end loop;
    end if;
    if acc_en and rden_i then
      data_o <= memory(to_integer(unsigned(addr)));
    end if;
  end if;
  end process;
end architecture;

architecture notok of imem2 is
  signal addr : std_ulogic_vector(7 downto 0);
  type ram_t is array(0 to 2**8-1) of std_ulogic_vector(31 downto 0);
  signal acc_en : std_ulogic;
  constant abb_c : std_logic_vector(31 downto 16) := (others=>'0');
begin
  addr <= addr_i(addr'left+2 downto 2); -- word aligned
  acc_en <= addr_i(abb_c'range) ?= IMEM_BASE(abb_c'range);
  process(clk_i)
  variable memory : ram_t;
  begin
  if rising_edge(clk_i) and acc_en='1' then
    ack_o <= rden_i or wren_i;
    if wren_i then
      for x in 0 to 3 loop
        if ben_i(x) then
          memory(to_integer(unsigned(addr)))((x+1)*8-1 downto x*8) := data_i((x+1)*8-1 downto x*8);
        end if;
      end loop;
    end if;
    if rden_i then
      data_o <= memory(to_integer(unsigned(addr)));
    end if;
  end if;
  end process;
end architecture;

architecture neitherok of imem2 is
    signal addr : std_ulogic_vector(15 downto 0);
    type ram_t is array(0 to 2**15-1) of std_ulogic_vector(31 downto 0);
    signal acc_en : std_ulogic;
    constant abb_c : std_logic_vector(31 downto 16) := (others=>'0');
  begin
    addr <= addr_i(addr'left+2 downto 2); -- word aligned
    acc_en <= addr_i(abb_c'range) ?= IMEM_BASE(abb_c'range);
    process(clk_i)
    variable memory : ram_t;
    begin
    if rising_edge(clk_i) then
      if acc_en then
        ack_o <= rden_i or wren_i;
        if wren_i then
          for x in 0 to 3 loop
            if ben_i(x) then
              memory(to_integer(unsigned(addr)))((x+1)*8-1 downto x*8) := data_i((x+1)*8-1 downto x*8);
            end if;
          end loop;
        end if;
        if rden_i then
          data_o <= memory(to_integer(unsigned(addr)));
        end if;
      end if;
    end if;
    end process;
  end architecture;
