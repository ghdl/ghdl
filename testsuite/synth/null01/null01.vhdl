library ieee;
use ieee.std_logic_1164.all;

entity null01 is
    port (sel    : std_ulogic_vector(7 downto 0);
          d_in   : std_ulogic_vector(63 downto 0);
          d_out  : out std_logic_vector(63 downto 0));
end;

architecture rtl of null01 is
    subtype idx_t is integer range 0 to 0;

    type reg_t is record
      idx          : idx_t;
      dummy : std_logic;
    end record;
    signal r : reg_t;

    type mem_t is array(idx_t) of std_ulogic_vector(63 downto 0);
    signal mem   : mem_t;
begin
    process(all)
        variable data_out : std_ulogic_vector(63 downto 0);
        variable j        : integer;
    begin
        data_out := mem(r.idx);
        for i in 0 to 7 loop
            j := i * 8;
            if sel(i) = '1' then
                data_out(j + 7 downto j) := d_in(j + 7 downto j);
            end if;
        end loop;

	d_out <= data_out;
    end process;
end;
