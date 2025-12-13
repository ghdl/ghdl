library ieee;
use ieee.std_logic_1164.all;

package pkg is

constant AHBDW : integer := 128;

type ahb_partial_t is record
  hwdata : std_logic_vector(AHBDW-1 downto 0);
  hready : std_logic;
end record;

type ahb_partial_vec_t is array (0 to 1) of ahb_partial_t;

component Design_Entity is
    port (
        do : in  std_logic := '0';
        b  : out std_logic
    );
end component Design_Entity;
end package pkg;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

entity Design_Entity is
    port (
        do : in  std_logic;
        -- do : in  std_logic := '0';
        b : out std_logic
    );
end entity Design_Entity;

architecture structural of Design_Entity is
    signal ahb_partial : ahb_partial_vec_t;
begin

process (all)
    variable res : std_logic := '0';
begin
    for s in 0 to ahb_partial_vec_t'high loop
        -- Some dummy operations
        for i in 0 to ahb_partial(s).hwdata'high loop
            ahb_partial(s).hwdata(i) <= do;
            b <= b xor ahb_partial(s).hwdata(i);
        end loop;

    ahb_partial(s).hready <= '1';
    end loop;

end process;

end architecture structural;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

entity top is
   port(a   : in std_logic;
        res : out std_logic_vector(127 downto 0);
        b   : out std_logic);
end entity;

architecture topa of top is
begin
   process (all)
      alias  cpusi is << signal DE.ahb_partial : ahb_partial_vec_t>>;
      variable ahb_intermediate : ahb_partial_t;
   begin
      for s in 0 to ahb_partial_vec_t'high loop
         if s = 0 then
            if cpusi(s).hready then
               ahb_intermediate := cpusi(s);
            else
               ahb_intermediate.hwdata := (others => '0');
               ahb_intermediate.hready := cpusi(s).hready;
            end if;
         elsif s = 1 then
            if not cpusi(s).hready then
               ahb_intermediate := cpusi(s);
            else
               ahb_intermediate.hwdata := cpusi(s).hwdata;
               ahb_intermediate.hready := a;
            end if;
         else
            assert false;
         end if;
         res              <= ahb_intermediate.hwdata;
      end loop;
   end process;
   DE : Design_Entity port map(b => b);


end architecture;
