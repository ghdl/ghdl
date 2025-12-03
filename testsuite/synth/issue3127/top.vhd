library ieee;
use ieee.std_logic_1164.all;
library gaisler;
use gaisler.pkg.all;

entity top is
   port(a   : in std_logic;
        res : out std_logic_vector(127 downto 0);
        b   : out std_logic);
end entity;

architecture topa of top is
begin
   DE : Design_Entity port map(b => b);

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

end architecture;
