-- dual_port_ram.vhd
-- This file is part of bladeRF-wiphy.
--
-- Copyright (C) 2021 Nuand, LLC.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

entity dual_port_ram is
   generic(
      ADDR_BITS  :  in  natural := 6;
      DATA_BITS  :  in  natural := 32
   );
   port(
      clock      :  in  std_logic;
      reset      :  in  std_logic;

      acc        :  in  std_logic;
      solo       :  in  std_logic;
      write      :  in  std_logic;

      addr_a     :  in  std_logic_vector(ADDR_BITS-1 downto 0);
      in_a       :  in  std_logic_vector(DATA_BITS-1 downto 0);
      data_a     :  out std_logic_vector(DATA_BITS-1 downto 0);

      addr_b     :  in  std_logic_vector(ADDR_BITS-1 downto 0);
      in_b       :  in  std_logic_vector(DATA_BITS-1 downto 0);
      data_b     :  out std_logic_vector(DATA_BITS-1 downto 0)
   );
end entity;

architecture arch of dual_port_ram is
   type ram_t is array(natural range <>) of std_logic_vector(DATA_BITS-1 downto 0);

   signal ram : ram_t((2**ADDR_BITS-1) downto 0);
begin
   sync : process(clock, reset)
      variable add_a, add_b : integer;
   begin
      if (reset = '1') then
         for i in ram'range loop
            ram(i) <= ( others => '0' );
         end loop;
      elsif (rising_edge(clock)) then
         if (acc = '1') then
            add_a := to_integer(unsigned(addr_a));
            add_b := to_integer(unsigned(addr_b));

            if (write = '1') then
               ram(add_a) <= in_a;
               data_a <= in_a;
               if (solo = '0') then
                  ram(add_b) <= in_b;
                  data_b <= in_b;
               else
                  data_b <= ( others => '0' );
               end if;
            else
               data_a <= ram(add_a);
               if (solo = '0') then
                  data_b <= ram(add_b);
               else
                  data_b <= ( others => '0' );
               end if;
            end if;
         end if;

      end if;
   end process;
end architecture;

architecture synth of dual_port_ram is
   type ram_t is array(natural range <>) of std_logic_vector(DATA_BITS-1 downto 0);
   signal ram : ram_t((2**ADDR_BITS-1) downto 0);
begin
   sync : process(clock, reset)
      variable addra : integer;
      variable addrb : integer;
   begin
      if (rising_edge(clock)) then
         addra := to_integer(unsigned(addr_a));
         addrb := to_integer(unsigned(addr_b));
         if (write = '1') then
            ram(addra) <= in_a;
            ram(addrb) <= in_a;
         end if;
         data_a <= ram(addra);
         data_b <= ram(addrb);
      end if;
   end process;

end architecture synth;
