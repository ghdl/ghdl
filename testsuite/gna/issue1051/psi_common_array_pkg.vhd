------------------------------------------------------------------------------
--  Copyright (c) 2018 by Paul Scherrer Institute, Switzerland
--  All rights reserved.
--  Authors: Waldemar Koprek, Oliver Bruendler
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Libraries
------------------------------------------------------------------------------
library ieee ;
	use ieee.std_logic_1164.all;

------------------------------------------------------------------------------
-- Package Header
------------------------------------------------------------------------------
package psi_common_array_pkg is
  type t_aslv2    is array (natural range <>) of std_logic_vector( 1 downto 0);
  type t_aslv3    is array (natural range <>) of std_logic_vector( 2 downto 0);
  type t_aslv4    is array (natural range <>) of std_logic_vector( 3 downto 0);
  type t_aslv5    is array (natural range <>) of std_logic_vector( 4 downto 0);
  type t_aslv6    is array (natural range <>) of std_logic_vector( 5 downto 0);
  type t_aslv7    is array (natural range <>) of std_logic_vector( 6 downto 0);
  type t_aslv8    is array (natural range <>) of std_logic_vector( 7 downto 0);
  type t_aslv9    is array (natural range <>) of std_logic_vector( 8 downto 0);
  type t_aslv10   is array (natural range <>) of std_logic_vector( 9 downto 0);
  type t_aslv11   is array (natural range <>) of std_logic_vector(10 downto 0);
  type t_aslv12   is array (natural range <>) of std_logic_vector(11 downto 0);
  type t_aslv13   is array (natural range <>) of std_logic_vector(12 downto 0);
  type t_aslv14   is array (natural range <>) of std_logic_vector(13 downto 0);
  type t_aslv15   is array (natural range <>) of std_logic_vector(14 downto 0);
  type t_aslv16   is array (natural range <>) of std_logic_vector(15 downto 0);
  type t_aslv17   is array (natural range <>) of std_logic_vector(16 downto 0);
  type t_aslv18   is array (natural range <>) of std_logic_vector(17 downto 0);
  type t_aslv19   is array (natural range <>) of std_logic_vector(18 downto 0);
  type t_aslv20   is array (natural range <>) of std_logic_vector(19 downto 0);
  type t_aslv21   is array (natural range <>) of std_logic_vector(20 downto 0);
  type t_aslv22   is array (natural range <>) of std_logic_vector(21 downto 0);
  type t_aslv23   is array (natural range <>) of std_logic_vector(22 downto 0);
  type t_aslv24   is array (natural range <>) of std_logic_vector(23 downto 0);
  type t_aslv25   is array (natural range <>) of std_logic_vector(24 downto 0);
  type t_aslv26   is array (natural range <>) of std_logic_vector(25 downto 0);
  type t_aslv27   is array (natural range <>) of std_logic_vector(26 downto 0);
  type t_aslv28   is array (natural range <>) of std_logic_vector(27 downto 0);
  type t_aslv29   is array (natural range <>) of std_logic_vector(28 downto 0);
  type t_aslv30   is array (natural range <>) of std_logic_vector(29 downto 0);
  type t_aslv32   is array (natural range <>) of std_logic_vector(31 downto 0);
  type t_aslv36   is array (natural range <>) of std_logic_vector(35 downto 0);
  type t_aslv48   is array (natural range <>) of std_logic_vector(47 downto 0);
  type t_aslv64   is array (natural range <>) of std_logic_vector(63 downto 0);
  
  type t_ainteger is array (natural range <>) of integer;
  type t_areal is array (natural range <>) of real;
  type t_abool is array (natural range <>) of boolean;

end psi_common_array_pkg;	 
