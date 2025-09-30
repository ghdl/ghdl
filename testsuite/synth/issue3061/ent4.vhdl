library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

package rec_pkg is
  type t_rec1 is record
    a1: std_logic;
    b1: std_logic_vector(3 downto 0);
  end record;

  type t_rec2 is record
    a2: std_logic_vector(1 downto 0);
    b2: std_logic_vector(3 downto 0);
  end record;
end rec_pkg;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.rec_pkg.all;

entity ent is
	port (sig1 : out t_rec1;
              sig2 : in t_rec2;
              sig3 : out std_logic);

end entity;

architecture a of ent is
begin
	sig3 <= sig2.a2(0);
        sig1.b1 <= sig2.b2;
        sig1.a1 <= sig2.a2(1);
end;
