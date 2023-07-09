-- Package A
use work.bPkg.all;

package aPkg is
end package aPkg;

-- Package B
use work.aPkg.all;

package bPkg is
end package bPkg;

-- Entity
use work.aPkg.all;

entity ent is
end entity ent;

architecture behaviour of ent is
begin
end architecture behaviour;
