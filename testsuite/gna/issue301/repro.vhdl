package pkgc is
  constant width : natural;
end pkgc;

package body pkgc is
  constant width : natural := 4;
end pkgc;

use work.pkgc.all;
package pkgcomp is
  component comp is
    generic (val : bit_vector (width -1 downto 0));
    port (b : out bit);
  end component;
end pkgcomp;

use work.pkgc.all;
entity comp is
  generic (val : bit_vector (width -1 downto 0));
  port (b : out bit);
end comp;

architecture behav of comp is
begin
  b <= val (val'left);
end behav;

entity repro is
end repro;

use work.pkgc.all;
use work.pkgcomp.all;

architecture behav of repro is
  signal res : bit;
begin
  inst : comp
    generic map (val => "0010")
    port map (b => res );
end behav;
