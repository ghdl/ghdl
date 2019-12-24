-- Enum literals should be made implicitly visible here
use work.pkg.alias_t;

package pkg2 is
  -- Fails with no declaration for "alpha"
  constant c : alias_t := alpha;
end package;
