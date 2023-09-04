package myPackageX2 is
  constant c : string := ('a', 'b');
end;

use work.myPackageX2;

package mySecondPackage2 is
  alias myPackage is myPackageX2;

  constant e : string := myPackage.c;
end;
