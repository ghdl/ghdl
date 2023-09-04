package myPackageX is
end package myPackageX;

use work.myPackageX;

package mySecondPackage is
    alias myPackage is myPackageX;
end package mySecondPackage;
