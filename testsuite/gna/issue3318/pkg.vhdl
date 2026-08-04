package expression_unary_logical_pkg is

    constant C_AND  : bit := and b"101";
    constant C_NAND : bit := nand b"101";
    constant C_OR   : bit := or b"101";
    constant C_NOR  : bit := nor b"101";
    constant C_XOR  : bit := xor b"101";
    constant C_XNOR : bit := xnor b"101";

end package;
