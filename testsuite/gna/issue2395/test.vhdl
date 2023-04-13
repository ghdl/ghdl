library ieee ;
    use ieee.std_logic_1164.all ;

package axi4s is

    type axis_t is record
        data    :   std_ulogic_vector ;
        dest    :   std_ulogic_vector ;
        id      :   std_ulogic_vector ;
        strb    :   std_ulogic_vector ;
        keep    :   std_ulogic_vector ;
        user    :   std_ulogic_vector ;
        last    :   std_ulogic ;
        valid   :   std_ulogic ;
        ready   :   std_ulogic ;
    end record ;

    type axis_array_t is array(natural range <>) of axis_t ;

    package make is
      generic (
        DATA_BYTES  :   positive    := 4 ;
        DEST_WIDTH  :   natural     := 0 ;
        ID_WIDTH    :   natural     := 0 ;
        USER_WIDTH  :   natural     := 0
      ) ;

        subtype DATA_RANGE is natural range DATA_BYTES*8-1 downto 0 ;
        subtype DEST_RANGE is natural range DEST_WIDTH-1 downto 0 ;
        subtype ID_RANGE   is natural range ID_WIDTH-1 downto 0 ;
        subtype KEEP_RANGE is natural range DATA_BYTES-1 downto 0 ;
        subtype USER_RANGE is natural range USER_WIDTH-1 downto 0 ;

        subtype axis_t is axi4s.axis_t(
            data(DATA_RANGE),
            dest(DEST_RANGE),
            id(ID_RANGE),
            keep(KEEP_RANGE),
            strb(KEEP_RANGE),
            user(USER_RANGE)
        ) ;

    end package ;

end package ;

package axis32 is new work.axi4s.make ;

entity test is
  port (
    clock   :   in  bit ;
    reset   :   in  bit ;
    rx      :   inout   work.axis32.axis_t ;
    tx      :   inout   work.axi4s.axis_t(data(31 downto 0), dest(-1 downto 0), id(-1 downto 0), keep(3 downto 0), strb(-1 downto 0), user(-1 downto 0))
  ) ;
end entity ;

architecture arch of test is

begin

    -- do nothing for now

end architecture ;
