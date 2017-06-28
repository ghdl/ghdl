library ieee ;
    use ieee.std_logic_1164.all ;

library std;
    use std.textio.all;


-- Utility package
package util is

    procedure nop( signal clock : in std_logic ; count : in natural ) ;

end package ;

package body util is

    procedure nop( signal clock : in std_logic ; count : in natural ) is
    begin
        for i in 1 to count loop
            wait until rising_edge( clock ) ;
        end loop ;
    end procedure ;

end package body ;

library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.numeric_std.all;
library std;
    use std.textio.all;


entity data_saver is
    generic(
        FILENAME : string := "file.dat";
        DATA_WIDTH : natural := 16
    );
    port(
        reset   : in std_logic;
        clock   : in std_logic;
        data    : std_logic_vector(DATA_WIDTH-1 downto 0);
        data_valid : std_logic
    );
end entity;


architecture arch of data_saver is
begin

    handler : process
        FILE fp : text;
        variable line_data : line;
    begin
        --
        wait until falling_edge(reset);

            file_open(fp, FILENAME, WRITE_MODE);

            while (reset = '0') loop
                wait until rising_edge(data_valid);
                    write(line_data, data);
                    writeline(fp,line_data);
            end loop;
            file_close(fp);
    end process;
end architecture;


library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.numeric_std.all;
library std;
    use std.textio.all;


entity signed_saver is
    generic(
        FILENAME : string := "file.dat";
        DATA_WIDTH : natural := 16
    );
    port(
        reset   : in std_logic;
        clock   : in std_logic;
        data    : signed(DATA_WIDTH-1 downto 0);
        data_valid : std_logic
    );
end entity;


architecture arch of signed_saver is
begin

    handler : process
        FILE fp : text;
        variable line_data : line;
    begin
        --
        wait until falling_edge(reset);

            file_open(fp, FILENAME, WRITE_MODE);

            while (reset = '0') loop
                wait until rising_edge(clock);

                if data_valid = '1' then
                    write(line_data, (to_integer(data)));
                    writeline(fp,line_data);
                end if;
            end loop;
            file_close(fp);
    end process;
end architecture;



library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.numeric_std.all;
library std;
    use std.textio.all;


entity data_reader is
    generic(
        FILENAME : string := "file.dat";
        DATA_WIDTH : natural := 16
    );
    port(
        reset   : in std_logic;
        clock   : in std_logic;
        data_request : in std_logic;
        data    : out std_logic_vector(DATA_WIDTH-1 downto 0);
        data_valid : out std_logic
    );
end entity;


architecture arch of data_reader is

    type character_array_t is array (natural range <>) of character;
begin

    handler : process
        variable line_data : line;
        variable tmp : integer;
        variable c : character;--_array_t(0 to 3);

        type bin_t is file of character ;
        file fp : bin_t ;
        variable fs : file_open_status ;
    begin
        --
        data <= (others => '0');
        data_valid <= '0';
        wait until falling_edge(reset);

            file_open(fs, fp, FILENAME, READ_MODE);

            if( fs /= OPEN_OK ) then
                report "File open issues" severity failure ;
             end if ;

            --readline(fp,line_data);
            while (reset = '0') loop

                wait until rising_edge(clock);
                data_valid <= '0';

                if data_request = '1' then
                    read(fp, c);
                    tmp := integer(natural(character'pos(c)));
                    data(7 downto 0) <= std_logic_vector(to_unsigned(tmp,8));
                    read(fp, c);
                    tmp := integer(natural(character'pos(c)));
                    data(15 downto 8) <= std_logic_vector(to_unsigned(tmp,8));
                    read(fp, c);
                    tmp := integer(natural(character'pos(c)));
                    data(23 downto 16) <= std_logic_vector(to_unsigned(tmp,8));
                    read(fp, c);
                    tmp := integer(natural(character'pos(c)));
                    data(31 downto 24) <= std_logic_vector(to_unsigned(tmp,8));

                    data_valid <= '1';
                    wait until rising_edge(clock);
                    data_valid <= '0';
                end if;

            end loop;
            file_close(fp);
    end process;
end architecture;
