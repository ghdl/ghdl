entity bug is end entity;
architecture arch of bug is
    component comp is port(a :in bit_vector); end component;
    constant DATAPATH :natural := 16;
    signal a :bit_vector(DATAPATH-1 downto 0);
begin
    i_comp: comp port map(a);
end architecture;

entity comp is port(a :in bit_vector); end entity;
architecture arch of comp is
    constant DATAPATH :natural := a'length;
    signal state :natural;
    signal tmp   :bit_vector(31 downto 0);
begin
    process(a) begin
        case DATAPATH is
        when 8=>
            case state is
                when 0=> tmp(1*DATAPATH-1 downto 0*DATAPATH)<=a;
                when 1=> tmp(2*DATAPATH-1 downto 1*DATAPATH)<=a;
-- When DATAPATH>10 this range violates bounds, but this code should not be reached because "case DATAPATH is when 8=>"
                when 2=> tmp(3*DATAPATH-1 downto 2*DATAPATH)<=a;
                when 3=> tmp(4*DATAPATH-1 downto 3*DATAPATH)<=a;
                when others=>
            end case;
        when 16=>
            case state is
                when 0=> tmp(1*DATAPATH-1 downto 0*DATAPATH)<=a;
                when 1=> tmp(2*DATAPATH-1 downto 1*DATAPATH)<=a;
                when others=>
            end case;
        when others=>
        end case;
    end process;
end architecture;
