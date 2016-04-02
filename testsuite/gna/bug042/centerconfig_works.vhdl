library ieee;
use ieee.std_logic_1164.all;

entity CenterConfig is
    generic (
        -- Width of S_AXI data bus
        C_S_AXI_DATA_WIDTH  : integer   := 32;
        -- Width of S_AXI address bus
        C_S_AXI_ADDR_WIDTH  : integer   := 4
    );
    port (
        center_height: out std_logic_vector(C_S_AXI_DATA_WIDTH - 1 downto 0);
        center_width: out std_logic_vector(C_S_AXI_DATA_WIDTH - 1 downto 0)
    );
end entity CenterConfig;

architecture foo of centerconfig is
    
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity instance is
    -- generic (
    --        CENTERHEIGHT: integer := 16;
    --        CENTERWIDTH:  integer := 16
    -- );
end entity;

architecture fum of instance is
    constant CENTERHEIGHT: integer := 32;   -- 16;
    constant  CENTERWIDTH:  integer := 32;  -- 16;
    constant C_S_AXI_DATA_WIDTH: integer   := 32;
    constant C_S_AXI_ADDR_WIDTH: integer   := 4;
    component centerconfig is
        generic (
            C_S_AXI_DATA_WIDTH  : integer   := 32;
            C_S_AXI_ADDR_WIDTH  : integer   := 4
        );
        port (
            center_height: 
                out std_logic_vector(C_S_AXI_DATA_WIDTH - 1 downto 0);
            center_width: 
                out std_logic_vector(C_S_AXI_DATA_WIDTH - 1 downto 0)
        );
    end component;
    signal std_center_height: std_logic_vector (CENTERHEIGHT - 1  downto 0);
    signal std_center_width: std_logic_vector (CENTERWIDTH - 1 downto 0);
begin
    
Config: 
    CenterConfig 
        generic map (
            C_S_AXI_DATA_WIDTH => C_S_AXI_DATA_WIDTH,
            C_S_AXI_ADDR_WIDTH => C_S_AXI_ADDR_WIDTH
        )
        port map (
            --center_height(std_center_height'range) => std_center_height,
            center_height(std_center_height'LEFT downto 
                          std_center_height'RIGHT) => std_center_height,
            -- center_height(C_S_AXI_DATA_WIDTH-1 downto std_center_height'length) => open, 
            -- not working, not elegant

            -- center_width(std_center_width'range) => std_center_width
            center_width(std_center_width'LEFT downto 
                         std_center_width'RIGHT) => std_center_width
        );
    
end architecture;