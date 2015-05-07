library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity my_regs is
    Port ( rd1 : in  STD_LOGIC_VECTOR (3 downto 0);
           rd2 : in  STD_LOGIC_VECTOR (3 downto 0);
           reg1 : out  STD_LOGIC_VECTOR (15 downto 0);
           reg2 : out  STD_LOGIC_VECTOR (15 downto 0);
           wr_idx : in  STD_LOGIC_VECTOR (3 downto 0);
           wr_data : in  STD_LOGIC_VECTOR (15 downto 0);
           we : in  STD_LOGIC;
           clk : in  STD_LOGIC);
end my_regs;

architecture Behavioral of my_regs is
    type Regs_Type is array(0 to 15) of Logic_Word;
    signal regs : Regs_Type := (others => (others => '0'));
begin

    rd_proc : process(rd1, rd2, regs, we, wr_idx, wr_data)
    begin
        -- replace reads from $zero with 0.
        if rd1 = "0000" then
            reg1 <= (others => '0');
        -- write-first by returning written value before next clock cycle
        elsif rd1 = wr_idx and we = '1' then
            reg1 <= wr_data;
        else
            reg1 <= regs(to_integer(u4(rd1)));
        end if;

        -- like above for 2nd register
        if rd2 = "0000" then
            reg2 <= (others => '0');
        elsif rd2 = wr_idx and we = '1' then
            reg2 <= wr_data;
        else
            reg2 <= regs(to_integer(u4(rd2)));
        end if;
    end process;

    wr_proc : process(clk)
    begin
        if rising_edge(clk) then
            if we = '1' and wr_idx /= "0000" then
                regs(to_integer(u4(wr_idx))) <= wr_data;
            end if;
        end if;
    end process;

end Behavioral;
