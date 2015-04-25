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
	-- replace reads from $zero with 0
	reg1 <= (others => '0') when rd1 = "0000" else regs(to_integer(u4(rd1)));
	reg2 <= (others => '0') when rd2 = "0000" else regs(to_integer(u4(rd2)));

	wr_proc: process(clk, we, wr_idx, wr_data)
	begin
		if rising_edge(clk) then
			-- allow writing to 0, we'll just ignore it on read
			if we = '1' then
				regs(to_integer(u4(wr_idx))) <= wr_data;
			end if;
			
			-- override writes to $zero
			regs(0) <= (others => '0');
		end if;
	end process;
end Behavioral;
