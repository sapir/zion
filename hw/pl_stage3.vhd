library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity pl_stage3 is
    Port (
        clk             : in std_logic;

        -- inputs from stage 2
        st3in           : in Stage_2_3_Interface;

        -- inputs from IRAM/DRAM
        iram_dout       : in Instr_Type;
        dram_douta      : in Logic_Byte;
        dram_doutb      : in Logic_Byte;

        -- outputs to Register File
        wr_reg_en       : out std_logic;    -- whether to write to register file
        wr_reg_idx      : out Reg_Index;    -- which register to write
        wr_reg_data     : out Logic_Word);  -- data to be written to register
end pl_stage3;


architecture Behavioral of pl_stage3 is
begin

    st3_output_proc : process(st3in, dram_douta, dram_doutb, iram_dout)
    begin

        wr_reg_en   <= st3in.wr_reg_en;
        wr_reg_idx  <= st3in.wr_reg_idx;

        case st3in.wr_reg_src is
            when rws_alu =>
                wr_reg_data <= st3in.alu_res;

            when rws_mem =>
                if st3in.cur_memobj = mo_iram then
                    wr_reg_data <= iram_dout(15 downto 0); -- TODO
                else
                    -- assume dram - I/O doesn't support reads
                    if st3in.mem_type = ma_byte then
                        wr_reg_data <= "00000000" & dram_douta;
                    else -- ma_word
                        wr_reg_data <= dram_douta & dram_doutb;
                    end if;
                end if;

            when rws_pc_plus_2 =>
                wr_reg_data <= mem_addr_to_word(st3in.pc_plus_2);

            when others =>
                wr_reg_data <= (others => '-');
        end case;
    end process;

end Behavioral;
