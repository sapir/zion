library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity pl_stage3 is
    Port (
        clk             : in std_logic;

        -- inputs from stage 2
        st3in           : in Stage_2_3_Interface;

        -- communication with IRAM
        iram_en         : out std_logic;
        iram_we         : out lvbit;
        iram_addr       : out MemWordAddr;
        iram_din        : out Logic_Word;
        iram_dout       : in Logic_Word;

        -- communication with DRAM
        dram_ena        : out std_logic;
        dram_wea        : out lvbit;
        dram_addra      : out DataByteAddr;
        dram_dina       : out Logic_Byte;
        dram_douta      : in Logic_Byte;

        dram_enb        : out std_logic;
        dram_web        : out lvbit;
        dram_addrb      : out DataByteAddr;
        dram_dinb       : out Logic_Byte;
        dram_doutb      : in Logic_Byte;

        -- communication with Register File
        wr_reg_en       : out std_logic;    -- whether to write to register file
        wr_reg_idx      : out Reg_Index;    -- which register to write
        wr_reg_data     : out Logic_Word;   -- data to be written to register

        -- communication with I/O register
        io_reg_we       : out lvbit;
        io_reg_inp      : out Logic_Byte);
end pl_stage3;


architecture Behavioral of pl_stage3 is

    -- memory object to use for memory read/write operations
    type MemObject_Type is (mo_dram, mo_iram, mo_io);
    -- memory object to use for current operation in stage 3
    signal cur_memobj   : MemObject_Type;

begin

    st3_mem_inps_proc : process(st3in)
        -- 16-bit address we're reading/writing
        constant mem_addr   : Logic_Word    := st3in.alu_res;

        -- value to be written to mem_addr (when writing).
        -- we only ever write $rt, never alu_res
        constant wr_val     : Logic_Word    := st3in.reg2_val;

        -- hi & lo bytes of wr_val
        constant wr_val_hi  : Logic_Byte    := wr_val(15 downto 8);
        constant wr_val_lo  : Logic_Byte    := wr_val(7 downto 0);

        variable tmp_dram_addra, tmp_dram_addrb : DataByteAddr;
    begin
        -- default values:

        tmp_dram_addra := mem_addr(10 downto 0);
        tmp_dram_addrb := std_logic_vector(unsigned(tmp_dram_addra) + 1);

        dram_ena        <= '0';
        dram_wea        <= "0";
        dram_addra      <= tmp_dram_addra;
        dram_dina       <= (others => '-');
        dram_enb        <= '0';
        dram_web        <= "0";
        dram_addrb      <= tmp_dram_addrb;
        dram_dinb       <= (others => '-');
        iram_en         <= '0';
        iram_we         <= "0";
        iram_addr       <= mem_addr(13 downto 1);   -- ignore lsb
        iram_din        <= (others => '-');
        io_reg_we       <= "0";
        io_reg_inp      <= (others => '-');


        if mem_addr(15) = '1' then
            -- Memory-mapped IO
            cur_memobj <= mo_io;

            -- only byte writes are supported
            if mem_addr = iomem_addr_leds and st3in.wr_type = wr_reg_to_memb then
                io_reg_we <= "1";
                io_reg_inp <= wr_val_lo;
            end if;

        elsif mem_addr(14) = '1' then
            -- Instruction memory
            cur_memobj <= mo_iram;

            -- only word read/writes are supported
            case st3in.wr_type is
                when wr_reg_to_memw =>
                    iram_en    <= '1';
                    iram_we    <= "1";

                when wr_memw_to_reg =>
                    iram_en    <= '1';

                when others =>
                    -- use defaults
            end case;

        else
            -- Regular memory
            cur_memobj <= mo_dram;

            case st3in.wr_type is
                when wr_reg_to_memb =>
                    dram_ena    <= '1';
                    dram_wea    <= "1";
                    dram_dina   <= wr_val_lo;

                when wr_reg_to_memw =>
                    dram_ena    <= '1';
                    dram_wea    <= "1";
                    dram_dina   <= wr_val_hi;
                    dram_enb    <= '1';
                    dram_web    <= "1";
                    dram_dinb   <= wr_val_lo;

                when wr_memb_to_reg =>
                    dram_ena    <= '1';

                when wr_memw_to_reg =>
                    dram_ena    <= '1';
                    dram_enb    <= '1';

                when others =>
                    -- use defaults
            end case;
        end if;
    end process;


    st3_output_proc : process(st3in, dram_douta, dram_doutb, iram_dout,
        cur_memobj)
    begin

        wr_reg_idx   <= st3in.wr_reg_idx;

        case st3in.wr_type is
            when wr_alu_to_reg =>
                wr_reg_data  <= st3in.alu_res;
                wr_reg_en    <= '1';

            when wr_memb_to_reg =>
                wr_reg_data  <= "00000000" & dram_douta;
                wr_reg_en    <= '1';

            when wr_memw_to_reg =>
                if cur_memobj = mo_iram then
                    wr_reg_data <= iram_dout;
                else
                    -- assume dram - I/O doesn't support reads
                    wr_reg_data <= dram_douta & dram_doutb;
                end if;

                wr_reg_en    <= '1';

            when wr_pc_plus_2_to_ra =>
                wr_reg_data  <= "000" & st3in.pc_plus_2;
                wr_reg_en    <= '1';

            when others =>
                wr_reg_data  <= (others => '-');
                wr_reg_en    <= '0';
        end case;
    end process;

end Behavioral;
