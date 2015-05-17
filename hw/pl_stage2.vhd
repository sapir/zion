library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use defs.ALL;


entity pl_stage2 is
    Port (
        clk             : in std_logic;

        -- inputs from stage 1
        st2in           : in Stage_1_2_Interface;

        -- inputs from hazard control
        reg1_fwd        : in FwdValue;
        reg2_fwd        : in FwdValue;

        -- outputs back to stage 1
        branch_flag     : out std_logic;
        branch_dest     : out MemWordAddr;

        -- outputs to stage 3
        st2out          : out Stage_2_3_Interface;

        -- outputs to IRAM
        iram_en         : out std_logic;
        iram_we         : out lvbit;
        iram_addr       : out MemWordAddr;
        iram_din        : out Instr_Type;

        -- outputs to DRAM
        dram_ena        : out std_logic;
        dram_wea        : out lvbit;
        dram_addra      : out DataByteAddr;
        dram_dina       : out Logic_Byte;
        dram_enb        : out std_logic;
        dram_web        : out lvbit;
        dram_addrb      : out DataByteAddr;
        dram_dinb       : out Logic_Byte;

        -- outputs to I/O register
        io_reg_we       : out lvbit;
        io_reg_inp      : out Logic_Byte);
end pl_stage2;


architecture Behavioral of pl_stage2 is

    COMPONENT alu
    PORT (
        op  : in Alu_Op_Type;
        a   : in Logic_Word;
        b   : in Logic_Word;
        res : out Logic_Word);
    END COMPONENT;

    -- value of $rs and $rt to actually use. may be different from
    -- st2in.value1/2.reg_val due to data hazards and forwarding.
    signal final_reg1_val : Logic_Word;
    signal final_reg2_val : Logic_Word;
    -- inputs for 'main_alu' instance
    signal alu_inp1 : Logic_Word;
    signal alu_inp2 : Logic_Word;

    signal alu_res : Logic_Word;

begin

    final_reg1_val <= reg1_fwd.fwd_val when reg1_fwd.use_fwd = '1'
        else st2in.value1.reg_val;

    final_reg2_val <= reg2_fwd.fwd_val when reg2_fwd.use_fwd = '1'
        else st2in.value2.reg_val;


    alu_inp1 <= final_reg1_val when st2in.value1.use_reg = '1'
        else st2in.value1.imm;

    alu_inp2 <= final_reg2_val when st2in.value2.use_reg = '1'
        else st2in.value2.imm;


    -- copy values from stage 1 (with some overrides)
    st2out.mem_type     <= st2in.mem_type;
    st2out.wr_reg_en    <= st2in.wr_reg_en and not st2in.invalid_flag;
    st2out.wr_reg_src   <= st2in.wr_reg_src;
    st2out.wr_reg_idx   <= st2in.wr_reg_idx;
    st2out.pc_plus_2    <= st2in.pc_plus_2;


    main_alu : alu PORT MAP (
        op  => st2in.alu_op,
        a   => alu_inp1,    -- decided in st2_alu_proc
        b   => alu_inp2,    -- decided in st2_alu_proc
        res => alu_res);


    branch_proc : process(st2in.branch_type, st2in.branch_dest, final_reg2_val,
        st2in.invalid_flag)
    begin
        -- default
        branch_flag <= '0';
        branch_dest <= (others => '-');

        -- ignore branches for invalidated instructions
        if st2in.invalid_flag = '0' then

            case st2in.branch_type is
                when b_always_imm =>
                    branch_flag <= '1';
                    branch_dest <= st2in.branch_dest;

                when b_always_reg =>
                    branch_flag <= '1';
                    branch_dest <= word_to_mem_addr(final_reg2_val);

                when b_eqz =>
                    if final_reg2_val = "0000000000000000" then
                        branch_flag <= '1';
                        branch_dest <= st2in.branch_dest;
                    end if;

                when b_nez =>
                    if final_reg2_val /= "0000000000000000" then
                        branch_flag <= '1';
                        branch_dest <= st2in.branch_dest;
                    end if;

                when others =>
                    -- no branch, use default
            end case;

        end if;
    end process;


    mem_inps_proc : process(alu_res, final_reg2_val,
        st2in.mem_wr_en, st2in.mem_type)

        -- 16-bit address we're reading/writing
        constant mem_addr   : Logic_Word    := alu_res;

        -- value to be written to mem_addr (when writing).
        -- we only ever write $rt, never alu_res
        constant wr_val     : Logic_Word    := final_reg2_val;

        -- hi & lo bytes of wr_val
        constant wr_val_hi  : Logic_Byte    := wr_val(15 downto 8);
        constant wr_val_lo  : Logic_Byte    := wr_val(7 downto 0);

        variable tmp_dram_addr, tmp_dram_addr_plus1 : DataByteAddr;
    begin
        -- set address inputs

        tmp_dram_addr := mem_addr(10 downto 0);
        tmp_dram_addr_plus1 := std_logic_vector(unsigned(tmp_dram_addr) + 1);

        -- dram port a reads/writes hi byte, port b reads/writes lo byte
        if st2in.mem_type = ma_word then
            dram_addra <= tmp_dram_addr;
            dram_addrb <= tmp_dram_addr_plus1;
        else -- ma_byte
            -- only lo byte is accessed. use the regular address (not +1)
            -- for lo byte
            dram_addra <= (others => '-');
            dram_addrb <= tmp_dram_addr;
        end if;

        iram_addr   <= mem_addr(13 downto 1);   -- ignore lsb

        -- set write enable bits by mem_wr_en.
        -- these depend on regular enable bit, set later depending on mem_addr.
        -- note io_reg_we is handled later as it doesn't have a separate
        -- enable bit.
        dram_wea    <= "0";
        dram_web    <= "0";
        iram_we     <= "0";
        if st2in.mem_wr_en = '1' then
            if st2in.mem_type = ma_word then
                dram_wea <= "1";
                dram_web <= "1";
            else -- ma_byte
                -- only write lo byte (port b)
                dram_web <= "1";
            end if;

            iram_we <= "1";
        end if;

        -- set data to write
        dram_dina <= wr_val_hi;
        dram_dinb <= wr_val_lo;
        iram_din <= "00" & wr_val;  -- TODO: can't access top 2 bits
        io_reg_inp <= wr_val_lo;    -- only bytes supported

        -- set enable bits by memory address
        dram_ena    <= '0';
        dram_enb    <= '0';
        iram_en     <= '0';
        io_reg_we   <= "0";
        if mem_addr(15) = '1' then
            -- Memory-mapped IO
            st2out.cur_memobj <= mo_io;

            -- only byte writes are supported
            if st2in.mem_wr_en = '1' and st2in.mem_type = ma_byte
                and mem_addr = iomem_addr_leds then

                io_reg_we <= "1";
            end if;

        elsif mem_addr(14) = '1' then
            -- Instruction memory
            st2out.cur_memobj <= mo_iram;
            iram_en <= '1';

        else
            -- Regular memory
            st2out.cur_memobj <= mo_dram;
            dram_ena <= '1';
            dram_enb <= '1';
        end if;
    end process;


    st2out.alu_res <= alu_res;

end Behavioral;
