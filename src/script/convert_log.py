with open('results/sw.txt', 'r') as f:
  print('Kanata\t0004')
  print('C=\t0')
  inst_ids = set()
  for line in f:
    if line.startswith('if_inst_id'):
      if_inst_id = int(line.split(':')[1], 16)
    elif line.startswith('if_reg_pc'):
      if_reg_pc = int(line.split(':')[1], 16)
    elif line.startswith('if_inst'):
      if_inst = int(line.split(':')[1], 16)
    elif line.startswith('if_stall_flg'):
      if_stall_flg = int(line.split(':')[1], 16)

    elif line.startswith('id_inst_id'):
      id_inst_id = int(line.split(':')[1], 16)
    elif line.startswith('id_reg_pc'):
      id_reg_pc = int(line.split(':')[1], 16)
    elif line.startswith('id_reg_inst'):
      id_reg_inst = int(line.split(':')[1], 16)
    elif line.startswith('id_inst'):
      id_inst = int(line.split(':')[1], 16)
    elif line.startswith('id_rs1_data'):
      id_rs1_data = int(line.split(':')[1], 16)
    elif line.startswith('id_rs2_data'):
      id_rs2_data = int(line.split(':')[1], 16)

    elif line.startswith('exe_inst_id'):
      exe_inst_id = int(line.split(':')[1], 16)
    elif line.startswith('exe_reg_pc'):
      exe_reg_pc = int(line.split(':')[1], 16)
    elif line.startswith('exe_reg_op1_data'):
      exe_reg_op1_data = int(line.split(':')[1], 16)
    elif line.startswith('exe_reg_op2_data'):
      exe_reg_op2_data = int(line.split(':')[1], 16)
    elif line.startswith('exe_alu_out'):
      exe_alu_out = int(line.split(':')[1], 16)

    elif line.startswith('mem_inst_id'):
      mem_inst_id = int(line.split(':')[1], 16)
    elif line.startswith('mem_reg_pc'):
      mem_reg_pc = int(line.split(':')[1], 16)
    elif line.startswith('mem_wb_data'):
      mem_wb_data = int(line.split(':')[1], 16)

    elif line.startswith('wb_inst_id'):
      wb_inst_id = int(line.split(':')[1], 16)
    elif line.startswith('wb_reg_wb_data'):
      wb_reg_wb_data = int(line.split(':')[1], 16)

    elif line.startswith('-----'):
      if if_inst_id != 0xffffffff and if_inst_id not in inst_ids:
        inst_ids.add(if_inst_id)
        print(f'I\t{if_inst_id}\t{if_inst_id}\t0')
        print(f'S\t{if_inst_id}\t0\tIf')
        print(f'L\t{if_inst_id}\t0\t{if_reg_pc:08x}: {if_inst:08x}')
        print(f'L\t{if_inst_id}\t2\tstall_flg: {if_stall_flg}')
      if id_inst_id != 0xffffffff:
        print(f'S\t{id_inst_id}\t0\tId')
        print(f'L\t{id_inst_id}\t2\trs1_data: {id_rs1_data:08x}, rs2_data: {id_rs2_data:08x}')
      if id_inst_id == 0xffffffff and id_inst == 0x00000013:
        print(f'R\t{if_inst_id-1}\t0\t1')
      if exe_inst_id != 0xffffffff:
        print(f'S\t{exe_inst_id}\t0\tEx')
        print(f'L\t{exe_inst_id}\t2\top1_data: {exe_reg_op1_data:08x}, op2_data: {exe_reg_op2_data:08x}, alu_out: {exe_alu_out:08x}')
      if mem_inst_id != 0xffffffff:
        print(f'S\t{mem_inst_id}\t0\tMe')
        print(f'L\t{mem_inst_id}\t2\twb_data: {mem_wb_data:08x}')
      if wb_inst_id != 0xffffffff:
        print(f'S\t{wb_inst_id}\t0\tWb')
        print(f'L\t{wb_inst_id}\t2\twb_data: {wb_reg_wb_data:08x}')
      print('C\t1')
      if wb_inst_id != 0xffffffff:
        print(f'R\t{wb_inst_id}\t0\t0')
