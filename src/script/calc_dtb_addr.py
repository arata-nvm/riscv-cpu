with open('./src/c/linux/default.dtb', 'rb') as f:
    dtb = f.read()
print(hex(0x80400000 - len(dtb)))
