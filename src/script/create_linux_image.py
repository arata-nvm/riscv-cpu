import sys

def create_combined_file(linux_bin_path, dtb_path, output_path):
    total_size = 0x400000  # 4MB

    # Read linux.bin
    with open(linux_bin_path, "rb") as f:
        linux_data = f.read()

    # Read default.dtb
    with open(dtb_path, "rb") as f:
        dtb_data = f.read()

    # Ensure the output size is correct
    if len(linux_data) + len(dtb_data) > total_size:
        raise ValueError("The combined size of linux.bin and default.dtb exceeds 4MB")

    # Create output data
    output_data = bytearray(total_size)
    output_data[:len(linux_data)] = linux_data
    output_data[-len(dtb_data):] = dtb_data

    # Write formatted hex output
    with open(output_path, "w") as f:
        for i in range(0, len(output_data)):
            f.write(f"{output_data[i]:02x}\n")

    print(f"Created {output_path} in formatted hex output.")

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python script.py <linux.bin> <default.dtb> <output>")
        sys.exit(1)

    linux_bin = sys.argv[1]
    dtb = sys.argv[2]
    output = sys.argv[3]

    create_combined_file(linux_bin, dtb, output)
