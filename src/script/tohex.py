import sys

def main(input_path, output_path):
    with open(input_path, "rb") as f:
        data = f.read()

    with open(output_path, "w") as f:
        for i in range(0, len(data)):
            f.write(f"{data[i]:02x}\n")

    print(f"Created {output_path} in formatted hex output.")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python tohex.py <input_file> <output_file>")
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]

    main(input_path, output_path)
