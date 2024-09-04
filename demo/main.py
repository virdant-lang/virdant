import serial
import time

ser = serial.Serial('/dev/ttyACM0', 9600, timeout=1)

try:
    while True:
        if ser.in_waiting > 0:
            data = ser.read(1)
            hex_data = data.hex()
            for i in range(0, len(hex_data), 32):  # 32 hex digits represent 16 bytes
                row = hex_data[i:i + 32]  # Extract 32 hex digits (16 bytes)
                print(' '.join(row[j:j + 2] for j in range(0, len(row), 2)))  # Format into pairs

except KeyboardInterrupt:
    print()
finally:
    ser.close()
