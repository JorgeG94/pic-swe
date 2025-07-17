import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import glob
import re

# Match files like: height_step_10.csv
def extract_step_number(filename):
    match = re.search(r"height_step_(\d+)\.csv", filename)
    return int(match.group(1)) if match else -1

# Get and sort all CSV files
csv_files = sorted(glob.glob("height_step_*.csv"), key=extract_step_number)

# Load the first one to initialize plot
initial_data = np.loadtxt(csv_files[0], delimiter=",")
fig, ax = plt.subplots()
im = ax.imshow(initial_data, origin="lower", cmap="Blues", animated=True)
cbar = fig.colorbar(im)
cbar.set_label("Water Height")

def update(frame):
    data = np.loadtxt(csv_files[frame], delimiter=",")
    im.set_array(data)
    ax.set_title(f"Step {extract_step_number(csv_files[frame])}")
    return [im]

ani = animation.FuncAnimation(fig, update, frames=len(csv_files), blit=True, interval=200)
plt.show()
