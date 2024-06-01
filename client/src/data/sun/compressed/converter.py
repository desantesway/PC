#import os
#from PIL import Image

#def compress_tga_files_to_png(directory):
    # List all files in the directory
#    files = os.listdir(directory)
    
    # Filter for TGA files
#    tga_files = [file for file in files if file.lower().endswith('.tga')]
    
#    for tga_file in tga_files:
#        # Open an image file
#        with Image.open(os.path.join(directory, tga_file)) as img:
#            # Compress and save it as PNG
#            output_file = os.path.join(directory, os.path.splitext(tga_file)[0] + '_compressed.png')
#            img.save(output_file, format='PNG', optimize=True)
#            
#            print(f"Compressed {tga_file} to {output_file}")#
#
#if __name__ == "__main__":
#    current_directory = os.path.dirname(os.path.abspath(__file__))
#    compress_tga_files_to_png(".")

import os

def rename_files(directory):
    # List all files in the directory
    files = os.listdir(directory)
    
    for file in files:
        # Check if the file name contains "_compressed_compressed" and is a PNG file
        if file.lower().endswith('.png') and '_compressed_compressed' in file:
            # Create the new file name by replacing "_compressed_compressed" with an empty string
            new_file = file.replace('_compressed_compressed', '')
            # Get the full paths for the old and new file names
            old_file_path = os.path.join(directory, file)
            new_file_path = os.path.join(directory, new_file)
            # Rename the file
            os.rename(old_file_path, new_file_path)
            print(f"Renamed {file} to {new_file}")

if __name__ == "__main__":
    current_directory = os.path.dirname(os.path.abspath(__file__))
    rename_files(".")
