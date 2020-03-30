import os

to_clip = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"


def clip(directory):
  for root, dirs, files in os.walk(directory):
    for file in files:
        if file.endswith('.xml'):
            with open(os.path.join(root, file), 'r+') as f:
                lines = f.readlines()
                if lines[0] == to_clip:
                    to_write = lines[5:]
                    f.seek(0)
                    f.writelines(to_write)
                    

clip("ISA_v82A_A64_xml_00bet3.1")


