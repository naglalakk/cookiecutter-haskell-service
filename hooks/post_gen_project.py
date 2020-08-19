import os
from shutil import copyfile

package_name = "{{ cookiecutter.package_name }}"
has_license = "{{ cookiecutter.license_file }}"

if has_license == "n":
    os.remove("LICENSE")
